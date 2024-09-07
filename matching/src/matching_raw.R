library(pacman)

pacman::p_load(tidyverse, MatchIt, optmatch, here, cobalt, scales, marginaleffects, data.table, sandwich)

do_matching <- function(df, condition, prefix, discard="none", method="full") {
  # This function does all the matching for a given condition.
  # It writes the diagnostic plots and the results file to disk.

  match_obj <- matchit(treat ~ area_final + forest1985_perc + temp + precip + elevation + slope + 
                              pop_dens + dist_roads + dist_rivers + dist_cities, 
                            data = df, 
                            method = method,
                            estimand = "ATT",
                       discard=discard,
                            k2k = TRUE, k2k.method = "mahalanobis")

  ctrl <- df %>% filter(treat == 0)
  mod <- lm(persistent_rest ~ area_final + forest1985_perc + temp + precip + elevation + slope + pop_dens + dist_roads + dist_rivers + dist_cities, data = ctrl)
  prog_score <- predict(mod, df)

  output_dir <- here(paste("matching/output/", prefix, sep=""))
  dir.create(output_dir, recursive=TRUE)

  matched_data <- as.data.table(match.data(match_obj))

  lm_per <- lm(persistent_rest ~ treat *( area_final + forest1985 + temp + precip + elevation + slope + pop_dens +
                           dist_roads + dist_rivers + dist_cities), 
                         data = matched_data)

  lm_eph <- lm(ephemeral_rest ~ treat * ( area_final + forest1985 + temp + precip + elevation + slope + pop_dens +
                        dist_roads + dist_rivers + dist_cities), 
                        data = matched_data)

  results_per <- avg_comparisons(lm_per, variables = "treat",
                  vcov = ~subclass,
                  newdata = subset(matched_data, treat == 1),
                  wts = "weights")

  results_eph <- avg_comparisons(lm_eph, variables = "treat",
                  vcov = ~subclass,
                  newdata = subset(matched_data, treat == 1),
                  wts = "weights")

  results_df_rematched <- bind_rows(list(
    persistent = as_tibble(results_per),
    ephemeral = as_tibble(results_eph)
  ), .id = 'outcome')

  results_df_rematched$condition = condition
  results_df_rematched$rematched = TRUE


  lm_per_direct <- lm(persistent_rest ~ treat *( area_final + forest1985 + temp + precip + elevation + slope + pop_dens +
                           dist_roads + dist_rivers + dist_cities), 
                         data = df)

  lm_eph_direct <- lm(ephemeral_rest ~ treat * ( area_final + forest1985 + temp + precip + elevation + slope + pop_dens +
                        dist_roads + dist_rivers + dist_cities), 
                        data = df)

  results_per_direct <- avg_comparisons(lm_per_direct, variables = "treat",
                  newdata = subset(df, treat == 1))

  results_eph_direct <- avg_comparisons(lm_eph_direct, variables = "treat",
                  newdata = subset(df, treat == 1))

  results_df_unmatched <- bind_rows(list(
    persistent = as_tibble(results_per_direct),
    ephemeral = as_tibble(results_eph_direct)
  ), .id = 'outcome')

  results_df_unmatched$condition = condition
  results_df_unmatched$rematched = FALSE
  
  results_df = bind_rows(results_df_unmatched, results_df_rematched)

  balance <- rownames_to_column(bal.tab(match_obj, distance=as.data.frame(prog_score), stats=c("m"), un=TRUE)$Balance, var="varname")
  balance$condition = condition
  balance$seed_index = prefix
  
  write_csv(results_df, paste(output_dir, "/", prefix, "_", condition, "_results.csv", sep=""))
  write_csv(balance, paste(output_dir, "/", prefix, "_", condition, "_balance.csv", sep=""))
  list(results=results_df, match_obj=match_obj, balance=balance)
}


conditions = c("i_priv", "as_priv", "pa_priv", "pub_priv", "iq_priv", "q_priv")

for (condition in conditions) {
  fn <- here(paste("import/output/com_", condition, ".csv", sep=""))
  print(fn)
  df_raw <- read_csv(fn)
  raw_match <- do_matching(df_raw, paste(condition, "_raw", sep=""), "raw", method="quick")
}


# # For showing how the sample size drops after CEM.
# # Leaving this out because it depends on the bin configurations, and don't
# # feel like that's a fruitful argument to get into.
# df_iq_priv_raw <- read_csv(here("import/output/com_iq_priv.csv"))
# iq_priv_raw <- do_matching(df_iq_priv_raw, "iq_priv_raw", "raw", method="quick")
# 
# cem_iq_priv_raw <- matchit(treat ~ area_final + forest1985_perc + temp + precip + elevation + slope + 
#                             pop_dens + dist_roads + dist_rivers + dist_cities, 
#                           data = df_iq_priv_raw, 
#                           method = "cem",
#                           estimand = "ATT")
# 
# # Number of treated units
# sum(cem_iq_priv_raw$treat)
# # Number of treated units after CEM
# sum(cem_iq_priv_raw$weights[cem_iq_priv_raw$treat == 1] > 0)
