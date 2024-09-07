### load libraries
library(pacman)

pacman::p_load(tidyverse, MatchIt, optmatch, here, cobalt, scales, marginaleffects, data.table, sandwich)

args <- commandArgs(trailingOnly = TRUE)
seed <- args[1]
index_size <- args[2]


read_condition <- function(df_name, index_size, seed) {
  # This function reads in the file corresponding to this condition.
  # If it involves a private land, then it reads in the superlands for the control.
  # Otherwise, it just reads in the data imported from the shapefile.
  
  if (grepl("priv", df_name)) {
    
    name_base <- paste(df_name, "_superlands_", index_size, "_", seed, sep="")
    
    superlands <- read_csv(here(paste("construct_superlands/output/", seed, "/", name_base, ".csv", sep="")), num_threads=1)
    used <- read_csv(here(paste("import/output/", df_name, ".csv", sep="")))
    
    df <- as.data.table(bind_rows(
      used %>% filter(treat == 1),
      superlands %>% mutate(treat = 0)))
  }
  else {
    df <- read_csv(here(paste("import/output/", df_name, ".csv", sep=""))) %>%
      select(-c(land_type2))
  }
  
  df
}

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

df_iq_priv <- read_condition("com_iq_priv", index_size, seed)
#df_iq_pub <- read_condition("com_iq_pub", index_size, seed)
#df_iq_pa <- read_condition("com_iq_pa", index_size, seed)
df_as_priv <- read_condition("com_as_priv", index_size, seed)
#df_as_pub <- read_condition("com_as_pub", index_size, seed)
#df_as_pa <- read_condition("com_as_pa", index_size, seed)

df_pub_priv <- read_condition("com_pub_priv", index_size, seed)
df_pa_priv <- read_condition("com_pa_priv", index_size, seed)
#df_pub_pa <- read_condition("com_pub_pa", index_size, seed)
#df_i_priv <- read_condition("com_i_priv", index_size, seed)
#df_q_priv <- read_condition("com_q_priv", index_size, seed)

prefix <- paste(seed, index_size, sep="_")

iq_priv <- do_matching(df_iq_priv, "iq_priv", prefix)
iq_priv_drop <- do_matching(df_iq_priv, "iq_priv", prefix, "both")
#i_priv <- do_matching(df_i_priv, "i_priv", prefix)
#q_priv <- do_matching(df_q_priv, "q_priv", prefix)

as_priv <- do_matching(df_as_priv, "as_priv", prefix)
as_priv_drop <- do_matching(df_as_priv, "as_priv", prefix, "both")
pub_priv <- do_matching(df_pub_priv, "pub_priv", prefix)
pub_priv_drop <- do_matching(df_pub_priv, "pub_priv", prefix, "both")
pa_priv <- do_matching(df_pa_priv, "pa_priv", prefix)
pa_priv_drop <- do_matching(df_pa_priv, "pa_priv", prefix, "both")


# Write out overall results file of main results
all_vs_priv <- rbind(iq_priv$results, as_priv$results, pa_priv$results)
all_vs_priv$drop = FALSE

all_vs_priv_drop <- rbind(iq_priv_drop$results, as_priv_drop$results, pa_priv_drop$results)
all_vs_priv_drop$drop = TRUE

all_vs_priv_both <- rbind(all_vs_priv, all_vs_priv_drop)

all_balance <- rbind(iq_priv$balance, as_priv$balance, pa_priv$balance)

write_csv(all_vs_priv_both, here(paste("matching/output/", prefix, "/all_results_", prefix, ".csv", sep="")))
write_csv(all_balance, here(paste("matching/output/", prefix, "/all_balance_", prefix, ".csv", sep="")))
