library(pacman)

pacman::p_load(tidyverse, here)

df_balance <- read_csv(here("summarize/output/balances.csv")) %>%
  mutate(seed=as.integer(sub("_10000$", "", seed_index))) %>%
  filter(seed < 500 & condition != "iq_priv")

df_results <- read_csv(here("summarize/output/results.csv")) %>%
  filter(seed < 500 & condition != "iq_priv")

# Average across all the seeds
summary_averages <- df_results %>% group_by(condition, outcome, rematched, drop) %>%
  summarize(avg_estimate = mean(estimate),
            sd_estimate = sd(estimate),
            prop_significant = sum(p.value < 0.05) / n()) %>%
  mutate(condition = case_when(
    condition == "iq_priv_weighted_drop" ~ "iq_priv",
    TRUE ~ condition
  )) %>%
  mutate(condition = case_when(
    condition == "pa_priv_weighted_drop" ~ "pa_priv",
    TRUE ~ condition
  ))

summary_averages
write_csv(summary_averages, here("visualize/output/summary_averages.csv"))

# Difference by prognostic scores
df_balance %>%
  filter(varname == "prog_score") %>%
  group_by(condition) %>%
  summarize(min_score = min(abs(Diff.Un)))

# Average differences in means
balance_means <- df_balance %>%
  group_by(varname, condition) %>%
  summarize(pre_mean = mean(Diff.Un),
            pre_sd = sd(Diff.Un),
            post_mean = mean(Diff.Adj),
            post_sd = sd(Diff.Adj))

# Minimax differences in means
balance_by_seed <- df_balance %>% filter(varname != "distance") %>%
  group_by(condition, seed_index) %>%
  summarize(highest_diff_un = max(abs(Diff.Un)),
            highest_diff_adj = max(abs(Diff.Adj)))

best_seeds_by_condition <- balance_by_seed %>% group_by(condition) %>%
  slice_min(order_by = highest_diff_adj, with_ties = FALSE) %>%
  summarize(best_diff_un = min(highest_diff_un),
            best_diff_adj = first(highest_diff_adj),
            seed_index = first(seed_index), .groups='drop')


df_balance_seed <- df_balance %>%
  inner_join(best_seeds_by_condition, by = c("condition", "seed_index"))

balance_plots <- df_balance_seed %>% 
  mutate(Covariate = factor(varname, 
                            levels=rev(c("distance", "prog_score", "area_final", "forest1985_perc", "temp", "slope", "precip", "pop_dens", "elevation", "dist_roads", "dist_rivers", "dist_cities")),
                            labels=rev(c("Propensity score", "Prognostic score", "Territory size", "Perc. Forest (1985)", "Temp.", "Slope", "Precip.", "Pop. dens.", "Elevation", "Dist. to roads", "Dist. to rivers", "Dist. to cities"))),
         Condition = factor(condition,
                            levels=c("as_priv", "pa_priv", "i_priv", "q_priv", "pub_priv"),
                            labels=c("Agrarian-reform settlements", "Protected areas", "Indigenous lands", "Quilombola territories", "Public lands"))) %>%
  ggplot() +
    geom_point(aes(x=Diff.Un, y=Covariate, color="Diff.Un")) +
    geom_point(aes(x=Diff.Adj, y=Covariate, color="Diff.Adj")) +
    geom_vline(xintercept=c(-.1, 0.1), linetype="dashed") +
    facet_wrap(~ Condition, scales="free_x") +
    xlab("Standardized Mean Difference") +
    ylab("") +
    scale_color_manual(name="", values=c("Diff.Un"="black", "Diff.Adj"="red"), labels=c("Diff.Un"="Before full matching", "Diff.Adj"="After full matching")) +
    guides(color = guide_legend(ncol = 2, reverse = TRUE)) +
    theme(legend.position = "bottom", panel.spacing = unit(1, "lines"))

balance_plots
ggsave(here("visualize/output/balance_plots.pdf"), height=8, width=8)
ggsave(here("visualize/output/balance_plots.png"), height=8, width=8)
