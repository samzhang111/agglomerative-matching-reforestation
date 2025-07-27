library(pacman)

pacman::p_load(tidyverse, here, ggtext)


df_balance <- read_csv(here("summarize/output/balances.csv")) %>%
  mutate(seed=as.integer(sub("_10000$", "", seed_index))) %>%
  filter(seed < 500 & condition != "iq_priv")


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

########
# Visualize balances
########

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

################################
# Generate results for table
################################
best_seeds_by_condition$seed <- sub("_.*", "", best_seeds_by_condition$seed_index) %>%
  as.numeric()

df_results <- read_csv(here("summarize/output/results.csv")) %>%
  filter(seed < 500 & condition != "iq_priv") %>%
  mutate(condition_readable = recode(condition,
                                     i_priv="Indigenous\nlands",
                                     as_priv="Agrarian-reform\nsettlements",
                                     q_priv="Quilombola\nterritories",
                                     pa_priv="Protected\nareas"),
         outcome_readable = recode(outcome,
                                   persistent="Long-term\nrestoration gains",
                                   ephemeral="Restoration\nreversals")) %>%
  mutate(
    condition_readable = factor(condition_readable,
                                levels=c(
                                  "Protected\nareas",
                                  "Quilombola\nterritories",
                                  "Agrarian-reform\nsettlements",
                                  "Indigenous\nlands"
                                )),
    outcome_readable = factor(outcome_readable, 
                              levels=c(
                                "Long-term\nrestoration gains",
                                "Restoration\nreversals"
                              )))


y_labels <- c(
  "Protected<br>areas",
  "<i>Quilombola</i><br>territories",
  "Agrarian-reform<br>settlements",
  "Indigenous<br>lands"
)

df_filtered <- df_results %>%
  inner_join(best_seeds_by_condition %>% select(condition, seed), by = c("condition", "seed"))

table_s1_results <- df_filtered %>%
  filter(rematched==TRUE & drop==FALSE & condition != "pub_priv")

print(table_s1_results)

table_s1_results %>%
  mutate(is_significant = p.value < 0.05) %>%
  ggplot() +
  #geom_errorbar(aes(y=condition_readable, xmin = estimate - std.error, xmax = estimate + std.error, color = is_significant), size = 1, width=0) +
  #geom_point(aes(x = estimate, y = condition_readable, color = is_significant, fill = is_significant), size = 2.5, shape=21, alpha=1, stroke=1.25) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "lightblue") +
  geom_pointrange(aes(
    y = condition_readable,
    x = estimate,
    xmin = estimate - std.error,
    xmax = estimate + std.error,
    fill = is_significant,
    color = is_significant
  ),
  shape = 21, size = 0.4, linewidth=1, stroke = 1.25
  ) +
  scale_color_manual(
    values = c("FALSE" = "#8c8f80", "TRUE" = "#eb7f03"), 
    labels = c("FALSE" = "Not statistically significant", "TRUE" = "p < 0.05"),
#    guide = guide_legend(reverse = TRUE), 
    guide = "none"
    ) +
  scale_fill_manual(
    values = c("FALSE" = "white", "TRUE" = "#eb7f03"), 
    labels = c("FALSE" = "Not statistically significant", "TRUE" = "p < 0.05"),
    #guide = guide_legend(reverse = TRUE),
    guide = guide_legend(
      override.aes = list(
        shape = 21,
        size = 0.3,
        stroke = 1.25,
        fill = c("#eb7f03", "white"),
        color = c("#eb7f03", "#8c8f80"),  # border (stroke) = errorbar color
        linetype = c("solid", "solid"),  # force line appearance in key
        linewidth = 1
      ),
      title = NULL,
      reverse = TRUE
    )
    ) +
  scale_y_discrete(labels = y_labels) +
  facet_wrap(outcome_readable ~ ., scale="fixed") +
  labs(x = "Amount of restoration (ha)",
       y = NULL,
       color = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_markdown(),
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    #panel.border = element_blank(),
    axis.line = element_line(),             # Enable axis lines
    axis.line.y.right = element_blank(),  
    axis.line.y.left = element_blank(),    
    axis.line.x.top = element_blank(),       # Remove top spine
    axis.line.x.bottom = element_blank()       # Remove top spine
  ) 

ggsave(here("visualize/output/main_results.pdf"), width=6, height=4)




####
# Average across all the seeds (not currently used)
####
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

print(summary_averages)
write_csv(summary_averages, here("visualize/output/summary_averages.csv"))

# Visualize averages:

summary_averages %>%
  filter(rematched==TRUE & drop==FALSE) %>%
  mutate(is_significant = prop_significant > 0.5) %>%
  ggplot() +
  geom_point(aes(x = avg_estimate, y = condition, color = is_significant), size = 3) +
  geom_errorbar(aes(y=condition, xmin = avg_estimate - 2*sd_estimate, xmax = avg_estimate + 2*sd_estimate, color = is_significant), width = 0.2) +
#  scale_color_manual(values = c("iq_priv" = "blue", "pa_priv" = "red", "iq_priv_weighted_drop" = "blue", "pa_priv_weighted_drop" = "red")) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  facet_wrap(outcome ~ ., scale="free") +
  labs(title = "Average Estimates with 1 SD Error Bars",
       x = "Outcome",
       y = "Average Estimate",
       color = "") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) 
