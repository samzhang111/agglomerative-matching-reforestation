library(pacman)

pacman::p_load(tidyverse, here)

### Balance of raw matching
balance_as_priv_raw <- read_csv(here("matching/output/raw/raw_as_priv_raw_balance.csv"))
balance_i_priv_raw <- read_csv(here("matching/output/raw/raw_i_priv_raw_balance.csv"))
balance_raw <- bind_rows(balance_as_priv_raw, balance_i_priv_raw)

balance_plots_raw <- balance_raw %>% 
  mutate(Covariate = factor(varname, 
                            levels=rev(c("distance", "prog_score", "area_final", "forest1985_perc", "temp", "slope", "precip", "pop_dens", "elevation", "dist_roads", "dist_rivers", "dist_cities")),
                            labels=rev(c("Propensity score", "Prognostic score", "Territory size", "Perc. Forest (1985)", "Temp.", "Slope", "Precip.", "Pop. dens.", "Elevation", "Dist. to roads", "Dist. to rivers", "Dist. to cities"))),
         Condition = factor(condition,
                            levels=c("i_priv_raw", "as_priv_raw"),
                            labels=c("Indigenous lands", "Agrarian-reform settlements"))) %>%
  ggplot() +
    geom_point(aes(x=Diff.Un, y=Covariate, color="Diff.Un")) +
    geom_point(aes(x=Diff.Adj, y=Covariate, color="Diff.Adj")) +
    geom_vline(xintercept=c(-.1, 0.1), linetype="dashed") +
    facet_wrap(~ Condition, scales="free_x") +
    xlab("Standardized Mean Difference") +
    ylab("") +
    scale_color_manual(name="", values=c("Diff.Un"="black", "Diff.Adj"="red"), labels=c("Diff.Un"="Unadjusted", "Diff.Adj"="Naive (non-agglomerative) matching")) +
    guides(color = guide_legend(ncol = 2, reverse = TRUE)) +
    theme(legend.position = "bottom", panel.spacing = unit(1, "lines"))

balance_plots_raw
ggsave(here("visualize/output/balance_plots_raw.pdf"), height=4, width=7)
ggsave(here("visualize/output/balance_plots_raw.png"), height=4, width=7)

