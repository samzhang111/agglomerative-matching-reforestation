library(pacman)
pacman::p_load(tidyverse, sf, here, lwgeom, units, data.table, ggrepel, patchwork, ggbreak, scales, ggtext)

# Turn off spherical geometry
sf_use_s2(FALSE)
lands_df <- st_read(dsn = here("import/input/v3/all_lands_v3.shp")) 

read_condition <- function(df_name, index_size, seed) {
  # This function reads in the file corresponding to this condition.
  # If it involves a private land, then it reads in the superlands for the control.
  # Otherwise, it just reads in the data imported from the shapefile.
  
  if (grepl("priv", df_name)) {
    
    name_base <- paste(df_name, "_superlands_", index_size, "_", seed, sep="")
    
    superlands <- read_csv(here(paste("construct_superlands/output/", seed, "/", name_base, ".csv", sep="")), num_threads=1)
    used <- read_csv(here(paste("import/output/", df_name, ".csv", sep="")), num_threads=1)
    
    df <- as.data.table(bind_rows(
      used %>% filter(treat == 1),
      superlands %>% mutate(treat = 0)))
  }
  else {
    df <- read_csv(here(paste("import/output/", df_name, ".csv", sep="")), num_threads=1) %>%
      select(-c(land_type2))
  }
  
  df
}

index_size = 5000
seed = 2
df_iq_priv <- read_condition("com_iq_priv", index_size, seed)

# from matching/src/write_matches.R
iq_matches <- df_iq_priv %>%
  filter(!is.na(control_ids)) %>%
  mutate(num_private_lands_in_superland = lengths(str_extract_all(control_ids, "\\d+")), # Count the number of IDs
         private_land_id = str_extract_all(control_ids, "\\d+")) %>%
  select(treatment_id, num_private_lands_in_superland, private_land_id) %>%
  unnest(private_land_id) %>%
  mutate(private_land_id = as.integer(private_land_id))

# Find a match 
example_lands <- iq_matches %>%
#  filter(treatment_id == (iq_matches %>% filter(num_private_lands_in_superland == 6))$treatment_id[7])
  filter(treatment_id == (iq_matches %>% filter(num_private_lands_in_superland == 5))$treatment_id[21])

## Find the match with the most matches
#example_lands <- iq_matches %>%
#  filter(treatment_id == (iq_matches %>% arrange(desc(num_private_lands_in_superland)))$treatment_id[1])

example_treatment_land <- lands_df %>% filter(id %in% c(example_lands$treatment_id[1]))
example_lands_geo <- lands_df %>% filter(id %in% example_lands$private_land_id)

example_treatment_land_normalized <- df_iq_priv |> filter(id == example_treatment_land$id[[1]])

ggplot(example_treatment_land) +
  geom_sf() +
  theme_void()

ggplot(example_lands_geo) +
  geom_sf() +
  theme_minimal()

# Function to normalize a polygon's coordinates
normalize_polygons <- function(polygons, offsets=list(list(x=0, y=0))) {
  bboxes <- map(polygons, st_bbox)
  
  results <- c()
  
  for (i in seq_along(polygons)) {
    current_bbox <- bboxes[[i]]
    offset <- offsets[[i]]
    polygon <- polygons[[i]]
  
    # Calculate translation to move the bbox's lower-left corner to the origin (0,0)
    x_translation <- -current_bbox$xmin
    y_translation <- -current_bbox$ymin
    
    # Apply translation and scaling
    polygon_translated <- st_geometry(polygon) + c(x_translation + offset$x, y_translation + offset$y)
    
    results[[i]] <- st_geometry(polygon_translated)
  }
  
  combined_results <- do.call(c, results)
  
  # Use some dummy crs
  combined_results <- st_sfc(combined_results, crs=st_crs(4326))
  
  # Update the geometry of the original sf object
  return(combined_results)
}

example_treatment_land <- example_treatment_land %>%
  mutate(
    normalized_geo = normalize_polygons(geometry),
    treatment=1,
    label="T",
    nudge_x=0,
    nudge_y=0
  )

# These are hand-picked values for the example.

x_base <- 0.07
y_base <- 0.01
offsets <- list(
  list(x=x_base, y=y_base + 0.029),
  list(x=x_base + 0.005, y=y_base + 0.026),
  list(x=x_base + 0.01, y=y_base + 0.029),
  list(x=x_base, y=y_base),
  list(x=x_base + 0.005, y=y_base)
)

example_lands_geo_plottable <- example_lands_geo %>%
  mutate(
    normalized_geo = normalize_polygons(geometry, offsets),
    treatment=0,
    label=c("C1", "C2", "C3", "C4", "C5"),
    #label=c("b", "c", "d", "e", "f"),
    nudge_x=c(-0.0035, -0.0035, 0, -0.0045, 0),
    nudge_y=c(0, 0, 0.0025, 0, 0.002)
  )

base_columns = c("normalized_geo", "treatment", "label", "nudge_x", "nudge_y")
plot_columns = c("temp", "precip", "elevatn", "slope", "dst_rds", "dst_rvr", "pop_dns")
all_columns = c(base_columns, plot_columns)
example_plottable <- rbind(
  example_treatment_land %>% select(all_of(all_columns)),
  example_lands_geo_plottable %>% select(all_of(all_columns))
)

# Make map
lands_plot <- ggplot(example_plottable) +
  geom_sf(aes(geometry=normalized_geo, fill=factor(treatment)), alpha=0.5) +
  geom_sf_text(aes(geometry=normalized_geo, label=label), nudge_x=example_plottable$nudge_x, nudge_y=example_plottable$nudge_y) +
  theme_void() +
  scale_fill_manual(labels=c("Private properties", "Treatment land"), values = c("#EE6677", "#66CCEE")) +
  guides(fill = guide_legend(reverse = TRUE, title=NULL, nrow=1)) +
  theme(legend.position=c(0.5, -0.1))
lands_plot

# Make plot of covariates


# Currently, the raw data is unnormalized, but the matched/superland data
# is scaled. We can recover the scaling factors by just dividing across
# two instances of the same land across the two datasets.
# 
# Intuitively, the map will be easier to read in unscaled units

df_for_scaling_factors <- rbind(
  example_treatment_land |> select(all_of(plot_columns)) |>
    as_tibble() |>
    select(-c(geometry)) |>
    mutate(normalized=0),
  example_treatment_land_normalized |>
    rename(elevatn=elevation,dst_rds=dist_roads,dst_rvr=dist_rivers,pop_dns=pop_dens) |>
    select(all_of(plot_columns)) |>
    mutate(normalized=1) |>
    as_tibble()
)

scaling_factors <- df_for_scaling_factors |>
  summarise(across(everything(), ~ .[normalized == 0] / .[normalized == 1])) |>
  select(-normalized)


example_superland_normalized <- df_iq_priv |> 
    rename(elevatn=elevation,dst_rds=dist_roads,dst_rvr=dist_rivers,pop_dns=pop_dens) |>
    filter(treatment_id == example_treatment_land$id[[1]]) |> select(all_of(c(plot_columns))) |>
    mutate(treatment=0,
           label="Agg",
           is_agg=1) |>
    as_tibble() 

example_superland <- example_superland_normalized %>%
  mutate(across(all_of(plot_columns), ~ . * scaling_factors[[cur_column()]]))

example_plottable_superland <- rbind(
  example_plottable |> select(all_of(c(plot_columns, "treatment", "label"))) |>
    mutate(is_agg=treatment) |>
    as_tibble() |>
    select(-c(geometry)),
  example_superland
)

example_plottable_long <- example_plottable_superland |>
  pivot_longer(
    cols=all_of(plot_columns),
    names_to="variable",
    values_to="value"
  ) |>
  as_tibble() |>
  select(treatment, label, is_agg, variable, value)

# This plot isn't actually used, but I'm keeping it in the code because
# it's useful for seeing all the covariates at a glance.
# 
# I've learned through a bunch of trial and error that this approach
# doesn't really extend to get us the desired output we want.
# We end up needing patchwork.

ggplot(example_plottable_long, aes(x = value, y = variable, color=factor(treatment), alpha=factor(is_agg))) +
  geom_point() +
  geom_text_repel(aes(label=label), nudge_y=0.4, direction="y") +
  facet_wrap(~ variable, scales = "free", drop=TRUE, ncol=1) +
  scale_alpha_manual(values = c(0.4, 1)) +
  scale_y_discrete(limits = unique(example_plottable_long$variable)) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  ) 

# Convenience function for patchwork.
plot_variable <- function(varname, labelname) {
  ggplot(example_plottable_long |> filter(variable==varname), aes(x = value, y = variable, color=factor(treatment), alpha=factor(is_agg))) +
    geom_point() +
    geom_text_repel(aes(label=label), nudge_y=0.4, direction="x") +
                   # , arrow=arrow(length=unit(0.05, "inches"), type="closed")) +
    scale_alpha_manual(values = c(0.35, 1)) +
    scale_color_manual(values = c("#EE6677", "#66CCEE")) +
    theme_minimal() +
    ylab(labelname) +
    theme(
#      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position="none"
    ) 
}

# We could plot more covariates but it starts getting messier.
#plot_elevatn <- plot_variable("elevatn", "Elevation (m)")
plot_slope <- plot_variable("slope", "Slope (degrees)")
plot_temp <- plot_variable("temp", "Temp. (°C)")
plot_dens <- plot_variable("pop_dns", "Pop dens. (pop/km²)")
#plot_rds <- plot_variable("dst_rds", "Dist. to roads (m)")

plot_covars <- (plot_temp / plot_slope / plot_dens)
plot_covars

((lands_plot & theme(plot.tag.position=c(0.1, 0.975))| (plot_covars & theme(plot.tag.position=c(-0.1, 0.92))))) + plot_layout(widths = c(1.5, 1), heights=1) + plot_annotation(tag_levels = list(c("A", "B"))) & theme(plot.tag = element_text(size = 16, face = "bold"))
ggsave(here("visualize/output/explanatory_map.pdf"), width=10, height=5)

################################################
# Visualization of covariates by different land tenure
################################################


all_lands <- lands_df %>% 
  rename(subclasse="subclss", area_orig="area_rg",area_final="are_fnl",elevation="elevatn",
         dist_roads="dst_rds",dist_rivers="dst_rvr",dist_cities="dst_cts",pop_dens="pop_dns",
         persistent_rest="prsstn_",ephemeral_rest="ephmrl_",forest1985="frs1985",
         land_type1="lnd_ty1",land_type2="lnd_ty2") %>% 
  filter(!(land_type2 %in% c("terr_com", "publicas"))) %>%
  mutate(land_type2=factor(land_type2),
         prop_forest1985=pmin(1, forest1985/area_orig),
         log_pop_dens = log(pop_dens),
         land_type_printable=recode(land_type2,
                                    "privadas" = "PPs",
                                    "assentamento" = "ARSs",
                                    "quilombola" = "QTs",
                                    "ti" = "ILs",
                                    "uc" = "PAs"
         ))

all_lands$land_type_printable <- factor(all_lands$land_type_printable, 
                                        levels = c("PPs", 
                                                   "ILs", 
                                                   "ARSs", 
                                                   "QTs",
                                                   "PAs"),
                                        ordered = TRUE)


private_lands <- all_lands %>% filter(land_type2=="privadas")
non_private_lands <- all_lands %>% filter(land_type2!="privadas")

non_private_lands_long <- non_private_lands %>%
  pivot_longer(
    cols = c(prop_forest1985, slope, temp, precip, elevation, pop_dens, dist_roads, dist_rivers, dist_cities),
    names_to = "variable",
    values_to = "value"
  ) 

private_lands_long <- private_lands %>%
  sample_n(100000) %>% # sample private lands for plotting
  pivot_longer(
    cols = c(prop_forest1985, slope, temp, precip, elevation, pop_dens, dist_roads, dist_rivers, dist_cities),
    names_to = "variable",
    values_to = "value"
  ) 


lands_long <- rbind(non_private_lands_long, private_lands_long)


plot_forest <- ggplot(all_lands) + geom_density(aes(x=prop_forest1985)) + 
  labs(x="Forest cover in 1985 (%)") + 
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  theme(
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank()
    ) +
  facet_grid(rows="land_type_printable", scales="free_y")

plot_slope <- ggplot(all_lands) + geom_density(aes(x=slope)) + labs(x="Slope (degrees)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_temp <- ggplot(all_lands) + geom_density(aes(x=temp)) + labs(x="Temperature (°C)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_precip <- ggplot(all_lands) + geom_density(aes(x=precip)) + labs(x="Precipitation (mm/year)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_elevation <- ggplot(all_lands) + geom_density(aes(x=elevation)) + labs(x="Elevation (m)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_pop_dens <- ggplot(all_lands) + geom_density(aes(x=log_pop_dens)) + labs(x="Pop. density log(pop/km²)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_dist_roads <- ggplot(all_lands) + geom_density(aes(x=dist_roads)) + labs(x="Distance to roads (km)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_dist_rivers <- ggplot(all_lands) + geom_density(aes(x=dist_rivers)) + labs(x="Distance to rivers (km)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")
plot_dist_cities <- ggplot(all_lands) + geom_density(aes(x=dist_cities)) + labs(x="Distance to cities (km)") + theme(axis.title.y=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + facet_grid(rows="land_type_printable", scales="free_y")




(plot_forest | plot_slope | plot_temp) /
  (plot_precip | plot_elevation | plot_pop_dens)/
  (plot_dist_roads | plot_dist_rivers | plot_dist_cities)
ggsave(here("visualize/output/distributions_by_land_types.pdf"), width=8, height=10)

################################################
# Turning Tables 1 and 2 into figures
################################################

all_lands <- lands_df %>% 
  rename(subclasse="subclss", area_orig="area_rg",area_final="are_fnl",elevation="elevatn",
         dist_roads="dst_rds",dist_rivers="dst_rvr",dist_cities="dst_cts",pop_dens="pop_dns",
         persistent_rest="prsstn_",ephemeral_rest="ephmrl_",forest1985="frs1985",
         land_type1="lnd_ty1",land_type2="lnd_ty2") %>% 
  filter(!(land_type2 %in% c("terr_com", "publicas"))) %>%
  mutate(land_type2=factor(land_type2),
         prop_forest1985=pmin(1, forest1985/area_orig),
         log_pop_dens = log(pop_dens),
         land_type_printable=recode(land_type2,
                                    "privadas" = "PPs",
                                    "assentamento" = "ARSs",
                                    "quilombola" = "QTs",
                                    "ti" = "ILs",
                                    "uc" = "PAs"
         ))

all_lands$land_type_printable <- factor(all_lands$land_type_printable, 
                                        levels = c("PPs", 
                                                   "ILs", 
                                                   "ARSs", 
                                                   "QTs",
                                                   "PAs"),
                                        ordered = TRUE)

land_summary <- all_lands %>%
  as.data.frame() %>%
  group_by(land_type_printable) %>%
  summarise(count = n(),
            log_count = log10(count),
            total_area = sum(area_final, na.rm = TRUE)) %>%
  mutate(log_area = log10(total_area)) %>%
  arrange(desc(count))

plot_counts <- land_summary %>%
  ggplot() +
  geom_bar(aes(x=land_type_printable, y=count), stat="identity") +
  scale_y_break(c(1500, 1990750), scales = "fixed") +
  scale_y_continuous(labels = comma) +
  labs(y = "Number of lands", x = "Land type") +
  theme(
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank()
  )

plot_areas <- land_summary %>%
  ggplot() +
  geom_bar(aes(x=land_type_printable, y=total_area), stat="identity") +
  scale_y_break(c(3000000, 69000000), scales = "fixed") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total land area (ha)", x = "Land type") +
  theme(
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank()
  )

plot_counts | plot_areas
ggsave(here("visualize/output/counts_and_areas_by_land_types.pdf"), width=6, height=3)

# No PP

land_summary_no_pp <- all_lands %>%
  as.data.frame() %>%
  filter(land_type_printable != 'PPs') %>%
  group_by(land_type_printable) %>%
  summarise(count = n(),
            log_count = log10(count),
            total_area = sum(area_final, na.rm = TRUE)) %>%
  mutate(log_area = log10(total_area)) %>%
  arrange(desc(count))

plot_counts_no_pp <- land_summary_no_pp %>%
  ggplot() +
  geom_bar(aes(x=land_type_printable, y=count), stat="identity") +
  scale_y_continuous(labels = comma) +
  labs(y = "Number of lands", x = "Land type") +
  theme(
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank()
  )

plot_areas_no_pp <- land_summary_no_pp %>%
  ggplot() +
  geom_bar(aes(x=land_type_printable, y=total_area), stat="identity") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total land area (ha)", x = "Land type") +
  theme(
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank()
  )

plot_counts_no_pp | plot_areas_no_pp
ggsave(here("visualize/output/counts_and_areas_by_land_types_no_pp.pdf"), width=6, height=3)