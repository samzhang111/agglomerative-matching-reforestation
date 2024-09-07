library(pacman)

pacman::p_load(tidyverse, MatchIt, sf, lme4, here, cobalt, scales, marginaleffects, data.table, lwgeom, units)

lands_df <- st_read(dsn = here("import/input/v2/all_lands_v2.shp")) 
colnames(lands_df)
all_lands_v2 <- lands_df %>% 
  rename(subclasse="subclss", area_orig="area_rg",area_final="are_fnl",elevation="elevatn",
         dist_roads="dst_rds",dist_rivers="dst_rvr",dist_cities="dst_cts",pop_dens="pop_dns",
         persistent_rest="prsstn_",ephemeral_rest="ephmrl_",forest1985="frs1985",
         land_type1="lnd_ty1",land_type2="lnd_ty2") %>% 
  mutate(land_type2=factor(land_type2))
rm(lands_df)

sf_use_s2(FALSE)
lands_df <- st_read(dsn = here("import/input/v3/all_lands_v3.shp")) 
colnames(lands_df)
all_lands_v3 <- lands_df %>% 
  rename(subclasse="subclss", area_orig="area_rg",area_final="are_fnl",elevation="elevatn",
         dist_roads="dst_rds",dist_rivers="dst_rvr",dist_cities="dst_cts",pop_dens="pop_dns",
         persistent_rest="prsstn_",ephemeral_rest="ephmrl_",forest1985="frs1985",
         forest2004="frs2004",forest2022="frs2022",pasture1985="pst1985",pasture2004="pst2004",pasture2022="pst2022",
         land_type1="lnd_ty1",land_type2="lnd_ty2") %>% 
  select(-"fr1985_",-"fr2004_",-"fr2022_",-"ps1985_",-"ps2004_",-"ps2022_",-"eph_prc",-"prs_prc") %>% 
  mutate(land_type2=factor(land_type2),
         area_final_original=area_final,
         area_final = set_units(st_area(geometry), value=ha),
         forest1985_perc = forest1985/area_final)
rm(lands_df)

superjoin <- read_csv(here("matching/scratch/iq_super_join.csv")) 
missing_joined <- superjoin %>% filter(is.na(land_type2))
nrow(missing_joined)
head(missing_joined)

