library(pacman)

pacman::p_load(tidyverse, MatchIt, sf, lme4, here, cobalt, scales, marginaleffects, data.table)

# METADATA ---------------------------------
# note that the first 15 columns came from the data source (IMAFLORA) and thus have names in portuguese
# from these columns, we will only use "id", "nm_uf" and "area_final" 
# 'tamanho' may also come in handy --> categorical variable for the size of private properties 

# columns: 
# id: unique identifier
# area_final: size of the territory
# nm_uf: state 
# temp: mean annual temperature (source: WorldClim) (units celsius)
# precip: mean annual precipitation (source: WorldClim)
# elevation: mean elevation (source: SRTM Digital Elevation Data Version 4)
# slope: mean slope (source: SRTM Digital Elevation Data Version 4)
# dist_roads: mean distance to roads (IBGE shapefiles)
# dist_rivers: mean distance to rivers (IBGE shapefiles)
# dist_cities: mean travel distance (Nelson, 2008) https://aquaknow.jrc.ec.europa.eu/en/content/accessibility-map%C2%A0-travel-time-major-cities 
# pop_dens: mean population density (source: IBGE)
# persistent_rest: total area of persistent regeneration in 2021 (hectares)
# ephemeral_rest: total area of ephemeral restoration between 1985-2021 (hectares) 
# note: definition of forest cover includes savanna, native forest, restinga and mangroves. silviculture was not calculated. 

# land_type2: assentamento, privadas, publicas, quilombola, terr_com, ti, uc

# within columns: 
# comunitarias: communal lands (four categories: indigenous lands, quilombos, assentamentos, and other communal lands)
# privadas: private lands
# uc: conservation unit (sustainable use or full protection)
# publicas: public lands

### read in data ---------------------------------

lands_df <- st_read(dsn = here("import/input/v2/all_lands_v2.shp")) 
colnames(lands_df)
all_lands <- lands_df %>% 
  rename(subclasse="subclss", area_orig="area_rg",area_final="are_fnl",elevation="elevatn",
         dist_roads="dst_rds",dist_rivers="dst_rvr",dist_cities="dst_cts",pop_dens="pop_dns",
         persistent_rest="prsstn_",ephemeral_rest="ephmrl_",forest1985="frs1985",
         land_type1="lnd_ty1",land_type2="lnd_ty2") %>% 
  mutate(land_type2=factor(land_type2))
rm(lands_df)

# create nine different model variations to run the matching analysis:
# 1) Indigenous and Quilombola (IQ) vs. private lands (priv)
# 2) IQ vs. public lands (pub)
# 3) IQ vs. protected areas (pa)
# 4) Assentamento (AS) vs. priv
# 5) AS vs pub
# 6) AS vs pa
# 7) priv vs pub
# 8) priv vs pa
# 9) pub vs pa

# 1) Indigenous and Quilombola (IQ) vs. private lands (priv)
com_iq_priv <- all_lands %>% 
  filter(land_type2=="quilombola" | land_type2 == "ti" | land_type2=="privadas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "quilombola"=1, "ti"=1, "privadas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 2) IQ vs. public lands (pub)
com_iq_pub <- all_lands %>% 
  filter(land_type2=="quilombola" | land_type2 == "ti" | land_type2=="publicas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "quilombola"=1, "ti"=1, "publicas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 3) IQ vs. protected areas (pa)
com_iq_pa <- all_lands %>% 
  filter(land_type2=="quilombola" | land_type2 == "ti" | land_type2=="uc") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "quilombola"=1, "ti"=1, "uc"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 4) Assentamento (AS) vs. priv
com_as_priv <- all_lands %>% 
  filter(land_type2=="assentamento" | land_type2=="privadas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "assentamento"=1, "privadas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 5) AS vs pub
com_as_pub <- all_lands %>% 
  filter(land_type2=="assentamento" | land_type2=="publicas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "assentamento"=1, "publicas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 6) AS vs pa
com_as_pa <- all_lands %>% 
  filter(land_type2=="assentamento" | land_type2=="uc") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "assentamento"=1, "uc"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 7) pub vs. priv
com_pub_priv <- all_lands %>% 
  filter(land_type2=="publicas" | land_type2=="privadas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "publicas"=1, "privadas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 


# 8) priv vs pa
com_pa_priv <- all_lands %>% 
  filter(land_type2=="uc" | land_type2=="privadas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "uc"=1, "privadas"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 

# 9) pub vs pa
com_pub_pa <- all_lands %>% 
  filter(land_type2=="uc" | land_type2=="publicas") %>% 
  # create new binary variable so that MatchIt knows which is treatment and control variable
  mutate(treat = recode(land_type2, "publicas"=1, "uc"=0)) %>% 
  drop_na(area_final, forest1985, temp, precip, elevation, slope, pop_dens,
          dist_roads, dist_rivers, dist_cities) 


write_normalized <- function(df, fn) {
  df_used <- df %>% as.data.frame() %>%
    select(land_type2, treat, area_final, forest1985, temp, precip, elevation, slope,
                                          pop_dens, dist_roads, dist_rivers, dist_cities,
           ephemeral_rest, persistent_rest)
    
  df_used_normalized <- df_used %>%
    mutate_at(vars(-land_type2, -treat, -ephemeral_rest, -persistent_rest), ~ as.vector(scale(.x, center=FALSE, scale=TRUE))) %>%
    mutate(id = row_number()) 
  
  write_csv(df_used_normalized, here(paste("import/output/", fn, ".csv", sep="")))
}

capture_names <- function(...) {
  list_names <- sapply(as.list(match.call())[-1], deparse)
  list_values <- list(...)
  names(list_values) <- list_names
  return(list_values)
}

dfs <- capture_names(com_iq_priv, com_iq_pub, com_iq_pa, com_as_priv, com_as_pub, com_as_pa, com_pub_priv, com_pa_priv, com_pub_pa)

for (name in names(dfs)) {
  print(paste("Writing...", name))
  write_normalized(dfs[[name]], name)
}
