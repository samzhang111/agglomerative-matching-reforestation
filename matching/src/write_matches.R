
# Write out matches (not strictly dependent on matching step, so putting it in cache)
as_matches <- (df_as_priv %>%
    filter(!is.na(control_ids)) %>%
    mutate(numbers = str_extract_all(control_ids, "\\d+")) %>%
    unnest(numbers) %>%
    mutate(numbers = as.integer(numbers)))$numbers

as_matches <- df_as_priv %>%
  filter(!is.na(control_ids)) %>%
  mutate(num_private_lands_in_superland = lengths(str_extract_all(control_ids, "\\d+")), # Count the number of IDs
         private_land_id = str_extract_all(control_ids, "\\d+")) %>%
  select(treatment_id, num_private_lands_in_superland, private_land_id) %>%
  unnest(private_land_id) %>%
  mutate(private_land_id = as.integer(private_land_id))

iq_matches <- df_iq_priv %>%
  filter(!is.na(control_ids)) %>%
  mutate(num_private_lands_in_superland = lengths(str_extract_all(control_ids, "\\d+")), # Count the number of IDs
         private_land_id = str_extract_all(control_ids, "\\d+")) %>%
  select(treatment_id, num_private_lands_in_superland, private_land_id) %>%
  unnest(private_land_id) %>%
  mutate(private_land_id = as.integer(private_land_id))

pa_matches <- df_pa_priv %>%
  filter(!is.na(control_ids)) %>%
  mutate(num_private_lands_in_superland = lengths(str_extract_all(control_ids, "\\d+")), # Count the number of IDs
         private_land_id = str_extract_all(control_ids, "\\d+")) %>%
  select(treatment_id, num_private_lands_in_superland, private_land_id) %>%
  unnest(private_land_id) %>%
  mutate(private_land_id = as.integer(private_land_id))

pub_matches <- df_pub_priv %>%
  filter(!is.na(control_ids)) %>%
  mutate(num_private_lands_in_superland = lengths(str_extract_all(control_ids, "\\d+")), # Count the number of IDs
         private_land_id = str_extract_all(control_ids, "\\d+")) %>%
  select(treatment_id, num_private_lands_in_superland, private_land_id) %>%
  unnest(private_land_id) %>%
  mutate(private_land_id = as.integer(private_land_id))


write_csv(as_matches, here("matching/cache/as_superlands.csv"))
write_csv(iq_matches, here("matching/cache/iq_superlands.csv"))
write_csv(pa_matches, here("matching/cache/pa_superlands.csv"))
write_csv(pub_matches, here("matching/cache/pub_superlands.csv"))

