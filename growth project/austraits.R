remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 

austraits <- load_austraits(version = "5.0.0", path = "data/austraits")

get_versions(path = "data/austraits")

data_wood_dens <- extract_trait(austraits, "wood_density")

data_wood_dens <- data_wood_dens %>% join_locations()

data_LMA <- extract_trait(austraits, "leaf_mass_per_area")

data_LMA <- data_LMA %>% join_locations()

pls <- data_LMA$traits %>% ungroup() %>% full_join(data_wood_dens$traits)

work <- pls %>% group_by(dataset_id, taxon_name, trait_name, location_name) %>% 
  mutate(value = mean(value)) %>% 
  distinct(value, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(dataset_id, taxon_name, trait_name, value, location_name) %>% 
  pivot_wider(names_from = trait_name, values_from = value) %>% 
  filter(!is.na(leaf_mass_per_area)) %>% 
  filter(!is.na(wood_density))

ggplot(data = (work), aes(log10(wood_density), log10(leaf_mass_per_area))) +
  geom_smooth(aes(group = location_name, colour = location_name), se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, size = 2) +
  theme(legend.position="none") +
  #geom_line(data =work, aes(log10(leaf_mass_per_area), log10(wood_density)))#+
  stat_poly_eq(use_label(c("R2")), size = 6, hjust = -0.5)


ggplot(data = (work), aes(log10(leaf_mass_per_area), log10(wood_density))) +
  geom_line(aes(group = location_name, colour = location_name)) +
  geom_smooth(se = FALSE, size = 2) +
  theme(legend.position="none") +
#geom_line(data =work, aes(log10(leaf_mass_per_area), log10(wood_density)))#+
stat_poly_eq(use_label(c("R2")), size = 6, hjust = -0.5)

df <- data_LMA$locations %>% inner_join(WD_sites) %>% 
  distinct(location_name, .keep_all = T) %>% 
  distinct(dataset_id, .keep_all = T)

datasetids <- list(df$dataset_id)

subset_multi_studies <- extract_dataset(austraits, 
                                        dataset_id = datasetids[[1]])  

data_wide_bound <- subset_multi_studies$traits %>%
  summarise_trait_means() %>% 
  trait_pivot_wider()

df <- data_wide_bound %>% 
  select(dataset_id, taxon_name, wood_density, leaf_mass_per_area) %>% 
  group_by(dataset_id, taxon_name) %>% 
  mutate(wood_density_m = mean(wood_density, na.rm = TRUE), 
         lma_m = mean(leaf_mass_per_area, na.rm = TRUE)) %>% 
  filter(!is.na(wood_density_m)) %>% 
  filter(!is.na(lma_m)) %>% 
  distinct(lma_m, .keep_all = TRUE)

df %>% filter(dataset_id == "Apgaua_2015") %>% 
  ggplot(aes(lma_m, wood_density_m)) +geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(use_label(c("R2")), size = 6, hjust = -0.5)
