remotes::install_github("traitecoevo/austraits", dependencies = TRUE, upgrade = "ask")

library(austraits) 

austraits <- load_austraits(version = "5.0.0", path = "data/austraits")

get_versions(path = "data/austraits")

data_wood_dens <- extract_trait(austraits, "wood_density")

data_wood_dens <- data_wood_dens %>% join_locations()

data_LMA <- extract_trait(austraits, "leaf_mass_per_area")

data_LMA <- data_LMA %>% join_locations()

pls <- data_LMA$traits %>% ungroup() %>% full_join(data_wood_dens$traits)

work <- pls %>%
  group_by(dataset_id, taxon_name, trait_name, location_name) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = trait_name, values_from = value) %>%
  filter(!is.na(leaf_mass_per_area) & !is.na(wood_density) & !is.na(location_name)) %>%
  group_by(location_name, dataset_id) %>% 
  mutate(taxon_count = n_distinct(taxon_name)) %>% 
  filter(taxon_count > 2) %>% 
  ungroup()

slopes <- work %>%
  group_by(location_name) %>%
  summarise(slope = lm(log10(leaf_mass_per_area) ~ log10(wood_density))$coefficients[2])

work <- merge(work, slopes, by = "location_name")

# Create the plot
wd_lma_austraits <- ggplot(data = work, aes(x = wood_density, y = leaf_mass_per_area)) +
  geom_point(aes(group = location_name, colour = location_name), alpha = 0.2) +
  geom_smooth(aes(group = location_name, colour = ifelse(slope < 0, "Negative", "Positive")), 
              linewidth = 0.3, se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, linewidth = 1, colour = "black", method = "lm") +
  theme(legend.position="none") +
  scale_color_manual(values = c("Positive" = "seagreen", "Negative" = "red")) +
  theme(text = element_text(size = 18), legend.text = element_text(size = 18), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key = element_rect(fill = "white"), axis.text = element_text(size = 12)) +
  ylab(bquote(LMA~(g/m^2))) + xlab(bquote(WD~(g/cm^3)))+ 
  scale_x_log10() + scale_y_log10() + stat_poly_eq(use_label(c("R2")), size = 5)

df <- data.frame(location_name = "our_study", taxon_count = 14, r = -0.737)

funnel_plot <- work %>%
  ungroup() %>% 
  mutate(
    leaf_mass_per_area = log10(leaf_mass_per_area),
    wood_density = log10(wood_density)) %>%
  group_by(location_name) %>% 
  mutate(
    r = cor(leaf_mass_per_area, wood_density)) %>%
  ungroup() %>% 
  add_row(df) %>% 
  ggplot(aes(taxon_count, r, colour = location_name, size = ifelse(location_name == "our_study", 2, 1))) + 
  geom_point() +
  scale_colour_manual(values = c("our_study" = "red", "other" = "black")) +
  theme(text = element_text(size = 18), legend.position = "none", 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key = element_rect(fill = "white"), axis.text = element_text(size = 12)) +
  ylab("Correlation of WD and LMA") + xlab("Number of species sampled") 

austraits_fig <- ggarrange(wd_lma_austraits, funnel_plot, ncol = 1, nrow = 2, align = c("v"), labels = c("a", "b"), font.label = list(size = 18))
ggsave("Fig S6. wd_lma_austraits.jpeg", width = 20, height = 25, units = "cm")


