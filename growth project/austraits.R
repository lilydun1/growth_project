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
  distinct(value, .keep_all = TRUE) %>%
  pivot_wider(names_from = trait_name, values_from = value) %>%
  filter(!is.na(leaf_mass_per_area) & !is.na(wood_density) & !is.na(location_name)) %>%
  filter(taxon_count > 3) %>%
  group_by(location_name) %>%
  mutate(
    taxon_count = n_distinct(taxon_name),
    leaf_mass_per_area = log10(leaf_mass_per_area),
    wood_density = log10(wood_density)) %>%
  filter(taxon_count > 2) %>%
  mutate(
    r = cor(leaf_mass_per_area, wood_density),
    model_summary = list(tidy(lm(leaf_mass_per_area ~ wood_density)))) %>%
  ungroup()

#this plot is gg
wd_lma_austraits <- ggplot(data = (work), aes((wood_density), (leaf_mass_per_area))) +
  geom_point(aes(group = location_name, colour = location_name), alpha = 0.2) +
  geom_smooth(aes(group = location_name, colour = location_name), linewidth = 0.62, se = FALSE, method = "lm") +
  geom_smooth(se = FALSE, linewidth = 2, colour = "black", method = "lm") +
  theme(legend.position="none") +
  stat_poly_eq(use_label(c("R2")), size = 6, hjust = -0.5) +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12)) +
  ylab(bquote(LMA~(g/m^2))) + xlab(bquote(WD~(g/cm^3)))+ scale_x_log10() + scale_y_log10() 
ggsave("Fig S4. wd_lma_austraits.jpeg", width = 25, height = 20, units = "cm")

work %>% distinct(location_name, .keep_all = T) %>% 
  ggplot(aes(taxon_count, r)) + geom_point()

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

plotting_cors <- function(data = growth_data, GR, response, x_label) {
  ggplot(data = data, aes(log(.data[[response]]), log(.data[[GR]]))) +
    geom_point() + 
    labs(x = x_label) +
    stat_poly_eq(use_label(c("R2")), size = 6, hjust = -0.5) +
    #scale_x_continuous(labels = scales::trans_format("exp")) +
    #scale_y_continuous(labels = scales::trans_format("exp")) +
    theme(text = element_text(size = 18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text = element_text(size = 12))
}

traits <- c("wood_density", "mean_LMA_s", "mean_leaf_m_whole_s", "mean_N_area_s")
traits_cor <- map2(traits, c(bquote(Wood~density~(g/cm^3)), bquote(Leaf~mass~area~(kg/m^2)), bquote(Leaf/total~mass~(mg/mg^-1)), bquote(Leaf~N[area]~(units))), 
                   ~plotting_cors(data = (growth_data %>% 
                                            distinct(mean_ratio_leaf_stem_s, .keep_all = TRUE) %>% 
                                            mutate(mean_LMA_s = (mean_LMA_s*10))),
                                  GR = "mean_P_area_s", response = .x, x_label = .y) +
                     ylab(bquote(Leaf~P[area]~(units))))

P_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"))





#Figure 3: The correlations of the traits at species level traits = c("wood_density", "mean_LMA_s","mean_P_area_s", "mean_N_area_s", "mean_leaf_m_whole_s")
#c(bquote(WD~(g/cm^3)), bquote(LMA~(g/m^2)), bquote(P[area]~(g/m^2)), bquote(N[area]~(g/m^2)), bquote(LMF~(g/g)))

traits <- c("wood_density")
traits_cor <- map2(traits, c(bquote(WD~(g/cm^3))), 
                   ~plotting_cors(data = (growth_data %>% 
                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                  response = .x, GR = "mean_LMA_s", x_label = .y) +
                     ylab(bquote(LMA~(g/m^2))))

#remember that the y axis of this one should not be logged
leaf_whole_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                               font.label = list(size = 18))

#remember this onwards need logging
N_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                      font.label = list(size = 18))

P_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                      font.label = list(size = 18))

LMA_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                        font.label = list(size = 18))

all_trait_cors <- ggarrange(leaf_whole_others, N_others, P_others, LMA_others, nrow = 4, align = "h")
ggsave("trait_cors.jpeg", width = 33, height = 33, units = "cm")
