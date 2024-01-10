pacman::p_load(tidyverse, ggeffects, ggpmisc, ggpubr, easystats, scales) 

species_meta <- read_csv("data/species.csv")

all_data_growth <- read_csv("SummaryInd.csv")

nutrient_data_raw <- read_csv("nutrient_data.csv") %>% 
  filter(!is.na(age)) %>% 
  filter(tissue == "leaves") %>%  
  group_by(species, age) %>% 
  mutate(mean_P = mean(P,na.rm = TRUE), 
         mean_N = mean(N, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(species, tissue) %>% 
  mutate(mean_P_s = mean(P,na.rm = TRUE), 
         mean_N_s = mean(N, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(species, age, mean_P, mean_N, mean_P_s, mean_N_s) %>% 
  distinct(mean_P,.keep_all = TRUE)

growth_data <- all_data_growth %>%  
  full_join(nutrient_data_raw) %>% 
  mutate(ratio_leaf_stem = leaf_weight/stem_weight,
         leaf_m_whole = leaf_weight/total_weight, 
         leaf_a_whole = leaf_area/total_weight, 
         mean_P_area = mean_P*leaf_size, 
         mean_N_area = mean_N*leaf_size,
         age_group = case_when(
           age < 2.5 ~ "Young (1.4, 2.4 yrs)",
           age > 2.5 ~ "Old (5, 7, 9, 32 yrs)")) %>% 
  group_by(species, age) %>% 
  mutate(mean_ratio_leaf_stem = mean(ratio_leaf_stem, na.rm = TRUE),
         mean_leaf_m_whole = mean(leaf_m_whole, na.rm = TRUE),
         mean_leaf_a_whole = mean(leaf_a_whole, na.rm = TRUE),
         mean_g_diameter = mean(growth_stem_diameter, na.rm = TRUE),
         mean_g_height = mean(growth_height, na.rm = TRUE),
         mean_g_inv = mean(growth_inv, na.rm = TRUE), 
         mean_g_leaf_area = mean(growth_leaf_area, na.rm = TRUE),
         mean_g_gross_inv = mean(gross_inv, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  mutate(mean_ratio_leaf_stem_s = mean(ratio_leaf_stem, na.rm = TRUE),
         mean_leaf_m_whole_s = mean(leaf_m_whole, na.rm = TRUE),
         mean_leaf_a_whole_s = mean(leaf_a_whole, na.rm = TRUE),
         mean_g_stem_diameter_s = mean(growth_stem_diameter, na.rm = TRUE),
         mean_g_height_s = mean(growth_height, na.rm = TRUE),
         mean_g_inv_s = mean(growth_inv, na.rm = TRUE), 
         mean_g_leaf_area_s = mean(growth_leaf_area, na.rm = TRUE),
         mean_g_gross_inv_s = mean(gross_inv, na.rm = TRUE), 
         mean_LMA_s = mean(LMA, na.rm = TRUE), 
         mean_ratio_leaf_stem_s = mean(ratio_leaf_stem, na.rm = TRUE), 
         mean_P_area_s = mean(mean_P_area, na.rm = TRUE), 
         mean_N_area_s = mean(mean_N_area, na.rm = TRUE)) %>% 
  ungroup()

growth_data$age <- factor(growth_data$age, levels = c("1.4", "2.4", "5", "7", "9", "32"))

# plotting traits and diameter growth
plotting_trait_growth <- function(data = growth_data, GR, response) {
  ggplot(data = data, aes((.data[[response]]), (.data[[GR]]), col = age)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    #scale_x_log10() +
    scale_y_log10() +
    #stat_poly_eq(use_label(c("R2", "P", "n", "eq"))) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
          axis.text = element_text(size=12)) +
    labs(colour = "Age (yrs)") +
    scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
}

growth_data %>% 
  ggplot(aes(log10(mean_leaf_m_whole), log10(mean_g_diameter), col = age)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#for the correlations
plotting_cors <- function(data = growth_data, GR, response, x_label) {
  ggplot(data = data, aes((.data[[response]]), (.data[[GR]]))) +
    geom_point() + 
    # geom_smooth(method = "lm") +
    stat_poly_eq(use_label(c("R2")), size = 5, label.x.npc = "left", label.y = "top") +
    scale_x_log10() +
    scale_y_log10() + # this one for LMF
    labs(x = x_label) +
    theme(text = element_text(size = 18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text = element_text(size = 12))
}

traits_Dgrowth_plots <- map(GR_types_abs, ~plotting_Dgrowth(response = "LMA", GR = .x))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE)

traits_Dgrowth_plots <- map(traits_1, ~plotting_Dgrowth(response = .x, GR = "GR_w_indiv"))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE)

cor.test(growth_data$LMA, growth_data$wood_density)

mod <- lm(log10(growth_diameter+0.2)~log10(LMA), data = (growth_data))
tab_model(mod)




