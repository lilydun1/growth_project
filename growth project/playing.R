pacman::p_load(tidyverse,ggeffects, ggpmisc, ggpubr) 
species_meta <- read_csv("data/species.csv")
all_data_growth <- read_csv("SummaryInd.csv")

all_data_growth %>% 
  ggplot(aes(species, age)) + geom_point()

growth_data <- all_data_growth %>%  
  mutate(relative_growth_diameter = growth_stem_diameter/diameter_0,
            relative_growth_height = growth_height/height_0, 
            ratio_leaf_stem = leaf_weight/stem_weight,
            age_half_reproduction = case_when(
              species == "BOLE" & age < 1.5 ~ "Y",
              species == "HATE" & age < 9.1 ~ "Y",
              species == "GRSP" & age < 2.5 ~ "Y",
              species == "PILI" & age < 1.5 ~ "Y",
              species == "HEPU" & age < 2.5 ~ "Y",
              species == "EPMI" & age < 5.1 ~ "Y",
              species == "GRBU" & age < 2.5 ~ "Y",
              species == "LEES" & age < 2.5 ~ "Y",
              species == "PUTU" & age < 2.5 ~ "Y",
              species == "COER" & age < 5.1 ~ "Y",
              species == "PHPH" & age < 2.5 ~ "Y",
              species == "BAER" & age < 9.1 ~ "Y",
              species == "PEPU" ~ "Y", 
              species == "PELA" & age < 9.1 ~ "Y", 
              .default = "N")) %>% 
  inner_join(LM_SM_allometric_trait_s_a, by = c("age" = "age", "species" = "species")) %>% 
  inner_join(LM_SM_allometric_trait_s, by = c("species" = "species")) %>% 
  inner_join(LA_SM_allometric_trait_s, by = c("species" = "species")) %>% 
  inner_join(species_meta, by = c("species" = "Abbreviation")) %>% 
  inner_join(GR_d, by = c("species" = "Spp")) %>% 
  inner_join(GR_h, by = c("species" = "Spp")) %>% 
  inner_join(GR_w, by = c("species" = "Spp")) %>% 
  inner_join(GR_la, by = c("species" = "Spp")) %>%
  inner_join(GR_d_each_age, by = c("species" = "Spp", "age" = "age")) %>% 
  inner_join(GR_h_each_age, by = c("species" = "Spp", "age" = "age")) %>% 
  inner_join(GR_w_each_age, by = c("species" = "Spp", "age" = "age")) %>% 
  inner_join(GR_la_each_age, by = c("species" = "Spp", "age" = "age")) %>%
  dplyr::select(-c("Family", "Common_name", "Previous_names", 
                   starts_with(c("m", "n", "slope_after_inflection", 
                                "slope_before_inflection", 
                                "breakpoint", "breakpoint_se", "GrowthRate_at")))) %>% 
  group_by(species) %>%   
  mutate(GR_d_max = max(GR_d_each_age), GR_h_max = max(GR_h_each_age), 
         GR_w_max = max(GR_w_each_age), GR_la_max = max(GR_la_each_age)) %>% 
  ungroup()
  

LM_SM_allometric_trait_s_a <- all_data_growth %>%
  ungroup() %>% 
  group_by(species, age) %>% 
  do(mod_lin = lm(log10(leaf_weight)~log10(stem_weight), data = .)) %>% 
  mutate(LM_SM_intercept_s_a = mod_lin$coefficients[1],
         LM_SM_slope_s_a = mod_lin$coefficients[2]) %>% 
  dplyr::select(-mod_lin)

LM_SM_allometric_trait_s <- all_data_growth %>%
  ungroup() %>% 
  group_by(species) %>% 
  do(mod_lin = lm(log10(leaf_weight)~log10(stem_weight), data = .)) %>% 
  mutate(LM_SM_intercept_s = mod_lin$coefficients[1],
         LM_SM_slope_s = mod_lin$coefficients[2]) %>% 
  dplyr::select(-mod_lin)

LA_SM_allometric_trait_s <- all_data_growth %>%
  ungroup() %>% 
  group_by(species) %>% 
  do(mod_lin = lm(log10(leaf_area)~log10(stem_weight), data = .)) %>% 
  mutate(LA_SM_intercept_s = mod_lin$coefficients[1],
         LA_SM_slope_s = mod_lin$coefficients[2]) %>% 
  dplyr::select(-mod_lin)

traits_1 = c("LMA", "leaf_size", "wood_density")
traits_2 = c("total_leaf_area", "leaf_weight", "stem_weight")
traits_3 = c("ratio_leaf_stem", "LM_SM_slope_s_a", 
             "LM_SM_slope_s", "LA_SM_slope_s")
GR_types = c("GR_d", "GR_h", "GR_w", "GR_la")
GR_types_abs = c("growth_stem_diameter", "growth_height", "growth_inv","growth_leaf_area")

formula1 <- y~x

growth_data$age <- as.character(growth_data$age)

# plotting traits and diameter growth
plotting_Dgrowth <- function(data = growth_data, GR, response) {
  ggplot(data = data, aes(log10(.data[[response]]), log10(.data[[GR]]))) +
    geom_point() + 
    geom_smooth(method = "lm") +
    stat_poly_eq(use_label(c("eq", "R2", "P")),
                 formula = formula1, size = 5) +
    theme(text = element_text(size = 15))
}

traits_Dgrowth_plots <- map(GR_types_abs, ~plotting_Dgrowth(response = "LMA", GR = .x))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE)

traits_Dgrowth_plots <- map(traits_1, ~plotting_Dgrowth(response = .x, GR = "GR_w_indiv"))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE)

cor.test(growth_data$LMA, growth_data$wood_density)

mod <- lm(log10(growth_diameter+0.2)~log10(LMA), data = (growth_data))
tab_model(mod)

#plotting traits and diameter growth with species and age
plotting_Dgrowth_spp_age <- function(data = growth_data, GR, response, colour) {
  data$age <- as.character(data$age)
  df = data
  ggplot(data = df, aes(log10(.data[[response]]), log10(.data[[GR]]+0.2), col = .data[[colour]])) + 
  geom_point() + 
  geom_smooth(method = "lm") +
    stat_poly_eq(use_label(c("eq", "R2", "P")),
                 formula = formula1) +
  theme(text = element_text(size = 15))
}

#growth diameter spp
traits_Dgrowth_plots_spp <- map(traits_1, ~plotting_Dgrowth_spp_age(data = growth_data,
                                                                    response = .x, GR = "growth_diameter",
                                                                    colour = "age"))
ggarrange(plotlist = traits_Dgrowth_plots_spp, common.legend = TRUE)

#growth diameter age 
traits_Dgrowth_plots_age <- map(traits_3, ~plotting_Dgrowth_spp_age(data = (growth_data %>% filter(age != 32)),
                                                                            response = .x, GR = "growth_diameter",
                                                                  colour = "age"))
df <- ggarrange(plotlist = traits_Dgrowth_plots_age, common.legend = TRUE)
annotate_figure(df, top = "no 32")

df <- lm(log10(growth_diameter + 0.2)~log10(LMA)+age, growth_data)
summary(df) 
anova(df)

#for each species make a table of each of the traits 
spp_GR_traits <- function(spp) {
  growth_data_spp <- growth_data %>% 
    filter(species == spp) %>% 
    ungroup()
  
rsquared <- growth_data_spp %>% 
  dplyr::select(growth_stem_diameter, LMA, leaf_weight, stem_weight, ratio_leaf_stem, 
                LM_SM_slope_s_a) %>% 
  map(~lm(log(growth_stem_diameter+1.1) ~ .x, growth_data_spp)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tidy %>% 
  rename(r.squared = x) %>% 
  filter(names %in% c("LMA", "leaf_weight", "stem_weight", "ratio_leaf_stem", 
                      "LM_SM_slope_s_a"))

pvalue <- growth_data_spp %>% 
  dplyr::select(growth_stem_diameter, LMA, leaf_weight, stem_weight, ratio_leaf_stem, 
                LM_SM_slope_s_a) %>% 
  map(~lm(log(growth_stem_diameter+1.1) ~ .x, growth_data_spp)) %>% 
  map(summary) %>% 
  map(c("coefficients")) %>% 
  map_dbl(8) %>%
  tidy %>% 
  rename(p.value = x) %>% 
  filter(names %in% c("LMA", "leaf_weight", "stem_weight", "ratio_leaf_stem", 
                      "LM_SM_slope_s_a"))

data <- merge(rsquared, pvalue)
}

p_r2_traits_DGR <- growth_data %>% 
  ungroup() %>% 
  dplyr::select(species) %>% 
  distinct() %>% 
  mutate(summary = map(species, spp_GR_traits)) %>% 
  #unnest(summary) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  unnest()

#stem versus leaf with age
lm_sm_plot <- function(data = growth_data, x, y, colour) {
  data$age <- as.character(data$age)
  df = data
  ggplot(data, aes(log10(.data[[x]]), log10(.data[[y]]), col = .data[[colour]])) + 
  geom_point() +
  stat_poly_eq(use_label(c("eq", "R2", "P"))) +
  geom_abline(intercept = 0, slope = 1) +  
  geom_smooth(method = "lm") +
  theme(text = element_text(size = 15))
}

growth_data %>%
  ungroup() %>% 
  #filter(RA_max_1 < 0.5) %>% 
  ggplot(aes(log10(stem_weight), log10(leaf_area))) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +  
  geom_smooth(method = "lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P"))) +
  theme(text = element_text(size = 15)) #+
  facet_wrap("Species_name") #+
  #ggtitle("no 50% allocation")

df <- lm_sm_plot(data = (all_data %>% filter(colname == "Y" )), x = "stem_weight", y = "total_leaf_area", colour ="Species_name")
annotate_figure(df, "no 50% allocation")

mod <- lm(log10(total_leaf_area)~log10(stem_weight), data = (growth_data))
tab_model(mod)

smatr_LM_SM <- sma(log10(leaf_weight)~log10(stem_weight), data = (growth_data  %>% 
                     filter(age != "32" )), robust = TRUE)
smatr_LA_SM <- sma(log10(total_leaf_area)~log10(stem_weight), data = (growth_data  %>% 
                                                                        filter(age != "32" ))
                   , robust = TRUE)
smatr_LM_SM_spp <- sma(log10(leaf_weight)~log10(stem_weight)*Species_name, growth_data, 
                   robust = TRUE)
smatr_LM_SM_spp_ad <- sma(log10(leaf_weight)~log10(stem_weight)+Species_name, growth_data, 
                       robust = TRUE)
smatr_LM_SM_age <- sma(log10(leaf_weight)~log10(stem_weight)*age, growth_data, 
                       robust = TRUE)
smatr_LA_SM_spp <- sma(log10(total_leaf_area)~log10(stem_weight)*Species_name, growth_data, 
                       robust = TRUE)
smatr_LA_SM_spp_ad <- sma(log10(total_leaf_area)~log10(stem_weight)+Species_name, growth_data, 
                          robust = TRUE)
smatr_LA_SM_age <- sma(log10(total_leaf_area)~log10(stem_weight)*age, growth_data, 
                       robust = TRUE)
plot(smatr_LM_SM_age)
legend()

growth_data %>% 
  filter(age != "32" ) %>% 
  ggplot(aes(log10(stem_weight), log10(total_leaf_area))) + 
  geom_point() +
  geom_abline(aes(slope = smatr_LA_SM$coef[[1]][2,1], 
                  intercept = smatr_LA_SM$coef[[1]][1,1], col = "RED")) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(method = "lm") +
  theme(legend.position="none") +
  stat_poly_eq(use_label(c("eq", "R2", "P"))) +
  ggtitle("no 32, Red = SMA, blue = linear, black = 1:1")

df <- lm(log10(to)~log10(stem_weight)*age, growth_data)
summary(df) 
anova(df)


#going through the species 
plots_stem_leaf <- growth_data %>% 
  mutate(species = Species_name) %>% 
  split(.$Species_name) %>% 
  map(~ggplot(.x, aes(log10(leaf_weight), log10(stem_weight), col = age)) + 
        geom_point() +
        stat_fit_glance(method = "lm",
                        label.x="right", label.y="bottom",
                        method.args = list(formula = formula1),
                        aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.2f',
                                            stat(..r.squared..),stat(..p.value..))),
                        parse = TRUE) +
        geom_abline(intercept = 0, slope = 1) +  
        geom_smooth(method = "lm") +
        ggtitle(first(.x)))

file_names_stem_leaf <- stringr::str_c(names(plots_stem_leaf), ".pdf")

pwalk(list(file_names_stem_leaf, plots_stem_leaf), ggsave, path = ".") 

#old species 
old_spp <- growth_data %>% 
  ungroup() %>% 
  filter(age == 32) %>% 
  distinct(species) %>% 
  as.list()

old_spp_plot <- growth_data %>% 
  ungroup() %>% 
  filter(species %in% old_spp$species) %>% 
  ggplot(aes(log10(LMA), log10(growth_diameter+0.2))) +
  geom_point() + 
  geom_smooth(method = "lm") +
  stat_fit_glance(method = "lm",
                  label.y = "top",
                  method.args = list(formula = formula1),
                  aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.2f',
                                      stat(..r.squared..),stat(..p.value..))),
                  parse = TRUE) +
  ggtitle("old_species")

growth_data %>% 
  ungroup() %>% 
  filter(species %in% old_spp$species) %>% 
  ggplot(aes(log10(LMA), log10(growth_diameter+0.2), col = Species_name)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  stat_fit_glance(method = "lm",
                  label.y = "top",
                  method.args = list(formula = formula1),
                  aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.2f',
                                      stat(..r.squared..),stat(..p.value..))),
                  parse = TRUE) +
  ggtitle("old_species")


