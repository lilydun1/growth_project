
GR_types_all = c("growth_stem_diameter", "GR_d","GR_d_age", "GR_d_50", "GR_d_25","GR_d_75",
                 "growth_height", "GR_h","GR_h_age", "GR_h_50", "GR_h_25", "GR_h_75",
                 "growth_inv", "GR_w", "GR_w_age", "GR_w_50", "GR_w_25", "GR_w_75", 
                 "growth_leaf_area", "GR_la", "GR_la_age", "GR_la_50", "GR_la_25", "GR_la_75")
GR_types_all = c("growth_stem_diameter", "growth_height", "growth_inv","growth_leaf_area", 
                 "GR_d_max", "GR_h_max", "GR_w_max", "GR_la_max")
traits_Dgrowth_plots <- map(GR_types_all, ~plotting_Dgrowth(response = "LM_SM_slope_s_a", GR = .x))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=2, ncol = 4)

growth_data %>% 
  ggplot(aes(log10(growth_leaf_area),log10(growth_stem_diameter))) + 
  geom_point() +
  geom_smooth(method ="lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               formula = formula1)

growth_data %>% 
  ggplot(aes(log10(GR_w), log10(growth_inv)))+
  geom_point() +
  geom_smooth(method ="lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               formula = formula1)

df <- growth_data %>% 
  group_by(species) %>%   
  mutate(GR_d_max = max(GR_d_each_age), GR_h_max = max(GR_h_each_age), 
         GR_w_max = max(GR_w_each_age), GR_la_max = max(GR_la_each_age)) %>% 
  ungroup()

growth_data %>% 
  ggplot(aes(log10(LMA), log10(growth_stem_diameter))) +
  geom_point() +
  geom_smooth(method ="lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               formula = formula1)

growth_data %>% 
  ggplot(aes(log10(LMA), log10(GR_h_each_age)))+
  geom_point() +
  geom_smooth(method ="lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               formula = formula1)


all_data_growth %>% #filter(RA_max_1 < 0.5) %>% 
  ggplot(aes(age, RA_max_1)) + 
  geom_point() + facet_wrap(vars(species)) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

df <- growth_data %>% 
  filter(start_end == "end") %>% 
  filter(age_half_reproduction == "Y")


growth_data %>%
  filter(start_end == "end") %>% 
  ggplot(aes((age), log10())) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
    facet_wrap(vars(species))

growth_data %>%
  ggplot(aes(log10(age), log10(height))) + 
  geom_point() + 
  geom_line() +
  facet_wrap(vars(Species_name))

growth_data %>% 
  ggplot(aes(age, LMA)) + 
  geom_point() +
  facet_wrap(vars(Species_name)) +
  geom_line() +
  geom_smooth(method = "lm")

data_per <- growth_data %>% 
  group_by(Species_name) %>% 
  arrange(age, .by_group = TRUE) %>% 
  mutate(pct_change = (LMA/lag(LMA) - 1) * 100)
  
data_per %>% 
  ggplot(aes(age, LMA)) + 
  geom_point() +
  facet_wrap(vars(Species_name)) +
  geom_line() 
  geom_smooth(method = "lm")

per_LMA <- function(spp) {
  data_per = data_per %>% 
  filter(Species_name == spp)  
  lm(pct_change ~ age, data_per)
  }

lma_g <- growth_data %>% 
  ungroup() %>% 
  select(Species_name) %>% 
  distinct() %>% 
  mutate(summary = map(Species_name, per_LMA), 
         coeffs = map(summary, coefficients),
         slope = map_dbl(coeffs, 2))

d_mod <- lm(pct_change ~ age, df)


  