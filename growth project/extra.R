
GR_types_all = c("growth_stem_diameter", "GR_d","GR_d_age", "GR_d_50", "GR_d_25","GR_d_75",
                 "growth_height", "GR_h","GR_h_age", "GR_h_50", "GR_h_25", "GR_h_75",
                 "growth_inv", "GR_w", "GR_w_age", "GR_w_50", "GR_w_25", "GR_w_75", 
                 "growth_leaf_area", "GR_la", "GR_la_age", "GR_la_50", "GR_la_25", "GR_la_75")
GR_types_all = c("growth_stem_diameter", "growth_height", "growth_inv", "growth_leaf_area", "gross_inv",
                 "mean_g_stem_diameter", "mean_g_height", "mean_g_inv", "mean_g_leaf_area", "mean_g_gross_inv")
GR_types_all = c("growth_stem_diameter", "growth_height", "growth_inv", "growth_leaf_area", 
                 "GR_d_each_age", "GR_h_each_age", "GR_w_each_age", "GR_la_each_age")
traits_Dgrowth_plots <- map(GR_types_all, ~plotting_Dgrowth(data = (growth_data %>% filter(age < 2.5)),
                                                            response = "LMA", GR = .x))
df <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=2, ncol = 5)
annotate_figure(df, "growth at 1.4 and 2.4 ages")

GR_types = c("growth_stem_diameter", "growth_inv", "growth_leaf_area", "gross_inv")
traits_Dgrowth_plots <- map(GR_types, ~plotting_Dgrowth(data = (growth_data %>% filter(age < 2.5)),
                                                                  response = .x, GR = "growth_height"))
c4 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

c3 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

c2 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

c1 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1)

c <- ggarrange(c1, c2, c3, c4, nrow = 4)
annotate_figure(c, "mean growth at 1.4 and 2.4 ages")

traits <- c("wood_density")
traits_Dgrowth_plots <- map(traits, ~plotting_Dgrowth(data = (growth_data %>% filter(age < 2.5)),
                                                        response = .x, GR = "mean_ratio_leaf_stem"))
d4 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

d3 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

d2 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1, ncol = 4)

d1 <- ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow=1)

d <- ggarrange(d1, d2, d3, d4, nrow = 4)
annotate_figure(d, "traits correlated 1.4 and 2.4 ages")

model <- aov(growth_stem_diameter~Species_name, data=growth_data)
summary(model)
TukeyHSD(model, conf.level=.95)

GR_against = c("mean_g_stem_diameter", "mean_g_height", "mean_g_inv", "mean_g_leaf_area")
traits_Dgrowth_plots <- map(GR_against, ~plotting_Dgrowth(data = (growth_data %>% group_by(species, age) %>%  mutate(mean_ratio_leaf_stem = mean(ratio_leaf_stem))), 
                                                          response = "LM_SM_slope_s_a", GR = .x))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE, nrow = 1, ncol = 4)



growth_data %>%
  ggplot(aes(log10(LMA), log10(mean_g_stem_diameter), col = age)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               label.y = "bottom", label.x = "right",
               formula = formula1) 

df <- lm(log10(growth_stem_diameter)~log10(LMA)*RA_group, growth_data)
summary(df) 
anova(df)

growth_data %>%
  mutate(RA_group = as.character(round(RA_max_1, 1))) %>% 
  #filter(RA_max_1 == 0) %>% 
  ggplot(aes(log10(LMA), log10(growth_height), col = RA_group)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size=2) +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               label.y = "bottom", label.x = "right",
               formula = formula1)

growth_data %>%
  mutate(RA_group = as.character(round(RA_max_1, 1))) %>% 
  #filter(RA_max_1 == 0) %>% 
  ggplot(aes(log10(LMA), log10(growth_inv), col = RA_group)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size=2) +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               label.y = "bottom", label.x = "right",
               formula = formula1)

growth_data %>%
  mutate(RA_group = as.character(round(RA_max_1, 1))) %>% 
  #filter(RA_max_1 == 0) %>% 
  ggplot(aes(log10(LMA), log10(growth_leaf_area), col = RA_group)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               label.y = "bottom", label.x = "right",
               formula = formula1)

df1 <- growth_data %>% 
  filter(growth_stem_diameter > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_stem_diameter)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) %>% 
  ggplot(aes(ranks, Species_name, col = age)) +
  geom_point(size = 5) +
  ggtitle("growth_stem_diameter")


df2 <- growth_data %>% 
  filter(growth_height > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_height)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) %>% 
  ggplot(aes(ranks, Species_name, col = age)) +
  geom_point(size = 5) +
  ggtitle("growth_height")

df3 <- growth_data %>% 
  filter(growth_inv > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_inv)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) %>% 
  ggplot(aes(ranks, Species_name, col = age)) +
  geom_point(size = 5) +
  ggtitle("growth_inv")


df4 <- growth_data %>% 
  filter(growth_leaf_area > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_leaf_area)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) %>% 
  ggplot(aes(ranks, Species_name, col = age)) +
  geom_point(size = 5) +
  ggtitle("growth_leaf_area")

ggarrange(df1, df2, df3, df4)

f1 <- growth_data %>% 
  filter(growth_stem_diameter > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_stem_diameter)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age))

f2 <- growth_data %>% 
  filter(growth_height > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_height)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) 

f3 <- growth_data %>% 
  filter(growth_inv > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_inv)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) 

f4 <- growth_data %>% 
  filter(growth_leaf_area > -0.000001) %>% 
  group_by(Species_name, age) %>% 
  summarise(mean = mean(growth_leaf_area)) %>% 
  ungroup() %>% 
  group_by(age) %>% 
  mutate(ranks = order(order(mean)), 
         age = as.character(age)) 

growth_data$age <- factor(growth_data$age, levels = c("1.4", "2.4", "5", "7", "9", "32"))

f4 %>% 
  bind_rows(f1) %>% 
  bind_rows(f3) %>% 
  bind_rows(f2) %>% 
  mutate(age = factor(age, levels = c("1.4", "2.4", "5", "7", "9", "32"))) %>% 
  mutate(ranks = factor(ranks)) %>%
  #filter(Species_name == "Banksia ericifolia") %>% 
  ggplot(aes(ranks, log10(mean))) +
  geom_boxplot() +
  facet_wrap("age")

growth_data %>% 
  ggplot(aes(log10(LMA),log10(growth_stem_diameter))) + 
  geom_point() +
  geom_smooth(method ="lm") +
  stat_poly_eq(use_label(c("eq", "R2", "P")),
               formula = formula1)

GR_types_all = c("growth_stem_diameter", "growth_height", "growth_inv", "growth_leaf_area")
traits_Dgrowth_plots <- map(GR_types_all, ~plotting_Dgrowth(response = "age", GR = .x))
ggarrange(plotlist = traits_Dgrowth_plots, common.legend = TRUE)

growth_data %>% 
  ggplot(aes((RA_max_1), log10(growth_inv), col = Species_name))+
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


  
  