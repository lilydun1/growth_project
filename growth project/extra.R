
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
  geom_smooth() +
  facet_wrap(vars(species))

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

  