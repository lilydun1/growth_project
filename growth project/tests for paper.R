#tests
GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
traits = c("wood_density", "mean_leaf_m_whole", "mean_P_area", "mean_N_area", "LMA")
ages <- c("1.4", "2.4","7", "5","9", "32")

df <- growth_data %>% select(species, age, mean_leaf_m_whole) %>% group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% ungroup() %>% group_by(age) %>% 
  mutate(mean = (mean(mean_leaf_m_whole))) %>% 
  mutate(max_gross = (max(mean_g_gross_inv*0.001)))

write.csv(df, "Growth_data.csv")

growth_data %>% select(LMA, mean_ratio_leaf_stem)


mod <- lm(log10(mean_g_height)~log10(wood_density)*log10(age), data = (growth_data %>% group_by(age, species) %>% 
                                               distinct(age, .keep_all = TRUE) %>% 
                                                 mutate(mean_g_gross_inv = mean_g_gross_inv*0.001)))
summary(mod)
#log10(wood_density)+log10(mean_leaf_m_whole)+log10(mean_P_area)+log10(mean_N_area)+log10(LMA)
#going through each of the ages 


for(i in ages) {
  mod <- lm(log10(mean_g_gross_inv)~log10(wood_density), 
            data = (growth_data %>% 
                      #filter(mean_g_leaf_area > -0.00001) %>% 
                      filter(age == i) %>% 
                      group_by(age, species) %>% 
                      distinct(age, .keep_all = TRUE)))
  print(summary(mod))
}

#going through each of the growth rate types 
for(i in GR_types_all) {
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(wood_density) + log10(mean_ratio_leaf_stem) + log10(LMA)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}

#interaction 
for(i in GR_types_all) {
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(age)*(mean_leaf_m_whole)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}

mod <- lm(log(mean_g_diameter)~log(wood_density)*log(age), data = (growth_data %>% 
                                                                     filter(mean_g_diameter > -0.00001) %>% 
                                                                     distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))

#interaction 
for(i in GR_types_all) {
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(mean_P_area)*(age)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}


#
mod <- lm(formula = log10(mean_g_diameter) ~ age +log10(LMA), 
          data = growth_data %>% 
            filter(mean_g_diameter > -0.00001) %>%  
            distinct(mean_ratio_leaf_stem, .keep_all = TRUE))

ggpredict(mod, terms = c("LMA [all] ", "age")) %>% 
  ggplot(aes(log10(x), log10(predicted), colour = (group))) +
  geom_line() +
  geom_point()


R_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")

mod <- lm(formula = log10(mean_g_diameter) ~ log10(wood_density)+ (mean_leaf_m_whole),
   data = (growth_data %>% 
             filter(mean_g_diameter > -0.00001) %>% 
             distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))

summary(mod)

 ggplot(aes(log10(x), log10(predicted), colour = (group)), 
         data = ggpredict(mod, terms = c("wood_density [all]", "age"))) +
  geom_line() +
  geom_point(aes(log10(wood_density), log10(mean_g_diameter), colour = age), 
             data = (growth_data %>% 
                        filter(mean_g_diameter > -0.00001) %>% 
                        distinct(mean_ratio_leaf_stem, .keep_all = TRUE))) + 
  labs(x = "log10(wod_density)", y = "log10(mean_g_diameter)", colour = "Age") 



mod <- lm(formula = (log10(mean_g_diameter) ~ (age)+log10(wood_density)),
          data = (growth_data %>% 
                    filter(mean_g_diameter > -0.00001) %>% 
                    distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
mod_1 <- lm(formula = log10(mean_g_diameter) ~ log10(wood_density)*log10(mean_ratio_leaf_stem)*log10(LMA), 
            data = (growth_data %>% 
                      #filter(mean_g_leaf_area > -0.00001) %>% 
                      #filter(mean_g_height > -0.00001) %>% 
                      distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
report(mod1)
summary(mod)

AIC(mod1)



mod <- lm(formula = log10(mean_g_diameter) ~ log10(wood_density), 
   data = (growth_data %>% 
             #filter(mean_g_leaf_area > -0.00001) %>% 
             #filter(mean_g_height > -0.00001) %>% 
             distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
   

traits = c("wood_density", "mean_leaf_m_whole", "mean_P_area", "mean_N_area", "LMA")

smatr_mod <- sma(log(mean_g_leaf_area) ~ (mean_leaf_m_whole)*log(age), data = (growth_data %>% filter(mean_g_leaf_area > -0.00001) %>%
                                                                                 distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))

smatr_mod <- sma(log(mean_g_gross_inv) ~ (mean_leaf_m_whole)+log(age), data = (growth_data %>% 
                                                                         distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))

summary(smatr_mod)
ancova(smatr_mod)

?sma

 
library(smatr)
