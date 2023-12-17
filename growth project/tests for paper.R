#tests
GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
traits = c("wood_density", "mean_ratio_leaf_stem", "LMA")
ages <- c("1.4", "2.4", "5", "7", "9", "32")

#going through each of the ages 
for(i in ages) {
  mod <- lm(log10(mean_g_gross_inv)~log10(wood_density)+log10(mean_ratio_leaf_stem), 
            data = (growth_data %>% 
                      #filter(mean_g_leaf_area > -0.00001) %>%
                      distinct(mean_ratio_leaf_stem, .keep_all = TRUE) %>% 
                      filter(age == i)))
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
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(wood_density)*log10(mean_ratio_leaf_stem)*log10(LMA)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}

for(i in GR_types_all) {
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(wood_density)*log10(mean_ratio_leaf_stem)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}


mod_1 <- lm(formula = log10(mean_g_diameter) ~ log10(wood_density)*log10(mean_ratio_leaf_stem)*log10(LMA), 
            data = (growth_data %>% 
                      #filter(mean_g_leaf_area > -0.00001) %>% 
                      #filter(mean_g_height > -0.00001) %>% 
                      distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
report(mod1)
summary(mod1)

AIC(mod1)