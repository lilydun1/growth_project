#tests
GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
traits = c("wood_density", "mean_leaf_m_whole", "mean_P_area", "mean_N_area", "LMA")
ages = c("1.4", "2.4","7","9", "32")

#log10(wood_density) (mean_leaf_m_whole) log10(mean_P_area) log10(mean_N_area) log10(LMA)
#c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
#going through each of the ages, DO NOT log mean_leaf_m_whole, remember to remove neg values of mean_g_height and mean_g_leaf_area
for(i in ages) {
  mod <- lm(log10(mean_g_height)~log10(mean_N), 
            data = (growth_data %>% 
                      filter(mean_g_height > -0.00001) %>% 
                      filter(age == i) %>% 
                      group_by(age, species) %>% 
                      distinct(age, .keep_all = TRUE)))
  print(summary(mod))
}

#interaction 
for(i in GR_types_all) {
  mod1 <- lm(formula = paste("log10(", i, ") ~ log10(age)*log10(mean_N_area)", sep = ""),
             data = (growth_data %>% 
                       filter(get(i) > -0.00001) %>% 
                       distinct(mean_ratio_leaf_stem, .keep_all = TRUE)))
  print(summary(mod1))
}