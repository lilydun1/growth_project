pacman::p_load(tidyverse, ggeffects, ggpmisc, ggpubr, easystats, scales, patchwork, broom) 

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
  mutate(LMA  = LMA*10000,
         mean_P = mean_P/(10^6), 
         mean_N = mean_N/100) %>% 
  mutate(ratio_leaf_stem = leaf_weight/stem_weight,
         leaf_m_whole = leaf_weight/total_weight, 
         leaf_a_whole = leaf_area/total_weight, 
         mean_P_area = mean_P*LMA, 
         mean_N_area = mean_N*LMA,
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
         mean_g_diameter_s = mean(growth_stem_diameter, na.rm = TRUE),
         mean_g_height_s = mean(growth_height, na.rm = TRUE),
         mean_g_inv_s = mean(growth_inv, na.rm = TRUE), 
         mean_g_leaf_area_s = mean(growth_leaf_area, na.rm = TRUE),
         mean_g_gross_inv_s = mean(gross_inv, na.rm = TRUE), 
         mean_LMA_s = mean(LMA, na.rm = TRUE), 
         mean_ratio_leaf_stem_s = mean(ratio_leaf_stem, na.rm = TRUE), 
         mean_P_s = mean(mean_P, na.rm = TRUE), 
         mean_N_s = mean(mean_N, na.rm = TRUE), 
         mean_P_area_s = mean(mean_P_area, na.rm = TRUE), 
         mean_N_area_s = mean(mean_N_area, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(species_meta, by = c("species" = "Abbreviation")) %>% 
  dplyr::select(-c("Family", "Common_name", "Previous_names")) %>% 
  mutate(Species_name = str_replace(Species_name, "Leucopogon esquamatus", "Styphelia esquamata")) %>% 
  arrange(Species_name) %>% 
  ungroup() %>% 
  mutate(
         mean_g_gross_inv = mean_g_gross_inv*0.001, 
         mean_g_inv = mean_g_inv*0.001)

growth_data$age <- factor(growth_data$age, levels = c("1.4", "2.4", "5", "7", "9", "32"))

# plotting traits and diameter growth
plotting_trait_growth <- function(data = growth_data, GR, response) {
  plot1 <- ggplot(data = data, aes((.data[[response]]), (.data[[GR]]), col = age)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    #stat_poly_eq(use_label(c("R2", "P", "n", "eq"))) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
          axis.text = element_text(size=12)) +
    labs(colour = "Age (yrs)") +
    scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
   if (response == "mean_leaf_m_whole") {
      plot2 <- plot1 + scale_y_log10() 
    } else {
      plot2 <- plot1 + scale_x_log10() + scale_y_log10() 
    }
}

#for the correlations
plotting_cors <- function(data = growth_data, GR, response, x_label) {
  
  
  plot1 <- ggplot(data = data, aes((.data[[response]]), (.data[[GR]]))) +
    geom_point() + 
    # geom_smooth(method = "lm") +
    stat_poly_eq(use_label(c("R2")), size = 5, label.x.npc = "left", label.y = "top") +
    #scale_y_log10() +
    labs(x = x_label) +
    theme(text = element_text(size = 18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text = element_text(size = 12))
  if (response == "mean_g_inv") {
    plot1 + scale_x_log10(breaks = c(1, 10, 100))
  } else {
    plot1 + scale_x_log10()
  } 
}

#model plots 
plotting_predict <- function(data = growth_data, trait) {
  
  list_stats_inter <- list()
  list_stats_no_inter <- list()
  plot_stats_list <- list()
  
  for (i in GR_types_all) {
    filtered_data = growth_data %>% 
      filter(get(i) > -0.00001) %>% 
      distinct(mean_ratio_leaf_stem, .keep_all = TRUE)
    
    if(trait == "mean_leaf_m_whole") {
      list_stats_inter[[i]] <- lm(formula = paste("log10(", i , ") ~ log10(age)*(", trait, ")", sep = ""),
                                  data = filtered_data)
      
      list_stats_no_inter[[i]] <- lm(formula = paste("log10(", i , ") ~ log10(age)+(", trait, ")", sep = ""),
                                     data = filtered_data)
    } else {
      list_stats_inter[[i]] <- lm(formula = paste("log10(", i , ") ~ log10(age)*log10(", trait, ")", sep = ""),
                                  data = filtered_data)
      
      list_stats_no_inter[[i]] <- lm(formula = paste("log10(", i , ") ~ log10(age)+log10(", trait, ")", sep = ""),
                                     data = filtered_data)
    }
  }
  
  for (i in 1:5) {
    data = growth_data %>% 
      filter(get(GR_types_all[i]) > -0.00001) %>% 
      distinct(mean_ratio_leaf_stem, .keep_all = TRUE)
    
    if (trait == "mean_leaf_m_whole") {
      if (summary(list_stats_inter[[i]])$coefficients[4,4] < 0.05) {
        plot_stats_list[[i]] <- 
          ggplot(aes((x), (predicted), colour = (group)), 
                 data = ggpredict(list_stats_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          scale_y_log10() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = (.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      } else {
        plot_stats_list[[i]] <- 
          ggplot(aes((x), (predicted), colour = (group)), 
                 data = ggpredict(list_stats_no_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          scale_y_log10() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = (.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      } 
    } 
    
    else {
      if (summary(list_stats_inter[[i]])$coefficients[4,4] < 0.05) {
        plot_stats_list[[i]] <- 
          ggplot(aes((x), (predicted), colour = (group)), 
                 data = ggpredict(list_stats_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          scale_x_log10() +
          scale_y_log10() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = (.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("log10(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      }
      else {
        plot_stats_list[[i]] <- 
          ggplot(aes((x), (predicted), colour = (group)), 
                 data = ggpredict(list_stats_no_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          scale_x_log10() +
          scale_y_log10() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = (.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("log10(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      }
    }
  }
  
  return(plot_stats_list)
}



