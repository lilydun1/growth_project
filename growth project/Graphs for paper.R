GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
#figure 1: Wood density against all GRS
traits_growth_plots_wd <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%  distinct(mean_ratio_leaf_stem, .keep_all = TRUE) 
                                                                    %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                               mean_g_inv = (mean_g_inv*0.001))),
                                                            response = "wood_density", GR = .x))
traits_growth_plots_wd[[1]] <- traits_growth_plots_wd[[1]] + ylab(bquote(Diameter~growth~(mm/yr))) + xlab(bquote(Wood~density~(g/cm^3))) +
                              theme(axis.title.x = element_text(colour="white")) + 
                              scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_wd[[2]] <- traits_growth_plots_wd[[2]] + ylab(bquote(Height~growth~(mm/yr))) + xlab(bquote(Wood~density~(g/cm^3))) +
                              theme(axis.title.x = element_text(colour="white")) +
                              scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_wd[[3]] <- traits_growth_plots_wd[[3]] + ylab(bquote(Leaf~area~growth~(mm^2/yr))) + xlab(bquote(Wood~density~(g/cm^3))) +
                              scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_wd[[4]] <- traits_growth_plots_wd[[4]] + ylab(bquote(Aboveground~growth~(g/yr))) + xlab(bquote(Wood~density~(g/cm^3))) +
                              scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89))+
                              scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_wd[[5]] <- traits_growth_plots_wd[[5]] + ylab(bquote(Gross~investment~growth~(g/yr))) + xlab(bquote(Wood~density~(g/cm^3))) +
                              scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
wd_paper <- ggarrange(plotlist = traits_growth_plots_wd, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), vjust = 1, hjust = -3, 
                      font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("wd_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#figure 2: LMA against all GRS
traits_growth_plots_lma <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%  distinct(mean_ratio_leaf_stem, .keep_all = TRUE)
                                                                    %>% mutate(LMA = (LMA*10), mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                               mean_g_inv = (mean_g_inv*0.001))),
                                                            response = "LMA", GR = .x))
traits_growth_plots_lma[[1]] <- traits_growth_plots_lma[[1]] + ylab(bquote(Diameter~growth~(mm/yr))) + xlab(bquote(Leaf~mass~area~(kg/m^2))) +
                              theme(axis.title.x = element_text(colour="white"))+ 
                              scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_lma[[2]] <- traits_growth_plots_lma[[2]] + ylab(bquote(Height~growth~(mm/yr)))+ xlab(bquote(Leaf~mass~area~(kg/m^2))) +
                              theme(axis.title.x = element_text(colour="white")) +
                              scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_lma[[3]] <- traits_growth_plots_lma[[3]] + ylab(bquote(Leaf~area~growth~(mm^2/yr))) + xlab(bquote(Leaf~mass~area~(kg/m^2))) +
                              scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_lma[[4]] <- traits_growth_plots_lma[[4]] + ylab(bquote(Aboveground~growth~(g/yr))) + xlab(bquote(Leaf~mass~area~(kg/m^2))) +
                              scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_lma[[5]] <- traits_growth_plots_lma[[5]] + ylab(bquote(Gross~investment~growth~(g/yr))) + xlab(bquote(Leaf~mass~area~(kg/m^2))) +
                              scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
lma_paper <- ggarrange(plotlist = traits_growth_plots_lma, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), vjust = 1, hjust = -3, 
                       font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("lma_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#figure leaf mass/ total mass 
traits_growth_plots_lm_total <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%  distinct(mean_ratio_leaf_stem, .keep_all = TRUE) %>% 
                                                                                mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                       mean_g_inv = (mean_g_inv*0.001))),
                                                                      response = "mean_leaf_m_whole", GR = .x))
leaf_m_total <- ggarrange(plotlist = traits_growth_plots_lm_total, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), vjust = 1, hjust = -3, 
                         font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("leaf_m_total.jpeg", width = 30.1, height = 21.12, units = "cm")


#Figure 3: The correlations of the traits at species level traits = c("wood_density", "mean_LMA_s", "mean_ratio_leaf_stem_s")
#mean_LMA_s*10 is to get the units to kg/m^2 instead of g/cm^2
traits <- c("mean_LMA_s")
traits_cor <- map(traits, ~plotting_cors(data = (growth_data %>% 
                                                                distinct(mean_ratio_leaf_stem_s, .keep_all = TRUE) %>% 
                                                                mutate(mean_LMA_s = (mean_LMA_s*10))),
                                                      response = .x, GR = "mean_ratio_leaf_stem_s"))
traits_cor[[1]] <- traits_cor[[1]] + 
                              xlab(bquote(Leaf~mass~area~(kg/m^2))) + ylab(bquote(Wood~density~(g/cm^3))) +
                              scale_x_continuous(breaks = c(-1.1, -0.9, -0.7, -0.5, -0.3), label = c(0.08, 0.13, 0.2, 0.32, 0.5)) +
                              scale_y_continuous(breaks = c(-0.3, -0.2, -0.1, 0), label = c(0.5, 0.63, 0.79, 1))
traits_cor[[2]] <- traits_cor[[2]] + 
                              xlab(bquote(Leaf:stem~mass~(mg/mg^-1))) + ylab(bquote(Wood~density~(g/cm^3))) +
                              scale_x_continuous(breaks = c(0.5, 1), label = c(3.2, 10)) +
                              scale_y_continuous(breaks = c(-0.25, -0.2, -0.15, -0.1, -0.05), label = c(0.56, 0.63, 0.71, 0.79, 0.89))

traits_cor[[1]] <- traits_cor[[1]] + 
                              xlab(bquote(Leaf~mass~area~(kg/m^2))) + ylab(bquote(Leaf:stem~mass~(mg/mg^-1))) +
                              scale_x_continuous(breaks = c(-1.1, -0.9, -0.7, -0.5, -0.3), label = c(0.08, 0.13, 0.2, 0.32, 0.5)) +
                              scale_y_continuous(breaks = c(0, 0.5, 1), label = c(1, 3.2, 10))

WD_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 2, labels = c("a", "b"), vjust = 1, hjust = -3, font.label = list(size = 18))

ratio_LMA <- ggarrange(plotlist = traits_cor, legend = "none", labels = c("c"), vjust = 1, hjust = -3, font.label = list(size = 18),
                nrow=1, ncol = 2) 

all_trait_cors <- ggarrange(WD_others, ratio_LMA, nrow = 2)
ggsave("trait_cors.jpeg", width = 30.1, height = 21.12, units = "cm")

#GR correlations for species at age c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
GR_types_mean = c("mean_g_diameter")
GR_cor_mean <- map(GR_types_mean, ~plotting_cors(data = (growth_data %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                      mean_g_inv = (mean_g_inv*0.001)) %>% 
                                         distinct(mean_ratio_leaf_stem, .keep_all = TRUE)),
                                                        response = .x, GR = "mean_g_height"))
gross_inv_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4)

inv_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4)

la_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4)

height_others <- ggarrange(plotlist = GR_cor_mean, common.legend = TRUE, nrow=1, ncol = 4)

GR_cor_mean_plots <- ggarrange(gross_inv_others, inv_others, la_others, height_others, nrow = 4)
ggsave("GR_cor_mean_plots.jpeg", width = 33, height = 33, units = "cm")


#GR correlations for individuals GR_types_ind = c("growth_stem_diameter", "growth_height", "growth_leaf_area", "growth_inv", "gross_inv)
GR_types_ind = c("growth_stem_diameter")
GR_cor_ind <- map(GR_types_ind, ~plotting_cors(data = (growth_data%>% mutate(growth_inv = (growth_inv*0.001), 
                                                                         gross_inv = (gross_inv*0.001))),
                                       response = .x, GR = "growth_height"))
gross_inv_others_ind <- ggarrange(plotlist = GR_cor_ind, legend = "none", nrow=1, ncol = 4)

inv_others_ind <- ggarrange(plotlist = GR_cor_ind, legend = "none", nrow=1, ncol = 4)

la_others_ind <- ggarrange(plotlist = GR_cor_ind, legend = "none", nrow=1, ncol = 4)

height_others_ind <- ggarrange(plotlist = GR_cor_ind, common.legend = TRUE, nrow=1, ncol = 4)

GR_cor_ind_plots <- ggarrange(gross_inv_others_ind, inv_others_ind, la_others_ind, height_others_ind, nrow = 4)

#each trait with age not logging the trait 
growth_data %>% ggplot(aes(log10(age), (wood_density))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% mutate(LMA = (LMA*10)) %>% ggplot(aes(log10(age), (LMA))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% ggplot(aes(log10(age), (mean_P_area))) + geom_point()+
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% ggplot(aes(log10(age), (mean_N_area))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

# trait against age logging the trait 
growth_data %>% ggplot(aes(log10(age), log10(wood_density))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(-0.25, -0.2, -0.15, -0.1, -0.05), label = c(0.56, 0.63, 0.71, 0.79, 0.89))

growth_data %>% mutate(LMA = (LMA*10)) %>% ggplot(aes(log10(age), log10(LMA))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(-0.9, -0.6, -0.3), label = c(0.13, 0.25, 0.5))

growth_data %>% ggplot(aes(log10(age), log10(mean_P_area))) + geom_point()+
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10, 31.6, 100, 316.2, 1000))

growth_data %>% ggplot(aes(log10(age), log10(mean_N_area))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.2))

#GRs against age not logged
growth_data %>% ggplot(aes(log10(age), (mean_g_diameter))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% ggplot(aes(log10(age), (mean_g_height))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% ggplot(aes(log10(age), (mean_g_leaf_area))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                       mean_g_inv = (mean_g_inv*0.001)) %>% ggplot(aes(log10(age), (mean_g_inv))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

growth_data %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                       mean_g_inv = (mean_g_inv*0.001)) %>% ggplot(aes(log10(age), (mean_g_gross_inv))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))

#GRs against age logged
growth_data %>% ggplot(aes(log10(age), log10(mean_g_diameter))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.32, 1, 3.2))

growth_data %>% ggplot(aes(log10(age), log10(mean_g_height))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))+
  scale_y_continuous(breaks = c(1, 1.5, 2, 2.5), label = c(10, 31.6, 100, 316.2))

growth_data %>% ggplot(aes(log10(age), log10(mean_g_leaf_area))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32))+
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3), label = c(0.01, 1, 10, 100, 1000))

growth_data %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                       mean_g_inv = (mean_g_inv*0.001)) %>% ggplot(aes(log10(age), log10(mean_g_inv))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.01, 1, 10, 100))

growth_data %>% mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                       mean_g_inv = (mean_g_inv*0.001)) %>% ggplot(aes(log10(age), log10(mean_g_gross_inv))) + geom_point() +
  geom_smooth(method = 'loess') +
  scale_x_continuous(breaks = c(0.146, 0.380, 0.699, 0.845, 0.954, 1.505), label = c(1.4, 2.4, 5, 7, 9, 32)) +
  scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
