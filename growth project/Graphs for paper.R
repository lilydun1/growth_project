GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
traits = c("wood_density", "mean_leaf_m_whole", "mean_P_area", "mean_N_area", "LMA")
#Wood density against all GRS
traits_growth_plots_wd <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "wood_density", GR = .x))
traits_growth_plots_wd[[1]] <- traits_growth_plots_wd[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_blank()) 
traits_growth_plots_wd[[2]] <- traits_growth_plots_wd[[2]] + ylab(bquote(G[height]~(mm/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_blank()) 
traits_growth_plots_wd[[3]] <- traits_growth_plots_wd[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_blank())
traits_growth_plots_wd[[4]] <- traits_growth_plots_wd[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_blank())
traits_growth_plots_wd[[5]] <- traits_growth_plots_wd[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(WD~(g/cm^3))) 
wd_paper <- ggarrange(plotlist = traits_growth_plots_wd, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                      font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), vjust = 1) 

#LMA against all GRS
### change the scales here 
traits_growth_plots_lma <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                              group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                    response = "LMA", GR = .x))
traits_growth_plots_lma[[1]] <- traits_growth_plots_lma[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMA~(g/m^2))) +
                                theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[2]] <- traits_growth_plots_lma[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMA~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[3]] <- traits_growth_plots_lma[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(LMA~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[4]] <- traits_growth_plots_lma[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMA~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[5]] <- traits_growth_plots_lma[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(LMA~(g/m^2))) +
  theme(axis.title.y = element_blank()) 
lma_paper <- ggarrange(plotlist = traits_growth_plots_lma, common.legend = TRUE, labels = c("f", "g", "h", "i", "j"), 
                       font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), hjust = 0.29, vjust = 1)

#combining WD and LMA to one figure 
LMA_WD <- ggarrange(wd_paper, lma_paper)
ggsave("Fig 4_WD_LMA.jpeg", width = 23.91, height = 33.91, units = "cm")

#leaf mass/ total mass 
traits_growth_plots_lmf <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                                   group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                         response = "mean_leaf_m_whole", GR = .x))
traits_growth_plots_lmf[[1]] <- traits_growth_plots_lmf[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMF~(g/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[2]] <- traits_growth_plots_lmf[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMF~(g/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[3]] <- traits_growth_plots_lmf[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(LMF~(g/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[4]] <- traits_growth_plots_lmf[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMF~(g/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[5]] <- traits_growth_plots_lmf[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(LMF~(g/g)))
lmf_paper <- ggarrange(plotlist = traits_growth_plots_lmf, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                         font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), vjust = 0.8)

#Mean leaf P area against all GRs
traits_growth_plots_P <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_P_area", GR = .x))
traits_growth_plots_P[[1]] <- traits_growth_plots_P[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(P[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[2]] <- traits_growth_plots_P[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(P[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #+ scale_y_continuous(breaks = c(1.2, 1.7, 2.3))
traits_growth_plots_P[[3]] <- traits_growth_plots_P[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(P[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[4]] <- traits_growth_plots_P[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(P[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[5]] <- traits_growth_plots_P[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(P[area]~(g/m^2))) +
  theme(axis.title.y = element_blank()) 
P_paper <- ggarrange(plotlist = traits_growth_plots_P, common.legend = TRUE, labels = c("f", "g", "h", "i", "j"), 
                          font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), hjust = 0.29, vjust = 0.8)

#Mean leaf N area against all GRs
traits_growth_plots_N <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_N_area", GR = .x))
traits_growth_plots_N[[1]] <- traits_growth_plots_N[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(N[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[2]] <- traits_growth_plots_N[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(N[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #+ scale_y_continuous(breaks = c(1.2, 1.7, 2.3))
traits_growth_plots_N[[3]] <- traits_growth_plots_N[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(N[area]~(g/m^2)))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[4]] <- traits_growth_plots_N[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(N[area]~(g/m^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[5]] <- traits_growth_plots_N[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(N[area]~(g/m^2))) +
  theme(axis.title.y = element_blank()) 
N_paper <- ggarrange(plotlist = traits_growth_plots_N, common.legend = TRUE, labels = c("k", "l", "m", "n", "o"), 
                          font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), hjust = 0.29, vjust = 0.8)

ggarrange(lmf_paper, P_paper, N_paper, ncol = 3)
ggsave("Fig 6 LMF_P_N.jpeg", width = 23.91, height = 33.91, units = "cm")

#The correlations of the traits at species level traits = c("wood_density", "mean_LMA_s","mean_P_area_s", "mean_N_area_s", "mean_leaf_m_whole_s")
#c(bquote(WD~(g/cm^3)), bquote(LMA~(g/m^2)), bquote(P[area]~(g/m^2)), bquote(N[area]~(g/m^2)), bquote(LMF~(g/g)))

traits <- c("wood_density", "mean_LMA_s","mean_P_area_s", "mean_N_area_s")
traits_cor <- map2(traits, c(bquote(WD~(g/cm^3)), bquote(LMA~(g/m^2)), bquote(P[area]~(g/m^2)), bquote(N[area]~(g/m^2))), 
                   ~plotting_cors_trait(data = (growth_data %>% 
                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                      response = .x, GR = "mean_leaf_m_whole_s", x_label = .y) +
                      ylab(bquote(LMF~(g/g))))

leaf_whole_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                               font.label = list(size = 18))

N_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                      font.label = list(size = 18))

P_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                      font.label = list(size = 18))

LMA_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                        font.label = list(size = 18))

all_trait_cors <- ggarrange(leaf_whole_others, N_others, P_others, LMA_others, nrow = 4, align = "h")
ggsave("Fig 5 trait_cors.jpeg", width = 33, height = 33, units = "cm")

#GR correlations for species at age c("mean_g_gross_inv","mean_g_inv", "mean_g_leaf_area", "mean_g_height", "mean_g_diameter")
#c(bquote(G[total]~(g/yr)), bquote(G[net]~(g/yr)), bquote(G[area]~(mm^2/yr)), bquote(G[height]~(mm/yr)), bquote(G[diam]~(mm/yr)))

GR_types_mean <- c("mean_g_gross_inv","mean_g_inv", "mean_g_leaf_area", "mean_g_height")
GR_cor_mean <- map2(GR_types_mean, c(bquote(G[total]~(g/yr)), bquote(G[net]~(g/yr)), bquote(G[area]~(mm^2/yr)), bquote(G[height]~(mm/yr))), 
                    ~plotting_cors_GR(data = (growth_data %>% 
                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                     response = .x, GR = "mean_g_diameter", x_label = .y) +
                     ylabbquote(G[diam]~(mm/yr)))

diam_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                              font.label = list(size = 18))

height_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                        font.label = list(size = 18))

la_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                       font.label = list(size = 18))

net_others <- ggarrange(plotlist = GR_cor_mean,legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                           font.label = list(size = 18))

mine <- ggarrange(plotlist = GR_cor_mean,legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                  font.label = list(size = 18))
GR_cor_mean_plots <- ggarrange(diam_others, height_others, la_others, mine, nrow = 4,  align = "h")

ggsave("Fig 1 GR_cor_mean_plots.jpeg", width = 33, height = 33, units = "cm")

# trait against age logging the trait 
LMA_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, LMA)) + 
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(LMA~(g/m^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes((age), (LMA), col = Species_name), alpha = 0.2) +
    geom_line(aes((age), (LMA), col = Species_name), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("eq", "R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
          axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none") 

#not logging LMF
leaf_whole_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_leaf_m_whole)) + 
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(LMF~(g/g))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    geom_point(aes((age), (mean_leaf_m_whole), col = Species_name),  alpha = 0.2) +
    geom_line(aes((age), (mean_leaf_m_whole), col = Species_name), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none")

P_age <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_P_area)) + 
    #geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(P[area]~(g/m^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_P_area), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_P_area), col = species), alpha = 0.2, size = 0.8) +
  #stat_poly_eq(use_label(c("P")), size = 5, ) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")
  
N_age <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ungroup() %>% 
  ggplot(aes(age, mean_N_area)) +
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(N[area]~(g/m^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_N_area), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_N_area), col = species), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

trait_age_legend <- ggplot(growth_data, aes(x = age, y = mean_N_area, color = Species_name))+
  geom_point(alpha = 0.2)+
  geom_line(alpha = 0.2, size = 0.8) +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size =  12, face = "italic"),
        legend.title = element_text(size = 18)) +
  labs(colour = "Species") + 
  guides(color = guide_legend(nrow = 7))

traits_age <- ggarrange(LMA_age, leaf_whole_age, trait_age_legend, P_age, N_age, labels = c("a", "b", "","c", "d"), font.label = list(size = 18))
ggsave("Fig 3 traits_age.jpeg", width = 36, height = 25.3, units = "cm")

#GRs against age logged
diam_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>%
  ggplot(aes(age, mean_g_diameter)) + 
    stat_poly_eq(use_label(c("eq", "R2", "P")), size = 5, label.x.npc = "right", p.digits = 2) +
    geom_smooth(method = 'lm', col = "black") + 
    ylab(bquote(G[diam]~(mm/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_g_diameter), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_diameter), col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none")

height_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_height)) +
  stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", p.digits = 2) +
  geom_smooth(method = 'lm', colour = "black") + 
  ylab(bquote(G[height]~(mm/yr))) + 
  xlab(bquote(Age~(yrs))) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(aes((age), (mean_g_height), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_height), col = species), alpha = 0.2, size = 0.8) +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none")

la_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_leaf_area)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", p.digits = 2) +
    geom_smooth(method = 'lm', colour = "black", linetype = "dashed") + 
    ylab(bquote(G[area]~(mm^2/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_g_leaf_area), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_leaf_area), col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

inv_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_inv)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", p.digits = 2, coef.digits = 2) +
    geom_smooth(method = 'lm', colour = "black") + 
    ylab(bquote(G[net]~(g/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_y_log10(breaks = c(1, 10, 100)) +
    scale_x_log10() +
  geom_point(aes((age), (mean_g_inv), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_inv), col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

gross_inv_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_gross_inv)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", p.digits = 2) +
    geom_smooth(method = 'lm', colour = "black") + 
    ylab(bquote(G[total]~(g/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes(age, mean_g_gross_inv, col = species),  alpha = 0.2) +
  geom_line(aes(age, mean_g_gross_inv, col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

GR_age <- ggarrange(diam_age, height_age, la_age, inv_age, gross_inv_age, trait_age_legend, labels = c("a", "b", "c", "d", "e"), font.label = list(size = 18))
ggsave("Fig 2 GR_age.jpeg", width = 36, height = 25.3, units = "cm")


#Plotting the prediction of the model with the raw data points 
result_plots <- map(traits, ~plotting_predict(data = (growth_data %>%  distinct(mean_ratio_leaf_stem, .keep_all = TRUE)), 
                                              trait = .x))

result_plots[[1]] <- lapply(result_plots[[1]], function(plot) {
  plot + xlab(bquote(WD~(g/cm^3)))
})
result_plots[[2]] <- lapply(result_plots[[2]], function(plot) {
  plot + xlab(bquote(LMF~(g/g)))
})
result_plots[[3]] <- lapply(result_plots[[3]], function(plot) {
  plot + xlab(bquote(P[area]~(g/m^2)))
})
result_plots[[4]] <- lapply(result_plots[[4]], function(plot) {
  plot + xlab(bquote(N[area]~(g/m^2)))
})
result_plots[[5]] <- lapply(result_plots[[5]], function(plot) {
  plot + xlab(bquote(LMA~(g/m^2)))
})

result_plots[[1]][[1]] <- result_plots[[1]][[1]] + ylab(bquote(G[diam]~(mm/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[2]][[1]] <- result_plots[[2]][[1]] + ylab(bquote(G[diam]~(mm/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[3]][[1]] <- result_plots[[3]][[1]] + ylab(bquote(G[diam]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[4]][[1]] <- result_plots[[4]][[1]] + ylab(bquote(G[diam]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[5]][[1]] <- result_plots[[5]][[1]] + ylab(bquote(G[diam]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 


result_plots[[1]][[2]] <- result_plots[[1]][[2]] + ylab(bquote(G[height]~(mm/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[2]][[2]] <- result_plots[[2]][[2]] + ylab(bquote(G[height]~(mm/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[3]][[2]] <- result_plots[[3]][[2]] + ylab(bquote(G[height]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[4]][[2]] <- result_plots[[4]][[2]] + ylab(bquote(G[height]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[5]][[2]] <- result_plots[[5]][[2]] + ylab(bquote(G[height]~(mm/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

result_plots[[1]][[3]] <- result_plots[[1]][[3]] + ylab(bquote(G[area]~(mm^2/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[2]][[3]] <- result_plots[[2]][[3]] + ylab(bquote(G[area]~(mm^2/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[3]][[3]] <- result_plots[[3]][[3]] + ylab(bquote(G[area]~(mm^2/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[4]][[3]] <- result_plots[[4]][[3]] + ylab(bquote(G[area]~(mm^2/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[5]][[3]] <- result_plots[[5]][[3]] + ylab(bquote(G[area]~(mm^2/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

result_plots[[1]][[4]] <- result_plots[[1]][[4]] + ylab(bquote(G[net]~(g/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[2]][[4]] <- result_plots[[2]][[4]] + ylab(bquote(G[net]~(g/yr))) +
  theme(axis.title.x = element_blank()) 
result_plots[[3]][[4]] <- result_plots[[3]][[4]] + ylab(bquote(G[net]~(g/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[4]][[4]] <- result_plots[[4]][[4]] + ylab(bquote(G[net]~(g/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
result_plots[[5]][[4]] <- result_plots[[5]][[4]] + ylab(bquote(G[net]~(g/yr))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 

result_plots[[1]][[5]] <- result_plots[[1]][[5]] + ylab(bquote(G[total]~(g/yr)))
result_plots[[2]][[5]] <- result_plots[[2]][[5]] + ylab(bquote(G[total]~(g/yr)))
result_plots[[3]][[5]] <- result_plots[[3]][[5]] + ylab(bquote(G[total]~(g/yr)))  +
  theme(axis.title.y = element_blank())
result_plots[[4]][[5]] <- result_plots[[4]][[5]] + ylab(bquote(G[total]~(g/yr))) +
  theme(axis.title.y = element_blank()) 
result_plots[[5]][[5]] <- result_plots[[5]][[5]] + ylab(bquote(G[total]~(g/yr))) +
  theme(axis.title.y = element_blank())

WD_model <- ggarrange(plotlist = result_plots[[1]], labels = c("a", "b", "c", "d", "e"), 
                      font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), common.legend = T, vjust = 1)
LMF_model <- ggarrange(plotlist = result_plots[[2]], labels = c("a", "b", "c", "d", "e"), 
                       font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), common.legend = T, vjust = 1)
P_model <- ggarrange(plotlist = result_plots[[3]], labels = c("f", "g", "h", "i", "j"), 
                     font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), common.legend = T, hjust = 0.29, vjust = 1)
N_model <- ggarrange(plotlist = result_plots[[4]], labels = c("k", "l", "m", "n", "o"), 
                     font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), common.legend = T, hjust = 0.29, vjust = 1)
LMA_model <- ggarrange(plotlist = result_plots[[5]], labels = c("f", "g", "h", "i", "j"), 
                       font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), common.legend = T, hjust = 0.29, vjust = 1)

ggarrange(WD_model, LMA_model)
ggsave("Fig S4 model_WD_LMA.jpeg", width = 24, height = 33.91, units = "cm")

ggarrange(LMF_model, P_model, N_model, ncol = 3, nrow = 1)
ggsave("Fig S5 model_LMF_P_N.jpeg", width = 23.91, height = 33.91, units = "cm")

#making site map
basemap <- get_map('Kuringai National Park', zoom = 12, maptype = "satellite")

data <- read.csv("data/sites.csv") %>% 
  filter(age_at_harvest != 3) %>% 
  mutate(age_at_harvest = case_when(age_at_harvest == 1.35 ~ 1.4, TRUE~age_at_harvest))

map <- ggmap(basemap) + 
  labs(x = "Longitude", y = "Latitude") +
  geom_point(data = data, aes(x=Longitude, y=Latitude), 
             colour = "red") +
  geom_label_repel(
    data = data,
    aes(x = Longitude, y = Latitude, label = data$age_at_harvest),
    box.padding = unit(0.5, "lines")) 

ggsave("Fig S1 map.jpeg", width = 15, height = 15, units = "cm")

#doing P and N as mass
traits_growth_plots_P_mass <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                   response = "mean_P", GR = .x))
traits_growth_plots_P_mass[[1]] <- traits_growth_plots_P_mass[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(P[mass]~(mg/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_P_mass[[2]] <- traits_growth_plots_P_mass[[2]] + ylab(bquote(G[height]~(mm/yr))) + xlab(bquote(P[mass]~(mg/g))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_P_mass[[3]] <- traits_growth_plots_P_mass[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(P[mass]~(mg/g))) +
  theme(axis.title.x = element_blank())
traits_growth_plots_P_mass[[4]] <- traits_growth_plots_P_mass[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(P[mass]~(mg/g))) +
  theme(axis.title.x = element_blank())
traits_growth_plots_P_mass[[5]] <- traits_growth_plots_P_mass[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(P[mass]~(mg/g))) 
P_mass_paper <- ggarrange(plotlist = traits_growth_plots_P_mass, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                      font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), vjust = 1)

traits_growth_plots_N_mass <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                              group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                    response = "mean_N", GR = .x))
traits_growth_plots_N_mass[[1]] <- traits_growth_plots_N_mass[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(N[mass]~(mg/g))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N_mass[[2]] <- traits_growth_plots_N_mass[[2]] + ylab(bquote(G[height]~(mm/yr))) + xlab(bquote(N[mass]~(mg/g))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N_mass[[3]] <- traits_growth_plots_N_mass[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(N[mass]~(mg/g))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
traits_growth_plots_N_mass[[4]] <- traits_growth_plots_N_mass[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(N[mass]~(mg/g))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N_mass[[5]] <- traits_growth_plots_N_mass[[5]] + ylab(bquote(G[total]~(g/yr))) + xlab(bquote(N[mass]~(mg/g))) +
  theme(axis.title.y = element_blank())
N_mass_paper <- ggarrange(plotlist = traits_growth_plots_N_mass, common.legend = TRUE, labels = c("f", "g", "h", "i", "j"), 
                          font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"), hjust = 0.29, vjust = 1)

P_N_mass <- ggarrange(P_mass_paper, N_mass_paper)
ggsave("Fig S2 P_N_mass.jpeg", width = 30, height = 33.91, units = "cm")

#mass : The correlations of the traits at species level traits = c("wood_density", "mean_LMA_s","mean_P_s", "mean_N_s", "mean_leaf_m_whole_s")
#c(bquote(WD~(g/cm^3)), bquote(LMA~(g/m^2)), bquote(P[mass]~(mg/g)), bquote(N[mass]~(g/g)), bquote(LMF~(g/g)))

traits_mass <- c("wood_density", "mean_LMA_s","mean_P_s", "mean_N_s")
traits_cor_mass <- map2(traits_mass, c(bquote(WD~(g/cm^3)), bquote(LMA~(g/m^2)), bquote(P[mass]~(mg/g)), bquote(N[mass]~(mg/g))), 
                        ~plotting_cors(data = (growth_data %>% 
                                                 group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                       response = .x, GR = "mean_leaf_m_whole_s", x_label = .y) +
                          ylab(bquote(LMF~(g/g))))

#remember that the y axis of this one should not be logged
leaf_whole_others_mass <- ggarrange(plotlist = traits_cor_mass, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                                    font.label = list(size = 18))

#remember this onwards need logging
N_others_mass <- ggarrange(plotlist = traits_cor_mass, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                           font.label = list(size = 18))

P_others_mass <- ggarrange(plotlist = traits_cor_mass, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                           font.label = list(size = 18))

LMA_others_mass <- ggarrange(plotlist = traits_cor_mass, legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                             font.label = list(size = 18))

all_trait_cor_mass <- ggarrange(leaf_whole_others_mass, N_others_mass, P_others_mass, LMA_others_mass, nrow = 4, align = "h")
ggsave("Fig S3 trait_cors_mass.jpeg", width = 33, height = 33, units = "cm")


LMA_height <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(height, LMA)) + 
  geom_smooth(method = 'lm', color = "black") + 
  ylab(bquote(LMA~(g/m^2))) + 
  xlab(bquote(Height~(cm))) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(aes((height), (LMA), col = Species_name), alpha = 0.2) +
  geom_line(aes((height), (LMA), col = Species_name), alpha = 0.2, size = 0.8) +
  #stat_poly_eq(use_label(c("eq", "R2", "P"))) +
  stat_poly_eq(use_label(c("eq", "R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none") 

leaf_whole_height <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(height, mean_leaf_m_whole)) + 
  geom_smooth(method = 'lm', color = "black") + 
  ylab(bquote(LMF~(g/g))) + 
  xlab(bquote(Height~(cm))) +
  scale_x_log10() +
  geom_point(aes((height), (mean_leaf_m_whole), col = Species_name),  alpha = 0.2) +
  geom_line(aes((height), (mean_leaf_m_whole), col = Species_name), alpha = 0.2, size = 0.8) +
  stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none")

P_height <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(height, mean_P_area)) + 
  #geom_smooth(method = 'lm', color = "black") + 
  ylab(bquote(P[area]~(g/m^2))) + 
  xlab(bquote(Height~(cm))) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(aes((height), (mean_P_area), col = species),  alpha = 0.2) +
  geom_line(aes((height), (mean_P_area), col = species), alpha = 0.2, size = 0.8) +
  stat_poly_eq(use_label(c("P")), size = 5, ) +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

N_height <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ungroup() %>% 
  ggplot(aes(height, mean_N_area)) +
  geom_smooth(method = 'lm', color = "black") + 
  ylab(bquote(N[area]~(g/m^2))) + 
  xlab(bquote(Height~(cm))) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(aes((height), (mean_N_area), col = species),  alpha = 0.2) +
  geom_line(aes((height), (mean_N_area), col = species), alpha = 0.2, size = 0.8) +
  stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
  theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

trait_height_legend <- ggplot(growth_data, aes(x = height, y = mean_N_area, color = Species_name))+
  geom_point(alpha = 0.2)+
  geom_line(alpha = 0.2, size = 0.8) +
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size =  12, face = "italic"),
        legend.title = element_text(size = 18)) +
  labs(colour = "Species") + 
  guides(color = guide_legend(nrow = 7))

traits_height <- ggarrange(LMA_height, leaf_whole_height, trait_height_legend, P_height, N_height, labels = c("a", "b", "","c", "d"), font.label = list(size = 18))
ggsave("traits_height.jpeg", width = 36, height = 25.3, units = "cm")
