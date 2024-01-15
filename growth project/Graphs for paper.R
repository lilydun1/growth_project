GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
#figure 1: Wood density against all GRS
traits_growth_plots_wd <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data%>% 
                                                                             mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                    mean_g_inv = (mean_g_inv*0.001)) %>%
                                                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "wood_density", GR = .x))
traits_growth_plots_wd[[1]] <- traits_growth_plots_wd[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_text(colour="white")) 
                              #scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              #scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_wd[[2]] <- traits_growth_plots_wd[[2]] + ylab(bquote(G[height]~(mm/yr))) + xlab(bquote(WD~(g/cm^3))) +
                              theme(axis.title.x = element_text(colour="white")) 
                              #scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              #scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_wd[[3]] <- traits_growth_plots_wd[[3]] + ylab(bquote(G[leaf~area]~(mm^2/yr))) + xlab(bquote(WD~(g/cm^3))) 
                              #scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_wd[[4]] <- traits_growth_plots_wd[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(WD~(g/cm^3))) 
                              #scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89))+
                              #scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_wd[[5]] <- traits_growth_plots_wd[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(WD~(g/cm^3))) 
                              #scale_x_continuous(label = c(0.56, 0.63, 0.7, 0.79, 0.89)) +
                              #scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
wd_paper <- ggarrange(plotlist = traits_growth_plots_wd, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                      font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("wd_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#figure 2: LMA against all GRS
### change the scales here 
traits_growth_plots_lma <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data%>% 
                                                                              mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                     mean_g_inv = (mean_g_inv*0.001)) %>%
                                                                              group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                    response = "LMA", GR = .x))
traits_growth_plots_lma[[1]] <- traits_growth_plots_lma[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMA~(g/cm^2))) +
                              theme(axis.title.x = element_text(colour="white"))
                              #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              #scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_lma[[2]] <- traits_growth_plots_lma[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMA~(g/cm^2))) +
                              theme(axis.title.x = element_text(colour="white")) 
                              #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              #scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_lma[[3]] <- traits_growth_plots_lma[[3]] + ylab(bquote(G[leaf~area]~(mm^2/yr))) + xlab(bquote(LMA~(g/cm^2))) 
                              #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_lma[[4]] <- traits_growth_plots_lma[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMA~(g/cm^2))) 
                              #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              #scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_lma[[5]] <- traits_growth_plots_lma[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(LMA~(g/cm^2))) 
                              #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                              #scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
lma_paper <- ggarrange(plotlist = traits_growth_plots_lma, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                       font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("lma_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#figure leaf mass/ total mass 
# REMEMBER TO NOT LOG IT 
traits_growth_plots_lmf <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                                   mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                          mean_g_inv = (mean_g_inv*0.001))%>% 
                                                                                   group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                         response = "mean_leaf_m_whole", GR = .x))
traits_growth_plots_lmf[[1]] <- traits_growth_plots_lmf[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMF~(mg/mg^-1))) +
                                  theme(axis.title.x = element_text(colour="white"))
                                  #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                                  #scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_lmf[[2]] <- traits_growth_plots_lmf[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMF~(mg/mg^-1))) +
                                  theme(axis.title.x = element_text(colour="white")) 
                                  #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                                  #scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_lmf[[3]] <- traits_growth_plots_lmf[[3]] + ylab(bquote(G[leaf~area]~(mm^2/yr))) + xlab(bquote(LMF~(mg/mg^-1))) 
                                  #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                                  #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_lmf[[4]] <- traits_growth_plots_lmf[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMF~(mg/mg^-1)))
                                  #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                                  #scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_lmf[[5]] <- traits_growth_plots_lmf[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(LMF~(mg/mg^-1)))
                                  #scale_x_continuous(breaks = c(-1.2, -0.9, -0.6, -0.3), label = c(0.06, 0.13, 0.25, 0.5)) +
                                  #scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
lmf_paper <- ggarrange(plotlist = traits_growth_plots_lmf, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                         font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("lmf_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#Mean leaf P area against all GRs
traits_growth_plots_P <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                            mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                   mean_g_inv = (mean_g_inv*0.001))%>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_P_area", GR = .x))
traits_growth_plots_P[[1]] <- traits_growth_plots_P[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(P[area]~(g/cm^2))) +
                            theme(axis.title.x = element_text(colour="white"))
                            #scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10,31.6, 100, 316.2, 1000)) +
                            #scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_P[[2]] <- traits_growth_plots_P[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(P[area]~(g/cm^2))) +
                              theme(axis.title.x = element_text(colour="white")) 
                            #scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10,31.6, 100, 316.2, 1000)) +
                            #scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_P[[3]] <- traits_growth_plots_P[[3]] + ylab(bquote(G[leaf~area]~(mm^2/yr))) + xlab(bquote(P[area]~(g/cm^2))) 
                            #scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10,31.6, 100, 316.2, 1000)) +
                            #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_P[[4]] <- traits_growth_plots_P[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(P[area]~(g/cm^2))) 
                            #scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10,31.6, 100, 316.2, 1000)) +
                            #scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_P[[5]] <- traits_growth_plots_P[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(P[area]~(g/cm^2))) 
                            #scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), label = c(10,31.6, 100, 316.2, 1000)) +
                            #scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
P_paper <- ggarrange(plotlist = traits_growth_plots_P, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                          font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("P_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#Mean leaf N area against all GRs
traits_growth_plots_N <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                            mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                   mean_g_inv = (mean_g_inv*0.001)) %>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_N_area", GR = .x))
traits_growth_plots_N[[1]] <- traits_growth_plots_N[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(N[area]~(g/cm^2))) +
                            theme(axis.title.x = element_text(colour="white"))
                            #scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.16)) +
                            #scale_y_continuous(breaks = c(-0.5, 0, 0.5), label = c(0.31, 1, 3.1))
traits_growth_plots_N[[2]] <- traits_growth_plots_N[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(N[area]~(g/cm^2))) +
                            theme(axis.title.x = element_text(colour="white")) 
                            #scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.16)) +
                            #scale_y_continuous(breaks = c(1, 1.5, 2, 2.5),label = c(10, 31, 100, 316))
traits_growth_plots_N[[3]] <- traits_growth_plots_N[[3]] + ylab(bquote(G[leaf~area]~(mm^2/yr))) + xlab(bquote(N[area]~(g/cm^2))) 
                          #scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.16)) +
                          #scale_y_continuous(breaks = c(-1, 0, 1, 2, 3),label = c(0.1, 1, 10, 100, 1000))
traits_growth_plots_N[[4]] <- traits_growth_plots_N[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(N[area]~(g/cm^2))) 
                            #scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.16)) +
                            #scale_y_continuous(breaks = c(-1, 0, 1, 2), label = c(0.1, 1, 10, 100))
traits_growth_plots_N[[5]] <- traits_growth_plots_N[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(N[area]~(g/cm^2))) 
                          #scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5), label = c(0.1, 0.32, 1, 3.16)) +
                          #scale_y_continuous(breaks = c(0, 1, 2), label = c(1, 10, 100))
N_paper <- ggarrange(plotlist = traits_growth_plots_N, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), vjust = 1, hjust = -3, 
                          font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("N_paper.jpeg", width = 30.1, height = 21.12, units = "cm")

#Figure 3: The correlations of the traits at species level traits = c("wood_density", "mean_LMA_s","mean_P_area_s", "mean_N_area_s", "mean_leaf_m_whole_s")
#c(bquote(WD~(g/cm^3)), bquote(LMA~(g/cm^2)), bquote(P[area]~(g/cm^2)), bquote(N[area]~(g/cm^2)), bquote(LMF~(mg/mg^-1)))

traits <- c("wood_density")
traits_cor <- map2(traits, c(bquote(WD~(g/cm^3))), 
                   ~plotting_cors(data = (growth_data %>% 
                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                      response = .x, GR = "mean_LMA_s", x_label = .y) +
                      ylab(bquote(LMA~(g/cm^2))))

#remember that the y axis of this one should not be logged
leaf_whole_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                               font.label = list(size = 18))

#remember this onwards need logging
N_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                      font.label = list(size = 18))

P_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                      font.label = list(size = 18))

LMA_others <- ggarrange(plotlist = traits_cor, legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                        font.label = list(size = 18))

all_trait_cors <- ggarrange(leaf_whole_others, N_others, P_others, LMA_others, nrow = 4, align = "h")
ggsave("trait_cors.jpeg", width = 33, height = 33, units = "cm")

#GR correlations for species at age c("mean_g_gross_inv","mean_g_inv", "mean_g_leaf_area", "mean_g_height", "mean_g_diameter")
#c(bquote(G[gross]~(g/yr)), bquote(G[net]~(g/yr)), bquote(G[leaf~area]~(mm^2/yr)), bquote(G[height]~(mm/yr)), bquoteG[diam]~(mm/yr)))

GR_types_mean = c("mean_g_gross_inv")
GR_cor_mean <- map2(GR_types_mean, c(bquote(G[gross]~(g/yr))), 
                    ~plotting_cors(data = (growth_data %>% 
                                             mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                         mean_g_inv = (mean_g_inv*0.001)) %>% 
                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                     response = .x, GR = "mean_g_inv", x_label = .y)+
                     ylab(bquote(G[net]~(g/yr))))

diam_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("a", "b", "c", "d"), 
                              font.label = list(size = 18))

height_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("e", "f", "g"), 
                        font.label = list(size = 18))

la_others <- ggarrange(plotlist = GR_cor_mean, legend = "none", nrow=1, ncol = 4, labels = c("h", "i"), 
                       font.label = list(size = 18))

net_others <- ggarrange(plotlist = GR_cor_mean,legend = "none", nrow=1, ncol = 4, labels = c("j"), 
                           font.label = list(size = 18))

GR_cor_mean_plots <- ggarrange(diam_others, height_others, la_others, net_others, nrow = 4,  align = "h")

ggsave("GR_cor_mean_plots.jpeg", width = 33, height = 33, units = "cm")

# trait against age logging the trait 
WD_age <- growth_data %>% 
  filter(age %in% c(7,9, 32)) %>% 
  group_by(age, species) %>% distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes((age), (wood_density))) + geom_point() + 
    ylab(bquote(WD~(g/cm^3))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

LMA_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, LMA)) + 
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(LMA~(g/cm^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(aes((age), (LMA), col = Species_name), alpha = 0.2) +
    geom_line(aes((age), (LMA), col = Species_name), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("eq", "R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
          axis.text = element_text(size=12), axis.title.x = element_text(colour="white"), legend.position="none") 

leaf_whole_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_leaf_m_whole)) + 
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(LMF~(mg/mg^-1))) + 
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
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(P[area]~(g/cm^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_P_area), col = species),  alpha = 0.2)+
  geom_line(aes((age), (mean_P_area), col = species), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")
  
N_age <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_N_area)) +
    geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(N[area]~(g/cm^2))) + 
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
        legend.text = element_text(size =  12),
        legend.title = element_text(size = 15)) +
  labs(colour = "Species")+ 
  guides(color = guide_legend(nrow = 7))

traits_age <- ggarrange(LMA_age, leaf_whole_age, P_age, N_age, WD_age, trait_age_legend, labels = c("a", "b", "c", "d", "e"))
ggsave("traits_age.jpeg", width = 38, height = 22, units = "cm")

#GRs against age logged
diam_age <- growth_data %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>%
  ggplot(aes(age, mean_g_diameter)) + 
    stat_poly_eq(use_label(c("eq", "R2", "P")), size = 5, label.x.npc = "right") +
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
  stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right") +
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
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right") +
    geom_smooth(method = 'lm', colour = "black") + 
    ylab(bquote(G[leaf~area]~(mm^2/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_g_leaf_area), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_leaf_area), col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

inv_age <- growth_data %>% 
  mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
         mean_g_inv = (mean_g_inv*0.001)) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_inv)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right") +
    geom_smooth(method = 'lm', colour = "black") + 
    ylab(bquote(G[net]~(g/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_g_inv), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_g_inv), col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

gross_inv_age <- growth_data %>% 
  mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
         mean_g_inv = (mean_g_inv*0.001)) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_gross_inv)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right") +
    geom_smooth(method = 'lm', colour = "black") + 
    ylab(bquote(G[gross]~(g/yr))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes(age, mean_g_gross_inv, col = species),  alpha = 0.2) +
  geom_line(aes(age, mean_g_gross_inv, col = species), alpha = 0.2, size = 0.8) +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")

GR_age <- ggarrange(diam_age, height_age, la_age, inv_age, gross_inv_age, trait_age_legend, labels = c("a", "b", "c", "d", "e"), 
                    font.label = list(size = 18), ncol = 3, nrow = 2, align = c("v"))
ggsave("GR_age.jpeg", width = 45, height = 27, units = "cm")
