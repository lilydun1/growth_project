GR_types_all = c("mean_g_diameter", "mean_g_height", "mean_g_leaf_area", "mean_g_inv", "mean_g_gross_inv")
traits = c("wood_density", "mean_leaf_m_whole", "mean_P_area", "mean_N_area", "LMA")
#figure 4: Wood density against all GRS
traits_growth_plots_wd <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data%>% 
                                                                             mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                    mean_g_inv = (mean_g_inv*0.001)) %>%
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
traits_growth_plots_wd[[5]] <- traits_growth_plots_wd[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(WD~(g/cm^3))) 
wd_paper <- ggarrange(plotlist = traits_growth_plots_wd, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                      font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v")) 

#figure 4: LMA against all GRS
### change the scales here 
traits_growth_plots_lma <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data%>% 
                                                                              mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                     mean_g_inv = (mean_g_inv*0.001)) %>%
                                                                              group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                    response = "LMA", GR = .x))
traits_growth_plots_lma[[1]] <- traits_growth_plots_lma[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMA~(g/cm^2))) +
                                theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[2]] <- traits_growth_plots_lma[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMA~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[3]] <- traits_growth_plots_lma[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(LMA~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[4]] <- traits_growth_plots_lma[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMA~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_lma[[5]] <- traits_growth_plots_lma[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(LMA~(g/cm^2))) +
  theme(axis.title.y = element_blank()) 
lma_paper <- ggarrange(plotlist = traits_growth_plots_lma, common.legend = TRUE, labels = c("f", "g", "h", "i", "j"), 
                       font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"))

#combining WD and LMA to one figure 
LMA_WD <- ggarrange(wd_paper, lma_paper)
ggsave("figure_4_WD_LMA.jpeg", width = 23.91, height = 33.91, units = "cm")

#figure leaf mass/ total mass 
traits_growth_plots_lmf <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                                   mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                          mean_g_inv = (mean_g_inv*0.001))%>% 
                                                                                   group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                         response = "mean_leaf_m_whole", GR = .x))
traits_growth_plots_lmf[[1]] <- traits_growth_plots_lmf[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(LMF~(mg/mg^-1))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[2]] <- traits_growth_plots_lmf[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(LMF~(mg/mg^-1))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[3]] <- traits_growth_plots_lmf[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(LMF~(mg/mg^-1))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[4]] <- traits_growth_plots_lmf[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(LMF~(mg/mg^-1))) +
  theme(axis.title.x = element_blank()) 
traits_growth_plots_lmf[[5]] <- traits_growth_plots_lmf[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(LMF~(mg/mg^-1)))
lmf_paper <- ggarrange(plotlist = traits_growth_plots_lmf, common.legend = TRUE, labels = c("a", "b", "c", "d", "e"), 
                         font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"))

#Mean leaf P area against all GRs
traits_growth_plots_P <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>% 
                                                                            mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                   mean_g_inv = (mean_g_inv*0.001))%>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_P_area", GR = .x))
traits_growth_plots_P[[1]] <- traits_growth_plots_P[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(P[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[2]] <- traits_growth_plots_P[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(P[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_y_continuous(breaks = c(1.2, 1.7, 2.3))
traits_growth_plots_P[[3]] <- traits_growth_plots_P[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(P[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[4]] <- traits_growth_plots_P[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(P[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_P[[5]] <- traits_growth_plots_P[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(P[area]~(g/cm^2))) +
  theme(axis.title.y = element_blank()) 
P_paper <- ggarrange(plotlist = traits_growth_plots_P, common.legend = TRUE, labels = c("f", "g", "h", "i", "j"), 
                          font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"))

#Mean leaf N area against all GRs
traits_growth_plots_N <- map(GR_types_all, ~plotting_trait_growth(data = (growth_data %>%
                                                                            mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                                   mean_g_inv = (mean_g_inv*0.001)) %>% 
                                                                            group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                                                                  response = "mean_N_area", GR = .x))
traits_growth_plots_N[[1]] <- traits_growth_plots_N[[1]] + ylab(bquote(G[diam]~(mm/yr))) + xlab(bquote(N[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[2]] <- traits_growth_plots_N[[2]] + ylab(bquote(G[height]~(mm/yr)))+ xlab(bquote(N[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_y_continuous(breaks = c(1.2, 1.7, 2.3))
traits_growth_plots_N[[3]] <- traits_growth_plots_N[[3]] + ylab(bquote(G[area]~(mm^2/yr))) + xlab(bquote(N[area]~(g/cm^2)))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[4]] <- traits_growth_plots_N[[4]] + ylab(bquote(G[net]~(g/yr))) + xlab(bquote(N[area]~(g/cm^2))) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
traits_growth_plots_N[[5]] <- traits_growth_plots_N[[5]] + ylab(bquote(G[gross]~(g/yr))) + xlab(bquote(N[area]~(g/cm^2))) +
  theme(axis.title.y = element_blank()) 
N_paper <- ggarrange(plotlist = traits_growth_plots_N, common.legend = TRUE, labels = c("k", "l", "m", "n", "o"), 
                          font.label = list(size = 18), ncol = 1, nrow = 5, align = c("v"))

ggarrange(lmf_paper, P_paper, N_paper, ncol = 3)
ggsave("figure_5_LMF_P_N.jpeg", width = 23.91, height = 33.91, units = "cm")

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
#c(bquote(G[gross]~(g/yr)), bquote(G[net]~(g/yr)), bquote(G[area]~(mm^2/yr)), bquote(G[height]~(mm/yr)), bquoteG[diam]~(mm/yr)))

GR_types_mean = c("mean_g_gross_inv","mean_g_inv")
GR_cor_mean <- map2(GR_types_mean, c(bquote(G[gross]~(g/yr)), bquote(G[net]~(g/yr))), 
                    ~plotting_cors(data = (growth_data %>% 
                                             mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
                                                                         mean_g_inv = (mean_g_inv*0.001)) %>% 
                                             group_by(age, species) %>% distinct(age, .keep_all = TRUE)),
                     response = .x, GR = "mean_g_leaf_area", x_label = .y) +
                     ylab(bquote(G[area]~(mm^2/yr))))

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
    #geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(P[area]~(g/cm^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_P_area), col = species),  alpha = 0.2)+
  geom_line(aes((age), (mean_P_area), col = species), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("P")), size = 5, label.x.npc = "right", label.y = "top") +
    theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
        axis.text = element_text(size=12), legend.position="none")
  
N_age <- growth_data %>% 
  filter(age != 5) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_N_area)) +
    #geom_smooth(method = 'lm', color = "black") + 
    ylab(bquote(N[area]~(g/cm^2))) + 
    xlab(bquote(Age~(yrs))) +
    scale_x_log10() +
    scale_y_log10() +
  geom_point(aes((age), (mean_N_area), col = species),  alpha = 0.2) +
  geom_line(aes((age), (mean_N_area), col = species), alpha = 0.2, size = 0.8) +
    stat_poly_eq(use_label(c("P")), size = 5, label.x.npc = "right", label.y = "top") +
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

traits_age <- ggarrange(LMA_age, leaf_whole_age, P_age, N_age, WD_age, trait_age_legend, labels = c("a", "b", "c", "d", "e"), font.label = list(size = 18))
ggsave("traits_age.jpeg", width = 36, height = 25.3, units = "cm")


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
  mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
         mean_g_inv = (mean_g_inv*0.001)) %>% 
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
  mutate(mean_g_gross_inv = (mean_g_gross_inv*0.001), 
         mean_g_inv = (mean_g_inv*0.001)) %>% 
  group_by(age, species) %>% 
  distinct(age, .keep_all = TRUE) %>% 
  ggplot(aes(age, mean_g_gross_inv)) + 
    stat_poly_eq(use_label(c("eq","R2", "P")), size = 5, label.x.npc = "right", p.digits = 2) +
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

GR_age <- ggarrange(diam_age, height_age, la_age, inv_age, gross_inv_age, trait_age_legend, labels = c("a", "b", "c", "d", "e"), font.label = list(size = 18))
ggsave("GR_age.jpeg", width = 36, height = 25.3, units = "cm")


#Plotting the prediction of the model with the raw data points 
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
          ggplot(aes((x), log10(predicted), colour = (group)), 
                 data = ggpredict(list_stats_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = log10(.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      } else {
        plot_stats_list[[i]] <- 
          ggplot(aes((x), log10(predicted), colour = (group)), 
                 data = ggpredict(list_stats_no_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          geom_point(data = data, aes(x = (.data[[trait]]), y = log10(.data[[GR_types_all[i]]]), colour = as.factor(age))) +
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
          ggplot(aes(log10(x), log10(predicted), colour = (group)), 
                 data = ggpredict(list_stats_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          geom_point(data = data, aes(x = log10(.data[[trait]]), y = log10(.data[[GR_types_all[i]]]), colour = as.factor(age))) +
          labs(x = paste("log10(", trait, ")"), y = paste("log10(", GR_types_all[i], ")"), colour = "Age") +
          theme(text = element_text(size = 18),legend.text=element_text(size=18), panel.background = element_blank(), 
                axis.line = element_line(colour = "black"), legend.key=element_rect(fill="white"), 
                axis.text = element_text(size=12)) +
          labs(colour = "Age (yrs)") +
          scale_color_manual(values=c("#c35f5d", "#e5874d","#b3a034", "#12a388", "#81d0e2", "#8282b4"))
      }
      else {
        plot_stats_list[[i]] <- 
          ggplot(aes(log10(x), log10(predicted), colour = (group)), 
                 data = ggpredict(list_stats_no_inter[[i]], terms = c(paste(trait, "[all]"), "age"))) +
          geom_line() +
          geom_point(data = data, aes(x = log10(.data[[trait]]), y = log10(.data[[GR_types_all[i]]]), colour = as.factor(age))) +
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

result_plots <- map(traits, ~plotting_predict(data = (growth_data %>%  distinct(mean_ratio_leaf_stem, .keep_all = TRUE)), 
                                              trait = .x))
label <- "label"
for (i in seq_along(result_plots[[1]])) {
  result_plots[[1]][2] <- result_plots[[1]][1] +
    scale_x_log10() +
    scale_y_log10() 
}


ggarrange(plotlist = result_plots[[1]], common.legend = TRUE)
ggsave("model_WD.jpeg", width = 33, height = 33, units = "cm")
ggarrange(plotlist = result_plots[[2]], common.legend = TRUE)
ggsave("model_LMF.jpeg", width = 33, height = 33, units = "cm")
ggarrange(plotlist = result_plots[[3]], common.legend = TRUE)
ggsave("model_P.jpeg", width = 33, height = 33, units = "cm")
ggarrange(plotlist = result_plots[[4]], common.legend = TRUE)
ggsave("model_N.jpeg", width = 33, height = 33, units = "cm")
ggarrange(plotlist = result_plots[[5]], common.legend = TRUE)
ggsave("model_LMA.jpeg", width = 33, height = 33, units = "cm")

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

ggsave("map.jpeg", width = 15, height = 15, units = "cm")
