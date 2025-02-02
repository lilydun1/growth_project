# Import packages ----
# install.packages("pacman")
pacman::p_load(tidyverse, tidyr, ggpubr, directlabels, segmented, corrplot)

#for d = -log(0.2 * 1.4 + 1, base = 0.9)
#for h = -log(0.2 * 1.4 + 1, base = 0.9)
#for tw = -log(0.3 * 1.4 + 1, base = 0.8)

#### fit a curve for biomass vs. age ----

Calc_GrowthCurve <- function(age, m, n){
  # age: x-axis
  # m, n: parameters 
  leaf_area <- -log(n * age + 1, base = m) # adopted from Salomon et al 2017
  return(leaf_area)
}

# Defining the function to minimize

minSS <- function(par, data){
  out <- with(data, sum((data$leaf_area+log(par[2]*age+1, base=par[1]))^2))
  return(out)
}

# Calculating growth rates

Calc_GrowthRate <- function(std_age, m, n){  
  # std_age: standard age; w1, w2: parameters 
  c1 <- log(m)
  c2 <- c1 / n
  GrowthRate <- -(1 / (c1 * std_age + c2))  # calculated using derivation of 
  return(GrowthRate)
}

########## growth fits, values, and plots
GRValueList <- list()  # this will be the list of growth rate values

GCPlotList <- list()  # this will be a list of plots with a growth curve (biomass vs. age) fitted for all species
GRPlotList <- list()  # this will be a list of plots with a growth rate vs. age curve fitted for all species
GRFits <- list()

curve_growth_data_filter <- growth_data

LoopOver <- unique(curve_growth_data_filter$species)

for (ll in LoopOver){
  tmpe <- curve_growth_data_filter[curve_growth_data_filter$species == ll,]

  
  # Initialization values of parameters
  mypara = c(0.8, 0.3) #do not know if these will work for growth data but can try
  
  # Optimizing fitting by testing different parameters      
  result <- optim(par = mypara,  minSS, data=tmpe)
  
  # Extracting the parameters
  fitpara <- result$par
  m <- fitpara[1]
  n <- fitpara[2]
  
  # Create a dataframe with the fit line coordinates for CWR vs. Psi
  xfit <- seq(0, max(tmpe$age), 0.01)
  yfit <- Calc_GrowthCurve(xfit, m, n)
  yfit2 <- Calc_GrowthRate(xfit, m, n)
  fitline <- data.frame(xfit, yfit, yfit2)
  
  
  # Calculating (potentially maximum) growth rate at a standard age
  std_age <- 32 # (unit=year) standard age
  
  GrowthRate_std <- Calc_GrowthRate(std_age, m, n)
  
  # inflection point using segmented package
  # 
  # out.lm <- lm(leaf_area~age, data=tmpe)
  # o <- segmented(out.lm, seg.Z = ~age)
  # slope1 <- slope(o)$age[1]
  # slope2 <- slope(o)$age[2]
  # CI <- -o$psi[2]-c(1.96,-1.96)*o$psi[3]
  # # 
  # summary(o)
  # plot(o, conf.level=0.95, shade=TRUE)
  # points(std1$WaterPotential_MPa, std1$SapCWR)
  # points(o, link=TRUE, col=2)
  # plot(o,add=TRUE,link=FALSE,lwd=2,col=2:3, lty=c(1,3))
  # lines(o,col=2,pch=19,bottom=FALSE,lwd=2) #for the CI for the breakpoint
  #
  # Compiling the cappacitance values
  GRValueList[[ll]] <- data.frame("Spp"=ll, "m"=m, "n"=n,
                               "GrowthRate_at_std_age"=GrowthRate_std)
  
  # Visualization of the curve and data points
  
  #inflection <- data.frame(x = c(0, -o$psi[2]), xend = c(-o$psi[2], max(-tmpe$WaterPotential_MPa)),
                            #y = c(0,o$coefficients[1]), yend = c(o$coefficients[1], o$coefficients[1]-slope1*(max(-tmpe$WaterPotential_MPa)+o$psi[2])))
  
  GRFits[[ll]] <- fitline
  
  GCPlotList[[ll]] <- ggplot(tmpe, aes(age, leaf_area)) + #plot a graph
    geom_point(size=5, colour="black") + #colour the points for confidence in data
    geom_line(data = fitline, aes(xfit, yfit), colour = "black", linewidth=2.5, linetype="dashed") +
    #ggtitle(Species_names$Genus_Species[match(ll, Species_names$Spp)]) +
    labs(x = "age / year", y = "leaf_area")
  
  
  GRPlotList[[ll]] <- ggplot(tmpe) + #plot a graph
    geom_line(data = fitline, aes(xfit, yfit2), colour = "black", linewidth=2.5) +
    #ggtitle(Species_names$Genus_Species[match(ll, Species_names$Spp)]) +
    labs(x = "age / year", y = "growth rate / /year") 
}

  ############ Exporting species-specific growth values and plots
# Saving the curves and fits
pdf("GCFits.pdf")
for (ll in LoopOver){
  print(GCPlotList[[ll]])
}
dev.off()

pdf("GRFits.pdf")
for (ll in LoopOver){
  print(GRPlotList[[ll]])
}
dev.off()

# Saving the growth rate values
GRValues_la_32 <- do.call(rbind, GRValueList)  # compiling

GRValues_d <- GRValues_d %>% rename(GR_d = GrowthRate_at_std_age)
GRValues_d_age <- GRValues_d_age %>% rename(GR_d_age = GrowthRate_at_std_age)
GRValues_d_25 <- GRValues_d_25 %>% rename(GR_d_25 = GrowthRate_at_std_age)
GRValues_d_50 <- GRValues_d_50 %>% rename(GR_d_50 = GrowthRate_at_std_age)
GRValues_d_75 <- GRValues_d_75 %>% rename(GR_d_75 = GrowthRate_at_std_age)

GR_d <- GRValues_d %>% 
  inner_join(GRValues_d_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_d_50, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_d_25, by = c("Spp" = "Spp")) %>%
  inner_join(GRValues_d_75, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                  "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at")))) %>% 
  rowwise() %>%
  mutate(row_max = pmap_chr(across(everything()), ~ names(c(...)[which.max(c(...))])))

GRValues_h <- GRValues_h %>% rename(GR_h = GrowthRate_at_std_age)
GRValues_h_age <- GRValues_h_age %>% rename(GR_h_age = GrowthRate_at_std_age)
GRValues_h_25 <- GRValues_h_25 %>% rename(GR_h_25 = GrowthRate_at_std_age)
GRValues_h_50 <- GRValues_h_50 %>% rename(GR_h_50 = GrowthRate_at_std_age)
GRValues_h_75 <- GRValues_h_75 %>% rename(GR_h_75 = GrowthRate_at_std_age)

GR_h <- GRValues_h %>% 
  inner_join(GRValues_h_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_h_50, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_h_25, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_h_75, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                                 "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at")))) %>% 
  rowwise() %>%
  mutate(row_max = pmap_chr(across(everything()), ~ names(c(...)[which.max(c(...))])))

GRValues_w <- GRValues_w %>% rename(GR_w = GrowthRate_at_std_age)
GRValues_w_age <- GRValues_w_age %>% rename(GR_w_age = GrowthRate_at_std_age)
GRValues_w_25 <- GRValues_w_25 %>% rename(GR_w_25 = GrowthRate_at_std_age)
GRValues_w_50 <- GRValues_w_50 %>% rename(GR_w_50 = GrowthRate_at_std_age)
GRValues_w_75 <- GRValues_w_75 %>% rename(GR_w_75 = GrowthRate_at_std_age)

GR_w <- GRValues_w %>% 
  inner_join(GRValues_w_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_w_50, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_w_25, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_w_75, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                                 "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at")))) %>%
  rowwise() %>%
  mutate(row_max = pmap_chr(across(everything()), ~ names(c(...)[which.max(c(...))])))

GRValues_la <- GRValues_la %>% rename(GR_la = GrowthRate_at_std_age)
GRValues_la_age <- GRValues_la_age %>% rename(GR_la_age = GrowthRate_at_std_age)
GRValues_la_25 <- GRValues_la_25 %>% rename(GR_la_25 = GrowthRate_at_std_age)
GRValues_la_50 <- GRValues_la_50 %>% rename(GR_la_50 = GrowthRate_at_std_age)
GRValues_la_75 <- GRValues_la_75 %>% rename(GR_la_75 = GrowthRate_at_std_age)

GR_la <- GRValues_la %>% 
  inner_join(GRValues_la_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_la_50, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_la_25, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_la_75, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                                 "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at")))) %>%
  rowwise() %>%
  mutate(row_max = pmap_chr(across(everything()), ~ names(c(...)[which.max(c(...))])))

GRValues_d_1.4 <- GRValues_d_1.4 %>% 
  mutate(age = 1.4) %>% 
  #filter(!Spp %in% c("PHPH")) %>%
  rename(GR_d_each_age = GrowthRate_at_std_age)

GRValues_d_2.4 <- GRValues_d_2.4 %>% 
  mutate(age = 2.4) %>% 
  rename(GR_d_each_age = GrowthRate_at_std_age)

GRValues_d_5 <- GRValues_d_5 %>% 
  mutate(age = 5)%>% 
  rename(GR_d_each_age = GrowthRate_at_std_age)

GRValues_d_7 <- GRValues_d_7 %>% 
  mutate(age = 7)%>% 
  rename(GR_d_each_age = GrowthRate_at_std_age)

GRValues_d_9 <- GRValues_d_9 %>% 
  mutate(age = 9)%>% 
  filter(!Spp %in% c("PILI")) %>% 
  rename(GR_d_each_age = GrowthRate_at_std_age)

GRValues_d_32 <- GRValues_d_32 %>% 
  mutate(age = 32) %>% 
  filter(!Spp %in% c("BOLE", "COER", "GRSP", "HEPU", "PILI"))%>% 
  rename(GR_d_each_age = GrowthRate_at_std_age)

GR_d_each_age <- GRValues_d_1.4 %>% 
  bind_rows(GRValues_d_2.4) %>% 
  bind_rows(GRValues_d_5) %>% 
  bind_rows(GRValues_d_7) %>% 
  bind_rows(GRValues_d_9) %>% 
  bind_rows(GRValues_d_32) %>% 
  as_tibble()

GRValues_h_1.4 <- GRValues_h_1.4 %>% 
  mutate(age = 1.4) %>% 
  #filter(!Spp %in% c("PHPH")) %>%
  rename(GR_h_each_age = GrowthRate_at_std_age)

GRValues_h_2.4 <- GRValues_h_2.4 %>% 
  mutate(age = 2.4)%>% 
  rename(GR_h_each_age = GrowthRate_at_std_age)

GRValues_h_5 <- GRValues_h_5 %>% 
  mutate(age = 5)%>% 
  rename(GR_h_each_age = GrowthRate_at_std_age)

GRValues_h_7 <- GRValues_h_7 %>% 
  mutate(age = 7)%>% 
  rename(GR_h_each_age = GrowthRate_at_std_age)

GRValues_h_9 <- GRValues_h_9 %>% 
  mutate(age = 9) %>% 
  filter(!Spp %in% c("PILI")) %>% 
  rename(GR_h_each_age = GrowthRate_at_std_age)

GRValues_h_32 <- GRValues_h_32 %>% 
  mutate(age = 32) %>% 
  filter(!Spp %in% c("BOLE", "COER", "GRSP", "HEPU", "PILI"))%>% 
  rename(GR_h_each_age = GrowthRate_at_std_age)

GR_h_each_age <- GRValues_h_1.4 %>% 
  bind_rows(GRValues_h_2.4) %>% 
  bind_rows(GRValues_h_5) %>% 
  bind_rows(GRValues_h_7) %>% 
  bind_rows(GRValues_h_9) %>% 
  bind_rows(GRValues_h_32) %>% 
  as_tibble()

GRValues_w_1.4 <- GRValues_w_1.4 %>% 
  mutate(age = 1.4) %>%
  #filter(!Spp %in% c("PHPH")) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GRValues_w_2.4 <- GRValues_w_2.4 %>% 
  mutate(age = 2.4) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GRValues_w_5 <- GRValues_w_5 %>% 
  mutate(age = 5) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GRValues_w_7 <- GRValues_w_7 %>% 
  mutate(age = 7) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GRValues_w_9 <- GRValues_w_9 %>% 
  mutate(age = 9) %>% 
  filter(!Spp %in% c("PILI")) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GRValues_w_32 <- GRValues_w_32 %>% 
  mutate(age = 32) %>% 
  filter(!Spp %in% c("BOLE", "COER", "GRSP", "HEPU", "PILI")) %>% 
  rename(GR_w_each_age = GrowthRate_at_std_age)

GR_w_each_age <- GRValues_w_1.4 %>% 
  bind_rows(GRValues_w_2.4) %>% 
  bind_rows(GRValues_w_5) %>% 
  bind_rows(GRValues_w_7) %>% 
  bind_rows(GRValues_w_9) %>% 
  bind_rows(GRValues_w_32) %>% 
  as_tibble()

GRValues_la_1.4 <- GRValues_la_1.4 %>% 
  mutate(age = 1.4) %>% 
  #filter(!Spp %in% c("PHPH")) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GRValues_la_2.4 <- GRValues_la_2.4 %>% 
  mutate(age = 2.4) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GRValues_la_5 <- GRValues_la_5 %>% 
  mutate(age = 5) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GRValues_la_7 <- GRValues_la_7 %>% 
  mutate(age = 7) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GRValues_la_9 <- GRValues_la_9 %>% 
  mutate(age = 9) %>% 
  filter(!Spp %in% c("PILI")) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GRValues_la_32 <- GRValues_la_32 %>% 
  mutate(age = 32) %>% 
  filter(!Spp %in% c("BOLE", "COER", "GRSP", "HEPU", "PILI")) %>% 
  rename(GR_la_each_age = GrowthRate_at_std_age)

GR_la_each_age <- GRValues_la_1.4 %>% 
  bind_rows(GRValues_la_2.4) %>% 
  bind_rows(GRValues_la_5) %>% 
  bind_rows(GRValues_la_7) %>% 
  bind_rows(GRValues_la_9) %>% 
  bind_rows(GRValues_la_32) %>% 
  as_tibble()


