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
  total_weight <- -log(n * age + 1, base = m) # adopted from Salomon et al 2017
  return(total_weight)
}

# Defining the function to minimize

minSS <- function(par, data){
  out <- with(data, sum((data$total_weight+log(par[2]*age+1, base=par[1]))^2))
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

curve_growth_data_filter <- growth_data_all %>% 
  filter(RA_max_1 < 0.5)

LoopOver <- unique(curve_growth_data_filter$species)

for (ll in LoopOver){
  tmpe <- curve_growth_data_filter[curve_growth_data_filter$species == ll,]

  
  # Initialization values of parameters
  mypara = c(0.9, 0.2) #do not know if these will work for growth data but can try
  
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
  std_age <- 1.4 # (unit=year) standard age
  
  GrowthRate_std <- Calc_GrowthRate(std_age, m, n)
  
  # inflection point using segmented package
  # 
  # out.lm <- lm(total_weight~age, data=tmpe)
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
  
  GCPlotList[[ll]] <- ggplot(tmpe, aes(age, total_weight)) + #plot a graph
    geom_point(size=5, colour="black") + #colour the points for confidence in data
    geom_line(data = fitline, aes(xfit, yfit), colour = "black", linewidth=2.5, linetype="dashed") +
    #ggtitle(Species_names$Genus_Species[match(ll, Species_names$Spp)]) +
    labs(x = "age / year", y = "total_weight")
  
  
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
GRValues_w_age <- do.call(rbind, GRValueList)  # compiling

GRValues_d <- GRValues_d %>% rename(GR_d = GrowthRate_at_std_age)
GRValues_d_age <- GRValues_d_age %>% rename(GR_d_age = GrowthRate_at_std_age)
GRValues_d_indiv <- GRValues_d_indiv %>% rename(GR_d_indiv = GrowthRate_at_std_age)

GR_d <- GRValues_d %>% 
  inner_join(GRValues_d_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_d_indiv, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                  "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at"))))

GRValues_h <- GRValues_h %>% rename(GR_h = GrowthRate_at_std_age)
GRValues_h_age <- GRValues_h_age %>% rename(GR_h_age = GrowthRate_at_std_age)
GRValues_h_indiv <- GRValues_h_indiv %>% rename(GR_h_indiv = GrowthRate_at_std_age)

GR_h <- GRValues_h %>% 
  inner_join(GRValues_h_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_h_indiv, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                                 "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at"))))

GRValues_w <- GRValues_w %>% rename(GR_w = GrowthRate_at_std_age)
GRValues_w_age <- GRValues_w_age %>% rename(GR_w_age = GrowthRate_at_std_age)
GRValues_w_indiv <- GRValues_w_indiv %>% rename(GR_w_indiv = GrowthRate_at_std_age)

GR_w <- GRValues_w %>% 
  inner_join(GRValues_w_age, by = c("Spp" = "Spp")) %>% 
  inner_join(GRValues_w_indiv, by = c("Spp" = "Spp")) %>% 
  dplyr::select(-c(starts_with(c("m", "n", "slope_after_inflection", 
                                 "slope_before_inflection", "breakpoint", "breakpoint_se", "GrowthRate_at"))))

