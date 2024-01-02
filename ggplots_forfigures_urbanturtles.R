#to make figures 

#download libraries


library(ggplot2)
library(tidyverse)
library(cowplot)
library(dplyr)

turtlt<-Urban_Turtle_Analysis_traps_OLM_Males_121222

all
turtlt$inv_chlo <- (turtlt$`1/chlor`)

plot1 <- turtlt %>%
  ggplot(aes(x= inv_chlo,y=nativeC)) + 
  geom_point(aes(), colour="green4") +
  geom_smooth(aes(y=nativeC), method="lm", se=TRUE) +
  labs(x ="Inverse Chlorophyll (ppb)", y = "Native Turtle Abundance")
plot1


plot2 <- turtlt %>%
  ggplot(aes(x= avg_submergent,y=nativeC)) + 
  geom_point(aes(), colour="green4") +
  geom_smooth(aes(y=nativeC), method="lm", se=TRUE) +
 labs(x ="Percent Submergent Vegetation", y = "Native Turtle Abundance")
plot2

plot_grid(plot1,plot2, nrow = 2, ncol = 1)

turtlt$nativeC <- turtlt$`CPUE natives`

turtlt$parea <-turtlt$`ln(pond area)`

plot3 <- turtlt %>%
  ggplot(aes(x=parea,y= nativeC)) + 
  geom_point(aes(), colour="blue") +
  geom_smooth(aes(y=nativeC), method="lm", se=TRUE) +
  labs(x =" Log Pond Area (ha)", y = "Native Turtle Abundance")

plot3

plot4 <- turtlt %>%
  ggplot(aes(x= `HighUrban/very developed_100 m` ,y=nativeC)) + 
  geom_point(aes(), colour="red") +
  geom_smooth(aes(y=nativeC), method="lm", se=TRUE) +
  labs(x ="Percent of High Urban Area within 100 m", y = "Native Turtle Abundance")
plot4



plot_grid(plot3,plot4, nrow = 2, ncol = 1)



### Figure 3

# the second one should work, these don't work bc 15 ponds have males not 17 so it doesn't line up
Model <- lm(permale ~ ln_dist +ln_nat_100 +asn_urban_100, data = df)
summary(Model)
df <-subset(turtlt, select=c(permale, ln_dist, ln_nat_100, asn_urban_100, na.pass(TRUE)))

df$residuals <-Model$resid




plot1a <- df %>%
  ggplot(aes(x=road_den_100, y=residuals$permale)) + 
  geom_point(aes(), colour="blue") +
  geom_smooth(aes(y=residuals$permale), method="lm", se=TRUE) +
  labs(x ="Road Density within 100 m ", y = "Percent Male Turtles")
plot1a

Model2 <- lm(ln_nat_sp ~ + inv_chlor +pH +inv_sal, data = turtlt)
summary(Model2)
df <-subset(turtlt, select=c(permale, road_den_100, ln_dist, ln_nat_100, asn_urban_100, na.pass(TRUE)))

turtlt$residuals <-Model2$resid



plot2a <- turtlt %>%
  ggplot(aes(x=road_den_100, y= residual_fig3a)) + 
  geom_point(aes(), colour="green4") +
  #theme_classic()+ 
  geom_smooth(aes(y=residual_fig3a), method="lm", se=TRUE) +
  labs(x ="Road Density within 100 m", y = "Native Species Richness Residuals")
  
plot2a

plot2b <- turtlt %>%
  ggplot(aes(x=road_den_100, y= residual_fig3b)) + 
  geom_point(aes(), colour="blue") +
  geom_smooth(aes(y=residual_fig3b), method="lm", se=TRUE) +
  labs(x ="Road Density within 100 m", y = "Percent Male Turtles Residuals")#+
  #lims(y= c(-20,35))


plot2b


plot_grid(plot2a,plot2b, nrow = 2, ncol = 1)

write.csv(df, file = "residuals_road_model.csv")
getwd()




