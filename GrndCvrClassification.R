require(car)
require(dplyr)
require(ggplot2)
require(gplots)
require(lme4)
require(multcomp)

#Set working directory
setwd("~/Google Drive/Urban")
#For Windows
#setwd("C:/Users/Sebastian/Google Drive/Urban")

#Ground cover type percentaghe of each plot
Plots <- read.csv("Data/Plots.csv")
#arboles <- read.csv("Data/arboles_2002-2014_2.csv" )

Plots$Plot.ID  = as.factor(Plots$Plot.ID)
str(Plots)
plot(x = Plots$Building, y = Plots$BA.02.14)
plot(x = Plots$Grass, y = Plots$BA2002)
plot(x = Plots$Building, y = Plots$BA.02.14,
     xlab ='Building Cover Percent',
     ylab = '2002-2014 Basal Area Change')
lines(x = aggregate(list(biomass = Plots$Biomass.2014),
          list(building = cut(Plots$Building, 20)),
          mean)$biomass,
      y = seq(from = 0, to = 100, by = 5))
length(aggregate(list(biomass = Plots$Biomass.2014),
                 list(building = cut(Plots$Building, 20)),
                 mean)$biomass)
fit1 = lmer(BA2002 ~ Permeable  + (1|Plot.ID), data=Plots)
fit1 = lm(BA2002 ~ Permeable, data=Plots)
summary(fit1)

hist(Plots$Biomass.2002)




factors = c('Building',
            'Cement',
            'Bare.soil',
            'Tar',
            'Herbs',
            'Grass',
            'Impermeable',
            'Permeable' )
responses = c('num_species',
              'num_trees_2002',
              'num_trees_2014',
              'removed_trees.2014',
              'ingrowth_trees.2014')

par(mfrow=c(2,2))
for (response in responses){
  #png(paste(factor, '.png'))
  for (factor in factors){
    plot(as.numeric(unlist(Plots[factor])),
         as.numeric(unlist(Plots[response])),
         xlab = factor, ylab = response,
         main = cor(Plots[factor], Plots[response])[1])
  }
} 
head(Plots)


Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + BA2002
plot()
# Plot #2: same as above, but add loess smoother in lower and correlation in upper
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + BA2002, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="BA2002")
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + BA2014, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="BA2014")
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + BA.02.14, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="BA.02.14")
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + Biomass.2002, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Biomass.2002")
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + Biomass.2002, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Biomass.2014")
pairs(~Building + Cement + Bare.soil + Tar + Herbs + Grass + Impermeable + Permeable + Biomass.02.14, data=Plots,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Biomass.02.14")



str(Plots$B)
col(Plots)
head (Plots)
#Plots$BA2014, Plots$Biomass.2002
fit1 = lm(BA2014 ~ Permeable, data=Plots)
plot(fit1)



fit2 = aov(Biomass.2002 ~ GrndCvrClass, data=Plots)

plot(cld(glht(fit2,linfct=mcp(GrndCvrClass='Tukey'))),
     main = 'GrndCvrClass')