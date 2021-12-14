#Some code from lecture
library(likert)
library(reshape) #for cast
if (!require(plyr)) install.packages("plyr")

library(magrittr)
library(dplyr)
library(tidyr)
library(rstatix)
library(ggpubr)

#define better colors
cs <- c("#e9505a","#f6b9bd","gray88","#cfcfe8","#7474b0")
#define order in which the levels of the group variable will appear
#plot
lk <- read.csv("./Likert.csv",header = TRUE)
lk
lk[3:9] <- lapply(lk[3:9],factor,level = 1:5)

flik <- likert::likert(lk[,c(3:9)], grouping = lk$Build)

order <- c("A", "B")

plot(flik,  plot.percents=TRUE, colors = cs, group.order = order)

#Wilcoxon Signed-Rank test

#Outliers
metrics <- read.csv("./anova.csv",header = TRUE)
metrics <- metrics[-c(5,6,15,16),]
#remove deaths and time 1
library(ggpubr)
ggboxplot(metrics,x = "Metric", y = "Time")
ggboxplot(metrics,x = "Metric", y = "Deaths")

metrics %>%
  group_by(Metric) %>%
  identify_outliers(Time)

#P3 and P8 are outliers

metrics %>%
  group_by(Metric) %>%
  shapiro_test(Time)

#pass test, both p > 0.05

library(car)
leveneTest(Time ~ Order*Metric ,data = metrics)


library(ez)
an <- ezANOVA(data = metrics,dv = .(Time),wid = .(Participant),within = .(Metric), between = .(Order),detailed = T, type =3 )
an

pairwise.t.test(metrics$Time,interaction(metrics$Order,metrics$Metric), paired = T,p.adjust.methods = "bonferroni")


library(ggplot2)
p <- ggplot(metrics, aes(x=Order, y=Deaths, group = Metric, color=Metric))+ 
  #comment the line below to hide error bars
  #geom_errorbar(aes(ymin=Time-sd(Order[Metric],na.rm = TRUE), ymax=Time+sd(Order[Metric],na.rm = TRUE)), width=.1, position=position_dodge(0.05)) + 
  geom_line(aes(linetype=Metric)) + 
  geom_point(aes(shape=Metric))+
  labs(x="Order", y = "Deaths")+
  theme_classic()
p + theme_classic() + scale_color_manual(values=c('#999999','#E69F00','666666'))

ggplot(metrics, aes(Metric, Time)) + 
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.2) + 
  labs(x = "Metric", y = "Time")

ggplot(metrics, aes(Metric, Deaths)) + 
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.2) + 
  labs(x = "Metric", y = "Deaths")
