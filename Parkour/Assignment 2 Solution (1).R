###################
#Part 1
###################

set.seed(5)
my_data<-data.frame(immersion = c(rnorm(12,70,10),rnorm(12,75,10),rnorm(12,100,10)), group = gl(3,12, labels = c("sitting","standing","walking")))
my_data

#assumptions

#Assumption #1: The dependent variable should be measured at the interval or ratio level.
#check!

#Assumption #2: The IV should consist of two or more categorical.
#check

#Assumption #3: There is independence of observation
#check

#Assumption #4: Outliers
library(ggpubr) #for stat_summary, ggboxplot
ggboxplot(my_data, x = "group", y = "immersion")
#no outlers

#verify that with this function
my_data %>%
  group_by(group) %>%
  identify_outliers(immersion)



#Assumption #5: Your dependent variable should be approximatelnormally distributed for each category of the independent variable
library(rstatix)
my_data %>%
  group_by(group) %>%
  shapiro_test(immersion)
#check!

#Assumption #6: There needs to be homogeneity of variances.
library(car)
leveneTest(immersion ~ group, data = my_data)
#check

#ANOV using aov()

# Compute the analysis of variance
res.aov <- aov(immersion ~ group, data = my_data)
res.aov
# Summary of the analysis
summary(res.aov)

#eta-squared
library(lsr)
etaSquared(res.aov)

#post-hoc test
TukeyHSD(res.aov)


#Figure 1

library(rstatix)  
stat.test <- aov(immersion ~ group, data = my_data) %>%
  tukey_hsd()
stat.test

ggplot(my_data, aes(group, immersion)) + 
  stat_summary(fun = mean, geom = "bar") + 
  stat_summary(fun.data = mean_sd, geom = "errorbar", width = 0.2) + 
  labs(x = "Group", y = "Immersion") +
  stat_pvalue_manual(
    stat.test, label = "p.adj.signif", 
    y.position = c(100, 140, 130)
  )

#Table 1: Summary statistics
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(immersion, na.rm = TRUE),
    sd = sd(immersion, na.rm = TRUE)
  )

#Report: 
#There was a significant effect of group on the immersion score, F(2,33) = 53.41, p < .001, η_p^2= 0.76. 
#The means and standard deviations of each groups are summarized in Table 1.  
#A Tukey HSD post-hoc test found that walking (M = 108, SD = 9.49) was different from both sitting (M = 69.7,SD = 9.71), p < .0001, and standing (M = 74.0, SD = 10.4), p < .0001. There was no significant difference between sitting and standing, p > .05. see Figure 1 below for details.

###################
#Part 2
###################
library("readxl")
my_data <- read_excel("SUS.xlsx")

#Mixed ANOVA
#a)
#Assumption #1: check
#Assumption #2: check
#Assumption #3: check
#Assumption #4: outliers
require(ggpubr)
ggplot(my_data, aes(x=Order,y=Score, fill = Tool)) + geom_boxplot()
#check!
##Assumption #5: normality
#normality tests
library(rstatix)
my_data %>%
  group_by(Order,Tool) %>%
  shapiro_test(Score) %>% 
  print(n = Inf) 
#check!
##Assumption #5: homogeneity of variances 
lev <- car::leveneTest(Score~Order*Tool, data = my_data)
lev
#check!

##Assumption #7: sphericity
#no sphericity issues here possible because only 2 levels in IV

#b)
library(ez)
M_AnovaModel <- ezANOVA(data = my_data, dv = .(Score), wid = .(Partcipants), within = .(Tool), between = .(Order), detailed = T, type = 3)
M_AnovaModel

#post-hoc
pairwise.t.test(my_data$Score, interaction(my_data$Order, my_data$Tool), paired=T, p.adjust.method ="bonferroni")

#c)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

ds <- data_summary(my_data, varname="Score", groupnames=c("Order", "Tool"))
library(ggplot2)
p <- ggplot(ds, aes(x=Order, y=Score, group = Tool, color=Tool))+ 
  #comment the line below to hide error bars
  geom_errorbar(aes(ymin=Score-sd, ymax=Score+sd), width=.1, position=position_dodge(0.05)) + 
  geom_line(aes(linetype=Tool)) + 
  geom_point(aes(shape=Tool))+
  labs(x="Order", y = "Tool")+
  theme_classic()
p + theme_classic() + scale_color_manual(values=c('#999999','#E69F00','666666'))

#Summary statistics
library(dplyr)
group_by(my_data, Tool) %>%
  summarise(
    count = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE)
  )

group_by(my_data, Order) %>%
  summarise(
    count = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE)
  )

group_by(my_data, Order, Tool) %>%
  summarise(
    count = n(),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE)
  )

#d) Report
#A mixed ANOVA revealed a significant main effect of Order, F(1,14) = 14.17, p < .05, η_g^2 = 0.43. 
#Overall, A→B (M = 75, SD = 5) was rated higher than B→A (M = 65.0, SD = 10.4). 
#The main effect of Tool was also significant, F(1,14) = 40.38, p < .05, η_g^2 = 0.42.
#Overall, A (M = 65.1, SD = 8.98) was rated lower than B (M = 74.9, B = 7.48).
#The interaction effect between Order and Tool was also significant, F(1,14) = 9.64, p < .05, η_g^2 = 0.14. 
#Among the interesting findings are the following: 
#A pairwise post-hoc t-test with Bonferroni adjustment further revealed that A was rated higher in A→B (M = 72.5, SD = 2.99) compared to B→A (M = 57.7,  SD = 6.20), p > .01,
#but the same was not true for B, p > .05.

#e) Even though A was rated higher in A→B, thanks to counterbalancing, this was offset by A getting rated significantly lower in B→A. 
#In the end B was still rated higher overall. So the answer is no. 
#However, because the order effect was significant, it means the counterbalancing was not successful. 
#So, to avoid this issue you can consider each group rating one condition (either A or B) and not being exposed to both conditions, that is use purely a between-subject design.