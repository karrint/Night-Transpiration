#principal component bins:
water stress: midday WP, transpiration?, day 
success: growth, biomass
competition: 











#mixed.lmer <- lmer(RESPONSE VARIABLE ~ (IV1*or+IV2) + (1|RANDOM VARIABLE), data = PREDAWN)  #random fx = tx
#PREDAWN$days <- scale(PREDAWN$day, center = TRUE, scale = TRUE)

``` {R gsw~day*WP_diff*tx}
library(readxl)
PREDAWN <- read_excel("PREDAWN.xlsx")
View(PREDAWN)
hist(PREDAWN$gsw)
basic.lm <- lm(gsw ~ days, data = PREDAWN)
summary(basic.lm)
library(ggplot2)
(prelim_plot <- ggplot(PREDAWN, aes(x = days, y = gsw)) 
   + geom_point() 
   + geom_smooth(method = "lm"))
plot(basic.lm, which = 1) #residuals
boxplot(gsw ~ tx, data = PREDAWN) #tx correlation
(colour_plot <- ggplot(PREDAWN, aes(x = days, y = gsw, colour = tx)) 
  + geom_point(size = 2) + geom_smooth(method = "lm") 
  + theme_classic() 
  + theme(legend.position = "top"))
(split_plot <- ggplot(aes(days, gsw), data = PREDAWN) 
  + geom_point() + facet_wrap(~tx) 
  + xlab("Day") + ylab("gsw"))  #split by tx
tx.lm <- lm(gsw ~ days + tx, data = PREDAWN)
summary(tx.lm)


library(Matrix)
library(lme4)
library(MuMIn)
library(lmerTest)
mixed.lmer <- lmer(gsw ~ (day * WP_diff * tx) + (1|rep/Tree), data = PREDAWN)  #random fx = tx
summary(mixed.lmer)
anova(mixed.lmer)
eta_sq(mixed.lmer, partial = TRUE) 
plot(mixed.lmer)  # no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))
````












