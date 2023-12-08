library(readxl)
no_wb <- read_excel("no_wb.xlsx")
library(ggplot2)
library(Matrix)
library(lme4)
library(MuMIn)
library(lmerTest)

no_wb$gsw <- as.numeric(no_wb$gsw)
no_wb$competition <- as.numeric(no_wb$competition)
no_wb$day <- as.numeric(no_wb$day)

no_wbtx <- no_wb[-c(1:35), ]
day35 <- (no_wbtx$day) <= 36 
day56 <- (no_wbtx$day) >= 36

no_wb35 <- no_wbtx[day35, ]
no_wb56 <- no_wbtx[day56, ]
##########################################
# gsn ~ competition #days 1- 35
gsn.comp.reptree <- lmer(gsw ~ competition + (1|rep/Tree), data = no_wb35) 
summary(gsn.comp.reptree) #
anova(gsn.comp.reptree) 
plot(gsn.comp.reptree)  # good
qqnorm(resid(gsn.comp.reptree))
qqline(resid(gsn.comp.reptree))

gsn.comp.tree <- lmer(gsw ~ competition + (1|Tree), data = no_wb35) #LOWEST AIC-360.9836
summary(gsn.comp.tree) #p 0.000964 ***
anova(gsn.comp.tree) 
plot(gsn.comp.tree)  # good
qqnorm(resid(gsn.comp.tree))
qqline(resid(gsn.comp.tree))

gsn.comp.tx <- lmer(gsw ~ competition + (1|tx), data = no_wb35) 
summary(gsn.comp.tx) #
anova(gsn.comp.tx) 
plot(gsn.comp.tx)  # good
qqnorm(resid(gsn.comp.tx))
qqline(resid(gsn.comp.tx))

gsn.comp.txtree <- lmer(gsw ~ competition + (1|tx/Tree), data = no_wb35) 
summary(gsn.comp.txtree) #
anova(gsn.comp.txtree) 
plot(gsn.comp.txtree)  # good
qqnorm(resid(gsn.comp.txtree))
qqline(resid(gsn.comp.txtree))

gsn.comp.daysm <- lmer(gsw ~ competition + (1|day/PD_sm), data = no_wb35) 
summary(gsn.comp.daysm) #
anova(gsn.comp.daysm) 
plot(gsn.comp.daysm)  # good
qqnorm(resid(gsn.comp.daysm))
qqline(resid(gsn.comp.daysm))

AIC(gsn.comp.reptree, gsn.comp.tree, gsn.comp.tx, gsn.comp.txtree, gsn.comp.daysm, k=5) #gsn.comp.txtree

#########################################
# gsn ~ competition #days 1- 56
gsn.comp.reptree <- lmer(gsw ~ competition + (1|rep/Tree), data = no_wb56) 
summary(gsn.comp.reptree) #
anova(gsn.comp.reptree) 
plot(gsn.comp.reptree)  # good
qqnorm(resid(gsn.comp.reptree))
qqline(resid(gsn.comp.reptree))

gsn.comp.tree <- lmer(gsw ~ competition + (1|Tree), data = no_wb56) #LOWEST AIC-329.4068
summary(gsn.comp.tree) #p 0.01007 *
anova(gsn.comp.tree) 
plot(gsn.comp.tree)  # good
qqnorm(resid(gsn.comp.tree))
qqline(resid(gsn.comp.tree))

gsn.comp.tx <- lmer(gsw ~ competition + (1|tx), data = no_wb56) 
summary(gsn.comp.tx) #0.2783
anova(gsn.comp.tx) 
plot(gsn.comp.tx)  # good
qqnorm(resid(gsn.comp.tx))
qqline(resid(gsn.comp.tx))

gsn.comp.txtree <- lmer(gsw ~ competition + (1|tx/Tree), data = no_wb56) 
summary(gsn.comp.txtree) #none
anova(gsn.comp.txtree) 
plot(gsn.comp.txtree)  # good
qqnorm(resid(gsn.comp.txtree))
qqline(resid(gsn.comp.txtree))

gsn.comp.daysm <- lmer(gsw ~ competition + (1|day/PD_sm), data = no_wb56) 
summary(gsn.comp.daysm) #none
anova(gsn.comp.daysm) 
plot(gsn.comp.daysm)  # good
qqnorm(resid(gsn.comp.daysm))
qqline(resid(gsn.comp.daysm))

AIC(gsn.comp.reptree, gsn.comp.tree, gsn.comp.tx, gsn.comp.txtree, gsn.comp.daysm, k=5) #gsn.comp.txtree

# gsn ~ tx #C*** ; C-X-C* ; all***
##################################################
gsn.tx.reptree <- lmer(gsw ~ tx + (1|rep/Tree), data = no_wb)  #random fx = rep/tree
summary(gsn.tx.reptree) #C*** ; C-X-C* ; all***
anova(gsn.tx.reptree)
plot(gsn.tx.reptree)  # no patterns evident
qqnorm(resid(gsn.tx.reptree))
qqline(resid(gsn.tx.reptree))

gsn.tx.onetree <- lmer(gsw ~ tx + (1|Tree), data = no_wb)  #lowest aic #random fx = tree
summary(gsn.tx.onetree) #C*** ; C-X-C* ; all***
anova(gsn.tx.onetree) #0.0009935 ***
plot(gsn.tx.onetree)  # no patterns evident
qqnorm(resid(gsn.tx.onetree))
qqline(resid(gsn.tx.onetree))

gsn.tx.comptree <- lmer(gsw ~ tx + (1|competition/Tree), data = no_wb)  
summary(gsn.tx.comptree) #C** ; all tx**
anova(gsn.tx.comptree)
plot(gsn.tx.comptree)  # no patterns evident
qqnorm(resid(gsn.tx.comptree))
qqline(resid(gsn.tx.comptree))

gsn.tx.daysoil <- lmer(gsw ~ tx + (1|day/PD_sm), data = no_wb)  
summary(gsn.tx.daysoil) #C*** ; C-X-C. ; C-WB* ; all tx***
anova(gsn.tx.daysoil)
plot(gsn.tx.daysoil)  # no patterns evident
qqnorm(resid(gsn.tx.daysoil))
qqline(resid(gsn.tx.daysoil))

gsn.tx.daymdsoil <- lmer(gsw ~ tx + (1|day/MD_sm), data = no_wb)  
summary(gsn.tx.daymdsoil) #C*** ; C-X-C. ; C-WB* ; all tx***
anova(gsn.tx.daymdsoil)
plot(gsn.tx.daymdsoil)  # no patterns evident
qqnorm(resid(gsn.tx.daymdsoil))
qqline(resid(gsn.tx.daymdsoil))

AIC(gsn.tx.reptree, gsn.tx.onetree, gsn.tx.comptree, gsn.tx.daysoil, gsn.tx.daymdsoil, k=5) #gsn.tx.onetree

##################

#intra vs inter
no_wb_comp <- read_excel("no_wb_comp.xlsx")

no_wb_comp$gsw <- as.numeric(no_wb_comp$gsw)
no_wb_comp$competition <- as.numeric(no_wb_comp$competition)

no_wb35 <- no_wb_comp[-c(141:270), ]
no_wb56 <- no_wb_comp[-c(1:140), ]

gsn.comptype.comp <- lmer(gsw ~ comp_type + (1|competition), data = no_wb_comp)#aic  
summary(gsn.comptype.comp) #inter*** ; intra*** ; all**
anova(gsn.comptype.comp)
plot(gsn.comptype.comp)  # no patterns evident
qqnorm(resid(gsn.comptype.comp))
qqline(resid(gsn.comptype.comp))

gsn.comptype.tree <- lmer(gsw ~ comp_type + (1|Tree), data = no_wb_comp)  
summary(gsn.comptype.tree) #inter*** ; intra*** ; all***
anova(gsn.comptype.tree) #0.0004657 ***
plot(gsn.comptype.tree)  # no patterns evident
qqnorm(resid(gsn.comptype.tree))
qqline(resid(gsn.comptype.tree))

AIC(gsn.comptype.comp, gsn.comptype.tree, k=2) #gsn.comptype.comp

#35
gsn.comptype.comp <- lmer(gsw ~ comp_type + (1|competition), data = no_wb35)#aic-435.7278  
summary(gsn.comptype.comp) #0.008202 **
anova(gsn.comptype.comp)
plot(gsn.comptype.comp)  # no patterns evident
qqnorm(resid(gsn.comptype.comp))
qqline(resid(gsn.comptype.comp))

gsn.comptype.tree <- lmer(gsw ~ comp_type + (1|Tree), data = no_wb35)  
summary(gsn.comptype.tree) #
anova(gsn.comptype.tree)
plot(gsn.comptype.tree)  # no patterns evident
qqnorm(resid(gsn.comptype.tree))
qqline(resid(gsn.comptype.tree))

AIC(gsn.comptype.comp, gsn.comptype.tree, k=2) #

#56
gsn.comptype.comp <- lmer(gsw ~ comp_type + (1|competition), data = no_wb56)#  
summary(gsn.comptype.comp) #
anova(gsn.comptype.comp)
plot(gsn.comptype.comp)  # no patterns evident
qqnorm(resid(gsn.comptype.comp))
qqline(resid(gsn.comptype.comp))

gsn.comptype.tree <- lmer(gsw ~ comp_type + (1|Tree), data = no_wb56)  #AIC -384.7486
summary(gsn.comptype.tree) #0.003783 **
anova(gsn.comptype.tree)
plot(gsn.comptype.tree)  # no patterns evident
qqnorm(resid(gsn.comptype.tree))
qqline(resid(gsn.comptype.tree))

AIC(gsn.comptype.comp, gsn.comptype.tree, k=2) #

################################

#gsn diff by comp type
gsn_diff.comptype.comp <- lmer(gsn_diff ~ comp_type + (1|competition), data = no_wb_comp)#aic  
summary(gsn_diff.comptype.comp) #inter. ; intra** ; all*
anova(gsn_diff.comptype.comp)
plot(gsn_diff.comptype.comp)  # no patterns evident
qqnorm(resid(gsn_diff.comptype.comp))
qqline(resid(gsn_diff.comptype.comp))

gsn_diff.comptype.tree <- lmer(gsn_diff ~ comp_type + (1|Tree), data = no_wb_comp)  
summary(gsn_diff.comptype.tree) #inter. ; intra** ; all*
anova(gsn_diff.comptype.tree)
plot(gsn_diff.comptype.tree)  # no patterns evident
qqnorm(resid(gsn_diff.comptype.tree))
qqline(resid(gsn_diff.comptype.tree))

AIC(gsn_diff.comptype.comp, gsn_diff.comptype.tree, k=2) #gsn_diff.comptype.comp
###########################################
# gsn35 ~ tx #
no_wb35 <- no_wb_comp[-c(141:270), ]
gsn.tx.reptree <- lmer(gsw ~ tx + (1|rep/Tree), data = no_wb35)  #random fx = rep/tree
summary(gsn.tx.reptree) #
anova(gsn.tx.reptree) #all 0.01107 *
plot(gsn.tx.reptree)  # no patterns evident
qqnorm(resid(gsn.tx.reptree))
qqline(resid(gsn.tx.reptree))

gsn.tx.onetree <- lmer(gsw ~ tx + (1|Tree), data = no_wb35) 
summary(gsn.tx.onetree) #C 5.47e-08 ***
anova(gsn.tx.onetree) #0.0182 *
plot(gsn.tx.onetree)  # no patterns evident
qqnorm(resid(gsn.tx.onetree))
qqline(resid(gsn.tx.onetree))

gsn.tx.daysoil <- lmer(gsw ~ tx + (1|day/PD_sm), data = no_wb35)  
summary(gsn.tx.daysoil) #C 2.57e-07 ***
anova(gsn.tx.daysoil) #0.03549 *
plot(gsn.tx.daysoil)  # no patterns evident
qqnorm(resid(gsn.tx.daysoil))
qqline(resid(gsn.tx.daysoil))

gsn.tx.daymdsoil <- lmer(gsw ~ tx + (1|day/MD_sm), data = no_wb35)  #lowest aic-406.4713
summary(gsn.tx.daymdsoil) #C 2.83e-07 ***
anova(gsn.tx.daymdsoil) #0.03677 *
plot(gsn.tx.daymdsoil)  # no patterns evident
qqnorm(resid(gsn.tx.daymdsoil))
qqline(resid(gsn.tx.daymdsoil))

AIC(gsn.tx.reptree, gsn.tx.onetree, gsn.tx.daysoil, gsn.tx.daymdsoil, k=4) #gsn.tx.daymdsoil

# gsn56 ~ tx #
no_wb56 <- no_wb_comp[-c(1:140), ]
gsn.tx.reptree <- lmer(gsw ~ tx + (1|rep/Tree), data = no_wb56)  #random fx = rep/tree
summary(gsn.tx.reptree) #
anova(gsn.tx.reptree) #all 0.006765 **
plot(gsn.tx.reptree)  # no patterns evident
qqnorm(resid(gsn.tx.reptree))
qqline(resid(gsn.tx.reptree))

gsn.tx.onetree <- lmer(gsw ~ tx + (1|Tree), data = no_wb56) #lowest aic-359.5456
summary(gsn.tx.onetree) #0.006765 **
anova(gsn.tx.onetree) #
plot(gsn.tx.onetree)  # no patterns evident
qqnorm(resid(gsn.tx.onetree))
qqline(resid(gsn.tx.onetree))

gsn.tx.daysoil <- lmer(gsw ~ tx + (1|day/PD_sm), data = no_wb56)  
summary(gsn.tx.daysoil) #
anova(gsn.tx.daysoil) #
plot(gsn.tx.daysoil)  # no patterns evident
qqnorm(resid(gsn.tx.daysoil))
qqline(resid(gsn.tx.daysoil))

gsn.tx.daymdsoil <- lmer(gsw ~ tx + (1|day/MD_sm), data = no_wb56) 
summary(gsn.tx.daymdsoil) #
anova(gsn.tx.daymdsoil) #
plot(gsn.tx.daymdsoil)  # no patterns evident
qqnorm(resid(gsn.tx.daymdsoil))
qqline(resid(gsn.tx.daymdsoil))

AIC(gsn.tx.reptree, gsn.tx.onetree, gsn.tx.daysoil, gsn.tx.daymdsoil, k=4) #gsn.tx.onetree

######################################
#Water potential trends
mdwp.day.tree <- lmer(MD_WP ~ day + (1|Tree), data=no_wb)
summary(mdwp.day.tree)
anova(mdwp.day.tree) #2.2e-16 ***
plot(mdwp.day.tree)
qqnorm(resid(mdwp.day.tree))
qqline(resid(mdwp.day.tree))

mdwp.day.tx <- lmer(MD_WP ~ day + (1|tx), data=no_wb)
summary(mdwp.day.tx)
anova(mdwp.day.tx) #2e-16 ***
plot(mdwp.day.tx)
qqnorm(resid(mdwp.day.tx))
qqline(resid(mdwp.day.tx))


#gsn_diff vs day or pdsm #DAY
ctest <- no_wb$tx == "C"
cctest <- no_wb$tx == "C-C"
cxctest <- no_wb$tx == "C-X-C"
cbtest <- no_wb$tx == "C - WB"
nobnoC <- no_wb[-c(1:35), ]
nobC <- no_wb[ctest, ]
nobCC <- no_wb[cctest, ]
nobCXC <- no_wb[cxctest, ]
nobCB <- no_wb[cbtest, ]

gsndiff.tx <- aov(gsn_diff ~ tx, data=nobnoC)
summary(gsndiff.tx) #0.0011 **
TukeyHSD(gsndiff.tx)
plot(gsndiff.tx)
qqnorm(resid(gsndiff.tx))
qqline(resid(gsndiff.tx))

gsndiff.day.tx <- lmer(gsn_diff ~ day + (1|Tree), data=nobnoC)
summary(gsndiff.day.tx)
anova(gsndiff.day.tx) #0.03457 *
plot(gsndiff.day.tx)
qqnorm(resid(gsndiff.day.tx))
qqline(resid(gsndiff.day.tx))

gsndiff.dayCC <- lmer(gsn_diff ~ day + (1|Tree), nobCC)
summary(gsndiff.dayCC) #0.151
gsndiff.dayCXC <- lmer(gsn_diff ~ day + (1|Tree), nobCXC)
summary(gsndiff.dayCXC) #0.227
gsndiff.dayCB <- lmer(gsn_diff ~ day + (1|Tree), nobCB)
summary(gsndiff.dayCB) #0.2414 

gsndiff.pdsm.tx <- lmer(gsn_diff ~ PD_sm + (1|tx), data=no_wb)
summary(gsndiff.pdsm.tx)
anova(gsndiff.pdsm.tx) #0.06387.
plot(gsndiff.pdsm.tx)
qqnorm(resid(gsndiff.pdsm.tx))
qqline(resid(gsndiff.pdsm.tx))


#gsn vs day or pdsm, mdsm
no_wb$PD_sm <- as.numeric(no_wb$PD_sm)
no_wb$MD_sm <- as.numeric(no_wb$MD_sm)
gsn.day.tx <- lmer(gsw ~ day + (1|tx), data=no_wb)
summary(gsn.day.tx)
anova(gsn.day.tx) #0.00138 **
plot(gsn.day.tx)
qqnorm(resid(gsn.day.tx))
qqline(resid(gsn.day.tx))

gsn.dayC <- lmer(gsw ~ day + (1|Tree), data=nobC)
summary(gsn.dayC) #0.872
gsn.dayCC <- lmer(gsw ~ day + (1|Tree), nobCC)
summary(gsn.dayCC) #0.0305 *
gsn.dayCXC <- lmer(gsw ~ day + (1|Tree), nobCXC)
summary(gsn.dayCXC) #0.019 *
gsn.dayCB <- lmer(gsw ~ day + (1|Tree), nobCB)
summary(gsn.dayCB) #0.2414


gsn.pdsm.tx <- lmer(gsw ~ PD_sm + (1|day), data=no_wb)
summary(gsn.pdsm.tx)
anova(gsn.pdsm.tx) #0.007786 **
plot(gsn.pdsm.tx)
qqnorm(resid(gsn.pdsm.tx))
qqline(resid(gsn.pdsm.tx))

gsn.mdsm.tx <- lmer(gsw ~ MD_sm + (1|day), data=no_wb)
summary(gsn.mdsm.tx)
anova(gsn.mdsm.tx) #0.05001 .
plot(gsn.mdsm.tx)
qqnorm(resid(gsn.mdsm.tx))
qqline(resid(gsn.mdsm.tx))

#growth ~ gsn
growthgsn <- read_excel("growthgsn.xlsx")
growthgsn$prim_growth <- as.numeric(growthgsn$prim_growth)
growthgsn$sec_growth <- as.numeric(growthgsn$sec_growth)
growthgsn$gsn <- as.numeric(growthgsn$gsn)
primgrow.avggsn.tx <- lmer(prim_growth ~ gsn +(1|tx), data=growthgsn)
summary(primgrow.avggsn.tx) #no sig 0.304
plot(primgrow.avggsn.tx)
qqnorm(resid(primgrow.avggsn.tx))

secgrow.avggsn.tx <- lmer(sec_growth ~ gsn +(1|tx), data=growthgsn)
summary(secgrow.avggsn.tx) #no sig 0.272
plot(secgrow.avggsn.tx)
qqnorm(resid(secgrow.avggsn.tx))

primgrow.hegyi <- lmer(prim_growth ~ hegyi + (1|tx), growthgsn)
summary(primgrow.hegyi) #0.961

secgrow.hegyi <- lmer(sec_growth ~ hegyi + (1|tx), growthgsn)
summary(secgrow.hegyi) #0.156



#max mdWP and gsn~pdsm regress CHECK W BILL
no_wb_regress <- read_excel("no_wb_gsndayregression.xlsx")
no_wb_regress$rl_gsn_pdsm <- as.numeric(no_wb_regress$rl_gsn_pdsm)
no_wb_regress$max_WP <- as.numeric(no_wb_regress$max_WP)
#Check w bill: tx/regress, regress*tx?
maxwp.gsnpdsmrltx <- lm(max_WP ~ (tx/rl_gsn_pdsm), data=no_wb_regress)
summary(maxwp.gsnpdsmrltx)#WB.
anova(maxwp.gsnpdsmrltx) 
plot(maxwp.gsnpdsmrltx)
qqnorm(resid(maxwp.gsnpdsmrltx))
qqline(resid(maxwp.gsnpdsmrltx))

#Enight days 1-35 ~ tx
En <- read_excel("En_boxplot.xlsx")
Enrate.tx <- aov(lm(En_rate ~ tx, data=En))
summary(Enrate.tx) #none 0.128
EntreeL.tx <- aov(lm(En_tree_L ~ tx, data=En))
summary(EntreeL.tx) #5.27e-05 ***
model.tables(EntreeL.tx, "means")
TukeyHSD(EntreeL.tx) #C-B/C, C-C/C-B, C-X-C/C-B

En2 <- read_excel("En_boxplot2.xlsx")
avgEnrate.tx <- aov(lm(avg_En_rate ~ tx, data=En2))
summary(avgEnrate.tx) #0.0421 *
TukeyHSD(avgEnrate.tx) #CC/CB, CXC/CB
avgEntreeL.tx <- aov(lm(En_tree_L ~ tx, data=En2))
summary(avgEntreeL.tx) #0.0022 **
TukeyHSD(avgEntreeL.tx) #CB/C, CC/CB, CXC/CB

En_per <- read_excel("percentE1.xlsx")
En_per <- (En_per[-c(141:270), ])
perEn.tx <- aov(lm(perE ~ tx, data=En_per))
summary(perEn.tx) #0.343, 0.0246 *
TukeyHSD(perEn.tx) # ,CB/C
En_per <- read_excel("percentE1.xlsx")
En_per <- (En_per[-c(1:140), ])
perEn.tx <- aov(lm(perE ~ tx, data=En_per))
summary(perEn.tx) #0.000514 ***
TukeyHSD(perEn.tx) # C-C/C-B, CB/C, CB/CXC

#fig1
no_wb_comp <- read_excel("no_wb_comp.xlsx")
no_wb_comp <- no_wb_comp[-c(141:270), ]

gsn35.tx <- aov(gsw ~ tx, no_wb_comp) #not averages
summary(gsn35.tx) #0.00333 **
TukeyHSD(gsn35.tx) #CB-CC 0.0145530 / CB-CXC 0.0022143
qqnorm(resid(gsn35.tx))
qqline(resid(gsn35.tx))

gsn35.comp <- aov(gsw ~ comp_type, no_wb_comp)
summary(gsn35.comp) #0.00139 **
TukeyHSD(gsn35.comp) #intrainter 0.0012241
qqnorm(resid(gsn35.comp))
qqline(resid(gsn35.comp))

#sm ~ day
pdsm.day <- aov(PD_sm ~ day, no_wb)
summary(pdsm.day) #<2e-16 ***
mdsm.day <- aov(MD_sm ~ day, no_wb)
summary(mdsm.day) #<2e-16 ***

#A~gsn
A.gsn <- lm(A_md ~ gsw, no_wb) #
summary(A.gsn) #0.00239 **

a.gsnC <- lmer(A_md ~ gsw + (1|Tree), data=nobC)
summary(a.gsnC) #0.204
a.gsnCC <- lmer(A_md ~ gsw + (1|Tree), nobCC)
summary(a.gsnCC) #0.522 
a.gsnCXC <- lmer(A_md ~ gsw + (1|Tree), nobCXC)
summary(a.gsnCXC) #0.343
a.gsnCB <- lmer(A_md ~ gsw + (1|Tree), nobCB)
summary(a.gsnCB) #0.000898 ***



#maxWP ~gsnrl
no_wb_regress <- read_excel("no_wb_gsndayregression.xlsx")
no_wb_regress$rl_gsn_day <- as.numeric(no_wb_regress$rl_gsn_day)
no_wb_regress$max_WP <- as.numeric(no_wb_regress$max_WP)

ctest <- no_wb_regress$tx == "C"
cctest <- no_wb_regress$tx == "C-C"
cxctest <- no_wb_regress$tx == "C-X-C"
cbtest <- no_wb_regress$tx == "C - WB"
regressC <- no_wb_regress[ctest, ]
regressCC <- no_wb_regress[cctest, ]
regressCXC <- no_wb_regress[cxctest, ]
regressCB <- no_wb_regress[cbtest, ]

maxwp.gsnrl <- aov(max_WP ~ rl_gsn_day, no_wb_regress)
summary(maxwp.gsnrl) #0.0268 *
maxwp.gsnrlC <- aov(max_WP ~ rl_gsn_day, regressC)
summary(maxwp.gsnrlC) #0.688
maxwp.gsnrlCC <- aov(max_WP ~ rl_gsn_day, regressCC)
summary(maxwp.gsnrlCC) #0.0653 .
maxwp.gsnrlCXC <- aov(max_WP ~ rl_gsn_day, regressCXC)
summary(maxwp.gsnrlCXC) #0.844
maxwp.gsnrlCB <- aov(max_WP ~ rl_gsn_day, regressCB)
summary(maxwp.gsnrlCB) #0.413

#%transp ~ %sm
figure9 <- read_excel("figure9.xlsx")
figure9 <- figure9[-c(1:270), ]
figure9$sm <- as.numeric(figure9$sm)
figure9$perE <- as.numeric(figure9$perE)

ctest <- figure9$tx == "C"
cctest <- figure9$tx == "C-C"
cxctest <- figure9$tx == "C-X-C"
cbtest <- figure9$tx == "C-WB"
figure9C <- figure9[ctest, ]
figure9CC <- figure9[cctest, ]
figure9CXC <- figure9[cxctest, ]
figure9CB <- figure9[cbtest, ]

transp.sm <- aov(perE ~ sm, figure9)
summary(transp.sm) #=1.49e-10 ***
transp.smC <- aov(perE ~ sm, figure9C)
summary(transp.smC) #0.0807 .
transp.smCC <- aov(perE ~ sm, figure9CC)
summary(transp.smCC) #0.128
transp.smCXC <- aov(perE ~ sm, figure9CXC)
summary(transp.smCXC) #0.0393 *
transp.smCB <- aov(perE ~ sm, figure9CB)
summary(transp.smCB) #3.41e-06 ***

