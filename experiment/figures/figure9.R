library(ggpubr)
library(readxl)
figure9 <- read_excel("figure9.xlsx")
figure9$sm <- as.numeric(figure9$sm)
figure9$perE <- as.numeric(figure9$perE)

(sm_perE_plot <- ggplot(data = figure9, mapping = aes(x = sm, y = perE, colour = tx))
  + theme_base() + xlim(100,0) + ylab("% Transpiration at Night") + xlab("Nighttime Soil Moisture")
  + geom_point(size = 2) + geom_smooth(method = "lm", se=FALSE)
  + scale_color_manual(values=c("darkviolet",'1',"blue", "steelblue1", "magenta"))
  + theme(legend.position = 'none')#
 )

#in base

ctest <- (figure9$tx) == "C"
cwbtest <- (figure9$tx) == "C-WB"
cctest <- (figure9$tx) == "C-C"
cxctest <- (figure9$tx) == "C-X-C"

with(figure9, plot(sm[ctest], perE[ctest], bty = "l", pch = 20, xlim = c(100,5), ylim = c(0,78), ylab = "Nighttime Percent of Midday Transpiration (%)"))
fig9linreg <- lm(perE~sm, figure9)
fig9linregc <- lm(perE[ctest]~sm[ctest], figure9)
abline(fig9linregc, lwd=2)
with(figure9, points(sm[cwbtest], perE[cwbtest], pch = 20, col="steelblue1"))
fig9linregcwb <- lm(perE[cwbtest]~sm[cwbtest], figure9)
abline(fig9linregcwb, lwd=2, col="steelblue1")
with(figure9, points(sm[cctest], perE[cctest], pch = 20, col="blue"))
fig9linregcc <- lm(perE[cctest]~sm[cctest], figure9)
abline(fig9linregcc, lwd=2, col="blue")
with(figure9, points(sm[cxctest], perE[cxctest], pch = 20, col="magenta"))
fig9linregcxc <- lm(perE[cxctest]~sm[cxctest], figure9)
abline(fig9linregcxc, lwd=2, col="magenta")
abline(fig9linreg, lwd=3, col= "darkviolet")
