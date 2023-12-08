library(readxl)
no_wb <- read_excel("no_wb.xlsx")
no_wb$gsn_diff <- as.numeric(no_wb$gsn_diff)
no_wb$PD_sm <- as.numeric(no_wb$PD_sm)
library(ggplot2)
(figure4 <- ggplot(no_wb, aes(x = PD_sm, y = gsn_diff, colour = tx))
  + xlim(100,0)
  + theme_base()
  + scale_color_manual(values=c('1',"blue", "magenta", "steelblue1")) #darkviolet
  + theme(legend.position = 'none')
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  #slopes + stat_regline_equation(mapping = NULL, data = NULL, formula = y ~ x, label.x.npc = "left", label.y.npc = "top",
      #label.x = NULL,  label.y = c(0.21,0.19,0.17,0.15),  output.type = "expression",  geom = "text",  position = "identity",  inherit.aes = TRUE)
)
#calculate slopes
#cc  0.000059
#cxc 0.0004
#cwb 0.00029