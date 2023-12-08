library(ggpubr)
library(readxl)
no_wb <- read_excel("PREDAWN.xlsx")
no_wb <-no_wb[-c(306:1048575), ] 
Difference_in_gsn_from_Solo_C <- no_wb$gsn_diff
Day <- no_wb$day
Planting_Group <- no_wb$tx
library(ggplot2)
(colour_plot <- ggplot(no_wb, aes(x = Day, y = Difference_in_gsn_from_Solo_C, colour = Planting_Group))
  + xlim(0,57)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta","darkviolet", "steelblue1"))
  + theme(text = element_text(size = 20))  
  + theme(legend.position = "right")
  + geom_rect(aes(xmin=0, xmax=7,ymin=-Inf,ymax=Inf), fill="palegreen", color = "palegreen")
  + geom_rect(aes(xmin=7, xmax=14,ymin=-Inf,ymax=Inf), fill="lightgoldenrodyellow", color = "lightgoldenrodyellow")
  + geom_rect(aes(xmin=14, xmax=35,ymin=-Inf,ymax=Inf), fill="navajowhite", color= "navajowhite")
  + geom_rect(aes(xmin=35, xmax=57,ymin=-Inf,ymax=Inf), fill="lightsalmon", color = "lightsalmon")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  + annotate("text", x = 3.5, y = -0.1, label = "100%")
  + annotate("text", x = 10.5, y = -0.1, label = "50%")
  + annotate("text", x = 24, y = -0.1, label = "33%")
  + annotate("text", x = 46, y = -0.1, label = "17%")
  + stat_regline_equation(
  mapping = NULL,
  data = NULL,
  formula = y ~ x,
  label.x.npc = "left",
  label.y.npc = "top",
  label.x = NULL,
  label.y = c(0.25,0.23,0.21,0.19),
  output.type = "expression",
  geom = "text",
  position = "identity",
  inherit.aes = TRUE)
)


PD_sm <- as.numeric(no_wb$PD_sm)
#Planting_Group <- no_wb$tx
no_wb$PD_sm <- PD_sm
(colour_plot_sm <- ggplot(no_wb, aes(x = PD_sm, y = Difference_in_gsn_from_Solo_C, colour = Planting_Group))
  + xlim(100,0)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "darkviolet", "steelblue1"))
  #+ theme(text = element_text(size = 20))  
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  #+ stat_regline_equation(
    # mapping = NULL,
    # data = NULL,
    # formula = y ~ x,
    # label.x.npc = "left",
    # label.y.npc = "top",
    # label.x = NULL,
    # label.y = c(0.25,0.23,0.21,0.19),
    # output.type = "expression",
    # geom = "text",
    # position = "identity",
    # inherit.aes = TRUE)
)

PD_sm <- as.numeric(no_wb$PD_sm)
no_wb$PD_sm <- PD_sm
no_wb <- subset(no_wb, tx!= "WB-C (WB)") ###Remove WB###
(colour_plot_sm <- ggplot(no_wb, aes(x = PD_sm, y = gsw, colour = tx))
  + xlim(100,5)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "steelblue1"))
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  + stat_regline_equation(
    mapping = NULL,
    data = NULL,
    formula = y ~ x,
    label.x.npc = "left",
    label.y.npc = "top",
    label.x = NULL,
    label.y = c(0.26,0.24,0.2,0.16),
    output.type = "expression",
    geom = "text",
    position = "identity",
    inherit.aes = TRUE)
)

MD_sm <- as.numeric(no_wb$MD_sm)
#Planting_Group <- no_wb$tx
no_wb$MD_sm <- MD_sm
no_wb <- subset(no_wb, tx!= "WB-C (WB)")
(colour_plot_sm <- ggplot(no_wb, aes(x = MD_sm, y = gsw, colour = tx))
  + xlim(100,5)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "steelblue1"))
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  + stat_regline_equation(
    mapping = NULL,
    data = NULL,
    formula = y ~ x,
    label.x.npc = "left",
    label.y.npc = "top",
    label.x = NULL,
    output.type = "expression",
    geom = "text",
    position = "identity",
    inherit.aes = TRUE)
)

(colour_plot_sm <- ggplot(no_wb, aes(x = day, y = gsw, colour = tx))
  + xlim(0,56)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "steelblue1")) #"darkviolet"
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_hline(yintercept = 0)
  + stat_regline_equation(
    mapping = NULL,
    data = NULL,
    formula = y ~ x,
    label.x.npc = "left",
    label.y.npc = "top",
    label.x = NULL,
    label.y = c(0.26,0.24,0.2,0.16),
    output.type = "expression",
    geom = "text",
    position = "identity",
    inherit.aes = TRUE)
)
#gsn~day slopes
#cxc 0.00066x
#cwb 0.00054x
#cc 0.00060x
#c 0.000049x

####OUTLIERS????MORE
PD_sm <- as.numeric(no_wb$PD_sm)
MD_WP <- as.numeric(no_wb$MD_WP)
WP_diff <- as.numeric(no_wb$WP_diff)
no_wb$PD_sm <- PD_sm
no_wb_outl <- no_wb#[-c(205,206,213,214,218,219,225,226,229,230), ]
Planting_Group_outl <- no_wb_outl$tx
(colour_plot_sm <- ggplot(no_wb_outl, aes(x = PD_sm, y = WP_diff, colour = Planting_Group_outl))
  + xlim(100,0)
  + ylim(0,15)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "darkviolet", "steelblue1"))
  #+ theme(text = element_text(size = 20))  
  + theme(legend.position = "top")
  + geom_point(size = 1) + geom_smooth(method = "lm", se=FALSE)
)
(daymdwp <- ggplot(no_wb, aes(x = day, y = MD_WP, colour = tx))
  + xlim(0,50)
  + ylim(25,5)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "darkviolet", "steelblue1"))
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  # + stat_regline_equation(
  #   mapping = NULL,
  #   data = NULL,
  #   formula = y ~ x)
)

PD_sm <- as.numeric(no_wb$PD_sm)
PD_WP <- as.numeric(no_wb$PD_WP)
no_wb$PD_sm <- PD_sm
no_wb_outl <- no_wb[-c(205,206,213,214,218,219,225,226,229,230), ]
Planting_Group_outl <- no_wb_outl$tx
(colour_plot_sm <- ggplot(no_wb_outl, aes(x = PD_sm, y = PD_WP, colour = Planting_Group_outl))
  + xlim(100,0)
  + ylim(20,3)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "darkviolet", "steelblue1"))
  #+ theme(text = element_text(size = 20))  
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  #+ geom_hline(yintercept = 0)
)

#make boxplot
gsw <- as.numeric(no_wb$gsw)
competition <- as.numeric(no_wb$competition)
no_wb$gsw <- gsw
no_wb$competition <- competition
(colour_plot_sm <- ggplot(no_wb, aes(x = competition, y = gsw, colour = tx))
  + xlim(-0.0051,0.055)
  + theme_classic()
  + scale_color_manual(values=c("darkviolet", '1',"blue", "magenta", "steelblue1")) #"magenta"darkviolet
  + theme(text = element_text(size = 20))  
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + geom_smooth(inherit.aes = FALSE, method = "lm", mapping = aes(x = competition, y = gsw, colour = "All"))
)
(nocolour_plot_sm <- ggplot(no_wb, aes(x = competition, y = gsw))
  + xlim(-0.01,0.08)
  + theme_classic()
  + theme(text = element_text(size = 20))  
  # + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm", colour = "red")
)



#convert A to md
A_md <- as.numeric(no_wb$A_md)
no_wb$A <- A_md
(colour_plot_sm <- ggplot(no_wb, aes(x = gsw, y = A_md, colour = tx))
  + xlim(0,0.3)
  + theme_base()
  + scale_color_manual(values=c('1',"blue", "magenta", "steelblue1")) #, "darkviolet"
  + theme(legend.position = 'none')
  + geom_point(size = 2) + geom_smooth(method = "lm")
  + ylim(-2,28)
)

gs_md <- as.numeric(no_wb$gs_md)
no_wb$A <- gs_md
(colour_plot_sm <- ggplot(no_wb, aes(x = gsw, y = gs_md, colour = tx))
  + xlim(0,0.3)
  + theme_classic()
  + scale_color_manual(values=c('1',"blue", "magenta", "darkviolet", "steelblue1"))
  #+ theme(text = element_text(size = 20))  
  + theme(legend.position = "top")
  + geom_point(size = 2) + geom_smooth(method = "lm")
)