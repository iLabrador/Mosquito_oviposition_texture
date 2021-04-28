library(tidyverse)
library(ggplot2)
library(car)
library(multcomp)
library(emmeans)
library(dplyr)
library(readr)
library(grid)

theme_update(plot.title = element_text(hjust = 0.5))

## input file

filenames2 <- c("ALL_ND_ORL_LVP_T2.txt")

## output file

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/ALL_ORL_LVP_violin.pdf",width=13)

## split violin code

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

## actual violin plot code

for (j in 1:length(filenames2)){
  data_file = read.csv(filenames2[j],sep="\t", skipNul=TRUE, stringsAsFactors = TRUE, header = TRUE, row.names = NULL)
  data_file$texture <- factor(data_file$texture, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))
  a <- ggplot(data_file, aes(x=texture, y=log(avg6), fill=genotype)) +
    geom_split_violin() +
    labs(x="texture (grit)", y="Average distance of 6 closest neighbours (mm)")+
    coord_cartesian(ylim=c(2,8))+
##    geom_boxplot(width=0.25,size=0.25,position = position_dodge(width=0.25),outlier.shape = NA,alpha=0)+
    scale_y_continuous(labels=function(x)x*60/3200)+
    stat_summary(fun=median,fun.min=median,fun.max=median, geom="crossbar", width=0.25,size=0.25,position=position_dodge(width=0.25))+
    theme(axis.text.x = element_text(size=13, colour="black"), 
          axis.title = element_text(size=13,colour="black", face="bold"),
          axis.text.y = element_text(size=12, colour="black"),
          legend.position=c(0.5,0.93),
          legend.direction = "horizontal",
          legend.title = element_text(colour="black",size=17,face="bold"),
          legend.text= element_text(colour="black",size=17),
          legend.background = element_rect(fill="gray95", size=1, linetype="solid")
    )
          

  plot(a)
}

dev.off()

## input file

filenames1 <- c("ALL_ND_ORL_T1_shortened_up_to_80.txt")

## output file

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_vioins_Orlando_term_1_averaged_shortened.pdf",width=13,height=6)

## non-split violin plot code

for (j in 1:length(filenames1)){
   data_file = read.csv(filenames1[j],sep="\t", skipNul=TRUE, stringsAsFactors = TRUE, header = TRUE, row.names = NULL)
   data_file$logavg6 <- log(data_file$avg6)
   y_breaks=c(10^0.025, 10^0.05,10^0.075,10^0.1)
   y_labels=formatC(y_breaks, format="f",digits=2)
   data_file$texture <- factor(data_file$texture, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))
   a <- ggplot(data_file, aes(factor(x=texture), y=logavg6)) +
     geom_violin(fill="#00BFC4") +
     coord_cartesian(ylim=c(2,7)) +
     labs(x="texture (grit)", y="Log of average distance of 6 closest neighbours (mm)")+
##     geom_boxplot(width=0.15,size=0.25,outlier.shape = NA,alpha=0)
     scale_y_continuous(labels=function(y)y*60/3200,breaks=c(80/60,160/60,240/60,320/60,400/60,480/60))+
     stat_summary(fun=median,fun.min=median,fun.max=median, geom="crossbar", width=0.25,size=0.25,position=position_dodge(width=0.25))+
     theme(axis.text.x = element_text(size=13, colour="black"), 
           axis.title = element_text(size=13,colour="black", face="bold"),
           axis.text.y = element_text(size=12, colour="black"))
plot(a)

}

dev.off()

## input file

filenames3 <- c("ALL_ND_ORL_T1.txt")

## output file

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_vioins_Orlando_term_1_averaged.pdf",width=13,height=6)

## non-split violin plot code

for (j in 1:length(filenames3)){
  data_file = read.csv(filenames3[j],sep="\t", skipNul=TRUE, stringsAsFactors = TRUE, header = TRUE, row.names = NULL)
  data_file$logavg6 <- log(data_file$avg6)
  y_breaks=c(10^0.025, 10^0.05,10^0.075,10^0.1)
  y_labels=formatC(y_breaks, format="f",digits=2)
  data_file$texture <- factor(data_file$texture, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))
  a <- ggplot(data_file, aes(factor(x=texture), y=logavg6)) +
    geom_violin(fill="#00BFC4") +
    coord_cartesian(ylim=c(2,7)) +
    labs(x="texture (grit)", y="Log of average distance of 6 closest neighbours (mm)")+
    ##     geom_boxplot(width=0.15,size=0.25,outlier.shape = NA,alpha=0)
    scale_y_continuous(labels=function(y)y*60/3200,breaks=c(80/60,160/60,240/60,320/60,400/60,480/60))+
    stat_summary(fun=median,fun.min=median,fun.max=median, geom="crossbar", width=0.25,size=0.25,position=position_dodge(width=0.25))+
    theme(axis.text.x = element_text(size=13, colour="black"), 
          axis.title = element_text(size=13,colour="black", face="bold"),
          axis.text.y = element_text(size=12, colour="black"))
  plot(a)
  
}

dev.off()
