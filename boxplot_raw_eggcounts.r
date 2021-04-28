library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(grid)
library(RColorBrewer)
library(plotrix)
library(cowplot)
library(gg.gap)

theme_set(
  theme_classic() + 
    theme(legend.position = "top")+
    theme(plot.background = element_rect(color = "black", size = 2))
)

## raw input text data file

data_df <- read.csv(file="raw_eggcounts_LVP_ORL.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

## function to get number of trials if needed

give.n <- function(x){
  return(data.frame(y=max(x)*1.15, label=length(x)))
}
nlabels <- table(data_df$microns)


##reorders genotypes

data_df$genotype3 <- factor(data_df$genotype, levels=c("LVP_WT","ORL_WT"))

## picks colours

nb.cols <- 20
m.pal = colorRampPalette(brewer.pal(20,"Dark2"))(20)

## creates new variable of genotype and micron groupings

data_df$genotypemicrons3 <- interaction(data_df$genotype3, data_df$microns)

## output file

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/raw_eggcounts_ORL_LVP_T2.pdf",width=10, height=5)

## graph title 

la_title <- expression(paste("Raw egg counts of ", italic("Aedes aegypti "), "(Liverpool and Orlando) females by texture"))

## graph code

a <- ggplot(data=data_df, aes(y=eggcount,x=microns,group=genotypemicrons3,fill=genotype3))+ geom_boxplot() + 
  ##  stat_summary(fun.data=give.n,geom="text",size=2,position=position_dodge(width=2.90)) +
  scale_x_continuous(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190), labels=c(0,30.2,35,40.5,46.2,68,82,92,115,190),expand=c(0.025,0),sec.axis=dup_axis(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190),labels=c("smooth", "P500","P400","P360","P320", "P220", "P180", "150", "120", "80"),name="grit")) +
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=7))+
  xlab("microns")+
  ylab("egg count")+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=10, face="bold", colour="black"), 
        axis.title = element_text(size=13,colour="black", face="bold"),
        axis.text.y = element_text(face="bold", colour="black"),
        legend.position=c(0.93,0.2),
        legend.title = element_text(colour="black",size=17,face="bold"),
        legend.text= element_text(colour="black",size=17),
        legend.background = element_rect(fill="gray95", size=1, linetype="solid")
  )+
  ggtitle(la_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="genotype",labels=c("LVP", "ORL"))


plot(a)


dev.off()
