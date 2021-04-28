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

data_df <- read.csv(file="texture_data_LVP_ORL.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

## function to give number of replicates if necessary

give.n <- function(x){
  return(data.frame(y=max(x)*1.15, label=length(x)))
}
nlabels <- table(data_df$microns)


##reorders genotypes (pick one)

data_df$genotype3 <- factor(data_df$genotype, levels=c("LVP_WT","ORL_WT"))

## picks colours

nb.cols <- 20
m.pal = colorRampPalette(brewer.pal(20,"Dark2"))(20)

## creates new variable for interaction between genotype and microns groupings (pick one)


data_df$genotypemicrons3 <- interaction(data_df$genotype3, data_df$microns)

## output file

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_PI_ORL_LVP_T2.pdf",width=10, height=5)

## graph titles 

la_title <- expression(paste("Preference index (PI) of ", italic("Aedes aegypti "), "(Liverpool and Orlando) females by texture"))

## graph code

a <- ggplot(data=data_df, aes(y=preference,x=microns,group=genotypemicrons3,fill=genotype3))+ geom_boxplot() + 
##  stat_summary(fun.data=give.n,geom="text",size=2,position=position_dodge(width=2.90)) +
  coord_cartesian(ylim=c(-1,1)) +
  scale_x_continuous(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190), labels=c(0,30.2,35,40.5,46.2,68,82,92,115,190),expand=c(0.025,0),sec.axis=dup_axis(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190),labels=c("smooth", "P500","P400","P360","P320", "P220", "P180", "150", "120", "80"),name="grit")) +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=7))+
  xlab("microns")+
  ylab("preference index (PI)")+
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

data_df <- read.csv(file="texture_data_LVP_IR76B.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

data_df$genotype1 <- factor(data_df$genotype, levels=c("wt","ir76b_61_61"))

data_df$genotypemicrons1 <- interaction(data_df$genotype1, data_df$microns)

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_PI_LVP_ir76b_T2.pdf",width=10, height=5)

a_title <- expression(paste("Preference index (PI) of ", italic("Aedes aegypti "), "(Liverpool and ", italic("Ir76b-/-)"), " females by texture"))

b <- ggplot(data=data_df, aes(y=preference,x=microns,group=genotypemicrons1,fill=genotype1))+ geom_boxplot() + 
  ##  stat_summary(fun.data=give.n,geom="text",size=2,position=position_dodge(width=2.90)) +
  coord_cartesian(ylim=c(-1,1)) +
  scale_x_continuous(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190), labels=c(0,30.2,35,40.5,46.2,68,82,92,115,190),expand=c(0.025,0),sec.axis=dup_axis(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190),labels=c("smooth", "P500","P400","P360","P320", "P220", "P180", "150", "120", "80"),name="grit")) +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=7))+
  xlab("microns")+
  ylab("preference index (PI)")+
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
  scale_fill_manual(values=c("#e76d44", "#92e62f"), name="genotype", labels=c("LVP", "Ir76b-/-"))



plot(b)


dev.off()


data_df <- read.csv(file="texture_data_Orlando.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_PI_ORL_T1.pdf",width=10, height=5)

xlabs <- paste(breaks.major)
my_title <- expression(paste("Preference index (PI) of ", italic("Aedes aegypti"), " (Orlando) females by texture"))


c <- ggplot(data=data_df, aes(y=preference,x=microns,group=grit2))+ geom_boxplot(fill="#00BFC4") + 
##  stat_summary(fun.data=give.n,geom="text",size=2,position=position_dodge(width=2.90)) +
  coord_cartesian(ylim=c(-1,1)) +
  scale_x_continuous(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190,265,425), labels=xlabs, expand=c(0.025,0),sec.axis=dup_axis(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190,265,425),labels=c("smooth", "P500","P400","P360","P320", "P220", "P180", "150", "120", "80", "60", "40"),name="grit")) +
  geom_hline(yintercept=0)+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=7))+
  xlab("microns")+
  ylab("preference index (PI)")+
  ggtitle(my_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="genotype", labels=c("Orlando(ORL)"))

plot(c)

dev.off()

data_df <- read.csv(file="texture_data_Orlando_shortened_up_to_80.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

pdf(file="C:/Users/HP/Desktop/RStudio/Texture_Plots/texture_PI_ORL_T1_shortened.pdf",width=10, height=5)

d <- ggplot(data=data_df, aes(y=preference,x=microns,group=grit2))+ geom_boxplot(fill="#00BFC4") + 
  ##  stat_summary(fun.data=give.n,geom="text",size=2,position=position_dodge(width=2.90)) +
  coord_cartesian(ylim=c(-1,1),xlim=c(0,195)) +
  scale_x_continuous(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190), labels=c(0,30.2,35,40.5,46.2,68,82,92,115,190), expand=c(0.025,0),sec.axis=dup_axis(breaks=c(0,30.2,35,40.5,46.2,68,82,92,115,190),labels=c("smooth", "P500","P400","P360","P320", "P220", "P180", "150", "120", "80"),name="grit")) +
  geom_hline(yintercept=0)+
  xlab("microns")+
  ylab("preference index (PI)")+
  theme(axis.text.x = element_text(angle = -60, vjust = 0.5, hjust=0.75, size=10, face="bold", colour="black"), 
        axis.title = element_text(size=13,colour="black", face="bold"),
        axis.text.y = element_text(face="bold", colour="black"))+
  ggtitle(my_title)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="genotype", labels=c("Orlando(ORL)"))

plot(d)

dev.off()



