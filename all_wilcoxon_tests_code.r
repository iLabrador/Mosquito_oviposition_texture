library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(grid)
library(RColorBrewer)
library(plotrix)
library(cowplot)
library(gg.gap)

## Wilcoxon for preference index graphs (lvp/orl and lvp/ir76b)

data1 <- read.csv(file="texture_data_LVP_ORL.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

data2 <- read.csv(file="texture_data_LVP_IR76B.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = FALSE)

data1$textures <- factor(data1$grit2, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))


listids <- list()
for (ids in unique(data1$textures)){
  print(ids)
  this_data <- subset(data1,grit2 == ids)
  ##  print(this_data)
  listids[[ids]] <- wilcox.test(preference ~ genotype, data=this_data)
}

listids

data2$textures <- factor(data2$grit2, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))


listids <- list()
for (ids in unique(data2$textures)){
  print(ids)
  this_data <- subset(data2,grit2 == ids)
  ##  print(this_data)
  listids[[ids]] <- wilcox.test(preference ~ genotype, data=this_data)
}

listids

## wilcoxon for violin plot (orl/lvp)

data_df <- read.csv(file="ALL_ND_ORL_LVP_T2.txt", head=TRUE,sep="\t", skipNul=TRUE, stringsAsFactors = FALSE)

data_df$logavg6 <- log(data_df$avg6)
data_df$textures <- factor(data_df$texture, levels= c("smooth","P500","P400","P360","P320","P220","P180","150","120","80"))



listids <- list()
for (ids in unique(data_df$textures)){
  print(ids)
  this_data <- subset(data_df,texture == ids)
  ##print(this_data)
  listids[[ids]] <- wilcox.test(logavg6 ~ genotype, data=this_data)
}

listids
