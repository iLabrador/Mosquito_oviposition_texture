library(tidyverse)
library(ggplot2)
library(car)
library(multcomp)
library(emmeans)
library(dplyr)

data_df <- read.csv(file="texture_data_Orlando_shortened_up_to_80.txt", head=TRUE,sep=",", skipNul=TRUE, stringsAsFactors = TRUE)

data_df 

dat <- data_df %>%
  dplyr::select(grit2,preference)

summary(dat)

res_aov <- aov(preference ~ grit2,
               data = dat
               )

##does shapiro test for all residuals together

shapiro.test(res_aov$residuals)

##does shapiro test for each grit separately

listids <- list()
for (ids in unique(data_df$grit2)){
  sub_df <- subset(x=data_df, subset=grit2==ids)
  listids[[ids]] <- shapiro.test(sub_df$preference)
}

listids

##does Krusrak Wallis for each

kw <- kruskal.test(preference ~ grit2,
                                data = dat)
kw

##Levene's test for homogeneity of variance across groups
leveneTest(preference ~ grit2,
           data = dat
           )

summary(res_aov)

##Tukey HSD test (compare two by two; no reference group)

post_test <- glht(res_aov,
                  linfct=mcp(grit2="Tukey")
                  )
summary(post_test)

inter.test1 <- emmeans(res_aov,"grit2")
cld(inter.test1, Letter="abcdefghijk")

data_df <- read.csv(file="ALL_ND_ORL_T1_shortened_up_to_80.txt", header=TRUE,sep="\t", skipNul=TRUE, stringsAsFactors = TRUE, row.names=NULL)

data_df$logavg6 <- log(data_df$avg6) 

dat <- data_df %>%
  dplyr::select(texture,logavg6)

summary(dat)

res_aov <- aov(logavg6 ~ texture,
               data = dat
)

##does shapiro test for all residuals together

shapiro.test(res_aov$residuals[0:5000])

##does shapiro test for each grit separately

listids <- list()
for (ids in unique(data_df$texture)){
  sub_df <- subset(x=data_df, subset=texture==ids)
  listids[[ids]] <- shapiro.test(sub_df$logavg6[0:5000])
}

listids

##does Krusrak Wallis for each

kw <- kruskal.test(logavg6 ~ texture,
                   data = dat)
kw

##Levene's test for homogeneity of variance across groups
leveneTest(logavg6 ~ texture,
           data = dat
)

summary(res_aov)

##Tukey HSD test (compare two by two; no reference group)

post_test <- glht(res_aov,
                  linfct=mcp(texture="Tukey")
)
summary(post_test)

inter.test1 <- emmeans(res_aov,"texture")
cld(inter.test1, Letter="abcdefghijk")

