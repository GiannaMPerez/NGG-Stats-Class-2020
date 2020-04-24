#Script to make violin plots of TMS/fMRI evoked data for my NGG QNC statistics presentation 
#Add libraries needed to do all of this 
library(dplyr)
library(reshape2)
library(geepack)
library(ggplot2)
library(ggpubr)
library(factoextra)
library(readxl)
library(ppcor)
library(ggThemeAssist)
library(Hmisc)
library(tidyverse)

#First, read in data: 
df_all_ER <- read.csv("C:/Users/Gianna/Downloads/ER_sp120_MNI_TNI_ZAPR01_03_19_2020.csv", header=FALSE)
#Naming the different column
names(df_all_ER) <- c("ID", "Study", "Condition","Image", "Seed", "ER", "Space" )

#Only want a subset of the data so you have to filter it
#First filter is for controls 
onlyControls<- filter(df_all_ER, str_detect(ID, "^C"))
#Second filter is to get rid of PreM1
noPreM1<- filter(onlyControls, str_detect(Condition, "PreIFG|PreSgACC")) 
#Third filter is so we only have seed of ListonSG
onlyListonSG<- filter(noPreM1,str_detect(Seed,"ListonSG"))

#Making plots to check for normalcy
ggdensity(onlyListonSG$ER,
          main= "Density plot of ER",
          xlab= "Evoked Response")

#QQ Plot
ggqqplot(onlyListonSG$ER)

#Shapiro-Wilk's Test
shapiro.test(onlyListonSG$ER) 

#Want to check for normalcy of each different condition 
NormalCheckPreIFG<- filter(onlyListonSG,str_detect(Condition,"PreIFG"))
NormalCheckPreSgACC<- filter(onlyListonSG,str_detect(Condition,"PreSgACC"))

ggdensity(NormalCheckPreIFG$ER,
          main= "Density plot of ER for PreIFG",
          xlab= "Evoked Response")

ggdensity(NormalCheckPreSgACC$ER,
          main= "Density plot of ER for PreSgACC",
          xlab= "Evoked Response")

#QQ plots
ggqqplot(NormalCheckPreIFG$ER)
ggqqplot(NormalCheckPreSgACC$ER)

#Shapiro-Wilk's Test
shapiro.test(NormalCheckPreIFG$ER)
shapiro.test(NormalCheckPreSgACC$ER)

#Making the Violin plots 
plot_ER <- ggplot(onlyListonSG, aes(y=ER, x=Condition, fill=Condition))
plot_ER + geom_violin(aes(), trim = FALSE) + stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") + scale_fill_manual(values=c("#E69F00", "#56B4E9")) + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)                                                                                                                                   

#Adding some stats
#seeing if vairances are equal
var.test(onlyListonSG$ER ~ onlyListonSG$Condition)

#Mann-Whitney Test
wilcox.test(onlyListonSG$ER ~ onlyListonSG$Condition)