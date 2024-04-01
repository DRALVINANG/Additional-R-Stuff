#-------------------------------------------------------------------------------
# Statistics with Tidyverse by Dr. Alvin Ang
#-------------------------------------------------------------------------------
# 1. Install Tidyverse Package

install.packages("tidyverse", dependencies=TRUE)

#-------------------------------------------------------------------------------
#2. Load Libraries

library(tidyverse)
library(tibble)
library(tidyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

#-------------------------------------------------------------------------------
#3a.Correlation I
df<-data.frame(
  X=c(90,90,60,60,30),
  Y=c(60,90,60,60,30))

b = cor(df)

#-------------------------------------------------------------------------------
#3b. Correlation II
heart<- read.csv(
  "https://www.alvinang.sg/s/heart.csv",
  header=FALSE,sep=",",na.strings = '?')

heart %>%
  select(age,chol,fbs,thalach,exang) %>%
  cor() 

#-------------------------------------------------------------------------------
#4a.  Hypothesis Testing (Two Tailed Test)
boxplot(extra~group,data=sleep)

t.test(extra~group,data=sleep)

#-------------------------------------------------------------------------------
#4b. Hypothesis Testing (One Tailed Test)
boxplot(weight~feed,data=chickwts)
d = subset(chickwts,feed == "casein" | feed =="horsebean")

#2 tailed test
t.test(weight~feed,data=d)

#1 tailed test
t.test(weight~feed,data=chickwts.test,alternative='less')

#-------------------------------------------------------------------------------
#5a. Chickwts ANOVA

boxplot(weight~feed, data = chickwts)

m <- aov(weight~feed,data=chickwts)
summary(m)

#-------------------------------------------------------------------------------
#5b. ANOVA on Shampoo

# Data frame
shampoo = data.frame(
  'A' = c(36.6,39.2,30.4,37.1,34.1),
  'B' = c(17.5,20.6,18.7,25.7,22.0),
  'C' = c(15.0,10.4,18.9,10.5,15.2))

# Tibble
shampoo <- as_tibble(shampoo)

# Box Plot
shampoo %>%
  gather(brand, effect) %>%
  boxplot(effect~brand,.)

# ANOVA
shampoo %>%
  gather(brand, effect) %>%
  aov(effect~brand,.)
  summary(.)

#-------------------------------------------------------------------------------
#THE END
#-------------------------------------------------------------------------------
