#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)

setwd("C:/Users/samue/Desktop")

data <- read.csv("relatedness_literature_review_working2.csv", stringsAsFactors = T)

colnames(data)

demo <- data %>% 
  filter(Index == "Keep")%>%
  droplevels

demo1 <- demo%>% 
  filter(Focus == "Demography")%>%
  droplevels

nlevels(demo1$Title)

demo2 <- demo %>%
  filter(Focus == "Popgen + Demography")%>%
  droplevels()

nlevels(demo2$Title)

demo_all <- rbind(demo1, demo2); demo_all

## Clean it up so we have desired columns...
dim(demo_all)
colnames(demo_all)

demo_all <- demo_all[, -c(1:4,21:27)]

demo_all$n_samples <- as.character(demo_all$n_samples)

demo_all$n_samples[3] = 183

demo_all$n_samples <- as.numeric(demo_all$n_samples)

demo_all ## all data for sample sizes should be good to analyse now

## Some summary stats 

summary(demo_all$n_samples)

sd(demo_all$n_samples)
