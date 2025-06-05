setwd("C:/Users/samue/Desktop")

#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)

data <- read.csv("relatedness_literature_review_working2.csv", stringsAsFactors = T)

colnames(data)

repro <- data %>% 
  filter(Index == "Keep")%>%
  droplevels

repro1 <- repro %>% 
  filter(Focus == "Reproduction")%>%
  droplevels

repro2 <- repro %>% 
  filter(Focus == "Popgen + Reproduction") %>%
  droplevels

repro_data <- rbind(repro1, repro2)
 
data.frame(repro_data$n_samples) #check the data is good

repro_data$n_samples <- as.numeric(as.character(repro_data$n_samples))


## Now that we've gotten around the data wrangling side of things lets do some data analysis

## What is the average sample size used to examine reproduction and popgen/reproduction

summary(repro_data$n_samples, na.rm = T)

sd(repro_data$n_samples, na.rm = T)

View(repro_data)

## Now for number of litters 
repro_data$N_litters <- as.numeric(as.character(repro_data$N_litters))

summary(repro_data$N_litters)

sd(repro_data$N_litters, na.rm = T)

## ------------------------------------------------------------------------------

## Look at the types of analysis used 

repro_data$Kinship.Method <- recode_factor(repro_data$Kinship.Method, "Allele counts  " = "Allele counts", "GERUD & COLONY " = "GERUD & COLONY", "COLONY + CERVUS" = "COLONY & CERVUS", "Allele counts & GERUD 1" = "Allele counts & GERUD", "Allele counts & GERUD 2.0" = "Allele counts & GERUD", "GERUD 2.0" = "GERUD", "GERUD 2.0 & COLONY" = "GERUD & COLONY", "Allele counts, GERUD 2.0 & COLONY2" = "Allele counts, GERUD 2.0 & COLONY", "IR Values & KINSHIP 1.3" = "Kinship 1.3", "Kinship 1.3 + Cervus 2.0 "= "Kinship 1.3 & Cervus", "GERUD 1.0, COLONY, STORM & allele counts" = "Allele counts, COLONY, GERUD & STORM", "GERUD 2.0, CERVUS 3.0.7 and COLONY" = "CERVUS, COLONY, & GERUD", "Allele counts, GERUD 2.0, COLONY" = "Allele counts, COLONY & GERUD", "GERUD & COLONY" = "COLONY & GERUD", "Allele counts, GERUD 2.0 & COLONY" = "Allele counts, COLONY & GERUD", "Sequoia, COLONY, dartR*" = "COLONY, dartR, Sequoia")

estimator <- repro_data %>% 
 group_by(Kinship.Method)%>%
  count(Kinship.Method, sort= T)

print(estimator, n = 25)

##----------------------------------------------------------------

## Marker types

View(repro_data)

marker_no <- data.frame(repro_data %>%
  group_by(No..Marker.Types, Title)%>%
  count(No..Marker.Types, sort = T))

marker_no <- tibble(marker_no[,-3])

table(marker_no$No..Marker.Types)

print(marker_no)

## Msats 

summary(repro_data$No..mSats)
sd(repro_data$No..mSats, na.rm = T)

summary(repro_data$No..SNPs)
sd(repro$No..SNPs, na.rm = T)

summary(repro_data$n_samples)
sd(repro_data$n_samples, na.rm = T)

## Now we can make plots (a later problem specifically when presenting to Pete??)

## Note that we include a double up of any combinations in our calculations...either that or we consider the cominations in isolation...I could also amalgamate them rather than have them as subsection of discussion within the results...