## Our popgen component...

setwd("C:/Users/samue/Desktop")

#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)

data <- read.csv("relatedness_literature_review_working2.csv", stringsAsFactors = T)

colnames(data)

popgen <- data %>% 
  filter(Index == "Keep")%>%
  droplevels

levels(popgen$Focus)

popgen_1 <- popgen %>% 
  filter(Focus == "Popgen")%>%
  droplevels

nlevels(popgen_1$Title)

dim(popgen_1) #19 pure popgen

popgen_2 <- popgen %>% 
  filter(Focus == "Popgen + Demography")%>%
  droplevels

dim(popgen_2) # 1 on demogrpahy/popgen??

popgen_3 <- popgen %>% 
  filter(Focus == "Popgen + Reproduction")%>%
  droplevels

nlevels(popgen_3$Title)

dim(popgen_3) ## 4 for repro + popgen...

popgen_all <- rbind(popgen_1, 
                    popgen_2, 
                    popgen_3)

View(popgen_all) ## So we have all of our popgen focussed papers rolled into one now

nlevels(popgen_all$Order)
levels(popgen_all$Order)

nlevels(popgen_all$Species)

### Estimator time 

popgen_all$Kinship.Method <- recode_factor(popgen_all$Kinship.Method, "Allele counts  " = "Allele counts", "GERUD & COLONY " = "GERUD & COLONY", "COLONY + CERVUS" = "COLONY & CERVUS", "Allele counts & GERUD 1" = "Allele counts & GERUD", "Allele counts & GERUD 2.0" = "Allele counts & GERUD", "GERUD 2.0" = "GERUD", "GERUD 2.0 & COLONY" = "GERUD & COLONY", "Allele counts, GERUD 2.0 & COLONY2" = "Allele counts, GERUD 2.0 & COLONY", "IR Values & KINSHIP 1.3" = "Kinship 1.3", "Kinship 1.3 + Cervus 2.0 "= "Kinship 1.3 & Cervus", "GERUD 1.0, COLONY, STORM & allele counts" = "Allele counts, COLONY, GERUD & STORM", "GERUD 2.0, CERVUS 3.0.7 and COLONY" = "CERVUS, COLONY, & GERUD", "Allele counts, GERUD 2.0, COLONY" = "Allele counts, COLONY & GERUD", "GERUD & COLONY" = "COLONY & GERUD", "Allele counts, GERUD 2.0 & COLONY" = "Allele counts, COLONY & GERUD", "Sequoia, COLONY, dartR*" = "COLONY, dartR & Sequoia", "Coancestry + Sequioa " = "Coancestry & Sequioa",  "KinGroup  " =  "KinGroup", "COLONY v.2." = "COLONY", "MLRELATE, COLONY v1.2, KINGROUP 1" = "ML-Relate, COLONY & KINGROUP 1",  "KINFERENCE" = "Kinference", "COANCESRTY, COLONY, Cervus" = "Cervus, Coancestry & COLONY", "CERVUS, COLONY & COANCESRTY" = "Cervus, Coancestry & COLONY", "COLONY, MLRELATE, COANCESTRY" = "Coancestry, COLONY & ML-Relate")

estimator <- popgen_all %>%
  group_by(Kinship.Method, Title)%>% 
  count(Kinship.Method, sort= T)

# this checks to see where there may be double ups (do we count by paper or by investigation into species? I say via paper in my study)

print(estimator, n = 24)

estimator <- estimator[,1]
estimator<- data.frame(estimator)

ET <- table(estimator)
ET <- data.frame(ET) # tidy and factors in multiple papers 

ET

