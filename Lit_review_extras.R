
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(reshape2)
require(gridExtra)
require(cowplot)
require(gt)
require(gtExtras)
library(viridis)
library(ggplot2)
library(ggthemes)
library(devEMF)



setwd("C:/Users/samue/Desktop/Honours/Chapter_1_lit_review/Metadata")

data_2 <- read.csv("relatedness_literature_review_working3.csv", stringsAsFactors = T)

data_2 <- data_2 %>% 
  filter(Index == "Keep")%>%
  droplevels

## Fixing estimator names

data_2$Kinship.Method <- recode_factor(data_2$Kinship.Method, "Allele counts  " = "Allele counts", "GERUD & COLONY " = "GERUD + COLONY", "COLONY + CERVUS" = "COLONY + CERVUS", "Allele counts & GERUD 1" = "Allele counts + GERUD", "Allele counts & GERUD 2.0" = "Allele counts + GERUD", "GERUD 2.0" = "GERUD", "GERUD 2.0 & COLONY" = "GERUD + COLONY", "Allele counts, GERUD 2.0 & COLONY2" = "Allele counts + GERUD 2.0 + COLONY", "IR Values & KINSHIP 1.3" = "Kinship 1.3", "Kinship 1.3 + Cervus 2.0 " = "Kinship 1.3 + Cervus", "GERUD 1.0, COLONY, STORM & allele counts" = "Allele counts + COLONY + GERUD + STORM", "GERUD 2.0, CERVUS 3.0.7 and COLONY" = "CERVUS + COLONY + GERUD", "Allele counts, GERUD 2.0, COLONY" = "Allele counts + COLONY + GERUD", "GERUD & COLONY" = "COLONY + GERUD", "Allele counts, GERUD 2.0 & COLONY" = "Allele counts + COLONY + GERUD", "Sequoia, COLONY, dartR*" = "COLONY + dartR + Sequoia", "KINFERENCE" = "Kinference", "COLONY2" = "COLONY", "COLONY v.2." = "COLONY", "MLRELATE, COLONY v1.2, KINGROUP 1" = "ML-Relate + COLONY + KinGroup", "KinGroup  " = "KinGroup", "Allele Counts + COLONY + GERUD" = "Allele counts + COLONY + GERUD", "Coancestry + COLONY+ CERVUS " = "Coancestry + COLONY + CERVUS", "COLONY & CERVUS" = "COLONY + CERVUS", "Kinship 1.3 & Cervus" = "Kinship 1.3 + Cervus", "Allele Counts" = "Allele counts", "Allele Counts + ML-Relate" = "Allele counts + ML-Relate")

estimator2 <- data_2 %>% 
  group_by(Kinship.Method, Focus)%>%
  count(Kinship.Method, sort= T)

estimator2$Kinship.Method <- as.character(estimator2$Kinship.Method)

expanded_data <- estimator2 %>%
  mutate(Kinship.Method = strsplit(Kinship.Method, " \\+ ")) %>% # Split the combinations
  unnest(Kinship.Method) %>%
  select(Kinship.Method, n, Focus)

expanded_data$Kinship.Method <- recode_factor(expanded_data$Kinship.Method, "Cervus" = "CERVUS", "CERVUS " = "CERVUS", "VCFtools (Yang)" = "VFCtools", "CKMRsim " = "CKMRsim", "Sequioa " = "Sequoia", "GENAlEX" = "GenAlEx", "KING" = "KING-Robust", "Allele counts  " = "Allele counts", "VFCtools" = "VCFtools", "KinGroup  " = "KinGroup")

expanded_data$Kinship.Method <- as.character(expanded_data$Kinship.Method)
expanded_data$Focus <- as.character(expanded_data$Focus)

expanded_data <- expanded_data %>%
  group_by(Kinship.Method, Focus) %>%
  summarise(Total_Frequency = sum(n, na.rm = TRUE), .groups = "drop")

print(expanded_data, n = 100)

expanded_data$Kinship.Method <- recode_factor(expanded_data$Kinship.Method, "VFCtools" = "VCFtools", "Coancestry" = "COANCESTRY")

expanded_data$Focus <- recode_factor(expanded_data$Focus, "Popgen" = "Population Genetics", "Social" = "Social Behaviour", "Reproduction" = "Reproductive Behaviour")

expanded_data$Focus <- factor(expanded_data$Focus, levels = c("Reproductive Behaviour", "Population Genetics", "Demography", "Social Behaviour"))

## Make Cat_Con_frame: 

Cat_Con_frame <- data.frame(Kinship.Method = c("Allele counts", "CERVUS", "CKMRsim", "COANCESTRY", "COLONY", "demerelate", "Endelman Jannik", "FAMOZ", "GenAlEx", "GERUD", "KINANALYZER", "Kinference", "KING-Robust", "KinGroup", "Kinship 1.3", "ML-Relate", "Sequoia", "SPAGEDI", "VCFtools"), 
                            Cat_Con = c("Cat", "Cat", "Cat", "Con", "Cat", "Con", "Con", "Cat", "Con", "Cat", "Cat", "Cat", "Con", "Cat", "Con", "Both", "Cat", "Con", "Con"))


estimator <- expanded_data %>%
  left_join(Cat_Con_frame, by = "Kinship.Method")


estimator <- estimator %>%
  mutate(Cat_Con = ifelse(is.na(Cat_Con), "Con", Cat_Con))

palette_sa <- c ("orange","skyblue", "grey")

## Use data not data_2 
 # For ascending order

print(estimator, n = 40)

estimator$Kinship.Method <- factor(estimator$Kinship.Method, levels=unique(estimator$Kinship.Method))

estimator$Total_Frequency <- as.numeric(as.character(estimator$Total_Frequency))

estimator$Kinship.Method <- factor(estimator$Kinship.Method, 
                                   levels = rev(sort(unique(estimator$Kinship.Method))))

# Now plot
estimatorplot_3B <- ggplot(estimator, aes(x = Kinship.Method, y = Total_Frequency, color = Cat_Con)) +
  geom_point(aes(size = 4), alpha = 0.8) +
  coord_flip() +# Use points as a dot chart
  geom_text(aes(label = Total_Frequency), color = "black", size = 3, vjust = 0.5) +  # Add numeric labels
  facet_wrap(~ Focus, scales = "fixed") +  # Facet by study focus
  scale_color_manual(values = palette_sa) +  # Apply color palette
  scale_size(range = c(3, 8)) +  # Adjust point size range
  labs(x = "Number of Studies", y = "Relatedness Estimator") +
  theme_cleveland() +
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30)) +# Clean theme
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    strip.background = element_rect(color = "black", linewidth = 0.5)
  )

# Print the plot
print(estimatorplot_3B)

#### Figure 3 A ####

## For all focuses 

estimator <- data %>% 
  group_by(Kinship.Method)%>%
  count(Kinship.Method, sort= T)

print(estimator, n = 45) # quick visual check

estimator$Kinship.Method <- as.character(estimator$Kinship.Method)

# Here we're essentially telling R to split anything with a + and place it into the column named Kinship.Method and then selecting only estiamtor names and their counts

estimator <- estimator %>%
  mutate(Kinship.Method = strsplit(Kinship.Method, " \\+ ")) %>% 
  unnest(Kinship.Method) %>%
  group_by(Kinship.Method) %>%
  summarise(n = sum(n), .groups = "drop")%>%
  arrange(desc(n))

print(estimator, n = 70)
# But there is a catch 
# There will be some data cleaning to do now that we have split things...it's just a part of it but can be prone to errors.

estimator$Kinship.Method <- recode_factor(estimator$Kinship.Method, "CERVUS " = "CERVUS", "Allele counts  " = "Allele counts", "CKMRsim " = "CKMRsim", "KinGroup  " = "KinGroup", "Sequioa " = "Sequoia", "Cervus" = "CERVUS", "VCFtools (Yang)" = "VCFtools", "CERVUS " = "CERVUS")

# Now, after some more visual checks we're confident we have removed any typos we can transform and count our estimator use properly

estimator$Kinship.Method <- as.character(estimator$Kinship.Method)

print(estimator, n = 50)

#Group it by the name, summarise it by calculating the sum of N for every row that each estimator occurs in (rather than counting how many rows it occurs in - easy mistake to make). Arrange it in descending order. 

estimator <- estimator %>%
  group_by(Kinship.Method) %>%
  summarise(Total_Frequency = sum(n, na.rm = TRUE)) %>%
  arrange(desc(Total_Frequency))

estimator$Kinship.Method

Kinship.Method <- unique(estimator$Kinship.Method)

Cat_Con <- c("Cat", "Cat", "Cat", "Con", "Both", "Cat","Cat", "Con", "Cat", "Con", "Con", "Cat", "Cat", "Cat", "Con", "Cat", "Cat", "Con", "Con", "Con")

Cat_Con_frame <- data.frame(Kinship.Method, Cat_Con) ## this is our referemce

glimpse(Cat_Con_frame)

estimator <- estimator %>%
  left_join(Cat_Con_frame, by = "Kinship.Method")

# Print it :)

print(estimator, n = 100)

estimator$Kinship.Method <- factor(estimator$Kinship.Method, 
                                   levels = rev(sort(unique(estimator$Kinship.Method))))

estimatorplot_3A <- ggplot(estimator, aes(x = Kinship.Method, y = Total_Frequency, color = Cat_Con)) +
  geom_point(aes(size = 4), alpha = 0.8) +
  coord_flip() +# Use points as a dot chart
  geom_text(aes(label = Total_Frequency), color = "black", size = 3, vjust = 0.5) +  # Add numeric labels
  scale_color_manual(values = palette_sa) +  # Apply color palette
  scale_size(range = c(3, 8)) +  # Adjust point size range
  labs(x = "Number of Studies", y = "Relatedness Estimator") +
  theme_cleveland() +
  scale_y_continuous(breaks = seq(0, 50, by = 5), limits = c(0, 50)) +# Clean theme
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.margin = unit(c(1, 1, 1, 1), "cm"), 
    strip.background = element_rect(color = "black", linewidth = 0.5)
  )

# Print the plot
print(estimatorplot_3A)

figure_3AB <- ggarrange(estimatorplot_3A, estimatorplot_3B,
          labels = c("A", "B"),
          common.legend = F,
          ncol = 2, nrow = 1)

emf("C:/Users/samue/Desktop/Honours/Chapter_1_lit_review/New_Plots/estimatorplot_3AB.emf", width = 12, height = 10)  # Set the width and height in inches
print(figure_3AB)
dev.off()


















