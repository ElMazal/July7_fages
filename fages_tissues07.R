title: "N. rachovi_fAGEs"
author: "Ellen"
date: "2024-07-07"
output: html_document

#load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
install.packages("readxl")
library(readxl)


#read in data
data_ages <- read_excel("tissues_nrachovi.xlsx")


reduce_ages <- data_ages[-c(1,2,3,4)] 
reduce_ages


str(reduce_ages)

reduce_ages2 <- reduce_ages %>% 
  group_by(Sex,Habitat, Age) %>% 
  summarise(variance = var(Mean_RFU))

reduce_ages2

reduce_ages2 |> group_by(Sex) |> tally()
reduce_ages2 |> count(Habitat)

###Stats
mean_reduce_ages3 <- reduce_ages |>
  group_by(Sex) |>
  summarize(mean = mean(Mean_RFU),
            sd = sd(Mean_RFU))
mean_reduce_ages3

mean_reduce_ages4 <- reduce_ages |>
  group_by(Habitat) |>
  summarize(mean = mean(Mean_RFU),
            sd = sd(Mean_RFU))
mean_reduce_ages4

mean_reduce_ages5 <- reduce_ages |>
  group_by(tissue) |>
  summarize(mean = mean(Mean_RFU),
            sd = sd(Mean_RFU))
mean_reduce_ages5

t.test(Mean_RFU ~ Habitat, data = reduce_ages, var.equal = TRUE)
t.test(Mean_RFU ~ Sex, data = reduce_ages, var.equal = TRUE)

#####

# plot 1
p <- ggplot(data = reduce_ages, aes(x = tissue, y = Mean_RFU, color = Sex)) + geom_boxplot()
p + facet_wrap(~Habitat + Age, nrow = 1) +
  labs(title = "Mean RFU in different killifish tissues from laboratory and mesocosm", x = "Tissue", y= "Mean RFU") +
  theme(text=element_text(size=15))


# Filter data for a specific tissue (1. Liver)
data_liver <- reduce_ages |>
  filter(tissue == "liver")

# Plotting boxplot for liver
ggplot(data_liver, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean RFU for Liver",
       x = "Habitat",
       y = "Mean RFU") +
  theme(text=element_text(size=15)


#add facet wrap ~age

# Plotting boxplot with facet wrap by Age
ggplot(data_liver, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  facet_wrap(~ Age) +  # Facet by Age
  labs(title = "Boxplot of Mean RFU by Habitat and Age",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()


# Filter data for a specific tissue (2. gills)
data_gills <- reduce_ages |>
  filter(tissue == "gills")
data_gills

# Plotting boxplot for gill
ggplot(data_gills, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean RFU for gill",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()


#add facet wrap ~age

# Plotting boxplot with facet wrap by Age
ggplot(data_gills, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  facet_wrap(~ Age) +  # Facet by Age
  labs(title = "Boxplot of Mean RFU by Habitat and Age",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()


# Filter data for a specific tissue (3. heart)
data_heart <- reduce_ages |>
  filter(tissue == "heart")
data_heart

# Plotting boxplot for heart
ggplot(data_heart, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean RFU for heart",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()


#add facet wrap ~age

# Plotting boxplot with facet wrap by Age
ggplot(data_heart, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  facet_wrap(~ Age) +  # Facet by Age
  labs(title = "Boxplot of Mean RFU by Habitat and Age",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()

##4
# Filter data for a specific tissue (4. brain)
data_brain <- reduce_ages |>
  filter(tissue == "brain")
data_brain

# Plotting boxplot for heart
ggplot(data_brain, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  labs(title = "Boxplot of Mean RFU for brain",
       x = "Habitat",
       y = "Mean RFU") 



#add facet wrap ~age

# Plotting boxplot with facet wrap by Age
ggplot(data_brain, aes(x = Habitat, y = Mean_RFU, fill = Habitat)) +
  geom_boxplot() +
  facet_wrap(~ Age) +  # Facet by Age
  labs(title = "Boxplot of Mean RFU by Habitat and Age",
       x = "Habitat",
       y = "Mean RFU") +
  theme_minimal()

