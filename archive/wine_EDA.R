# Importing packages
library(tidyverse)
library(ggplot2)
library(janitor)
library(tidyr)
library(here)
library(readr)
library(corrplot)

#importing data
wineq <- read_csv ("~/Documents/misk-DSI/individual-assignment-2-dinaa-kh/Data/winequality-red.csv")
view(wineq)

#cleaning up names in the data set using janitor + change ph.
wineqc <- wineq %>%
  clean_names()

names(wineqc)[9] <- "ph"
view(wineqc)

#taking a look 
str(wineqc)
summary(wineqc)
length(wineqc)
dim(wineqc)
names(wineqc)
summary(wineqc$quality)
table(wineqc$quality) 

# the correlation
wine = cor(wineqc)
corrplot(wine)
#As the plot offers, alcohol has the most robust correlation with wine quality.

##boxplot multiple
wineqc%>%
  gather(-quality, key = "var", value = "value") %>% 
  ggplot(aes(x = factor(quality), y = value, color = quality, group = quality)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  guides(fill = guide_legend(title = "Quality"))+ xlab("quality")
# plots quality/ alcohol
ggplot(wineqc, aes(x = factor(quality), y =alcohol, fill = quality)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  geom_boxplot() +
  guides(fill = guide_legend(title = "Quality"))+ xlab("quality")
