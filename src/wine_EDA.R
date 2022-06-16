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
#Volatile & acidity

ggplot(wineqc, aes(quality, volatile_acidity)) +
  geom_jitter(width = 0.550, alpha = 0.5, colour = "light blue") +
  geom_smooth(method=lm, se=FALSE, colour = "blue")

# quality and ph
ggplot(wineqc, aes(quality, ph)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "light pink") +
  geom_smooth(method=lm, se=FALSE, colour = "pink")
#fixed acidity and quality
wineqc%>%
  ggplot(aes(factor(quality), `fixed_acidity`, group=quality))+geom_boxplot()+ggtitle("Fixed Acidity/quality")+theme_classic()



# quality and sulphates
ggplot(wineqc, aes(quality, sulphates)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "yellow") +
  geom_smooth(method=lm, se=FALSE, colour = "orange")

# quality & total sulfur dioxide
ggplot(wineqc, aes(quality, total_sulfur_dioxide)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "light pink") +
  geom_smooth(method=lm, se=FALSE, colour = "red")


# quality & chlorides
ggplot(wineqc, aes(quality, chlorides)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "light blue") +
  geom_smooth(method=lm, se=FALSE, colour = "blue")



# quality & citric acid
ggplot(wineqc, aes(quality, citric_acid)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "yellow") +
  geom_smooth(method=lm, se=FALSE, colour = "orange")

#Quality & Residual Sugar
wineqc%>%
  ggplot(aes(factor(quality), `residual_sugar`, group=quality))+geom_boxplot()+ggtitle("Residual Sugar & Quality")+theme_classic()


#Quality & density
wineqc%>%
  ggplot(aes(factor(quality), density, group=quality))+geom_boxplot()+ggtitle("Density & Quality")+theme_classic()

# free sulfur dioxide & quality
ggplot(wineqc, aes(quality, free_sulfur_dioxide)) +
  geom_jitter(width = 0.55, alpha = 0.5, colour = "yellow") +
  geom_smooth(method=lm, se=FALSE, colour = "orange")
