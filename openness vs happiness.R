rm(list=ls())

setwd("~/Desktop/comp sci project datasets")

library(dplyr)

#Reading in both datasets
open_data <- read.csv("countries.csv")
happiness <- read.csv("2016.csv")

#Renaming "Country.Name" to "Country"
colnames(open_data)[2] <- "Country" 

#Combining open_data and happiness datasets
open_data_happiness <- open_data %>% 
  left_join(happiness, by = "Country") %>%
  mutate(Country = factor(Country)) %>%
  select(Country, Region, X2015.Score, Happiness.Score, Economy..GDP.per.Capita., 
         Family, Health..Life.Expectancy., Freedom, Trust..Government.Corruption., 
         Generosity, Dystopia.Residual)

#Renaming columns
colnames(open_data_happiness) <- c("Country", "Region", "Openness", "Happiness", 
                                   "GDP", "Family", "Health", "Freedom", "Trust", 
                                   "Generosity", "DystopiaResidual")

library(formattable)

open_data_happiness %>%
  #Arranges by which countries are the most open
  arrange(desc(Openness)) %>%
  #Rounds out numeric variables to two decimal places
  mutate_each(funs(round(., 2)), -c(Country, Region, Openness)) %>%
  head(10) %>%
  formattable(list(
    Openness = color_bar("yellow"),
    Happiness = color_bar("lightgreen"),
    GDP = color_bar("deepskyblue"),
    Family = color_bar("deepskyblue"),
    Health = color_bar("deepskyblue"),
    Freedom = color_bar("deepskyblue"),
    Trust = color_bar("deepskyblue"),
    Generosity = color_bar("deepskyblue"),
    DystopiaResidual = color_bar("deepskyblue")
  ), align = "l")

library(ggplot2)
library(ggthemes)
library(viridis)

pdf("Open Data vs. Happiness.PDF")
ggplot(open_data_happiness, 
       aes(x = Openness, 
           y = Happiness)) +
  geom_point(aes(color = Region),
             size = 2) +
  geom_smooth(method="lm") +
  labs(x = "Openness Score",
       y = "Happiness Score",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data openness and happiness by country in 2015") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))
dev.off()





