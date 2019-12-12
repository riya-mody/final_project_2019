rm(list=ls())

setwd("~/Desktop/comp sci project datasets")

#Reading in both datasets
open_data <- read.csv("countries.csv")
happiness <- read.csv("2016.csv")

#Renaming "Country.Name" to "Country"
colnames(open_data)[2] <- "Country" 

#Combining open_data and happiness datasets
library(dplyr)

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

#Ranking Countries by most --> least openness 

library(formattable)
open_data_happiness %>%
  arrange(desc(Openness)) %>%
  # Round our numeric variables to two decimal places
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

#Happiness vs. government trust graph

#Creating and saving graph
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggpmisc)

pdf("Happiness vs. Government Trust.PDF")

my.formula <- y ~ x

ggplot(open_data_happiness, 
       aes(x = Trust, 
           y = Happiness)) +
  geom_point(aes(color = Region),
             size = 2) +
  geom_smooth(method="lm",se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(x = "Government Trust",
       y = "Happiness Score",
       title = "Government Trust vs. Happiness by Region") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))

dev.off()

#Openness vs. Govt Trust graph

pdf("Opennes vs. Government Trust.PDF")

ggplot(open_data_happiness, 
       aes(x = Trust, 
           y = Openness)) +
  geom_point(aes(color = Region),
             size = 2) +
  geom_smooth(method="lm",se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(x = "Government Trust",
       y = "Openness",
       title = "Government Trust vs. Openness by Region") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))

dev.off()

#Correlation plot

#Dataset without these variables
happiness.2 =
  subset(happiness,select=-c(Country,Region,Lower.Confidence.Interval,Upper.Confidence.Interval,
                             Happiness.Rank,Dystopia.Residual))
#Renaming columns
colnames(happiness.2) <- c("Happiness","GDP",
                           "Family","Health",
                           "Freedom","Trust","Generosity")

#Plotting correlation
library(corrplot)

happiness.cor<-cor(happiness.2)
pdf("Happiness Correlation.PDF")
corrplot(happiness.cor,
         method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black",
         title="Correlation of Happiness and 6 Measured Factors")
dev.off()

#Open Data vs. Happiness graph

pdf("Open Data vs. Happiness.PDF")

ggplot(open_data_happiness, 
       aes(x = Openness, 
           y = Happiness)) +
  geom_point(aes(color = Region),
             size = 2) +
  geom_smooth(method="lm",se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(x = "Openness Score",
       y = "Happiness Score",
       title = "Are open data friendly countries happy countries?",
       subtitle = "Data Openness and Happiness by Region") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))

dev.off()




