library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(readr)

#Assumes player_info is loaded and cleaned in Exploring_1 First

#I want to look closer at numeric variables, relationships etc.

#There appears to be a relationship between transfer fees recieved and wages.
ggplot(player_info, aes(x = Transfer.Fees.Received / 1000000, y = Wage / 1000)) +
  geom_point(alpha = 0.4) +
  labs(y = "Wage (£thousands p/w", 
       x = "Transfer Fees recieved (£ millions)", 
       title = "Do more expensive players also have higher wages?")
player_info |> 
  filter(Wage / 1000 < 600, Transfer.Fees.Received/ 1000000 < 200) |> 
  ggplot(aes(x = Transfer.Fees.Received / 1000000, y = Wage / 1000)) +
  geom_point(alpha = 0.4) +
  labs(y = "Wage (£thousands p/w", 
       x = "Transfer Fees recieved (£ millions)", 
       title = "Do players who have made more in transfer fees also have higher wages?") +
  stat_smooth(method = "lm", se = FALSE)

cor(player_info$Transfer.Fees.Received, player_info$Wage)


#Comparing upper transfer value to wage
#This graph is a bit misleading, so the correlation label is included.
#most of the points are in the bottom left, where less correlation is present.
#There does seem to be some correlation past this area however.
#Extreme values are excluded to make the graph more clear

temp_cor <- as.character(cor(player_info$Upper, player_info$Wage))
label <- paste("Correlation: ", temp_cor)

player_info |> 
  filter(Upper < 1000000000 & Wage < 800000) |> 
  ggplot(aes(x = Upper/1000000, y = Wage/1000)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm", se= FALSE) +
  labs(x = "Max Transfer Value (£M)", y = "Wage (£K)", title = "Do More expensive players get higher wages?") +
  annotate("text", x = 50, y = 400, parse = TRUE, label = label)

           