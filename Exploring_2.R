library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)

#Assumes player_info is loaded and cleaned in Exploring_1 First

#I want to look closer at numeric variables, relationships etc.

#There appears to be a relationship between transfer fees recieved and wages.
ggplot(player_info, aes(x = Transfer.Fees.Received / 1000000, y = Wage / 1000)) +
  geom_point() +
  labs(y = "Wage (£thousands p/w", 
       x = "Transfer Fees recieved (£ millions)", 
       title = "Do more expensive players also have higher wages?")
player_info |> 
  filter(Wage / 1000 < 600, Transfer.Fees.Received/ 1000000 < 200) |> 
  ggplot(aes(x = Transfer.Fees.Received / 1000000, y = Wage / 1000)) +
  geom_point() +
  labs(y = "Wage (£thousands p/w", 
       x = "Transfer Fees recieved (£ millions)", 
       title = "Do more expensive players also have higher wages?") +
  stat_smooth(method = "lm", se = FALSE)

cor(player_info$Transfer.Fees.Received, player_info$Wage)

#Cleaning Transfer Value -- INCOMPLETE
player_info$Transfer.Value <- gsub("Not for Sale", "1000000000 - 1000000000",player_info$Transfer.Value)
player_info$Transfer.Value <- gsub("£", "",player_info$Transfer.Value)

player_info |> 
  separate_wider_delim(Transfer.Value, " - ", names = c("lower", "upper"))
