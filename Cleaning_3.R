library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#Read in and clean Data
#This data is for players in the Premier League, 
prem_performance <- readxl::read_excel("prem_performance.xlsx")

names(prem_performance)[names(prem_performance) == "Best Role...3"] <- "Ast.Best.Pos"
names(prem_performance)[names(prem_performance) == "Best Role...4"] <- "Ast.Best.Role"

colnames(prem_performance) <- make.names(colnames(prem_performance))

prem_performance$NP.xG.90 <- as.numeric(prem_performance$NP.xG.90)
prem_performance$xA.90 <- as.numeric(prem_performance$xA.90)