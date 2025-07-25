library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#Read in and clean Data
player_info <- readxl::read_excel("Very_General.xlsx", sheet = 2)

#There are 15 rows with just NA values, just remove them
player_info <- player_info |> 
  filter(!is.na(Name))

#clean up column names
colnames(player_info) <- make.names(colnames(player_info))

names(player_info)[names(player_info) == "Nat...11"] <- "Nation"
names(player_info)[names(player_info) == "Nat...47"] <- "Natural.Fitness"


#removing columns that are not of use
no_use <- c("Inf.", "Rec", "No.", "UID", "WR")
player_info <- subset(player_info, select = !(names(player_info) %in% no_use))

summary(player_info)

#problems: transfer value, wage--, release clause--, pref, transfer fees received--, goals--, height--, weight-- should be numeric

#HEIGHT/WEIGHT ---
player_info$Height <- substring(player_info$Height, first = 1, last  = 3)
player_info$Height <- as.numeric(player_info$Height)
player_info$Weight <- substring(player_info$Weight, first = 1, last = 3)
player_info$Weight <- as.numeric(player_info$Weight)

#PREF SHIRT NUMBER ----
player_info$Pref.[player_info$Pref. == "-"] <- NA
player_info$Pref. <- as.numeric(player_info$Pref.)

#WAGE ---- with wage, all are in similar form "£xxx,xxx p/w"
player_info$Wage <- substring(player_info$Wage, first = 2, last = nchar(player_info$Wage) - 4)
player_info$Wage <- as.integer(gsub("," , "", player_info$Wage))

#GOALS ---- Goals should be int, but na is "-"
player_info$Goals[player_info$Goals == "-"] <- 0
player_info$Goals <- as.integer(player_info$Goals)

#MIN FEE RELEASE CLAUSE ----, should replace "-" with na. also convert values from "£27k" or "£39.2M" to 27000 etc.
player_info$Min.Fee.Rls[player_info$Min.Fee.Rls == "-"] <- NA

#values are either NA, K, M or B
unique(substring(player_info$Min.Fee.Rls, first = nchar(player_info$Min.Fee.Rls)))

#remove "£"
player_info$Min.Fee.Rls <- gsub("£" , "", player_info$Min.Fee.Rls)

#separate corresponding unit and multiply to get full value in £.
player_info <- player_info |> 
  mutate(mult = if_else(is.na(Min.Fee.Rls), NA, substring(Min.Fee.Rls, first = nchar(Min.Fee.Rls))),
         Min.Fee.Rls = substring(Min.Fee.Rls, first = 1, last = nchar(Min.Fee.Rls) - 1),
         Min.Fee.Rls = as.numeric(Min.Fee.Rls),
         mult = case_when(
           mult == "B" ~ 1000000000,
           mult == "M" ~ 1000000,
           mult == "K" ~ 1000,
           mult == NA ~ NA
         ),
         Min.Fee.Rls = Min.Fee.Rls * mult
  ) |> 
  select(!mult)

#TRANSFER FEES RECIEVED ---- similar problem to release clause
player_info$Transfer.Fees.Received <- gsub("£" , "", player_info$Transfer.Fees.Received)

player_info <- player_info |> 
  mutate(mult = if_else(Transfer.Fees.Received == "0", 
                        "0", 
                        substring(Transfer.Fees.Received, first = nchar(Transfer.Fees.Received))),
         Transfer.Fees.Received = if_else(Transfer.Fees.Received == "0",
                                          "0", 
                                          substring(Transfer.Fees.Received, first = 1, last = nchar(Transfer.Fees.Received) - 1)),
         Transfer.Fees.Received = as.numeric(Transfer.Fees.Received),
         mult = case_when(
           mult == "B" ~ 1000000000,
           mult == "M" ~ 1000000,
           mult == "K" ~ 1000,
           mult == "0" ~ 0
         ),
         Transfer.Fees.Received = Transfer.Fees.Received * mult
  ) |> 
  select(!mult)



#Now lets clean Home-Grown Status, ordinal encoding is fine, the order works with respect to how useful they
#are in registration rules. While none are necessarily bad on their own.
player_info |> 
  distinct(Home.Grown.Status)

player_info <- player_info |> 
  mutate(Home.Grown.Status = as.numeric(case_when(
    Home.Grown.Status == "-" ~ 0,
    Home.Grown.Status == "Trained in nation (15-21)" ~ 1,
    Home.Grown.Status == "Trained in nation (0-21)" ~ 2,
    Home.Grown.Status == "Trained at club (0-21)" ~ 3
  )))



#Note for looking at preferred foot, some players are Left Only rather than Left. this is a preference, not to do with
#foot strength necessarily

#creating levels for foot strength, (Very Weak, Weak, Reasonable, Fairly Strong, Strong, Very Strong)
player_info |> 
  distinct(Left.Foot)
player_info |> 
  distinct(Right.Foot)

foot_levels <- c('Very Weak', 'Weak', 'Reasonable', 'Fairly Strong', 'Strong', 'Very Strong')

player_info$Left.Foot <- factor(player_info$Left.Foot, levels = foot_levels)
player_info$Right.Foot <- factor(player_info$Right.Foot, levels = foot_levels)

player_info <- player_info |> 
  mutate(Strict.Foot = grepl(" Only", Preferred.Foot),
         Preferred.Foot = gsub(" Only", "", Preferred.Foot))


#In the favoured columns, there is some error in how the data was exported.
num_errors <- player_info$Favoured.Clubs |> 
  str_detect("|c:disabled|") |> 
  sum()
#this error is in every column
num_errors / nrow(player_info)
#Generally however this information wont be of much use. 



#Cleaning Transfer Value
player_info$Transfer.Value <- gsub("Not for Sale", "1000000000 - 1000000000",player_info$Transfer.Value)
player_info$Transfer.Value <- gsub("£", "",player_info$Transfer.Value)

#this adds in a separator so that values are consistent
player_info$Transfer.Value <- if_else(grepl(" - ", player_info$Transfer.Value),
                                      player_info$Transfer.Value,
                                      paste(player_info$Transfer.Value , " - " , player_info$Transfer.Value))

#separate lower and upper values into two columns
player_info <- player_info |> 
  separate_wider_delim(Transfer.Value, " - ", names = c("Lower", "Upper"))

player_info <- player_info |> 
  mutate(mult = str_extract(Lower, "[A-Z]+"),
         Lower = parse_number(Lower),
         mult = case_when(
           mult == "B" ~ 1000000000,
           mult == "M" ~ 1000000,
           mult == "K" ~ 1000,
           TRUE ~ 1        
         ),
         Lower = Lower * mult,
         
         mult = str_extract(Upper, "[A-Z]+"),
         Upper = parse_number(Upper),
         mult = case_when(
           mult == "B" ~ 1000000000,
           mult == "M" ~ 1000000,
           mult == "K" ~ 1000,
           TRUE ~ 1        
         ),
         Upper = Upper * mult
  ) |> 
  select(!mult)

