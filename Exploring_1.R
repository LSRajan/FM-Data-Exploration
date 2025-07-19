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



#Exploring Columns -----

#Exploring preferred number
preferred_numbers <- player_info |> 
  group_by(Pref.) |> 
  summarise(n = n()) |> 
  arrange(desc(n ))

preferred_numbers$Pref. <- as.factor(preferred_numbers$Pref.)

preferred_numbers |> head(10) |> 
  ggplot(aes(x = n, y = reorder(Pref., n))) +
  geom_bar(stat = "identity") +
  labs(y = "Preferred Shirt Number", title = "Evidently, most players have no known number preference") 

preferred_numbers |> filter(!is.na(Pref.)) |> head(10) |> 
  ggplot(aes(x = n, y = reorder(Pref., n))) +
  geom_bar(stat = "identity") +
  labs(y = "Preferred Shirt Number", x = "", title = "The Most commonly preferred squad numbers")

#numbers with only one player with said preference
preferred_numbers |> filter(!is.na(Pref.)) |> filter(n == 1)
#and how many is that?
preferred_numbers |> filter(!is.na(Pref.)) |> filter(n == 1) |> nrow()

#who has a preference for 50 and 99?
player_info |> 
  filter(Pref. == 99 | Pref. == 50) |> 
  select(Name, Pref., Club, Position)


#squad size
squad_size <- player_info |> 
  group_by(Club) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

#biggest vs smallest squad
squad_size |> filter(n == max(n) | n == min(n))

#This presents an interesting problem, Frosinone should not have just 8 players
#Player count will be different based on the search criteria to get the data, e.g. not including youth players
#Upon inspection, the criteria excludes players with below 100 Appearances and frosinone's squad was very young with many
#loans, who hadnt had much match experience and so were excluded from the search.
#It will be useful to account for this in my exploration.

#most common nationalities, partly biased in that different leagues might have more teams,
#meaning the country of a league with more players is likely to have more players here.
nationalities <- player_info |> 
  group_by(Nation) |> 
  summarise(n = n()) |> 
  arrange(desc(n))

nationalities |> head(10) |> 
  ggplot(aes(x = n, y = reorder(Nation, n))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), 
            hjust = 1.4,
            color = "white",
            size = 4,
            fontface = "bold"
  ) +
  labs(title = "Most represented Nationalities in the top 5 leagues", x = "", y ="") +
  scale_x_continuous(expand = c(0,1)) +
  theme(panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

#least represented nationalities
nationalities |> filter(n == 1)

#What percentage of players play for their nation of birth? (This is not wholy representative of country
#trends, no data is present about the player's upbringing, other eligibility etc.)

player_info |> 
  filter(Nation == NoB) |> 
  nrow() / nrow(player_info) * 100

#Which countries are most often not represented despite being born there?
switched <- player_info |> 
  mutate(switched = Nation != NoB)

switched |> 
  group_by(NoB) |> 
  summarise(changed_nation = sum(switched)) |> 
  arrange(desc(changed_nation))

#this data isnt representative given players are more commonly from these countries, proportion should be used instead
switched |> 
  group_by(NoB) |> 
  summarise(changed_nation = sum(switched) / n(),
            total_players = n()) |> 
  arrange(desc(changed_nation))

#Which countries have players who stick to their nation of birth
switched |> 
  group_by(NoB) |> 
  summarise(changed_nation = sum(switched) / n(),
            total_players = n()) |> 
  arrange(desc(changed_nation)) |> tail(10)


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

#which leagues have the most home grown players?
#Ok, turns out home grown status only means home grown for premier league. 
#Still interesting that there are very few players playing abroad in the top 5 leagues
home_grown_division <- player_info |> 
  filter(Home.Grown.Status > 0) |> 
  group_by(Division) |> 
  summarise(num_hg = n())

#same thing but as a percentage

num_players_division <- player_info |> 
  group_by(Division) |> 
  summarise(num_total = n())

hg_percentages_division <- left_join(home_grown, num_players, by = "Division") |>
  mutate(percent = num_hg / num_total * 100)

#look at hg_precent by club, just looking at premier league
home_grown_club <- player_info |>  
  filter(Division == "English Premier Division") |> 
  filter(Home.Grown.Status > 0) |> 
  group_by(Club) |> 
  summarise(num_hg = n())

num_players_club <- player_info |> 
  filter(Division == "English Premier Division") |> 
  group_by(Club) |> 
  summarise(num_total = n())

hg_percentages_club <- left_join(home_grown_club, num_players_club, by = "Club") |>
  mutate(percent = num_hg / num_total * 100)

#We see some interesting things, partly, better performing teams tend to be able to sign more players from abroad
#so we see smaller homegrown percentages with these teams.
#Wolves appear to have a preference toward portugese speaking players, which is reflected here in their low hg percent
hg_percentages_club |> arrange(desc(percent))

#There is another trend we can see due to how the data was collected, Brighton and Chelsea must have many players
#with less than 100 appearances, because only 15 players is unreasonable.
#On the other hand, tottenham, aston villa and arsenal are three top clubs who seem to not use inexperienced players as much
hg_percentages_club |> arrange(num_total)

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

player_info |> 
  group_by(Preferred.Foot) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = Preferred.Foot, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Preferred Foot", y = "", title = "Player foot preferences") +
  geom_text(aes(label = n), 
            vjust = -0.5,
            color = "black",
            size = 4,
            fontface = "bold"
  ) +
  theme(panel.grid = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

#Which players can use either foot?
player_info |> 
  filter(Preferred.Foot == "Either") |> 
  select(Name, Preferred.Foot, Club)

#Exploring personalities
player_info |> 
  distinct(Personality) |> 
  View()

player_info |> 
  group_by(Personality) |> 
  summarise(n = n()) |> 
  arrange(desc(n))
player_info |> 
  group_by(Personality) |> 
  summarise(n = n()) |> 
  arrange(n)

#Some interesting players, the only ones with these personalities
player_info |> 
  filter(Personality == "Iron Willed" | Personality == "Loyal") |> 
  select(Name, Club, Personality)

#Just using transfer fees received, its interesting that the only player with loyal personality has clearly made a big move,
#possibly more than one.
player_info |> 
  filter(Personality == "Loyal") |> 
  select(Name, Club, Transfer.Fees.Received)

#In the favoured columns, there is some error in how the data was exported.
num_errors <- player_info$Favoured.Clubs |> 
  str_detect("|c:disabled|") |> 
  sum()
#this error is in every column
num_errors / nrow(player_info)
#Generally however this information wont be of much use. 

