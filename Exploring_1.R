library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

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

