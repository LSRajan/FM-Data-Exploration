#What if I want a list of players who are for example:
# At least 180cm, at least 80th percentile for progressive passes, play Center Back and will cost at between £30M - £40M?

#I will do it manually first to see the workflow

min_height <- 180
min_percentile <- 80
max_cost <- 40000000
min_cost <- 30000000
metric <- "Pr.Passes"
pos <- "Centre_Back"

#the problem with this is that players can have the same name,
left_join(performance, player_info, by = "Name")
#although we should be able to remove this problem by combining name, age, and club which are in both
#an improvement would be to preserve the ingame UIDs
performance |> 
  select(Name, Age, Club) |> 
  head(1)
player_info |> 
  select(Name, Age, Club) |> 
  head(1)

players <- left_join(performance, player_info, by = c("Name" = "Name", "Club" = "Club", "Age" = "Age"))

#if this works, we should have kept all players from performance

players |> 
  filter(is.na(Name)) |> 
  sum()
players |> 
  nrow()
performance |> 
  nrow()


#A look at calculating percentiles
players |> 
  mutate(percentile = ntile(get(metric),100)) |>
  select(Name, all_of(metric), percentile) |>
  arrange(desc(percentile), desc(get(metric)))

#A manual way to find this player list based on requirements
players1 <- players |> 
  mutate(percentile = ntile(get(metric),100))

players1 |> 
  filter(Height >= min_height & 
           Lower >= min_cost & 
           Upper <= max_cost &
           percentile >= 80 &
           get(pos) == TRUE
         ) |> 
  select(Name, Height, Lower, Upper, Pr.Passes, percentile, all_of(pos))


#Complete function that allows to filter through and ifnd targets for a position,
#Limitations: currently only one position and performance metric,
#             limited number of restrictions / filters,
#             very difficult to use  metric without knowing the table, some column names are
#             unreadable without context,
#             would require knowing how the function works. e.g. knowing what units to use

player_search <- function(min_height, min_age, max_age, min_cost, max_cost, metric, min_percentile, position){
  
  min_cost <- min_cost * 1e6
  max_cost <- max_cost * 1e6
  
  players <- left_join(performance, player_info, by = c("Name" = "Name", "Club" = "Club", "Age" = "Age"))
  
  players <- players |> 
    filter(get(position))
   # mutate(percentile = ntile(get(metric),100))   <---- causes error, unsure why but did it below instead
    
  players <- mutate(players, percentile = ntile(players[metric], 100))
  
  players |> 
    filter(Height >= min_height & 
             Age >= min_age &
             Age <= max_age &
             Lower >= min_cost & 
             Upper <= max_cost &
             percentile >= min_percentile
    ) |> 
    select(Name, Height, Lower, Upper, Age, all_of(metric), percentile, all_of(position)) |> 
    arrange(desc(percentile), desc(metric)) |> 
    mutate(Min_Cost_M = Lower/1e6, Max_Cost_M = Upper/1e6) |> 
    select(!c("Lower", "Upper", all_of(position)))
}

player_search(0, 15, 40, 0, 60,"Gls", 70, "Striker")
player_search(0, 20, 30, 0, 100, "ShT", 90, "Attacking_Midfield")
player_search(190, 20, 30, 20, 50, "Int.90", 70, "Centre_Back")
