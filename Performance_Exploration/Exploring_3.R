library(cluster)
library(mclust)
library(ggrepel)
library(factoextra)
library(dbscan)

performance |> 
  filter(Striker | Attacking_Midfield) |> 
  ggplot(aes(x = xG, y = xA)) +
  geom_point(alpha = .9) +
  labs(x = "Expected Goals (xG)", y = "Expected Assits (xA)", title = "Expected Attacking Output of Attackers")

xOutput <- performance |> 
  filter(Striker | Attacking_Midfield) |> 
  select(xG, xA)


xOutput$cluster1 <- as.factor(kmeans(xOutput, centers = 4, nstart = 10)$cluster)

set.seed(123)
d <- dist(xOutput, method = "euclidian")
hc <- hclust(d, method = "complete")
tree <- cutree(hc, k = 7)
xOutput$cluster2 <- tree

#add name back in
name <- performance |> 
  filter(Striker | Attacking_Midfield) |> 
  select(Name)

xOutput$Name <- name$Name

#One player is significantly futher ahead than the rest, to the point they have their own category
performance |> filter(xG > 25) |> 
  select(Name, Gls, xG)

xOutput <- xOutput |> 
  mutate(cluster2_label = case_when(
    cluster2 == 1 ~ "Not notable",
    cluster2 == 2 ~ "Effective shooting",
    cluster2 == 3 ~ "Creatively oriented",
    cluster2 == 4 ~ "Highly Effective shooting",
    cluster2 == 5 ~ "World Class Effective shooting",
    cluster2 == 6 ~ "Effctive shooting and Highly Creative",
    cluster2 == 7 ~ "Erling Haaland"
  ))

xOutput$cluster2 = as.factor(xOutput$cluster2)

xOutput |> 
  ggplot(aes(x = xG, y = xA, colour = cluster2_label)) +
  geom_point(alpha = .9) +
  labs(x = "Expected Goals (xG)", y = "Expected Assits (xA)", title = "Total Expected Attacking Output of Attackers") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank())

notable <- xOutput |> 
  filter(xA == max(xA) | (xA > 10 & xG > 20))
particular <- xOutput |> 
  filter(cluster2_label != "Not notable", !(cluster2_label %in% notable$cluster2_label)) |> 
  group_by(cluster2) |> 
  sample_n(size = 1)

particular <- bind_rows(notable, particular)

xOutput |> 
  ggplot(aes(x = xG, y = xA, colour = cluster2_label)) +
  geom_point(alpha = .7) +
  labs(x = "Expected Goals (xG)", y = "Expected Assists (xA)", title = "Total Expected Attacking Output of Attackers") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) +
  geom_point(data = particular, colour = "black", size = 3) +
  geom_label_repel(data = particular, aes(label = Name))

#Clustering like this doesnt reveal much, patterns are mostly vertical bars, scaling data would work but
#loses information and difficult to interpret

progression <- performance |> 
  filter(Midfield|Attacking_Midfield|Defensive_Midfield) |> 
  select(Pr.passes.90, xA.90)

#scale data
num_cols <- progression |> 
  select(Pr.passes.90, xA.90)

num_cols$Pr.passes.90 <- scale(num_cols$Pr.passes.90)
num_cols$xA.90 <- scale(num_cols$xA.90)

#cluster on scaled data
set.seed(123)
progression$cluster1 <- as.factor(kmeans(num_cols, centers = 6, nstart = 10)$cluster)

set.seed(123)
d <- dist(num_cols, method = "euclidian")
hc <- hclust(d, method = "average")
tree <- cutree(hc, k = 6)
progression$cluster2 <- tree

progression$cluster2 <- as.factor(progression$cluster2)

#first plot much better
pr1 <- progression |> 
  ggplot(aes(x = Pr.passes.90, y = xA.90, colour = cluster1)) +
  geom_jitter(alpha = .8) +
  labs(x = "Progressive Passes per 90 mins", y = "Expected Assists per 90 mins", title = "Passing influence of midfielders")

pr2 <- progression |> 
  ggplot(aes(x = Pr.passes.90, y = xA.90, colour = cluster2)) +
  geom_jitter(alpha = .8) +
  labs(x = "Progressive Passes per 90 mins", y = "Expected Assists per 90 mins", title = "Passing influence of midfielders")
pr1
#Focusing on Defensive Midfield Progression -------

DM_progression <- performance |> 
  filter(Defensive_Midfield) |> 
  select(Pr.passes.90, Ps.C.90) |> 
  mutate(Pr.Percent = Pr.passes.90 / Ps.C.90 *100) |> 
  select(!Pr.passes.90)
num_cols <- DM_progression |> 
  select(Pr.Percent, Ps.C.90)

num_cols$Pr.Percent <- scale(num_cols$Pr.Percent)
num_cols$Ps.C.90 <- scale(num_cols$Ps.C.90)

set.seed(123)
DM_progression$cluster1 <- as.factor(kmeans(num_cols, centers = 5, nstart = 10)$cluster)

DM_progression <- DM_progression |> 
  mutate(cluster1_label = case_when(
    cluster1 == 1 ~ "Average passes, Averagely progressive",
    cluster1 == 2 ~ "More passes, More progressive",
    cluster1 == 3 ~ "More passes, Less progressive",
    cluster1 == 4 ~ "Less passes, Less progressive",
    cluster1 == 5 ~ "Less passes, More progressive"
  ))

set.seed(123)
d <- dist(num_cols, method = "euclidian")
hc <- hclust(d, method = "complete")
tree <- cutree(hc, k = 5)
DM_progression$cluster2 <- tree

DM_progression$cluster2 <- as.factor(DM_progression$cluster2)

DM_scan <- dbscan(num_cols, eps = .6, minPts = 3)
DM_progression$cluster3 <- as.factor(DM_scan$cluster)

m_res <- Mclust(num_cols, G = 5, modelNames = "VVV", verbose = FALSE)
DM_progression$cluster4 <- as.factor(m_res$classification)

name <- performance |> 
  filter(Defensive_Midfield) |> 
  select(Name)

DM_progression$Name <- name$Name

particular <- DM_progression |> 
  group_by(cluster1) |> 
  sample_n(size = 2)

p1 <- DM_progression |> 
  ggplot(aes(x = Ps.C.90 , y = Pr.Percent, colour = cluster1_label)) +
  geom_point() +
  labs(x = "Passes Completed per 90", y = "% Progressive",
       title = "How progressive are Top Defensive Midfielders?")  +
  scale_color_brewer(palette = "Set1") +
  theme(legend.title = element_blank()) +
  geom_point(data = particular, colour = "black", size = 3) +
  geom_label_repel(data = particular, aes(label = Name))

p2 <- DM_progression |> 
  ggplot(aes(x = Ps.C.90 , y = Pr.Percent, colour = cluster2)) +
  geom_point() +
  labs(x = "Passes Completed per 90", y = "% Progressive",
       title = "How progressive are Top Defensive Midfielders?")  +
  scale_color_brewer(palette = "Set1")
p3 <- DM_progression |> 
  ggplot(aes(x = Ps.C.90 , y = Pr.Percent, colour = cluster3)) +
  geom_point() +
  labs(x = "Passes Completed per 90", y = "% Progressive",
       title = "How progressive are Top Defensive Midfielders?")  +
  scale_color_brewer(palette = "Set1")
p4 <- DM_progression |> 
  ggplot(aes(x = Ps.C.90 , y = Pr.Percent, colour = cluster4)) +
  geom_point() +
  labs(x = "Passes Completed per 90", y = "% Progressive",
       title = "How progressive are Top Defensive Midfielders?")  +
  scale_color_brewer(palette = "Set1")

p1
#p2
#p3
#p3
#p1 generally has the best results in allowing to compare groups of players

#For further exploration: say we wanted to find what cluster Casemiro is in
DM_progression |> 
  filter(Name == "Casemiro") |> 
  select(Name, cluster1_label)
#So lets make a function for this

what_cluster <- function(player_name){
  return(DM_progression |> 
           filter(Name == player_name) |> 
           select(Name, cluster1_label))
}

what_cluster("Casemiro")
