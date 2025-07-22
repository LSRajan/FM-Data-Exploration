library(cluster)
library(mclust)

performance |> 
  filter(Striker == TRUE | Attacking_Midfield == TRUE) |> 
  ggplot(aes(x = xG, y = xA)) +
  geom_point(alpha = .9) +
  labs(x = "Expected Goals (xG)", y = "Expected Assits (xA)", title = "Expected Attacking Output of Attackers")

xOutput <- performance |> 
  filter(Striker == TRUE | Attacking_Midfield == TRUE) |> 
  select(xG, xA)


xOutput$cluster1 <- as.factor(kmeans(xOutput, centers = 4, nstart = 10)$cluster)

d <- dist(xOutput, method = "euclidian")
hc <- hclust(d, method = "complete")
tree <- cutree(hc, k = 7)
xOutput$cluster2 <- as.factor(tree)

xOutput |> 
  ggplot(aes(x = xG, y = xA, colour = cluster2)) +
  geom_point(alpha = .9) +
  labs(x = "Expected Goals (xG)", y = "Expected Assits (xA)", title = "Total Expected Attacking Output of Attackers") +
  scale_color_brewer(palette = "Set1")
  
