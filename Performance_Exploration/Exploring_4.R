strikers <- prem_performance |> 
  filter(Best.Pos == "ST (C)", Mins > 1000)
my_strikers <- strikers |> 
  filter(Club == "Brentford")


strikers |> 
  ggplot(aes(x = NP.xG, y = xA)) +
  geom_point(alpha = .3) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$xA)) +
  geom_vline(xintercept = mean(strikers$NP.xG)) +
  labs(x = "Non-Penalty Expected Goals (NPxG)", y = "Expected Assists (xA)",
       title = "Expected attacking output among strikers",
       subtitle = "Toney is below average in both, but was suspended for the first half of the season") +
  geom_text_repel(data = my_strikers,
                  aes(x = NP.xG, y = xA, label = Name),
                  fontface = "bold")

strikers |> 
  ggplot(aes(x = NP.xG.90, y = xA.90)) +
  geom_point(alpha = .3) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$xA.90)) +
  geom_vline(xintercept = mean(strikers$NP.xG.90)) +
  labs(x = "Non-Penalty Expected Goals (NPxG)", y = "Expected Assists (xA)",
       title = "Expected attacking output (per 90) among strikers",
       subtitle = "Toney is Much better here, using per 90 to account for missed matches") +
  geom_text_repel(data = my_strikers,
                  aes(x = NP.xG.90, y = xA.90, label = Name),
                  fontface = "bold")