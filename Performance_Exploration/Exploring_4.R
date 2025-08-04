library(ggrepel)

#In this save, Ivan Toney will likey be leaving, I will use this sitation
#as a basis to do some exploration, how good was Ivan Toney and can Brentford replace him?

strikers <- prem_performance |> 
  filter(Best.Pos == "ST (C)", Mins > 1000)
my_strikers <- strikers |> 
  filter(Club == "Brentford")


strikers |> 
  ggplot(aes(x = NP.xG, y = xA)) +
  geom_point(alpha = .3, size = 2.5) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$xA)) +
  geom_vline(xintercept = mean(strikers$NP.xG)) +
  labs(x = "Non-Penalty Expected Goals (NPxG)", y = "Expected Assists (xA)",
       title = "Expected attacking output among strikers",
       subtitle = "Toney is below average in both, but was suspended for the first half of the season") +
  geom_text_repel(data = my_strikers,
                  aes(x = NP.xG, y = xA, label = Name),
                  fontface = "bold") +
  annotate(geom = "text",
           x = 1, y = 2,
           label = "Below Average xA,\nBelow Average NPxG",
           hjust = "left", fontface = "bold")

strikers |> 
  ggplot(aes(x = NP.xG.90, y = xA.90)) +
  geom_point(alpha = .3, size = 2.5) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$xA.90)) +
  geom_vline(xintercept = mean(strikers$NP.xG.90)) +
  labs(x = "Non-Penalty Expected Goals (NPxG)", y = "Expected Assists (xA)",
       title = "Expected attacking output (per 90) among strikers",
       subtitle = "Toney is Much better here, using per 90 to account for missed matches") +
  geom_text_repel(data = my_strikers,
                  aes(x = NP.xG.90, y = xA.90, label = Name),
                  fontface = "bold") +
  annotate(geom = "text",
           x = 0.38, y = 0.08,
           label = "Below Average xA p90,\nAbove Average NPxG p90",
           hjust = "left", fontface = "bold")

strikers |> 
  ggplot(aes(x = Pres.A.90, y = Pres.C.90)) +
  geom_point(alpha = .3, size = 2.5) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$Pres.C.90)) +
  geom_vline(xintercept = mean(strikers$Pres.A.90)) +
  geom_text_repel(data = my_strikers,
                  aes(x = Pres.A.90, y = Pres.C.90, label = Name),
                  fontface = "bold") +
  labs(x = "Presses Attempted per 90", y = "Presses Completed per 90",
       title = "Ivan Toney seems average in pressing, but most players have a similar press success rate",
       subtitle = "This graph seems fairly uninformative, and is more representative of a players requirement to press")

strikers |> 
  ggplot(aes(x = (Hdrs.A / Mins * 90), y = Hdrs.W.90)) +
  geom_point(alpha = .3, size = 2.5) +
  geom_point(data = my_strikers, colour = "red", size = 3) +
  geom_hline(yintercept = mean(strikers$ Hdrs.W.90)) +
  geom_vline(xintercept = mean(strikers$Hdrs.A / strikers$Mins * 90)) +
  geom_text_repel(data = my_strikers,
                  aes(x = (Hdrs.A / Mins * 90), y = Hdrs.W.90, label = Name),
                  fontface = "bold") +
  labs(x = "Headers Attempted per 90", y = "Headers Won per 90",
       title = "Ivan Toney was good aerielly, but it doesnt seem to be something Brentford relied upon",
       subtitle = "It could still hurt to lose a reliable header of the ball")

