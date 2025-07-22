library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

#Read in and clean Data
#This data is for players in the Bundesliga, Premier League, 
performance <- readxl::read_excel("Book1.xlsx", sheet = 3)

colnames(performance) <- make.names(colnames(performance))

summary(performance)
head(performance)

#remove the NA row
tail(performance)
remove <- which(is.na(performance$Tck.90))
performance <- performance[-c(963),]
  
#still have NAs in Last.5.Games and last.5.FT.Games
sum(is.na(performance$Last.5.FT.Games)) / nrow(performance)
sum(is.na(performance$Last.5.Games)) / nrow(performance)
#completely NA, just remove
performance <- performance |> 
  select(!c("Last.5.FT.Games", "Last.5.Games"))



#a lot of columns are character but should be numeric, need to replace "-" with NA and then convert to number
performance <- performance |> 
  mutate_at(vars(Tck.90, Tck.C, Tck.R, Shot.90, Shot.., ShT.90, ShT, Shots.Outside.Box.90, Shts.Blckd.90, Shts.Blckd,
                 Svt, Svp, Svh, Sv.., OP.Crs.C.90, OP.Crs.C, OP.Crs.A.90, OP.Crs.A, OP.Cr.., K.Ps.90, Int.90, Hdr..,
                 Hdrs, Hdrs.L.90, Goals.Outside.Box, FK.Shots, xSv.., xGP.90, xGP, Drb.90, Dist.90, Distance, Cr.C.90, 
                 Cr.C, Crs.A.90, Cr.A, Cr.C.A, Conv.., Clear, Ch.C.90, Blk.90, Blk, Asts.90, Aer.A.90, Hdrs.A, 
                 Saves.90, Tcon, Starts, Pen.R, Pens.Saved.Ratio, Pens.Saved, Pens.Faced, Last.Gl, Last.C, Int.Conc,
                 Int.Av.Rat, Int.Ast, Int.Apps, Gls.90, Conc, G..Mis, Con.90, Cln.90, Clean.Sheets, Mins.Gl, AT.Lge.Gls,
                 AT.Gls), ~gsub("-", NA, .)) |> 
  mutate_at(vars(Tck.90, Tck.C, Tck.R, Shot.90, Shot.., ShT.90, ShT, Shots.Outside.Box.90, Shts.Blckd.90, Shts.Blckd,
                 Svt, Svp, Svh, Sv.., OP.Crs.C.90, OP.Crs.C, OP.Crs.A.90, OP.Crs.A, OP.Cr.., K.Ps.90, Int.90, Hdr..,
                 Hdrs, Hdrs.L.90, Goals.Outside.Box, FK.Shots, xSv.., xGP.90, xGP, Drb.90, Dist.90, Distance, Cr.C.90, 
                 Cr.C, Crs.A.90, Cr.A, Cr.C.A, Conv.., Clear, Ch.C.90, Blk.90, Blk, Asts.90, Aer.A.90, Hdrs.A, 
                 Saves.90, Tcon, Starts, Pen.R, Pens.Saved.Ratio, Pens.Saved, Pens.Faced, Last.Gl, Last.C, Int.Conc,
                 Int.Av.Rat, Int.Ast, Int.Apps, Gls.90, Conc, G..Mis, Con.90, Cln.90, Clean.Sheets, Mins.Gl, AT.Lge.Gls,
                 AT.Gls), ~parse_number(.))

#this column has one part with appearances, second with how many subs on
performance$Apps

#separating subs and appearances with funny looking regex
performance <- performance |> 
  mutate(Subbed.On = if_else(str_detect(Apps, "\\("), str_extract(Apps, "(?<=\\()[^)]+"), "0"),
         Apps = gsub("\\s*\\([^\\)]+\\)", "", Apps))

#Will be important to look at stats by position. 
#I'll separate them into GK, Full back, Center Back, Defensive Midfielder, Midfielder, Attacking Midfielder, Striker
#Midfield needs to contain M on its own, but it overlaps with AM so is difficult to extract
# Full back can be either D (RL) or WB (RL)

performance$Position

performance <- performance |> 
  mutate(
    Goalkeeper = str_detect(Position,"GK"),
    Centre_Back = str_detect(Position, "D \\(C\\)|D \\(LC\\)|D \\(RC\\)|D \\(RLC\\)"),
    Full_Back = str_detect(Position, c("WB")),
    Defensive_Midfield = str_detect(Position, "DM"),
    Midfield = str_detect(Position, "\\bM\\b|M/AM"),
    Attacking_Midfield = str_detect(Position, "AM"), 
    Striker = str_detect(Position, "ST"),
  ) 

#Make sure everyone has at least one position
performance |> 
  select(Goalkeeper,Centre_Back,Midfield,Defensive_Midfield, Full_Back, Attacking_Midfield, Striker) |> 
  filter(Goalkeeper == FALSE & Centre_Back == FALSE & Midfield == FALSE & Defensive_Midfield == FALSE & 
         Full_Back == FALSE & Attacking_Midfield == FALSE & Striker == FALSE) |> 
  nrow()


