#CAN PREDICT AVERAGE RATING MAYBE, USE DEEP LEARNING.
library(dplyr)
library(keras) 
library(tfruns)
library(tensorflow)
library(tfestimators)

core_performance <- performance |> 
  select(
    Name, Age, Position, Club, Division, Mins, Starts, Apps, Subbed.On,
    Tck.90, Shot.90, ShT.90, Shots.Outside.Box.90, Shts.Blckd.90,
    Pr.passes.90, Pres.C.90, Pres.A.90, Poss.Won.90, Poss.Lost.90,
    Ps.C.90, Ps.A.90, OP.KP.90, OP.Crs.C.90, OP.Crs.A.90,
    K.Tck.90, K.Ps.90, K.Hdrs.90, Int.90, Sprints.90,
    Hdrs.W.90, Goals.Outside.Box, FK.Shots, xGP.90, xG.shot,
    Drb.90, Dist.90, Cr.C.90, Crs.A.90, Clr.90,
    Ch.C.90, Blk.90, Asts.90, Aer.A.90,
    Saves.90, Tgls.90, Tcon.90,
    Pts.Gm, PoM, Pen.R, Pens.S, Pens.Saved.Ratio, NP.xG.90,
    Gls.90, xG.90, xA.90, Con.90, Cln.90, Mins.Gl,
    Inf., Rec, AT.Lge.Gls, AT.Lge.Apps, AT.Gls,
    Goalkeeper, Centre_Back, Full_Back, Defensive_Midfield,
    Midfield, Attacking_Midfield, Striker
    )

feature <- performance |> 
  select(Av.Rat)

model <- keras_model_sequential()

model |> 
  layer_dense(units = 128, activation = 'relu') |> 
  layer_dense(units = 128, activation = 'relu') |> 
  layer_dense(units = 1, activation = 'softmax')

fit1 <- model %>%
  fit(
    x = core_performance,
    y = feature,
    epochs = 25,
    batch_size = 128,
    validation_split = 0.2,
    verbose = FALSE
  )