#https://r4ds.hadley.nz/data-visualize
library(tidyverse)
library(palmerpenguins)
library(ggthemes)

script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)

ggplot(
  data = penguins,
  mapping = aes(x=flipper_length_mm, y = body_mass_g),
) + 
  geom_point(mapping = aes(color=species,shape = species)) + 
  geom_smooth(method="lm") +
  labs(title = "Body mass and flipper lenght",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Flipper length (mm)", y = "Body mass(g)",
       color = "Species", shape = "Species"
       ) +
  scale_color_colorblind()

#Exercises
#How many rows are in penguins? How many columns?
#1) 344 rows, 8 columns
#2) A number denoting bill depth (millimeters)
#3) When bill length increase, depth decrease
ggplot(
  data = penguins,
  mapping = aes(x= bill_length_mm, y = bill_depth_mm),
) + 
  geom_point(mapping = aes(color=species)) +
  geom_smooth(method ="lm")
#4) Boxplot is a better choice
ggplot(
  data = penguins,
  mapping = aes(x=species, y = bill_depth_mm),
) +
  geom_boxplot()¨
#5)Geom_points mangler data punkter for x og y værdier
ggplot(data = penguins) + 
  geom_point()
#6) remove missing data
ggplot(
  data = penguins,
  mapping = aes(x=species, y = bill_depth_mm),
) +
  geom_point(na.rm = TRUE)
#7) Add labels
ggplot(
  data = penguins,
  mapping = aes(x=species, y = bill_depth_mm),
) +
  geom_point(na.rm = TRUE) +
  labs(title= "Penguins",subtitle = "Data come from the palmerpenguins package.", x= "Species", y="Bill dept (mm)")

