#https://r4ds.hadley.nz/data-visualize
library(tidyverse)
library(palmerpenguins)
library(ggthemes)


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
#4) 
ggplot(
  data = penguins,
  mapping = aes(x=species, y = bill_depth_mm)
  
) +
  geom_jitter()