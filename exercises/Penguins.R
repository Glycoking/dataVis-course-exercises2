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
  geom_boxplot()
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

#1.4
ggplot(penguins, aes(x = species)) +
  geom_bar()
# With reorder
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
#Histogram
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 500)
#Execsises 1.4.3
#Exersie 1
#Barplot of spcies of penguins
ggplot(penguins, aes(y=species)) +
  geom_bar(aes(y=species),color = "blue")
#Exersise 2¨
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red", color="blue")
#Fill is better at coloring the whole bar
#exercise 3
#Bins controll the amount of obervations in a single pile

ggplot(diamonds, aes(x=carat))+
  geom_histogram(bins=1000)

ggplot(penguins, aes(y = species, x = body_mass_g)) +
  geom_boxplot()


ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)
#1.5.5
#Exercise 1
#Which variables in mpg are categorical? Which variables are numerical? 
#Numerical:Displ, year,cyl,cty,hwy,
#Catagorical: Manufacturer, model, trans, drv, fl, class
#Exercise 3
ggplot(mpg, aes(x=hwy, y=displ,linewidth=cyl ))+
  geom_point(aes(color=class))
#Exercise 4
# Then it shows in multiple aestetics
#Exercise 5
ggplot(penguins, aes(x=bill_depth_mm, y=bill_length_mm)) +
  geom_point(aes(color=species))
#Exercise 6

ggplot(
  data = penguins,aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
#Remove extra labels (labs)
#Exercise 7
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
#With this plot i can anwser question about the population of penguins on a given island and their species
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")
#With this plot i can anwser questions about where different penguin species are located on islands
#I can see weether they are spread around or on a single island

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color=species))
ggsave(filename = "penguin-plot.png", width = 8, height = 8)