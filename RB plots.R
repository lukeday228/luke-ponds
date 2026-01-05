

#load packages
library(tidyverse)


### Field Data ###

#read data
RB_field <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/RB_field.csv")

#rename columns
colnames(RB_field)[1] <- "site"
colnames(RB_field)[5] <- "lat"
colnames(RB_field)[6] <- "lon"
colnames(RB_field)[11] <- "depthtrax"
colnames(RB_field)[12] <- "pole"
colnames(RB_field)[13] <- "dtr"
colnames(RB_field)[14] <- "deeper_min"
colnames(RB_field)[15] <- "deeper_max"

#select data
RB_depths <- RB_field |>
  select("site", "lat", "lon", "depthtrax", "pole", "dtr", "deeper_min", "deeper_max")

#plot depths comparison
ggplot(RB_depths, aes(x = 1:16)) +
  geom_point(aes(y = as.numeric(depthtrax)), color = "red") +
  geom_point(aes(y = as.numeric(pole)), color = "blue") +
  geom_point(aes(y = as.numeric(dtr)), color = "green") +
  geom_point(aes(y = as.numeric(deeper_min)), color = "orange") +
  geom_point(aes(y = as.numeric(deeper_max)), color = "black") +
  labs(
    x = "Location along transect",
    y = "Depth (cm)")



###LAB DATA###


#read data
RB_lab <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/RB_lab.csv")


#manipulate data
RB_measurements <- RB_lab |>
  select(wet = 12, dry = 13, bulk_density = 16, core = "core #") |>
  mutate(
    across(c(wet, dry, bulk_density), ~ suppressWarnings(as.numeric(.))),
    percent_water = (wet - dry) / wet * 100
  )




RB_measurements <- RB_measurements |>
  mutate(
    tin_num = row_number(),
    core = as.factor(core)
  )



#plot percent water
ggplot(RB_measurements, aes(x = tin_num, y = percent_water, color = core)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Tin #",
    y = "Percent water by weight",
    color = "Core"
  ) +
  theme_minimal()


#plot bulk density
ggplot(RB_measurements, aes(x = tin_num, y = bulk_density, color = core)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(
    x = "Tin #",
    y = "bulk density (g/cm^3)",
    color = "Core"
  ) +
  theme_minimal()
