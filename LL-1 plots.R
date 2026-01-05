

#load packages
library(tidyverse)


### Field Data ###

#read data
LL1_field <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/LL1_field.csv")

#rename columns
colnames(LL1_field)[1] <- "site"
colnames(LL1_field)[5] <- "lat"
colnames(LL1_field)[6] <- "lon"
colnames(LL1_field)[11] <- "depthtrax"
colnames(LL1_field)[12] <- "pole"
colnames(LL1_field)[13] <- "dtr"
colnames(LL1_field)[14] <- "deeper_min"
colnames(LL1_field)[15] <- "deeper_max"

#select data
LL1_depths <- LL1_field |>
  select("site", "lat", "lon", "depthtrax", "pole", "dtr", "deeper_min", "deeper_max")

#plot depths comparison
ggplot(LL1_depths, aes(x = 1:16)) +
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
LL1_lab <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/LL1_lab.csv")


#manipulate data
LL1_measurements <- LL1_lab |>
  select(wet = 12, dry = 13, bulk_density = 16, core = 21) |>
  mutate(
    percent_water = (wet - dry) / wet * 100
  )


LL1_measurements <- LL1_measurements |>
  mutate(
    tin_num = row_number(),
    core = as.factor(core)
  )



#plot percent water
ggplot(LL1_measurements, aes(x = tin_num, y = percent_water, color = core)) +
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
ggplot(LL1_measurements, aes(x = tin_num, y = bulk_density, color = core)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(
    x = "Tin #",
    y = "bulk density (g/cm^3)",
    color = "Core"
  ) +
  theme_minimal()

