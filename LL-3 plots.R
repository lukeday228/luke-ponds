
#load packages
library(tidyverse)


### Field Data ###

#read data
LL3_field <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/LL3_field.csv")

#rename columns
colnames(LL3_field)[1] <- "site"
colnames(LL3_field)[5] <- "lat"
colnames(LL3_field)[6] <- "lon"
colnames(LL3_field)[11] <- "depthtrax"
colnames(LL3_field)[12] <- "pole"
colnames(LL3_field)[13] <- "dtr"
colnames(LL3_field)[14] <- "deeper_min"
colnames(LL3_field)[15] <- "deeper_max"

#select data
LL3_depths <- LL3_field |>
  select("site", "lat", "lon", "depthtrax", "pole", "dtr", "deeper_min", "deeper_max")

#plot depths comparison
ggplot(LL3_depths, aes(x = 1:16)) +
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
LL3_lab <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/LL3_lab.csv")


#manipulate data
LL3_measurements <- LL3_lab |>
  select(wet = 12, dry = 13, bulk_density = 16, core = "core #") |>
  mutate(
    percent_water = (wet - dry) / wet * 100
  )


LL3_measurements <- LL3_measurements |>
  mutate(
    tin_num = row_number(),
    core = as.factor(core)
  )



#plot percent water
ggplot(LL3_measurements, aes(x = tin_num, y = percent_water, color = core)) +
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
ggplot(LL3_measurements, aes(x = tin_num, y = bulk_density, color = core)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(
    x = "Tin #",
    y = "bulk density (g/cm^3)",
    color = "Core"
  ) +
  theme_minimal()

