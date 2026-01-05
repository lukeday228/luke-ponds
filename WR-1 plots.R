

#load packages
library(tidyverse)


### Field Data ###

#read data
WR1_field <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/WR1_field.csv")

#rename columns
colnames(WR1_field)[1] <- "site"
colnames(WR1_field)[5] <- "lat"
colnames(WR1_field)[6] <- "lon"
colnames(WR1_field)[11] <- "depthtrax"
colnames(WR1_field)[12] <- "pole"
colnames(WR1_field)[13] <- "dtr"
colnames(WR1_field)[14] <- "deeper_min"
colnames(WR1_field)[15] <- "deeper_max"

#select data
WR1_depths <- WR1_field |>
  select("site", "lat", "lon", "depthtrax", "pole", "dtr", "deeper_min", "deeper_max")

#plot depths comparison
ggplot(WR1_depths, aes(x = 1:16)) +
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
WR1_lab <- read_csv("https://raw.githubusercontent.com/lukeday228/luke-ponds/refs/heads/main/WR1_lab.csv")


#manipulate data
WR1_measurements <- WR1_lab |>
  select(wet = 12, dry = 13, bulk_density = 16, core = "core #") |>
  mutate(
    percent_water = (wet - dry) / wet * 100
  )


WR1_measurements <- WR1_measurements |>
  mutate(
    tin_num = row_number(),
    core = as.factor(core)
  )

### IMPORTANT NOTE ###
# the first 8 rows in the WR1 files are missing a value for "Mass after GS (Grain Size Analysis)
# this results in missing values for cores 1 and 2 in the plots of "% water"
# it also resulted in inaccurate values for bulk density on cores 1 and 2

#plot percent water
ggplot(WR1_measurements, aes(x = tin_num, y = percent_water, color = core)) +
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

##toggle removal of incorrect values referenced above by commenting out the labeled line

ggplot(
  WR1_measurements |> 
    filter(tin_num > 8),   # ← comment out this line to include removed points
  aes(x = tin_num, y = bulk_density, color = core)
) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple")) +
  labs(
    x = "Tin #",
    y = "Bulk density (g/cm^3)",
    color = "Core"
  ) +
  theme_minimal()

