# ----------------------
# Load libraries and data
# ----------------------

library(tidyverse)
library(janitor)
library(assertr)

meteor_raw <- read_csv("data/meteorite_landings.csv")

# ----------------------
# Assertive Programming
# ----------------------

# To ensure the data has the variable names we expect (“id”, “name”, “mass (g)”, “fall”, “year”, “GeoLocation”).
# per questions

meteor_raw %>% 
  verify(has_all_names("id", "name", "mass (g)", "fall", "year", "GeoLocation"))


# ----------------------
# Cleaning
# ----------------------

# Clean names
# per questions and best practice
meteor_clean <- meteor_raw %>% 
  clean_names()

# Split column GeoLocation into latitude and longitude
# per questions

meteor_clean <- meteor_clean %>% 
  separate(
    col = geo_location,
    into = c("latitude", "longitude"),
    sep = ","
  )

# remove brackets and format as numeric or latitude and longitude
# for clarity      # per questions
meteor_clean <- meteor_clean %>% 
  mutate(
    latitude = as.numeric(str_remove(latitude, "\\(")),
    longitude = as.numeric(str_remove(longitude, "\\)"))
  )

meteor_keep_nas <- meteor_clean

# Replace any missing values in latitude and longitude with zeros
# per questions

meteor_clean <- meteor_clean %>% 
  mutate(
    latitude = if_else(is.na(latitude), 0, latitude),
    longitude = if_else(is.na(longitude), 0, longitude)
  )

# ----------------------
# Assertive Programming
# ----------------------

# Replace latitude and longitude in row index 2936, or ID 32789 because longitude is not a valid value

meteor_clean[29436, "latitude"] <- NA
meteor_clean[29436, "longitude"] <- NA

meteor_keep_nas[29436, "latitude"] <- NA
meteor_keep_nas[29436, "longitude"] <- NA

# To ensure latitude and longitude are valid values. (Latitude between -90 and 90, longitude between -180 and 180).
# per questions
meteor_clean %>% 
  verify(latitude >= -90 & latitude <= 90) %>% 
  verify(longitude >= -180 & longitude <= 180)


# ----------------------
# Cleaning
# ----------------------

# Rename mass_g to mass_in_grams 
# for clarity per information at https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh

meteor_clean <- meteor_clean %>% 
  rename(mass_in_grams = mass_g)

meteor_keep_nas<- meteor_keep_nas %>% 
  rename(mass_in_grams = mass_g)

# Remove meteorites less than 1000g in weight from the data.
# per questions
# Removes 40893 observations so I created a new variable

meteor_clean_over_1000 <- meteor_clean %>% 
  filter(mass_in_grams > 1000)

# Order the data by the year of discovery.
# per questions

meteor_clean_over_1000 <- meteor_clean_over_1000 %>% 
  arrange(year)

meteor_clean <- meteor_clean %>% 
  arrange(year)

#Export as .csv

write_csv(meteor_clean, "data/meteor_clean.csv")
write_csv(meteor_clean_over_1000, "data/meteor_over_1000")
write_csv(meteor_keep_nas, "data/meteor_with_nas")
