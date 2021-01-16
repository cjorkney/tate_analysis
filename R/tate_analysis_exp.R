library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(stringr)

source("R/choose_city_country.R")

tuesdata <- tidytuesdayR::tt_load('2021-01-12')

artwork <- tuesdata$artwork
artists <- tuesdata$artists


# Clean datasets ----------------------------------------------------------

artists_clean <- artists %>% 
  mutate(
    across(c("id", "gender"), ~ as.factor(.x)),
    bplace_commas = str_count(placeOfBirth, ","),
    placeOfBirth = str_replace(placeOfBirth, ",\\s.+,", ","),
    ) %>% 
  separate(placeOfBirth,
           into = c("birth_city", "birth_country"), sep = ", ",
           remove = FALSE) %>%
  choose_city_country("birth")
  
  
         
blah <- artists_clean %>% 
  mutate(
    logic = placeOfBirth %in% pull(artists_clean, birth_country),
    test = sum(str_detect(pull(artists_clean, birth_country), placeOfBirth), na.rm = TRUE),
    city = sum(str_detect(.$birth_city, placeOfBirth), na.rm = TRUE),
    country = sum(str_detect(.$birth_country, placeOfBirth), na.rm = TRUE)
    )

blah %>% 
  filter(bplace_commas == 0) %>%
  glimpse()
