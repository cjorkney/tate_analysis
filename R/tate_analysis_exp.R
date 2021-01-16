library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(stringr)

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
           remove = FALSE)
  
  
         
