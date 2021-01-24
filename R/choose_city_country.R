#' Allocate a comma-less birthplace to either a city or country
#' 
#' @param df a dataframe
#' 
#' 
#' 
#' 
#' 
#' 


choose_city_country <- function(df) {
  
  # Make reduced df that takes out all non-comma entries, then count occurances
  # of each birth/death city/country
  # 
  # Non-commas must be removed because we know that some countries have been
  # incorrectly allocated as cities already
  
  df_redux <- dplyr::filter(df, bplace_commas != 0)
  
  birth_city_counts <- dplyr::count(df_redux, birth_city) %>%
    rename("birth_city_n" = "n")
  birth_country_counts <- dplyr::count(df_redux, birth_country) %>%
    rename("birth_country_n" = "n")
  death_city_counts <- dplyr::count(df_redux, death_city) %>% 
    rename("death_city_n" = "n")
  death_country_counts <- dplyr::count(df_redux, death_country) %>% 
    rename("death_country_n" = "n")
  
  df_new <- df %>%
    dplyr::left_join(birth_city_counts,
              by = c("placeOfBirth" = "birth_city")) %>%
    dplyr::left_join(birth_country_counts,
              by = c("placeOfBirth" = "birth_country")) %>%
    dplyr::left_join(death_city_counts,
              by = c("placeOfDeath" = "death_city")) %>%
    dplyr::left_join(death_country_counts,
              by = c("placeOfDeath" = "death_country")) %>%
    tidyr::replace_na(list(birth_city_n = 0,
                           birth_country_n = 0,
                           death_city_n = 0,
                           death_country_n = 0)) %>%
    dplyr::mutate(
      birth_city_correct = dplyr::case_when(
        bplace_commas > 0 ~ birth_city,
        birth_city_n > birth_country_n ~ placeOfBirth,
        TRUE ~ "NA"
      ),
      birth_country_correct = dplyr::case_when(
        bplace_commas > 0 ~ birth_country,
        birth_city_n <= birth_country_n ~ placeOfBirth,
        TRUE ~ "NA"
      ),
      death_city_correct = dplyr::case_when(
        dplace_commas > 0 ~ death_city,
        death_city_n > death_country_n ~ placeOfDeath,
        TRUE ~ "NA"
      ),
      death_country_correct = dplyr::case_when(
        dplace_commas > 0 ~ death_country,
        death_city_n <= death_country_n ~ placeOfDeath,
        TRUE ~ "NA"
      )
    )
    

}