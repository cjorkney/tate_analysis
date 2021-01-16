#' Allocate a comma-less birthplace to either a city or country
#' 
#' @param df a dataframe
#' 
#' 
#' 
#' 
#' 
#' 


choose_city_country <- function(df, birth_or_death) {
  
  colname_orig <- paste0("placeOf", stringr::str_to_title(birth_or_death))
  colname_city <- paste0(birth_or_death, "_city")
  colname_country <- paste0(birth_or_death, "_country")
  
  df_new <- df %>%
    mutate(count_in_country = stringr::str_count(.$colname_country, colname_orig))
  
  
  
}