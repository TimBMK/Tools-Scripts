# Merge all variables to retrieve maximum non-NA values after a join

require(dplyr)

merge_variables <- function(x, pref = "x") {
  variables <- select(x, ends_with(".x")) %>% names() %>% str_extract(".*(?=.x)")
  for (i in 1:length(variables)) {
    if (pref == "x"){
      x <- x %>% mutate(!!as.name(variables[i]) := ifelse(is.na(!!as.name(paste0(variables[i],".x"))),   # strange naming convention with :=
                                                          !!as.name(paste0(variables[i],".y")),          # favors x values when both are non-NA
                                                          !!as.name(paste0(variables[i],".x"))))
      }
    if (pref == "y") {
      x <- x %>% mutate(!!as.name(variables[i]) := ifelse(is.na(!!as.name(paste0(variables[i],".y"))),   # favors y values when both are non-NA
                                                          !!as.name(paste0(variables[i],".x")),          
                                                          !!as.name(paste0(variables[i],".y"))))
    }
  }
  x <- x %>% select(!ends_with(c(".x", ".y")))
  return(x)
}


# Example: list_07 <- left_join(list_07, twitter_data, by="user_id") %>% merge_variables()
