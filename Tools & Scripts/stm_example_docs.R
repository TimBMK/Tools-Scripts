### Get example docs by variable for stm models ###

library(stm)
library(tidyverse)


# example data
load("D:/academicCloud/R/RT Disinformation/V2/RT_EU2_de_out.RDa")
load("D:/academicCloud/R/RT Disinformation/V2/RT_stm_de_prevalence_12.RDa")

# requires dt data structure
stm_dt_de <- make.dt(RT_stm_de_prevalence_12, meta = out_de$meta)


# get days as dates (for nicer indication of time)
date_range <- data.frame(date = seq(as.Date("2019-02-23"), by = "day", length.out = max(stm_dt_de$day)), day = 1:max(stm_dt_de$day))
stm_dt_de <- left_join(stm_dt_de, date_range, by = "day")



## Function

example_docs <- function(input, n, var, add_var, dir){ 
  
  if (dir.exists(dir)) {
    cat("Directory already exists. Files may be overwritten.")
  }
  
  for (i in 1:length(names(select(input, starts_with("Topic"))))) {
    
    topic <- input %>% arrange(desc(!!as.name(paste0("Topic", i)))) %>%       # !! for unquoting the variable. Requires as.name() for columns to work properly
      group_by(!!as.name(var)) %>% slice(1:n) %>%  
      select(!starts_with("Topic"), !!as.name(paste0("Topic", i)))
    
    for (j in 1:nrow(unique(select(topic, !!as.name(var))))) {
      value <- unique(select(topic, !!as.name(var)))[[1]][j]
      rows <- topic %>% filter(!!as.name(var) == value)
      
      for (k in 1:nrow(rows)) {
        data_path <- paste0(dir, "/", "topic", i, "/", as.character(value))
        
        dir.create(file.path(data_path), recursive = T, showWarnings = F)
        writeLines(paste0("Topic ", i, "\n", 
                          var, ": ", slice(select(ungroup(rows), !!as.name(var)), k)[[1]], " | ", 
                          add_var, ": ", slice(select(ungroup(rows), !!as.name(add_var)), k)[[1]], " | ", # only one additional variable is supported as of now
                          "Topical Prevalence: ", slice(select(ungroup(rows), !!as.name(paste0("Topic", i))), k),
                          "\n \n \n", rows$headline[k], "\n \n", rows$text[k]),
                   con = paste0(data_path, "/", k,".txt"))
      }
      
    }
    
  }
  
}


# Try it
example_docs(stm_dt_de, n = 5, var = "source", add_var = "date", dir = "example_docs_de")
