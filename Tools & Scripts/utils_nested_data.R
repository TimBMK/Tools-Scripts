save_nested_tarball <- function(nested_object,
                                index = NULL, # index of the item in each object of the nested list, e.g. first, second, etc. NULL to save the whole item
                                file_name, # name of the tarball
                                compression = "none" # compression format of the tarball (see tar() for details)
)
  
{
  
  require(vroom)
  require(purrr)
  
  # Write each data frame to a temporary CSV file
  files <- purrr::map(names(nested_object), function(x) {
    file <- paste0(x, ".csv")
    if(is.null(index)) {
      vroom::vroom_write(nested_object[[x]], file, delim = ",")
    } else {
      vroom::vroom_write(nested_object[[x]][[index]], file, delim = ",")
    }
    c(file)
  })
  
  # Create a tarball of the temporary CSV files
  tar(file_name, files = unlist(files), compression)
  
  # Remove the temporary CSV files
  invisible(file.remove(unlist(files))) # invisible() hides the output
  
}



save_nested_plots_tarball <- function(nested_object,
                                      index = 2, # index of the item in each object of the nested list, e.g. first, second, etc. NULL to save the whole item
                                      # level = NULL, # specify the levels of the nested objects; not currently used
                                      file_name, # name of the tarball
                                      compression = "none", # compression format of the tarball (see tar() for details)
                                      format = "png", # format (file ending) of the plots to be saved in
                                      progress = TRUE # progress bar (only in multisession - see future_map() )
                              )
{
  require(ggplot2)
  require(purrr)
  require(furrr)
  require(future)
  # if(!is.null(index)) {
  #   files <- imap(nested_object[[index]], ~ggsave(.x, filename = paste0(.y, ".png"), path = "test"))
  # } else {
  #   files <- imap(nested_object, ~ggsave(.x, filename = paste0(.y, ".png"), path = "test"))
  # }
  
  temp_folder <- tempdir()
  
  # get current WD to revert later
  wd <- getwd()
  

  
  # future::future({ # do so for all the futures future_map is using
  #   setwd(temp_folder)
  # })
  
  # Write plots for each group in each time frame
  folders <- furrr::future_map(names(nested_object), function(x) {
    folder <- x
    
    purrr::map(names(nested_object[[x]][[index]]), function(y) {
      file <- paste0(y, ".", format)
      suppressMessages({
        ggplot2::ggsave(nested_object[[x]][[index]][[y]], 
                        filename = file, 
                        path = file.path(temp_folder, folder))
      })
    })
    
    c(folder)},
    
    .progress = progress
  )
  
  # temporarily move the WD to prevent the temp path from showing in the tarball
  setwd(temp_folder)
  
  # Create a tarball of the temporary CSV files
  tar(tarfile = file.path(wd, file_name), 
      files = unlist(folders), 
      compression = compression)
  
  # move back to the original working directory
  setwd(wd)
  
  # Remove the temporary CSV files
  # invisible(file.remove(unlist(files)))
  unlink(file.path(temp_folder, folders), recursive = T)
}
  


load_nested_tarball <- function(file_name, # name of the tarball
                                verbose = F, # verbosity of the vroom() function
                                ... # additional arguments to pass to vroom()
)
{
  
  require(vroom)
  require(purrr)
  require(stringr)
  
  # Extract the CSV files from the tarball
  tar_files <- untar(tarfile = file_name, list = T) # list of files to be untared
  untar(tarfile = file_name, list = F) # untar files
  
  # Read in the CSV files using vroom
  if (verbose == FALSE) {
    suppressMessages({
      dfs <- map(tar_files, vroom, ...)
    })
  } else {
    dfs <- map(tar_files, vroom, ...)
    
  }
  
  # Assign the data frames to new objects with the same names as the original list elements
  names(dfs) <-  stringr::str_extract(tar_files, "(?<=\\\\|/|^)[^\\\\/]*(?=\\.csv)")
  
  # Remove the temporary CSV files
  invisible(file.remove(tar_files)) # invisible() hides the output
  
  return(dfs)
}



