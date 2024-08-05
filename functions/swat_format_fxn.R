read_swat <- function(file_path){
  
  raw_file <- readr::read_delim(file_path, skip = 1, delim = " +")
  
  into_cols <- unlist(strsplit(names(raw_file), " +"))
  
  into_cols[1] <- "trash" # empty initial column to throwaway
  
  names(raw_file) <- "data" # name this "single column" data
  
  new_file <- tidyr::separate(raw_file, 
                       data, 
                       sep = " +",
                       into = into_cols) %>% 
    dplyr::select(-trash)
  
  return(new_file)
  
}