################################ clean_participant_file ################################
clean_participant_file <- function(fname) {
  print(paste0("reading ", fname))
  df <- read_table(paste0(data_path, fname))
  
  #remove first four rows
  df <- df[5:nrow(df), ]
  
  #add subid and slide number columns
  df <- df %>% 
    mutate(subid = colnames(df), slidenumber = 1:nrow(df)) %>% 
    separate(subid, into = c("slideshow", "subid"), sep = " ") %>% 
    dplyr::select(-slideshow) 
  
  #change name of first column
  colnames(df)[1] <- "toSeparate"
  
  #create columns for condition and looking time
  df <- df %>% 
    mutate(condition = gsub(" .*$", "", toSeparate),
           rawDT = as.numeric(gsub(".*\\s", "", toSeparate))) %>% 
             dplyr::select(-toSeparate)
  
  #remove numbers from condition column
  df <- df %>% 
    mutate(condition = case_when(str_detect(condition, "barrier") ~ "barrier",
                                 str_detect(condition, "nothing") ~ "nothing",
                                 str_detect(condition, "straight") ~ "straight"))
  
  return(df)
}

################################ clean_pixel_file ################################
clean_pixel_file <- function(fname) {
  print(paste0("reading ", fname))
  df <- read_delim(paste0(data_path, fname), delim = " ", col_names = FALSE)
  
  #get only the columns we want
  df <- df %>% 
    mutate(condition = rep(fname, nrow(df)),
           slide_num_adj = seq(from = 2, to = nrow(df)+1)) %>% 
    rename("pix_change" = X4) %>% 
    dplyr::select(-starts_with("X"))
  
  #extract condition name from condition column
  df <- df %>%
    mutate(condition = gsub('_[^_]*$', '', condition))
  
  return(df)
}

