# Modified by Michael Cahana in early Apr. 2019
# Matches all group names in all_groups.csv

#===========
# INPUTS
# group names
#===========

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# needed libraries
#===========
library(tidyverse)

#===========
# functions
#===========

source(file.path(root, 'code', 'functions', 'match_names.R'))

#===========
# read in 
#===========

# gather list of group names
df <- 
	read_csv(file.path(ddir, 'grouped_matches', 'all_groups.csv')) %>% 
	count(group_name) %>% 
	rename(name = group_name)

review_directory <- file.path(vdir, 'names_grouped') 
output_file <- file.path(ddir, "matches", 'group_name_matches.csv')

#===========
# identify potential "cluster of clusters" - multiple distinct clusters that
# belong together in one cluster
#===========

# match group names
df <- 
	df %>% 
	match_names(output_file, cosine_threshold =  0.65, write_csv = F) %>% 
	# remove pure shared word matches, add keep column 
	filter(!(is.na(cosine_similarity)) | !(is.na(jw_distance))) %>% 
	mutate(keep = NA_real_) 

reviewed_files <- 
  list.files(review_directory, full.names = TRUE, pattern = ".csv") %>%
  .[!str_detect(., "TODO")]

if (length(reviewed_files>0)) {
  reviewed_pairs <- 
    reviewed_files %>% 
    map_df(read_csv) %>% 
    dplyr::select(name, match, keep) %>%
    filter(!is.na(keep))
  
  df <- df %>%
    left_join(reviewed_pairs, by = c('name', 'match')) %>%
    mutate(keep = if_else(is.na(keep.y), keep.x, keep.y)) %>%
    select(-keep.y, -keep.x)
  
  need_review <-
    df %>% 
    anti_join(reviewed_pairs, by = c('name', 'match'))
} else {
  need_review <- df
}
write_csv(need_review, file.path(review_directory, 'TODO.csv'))


write_csv(df, output_file)