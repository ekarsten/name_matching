# Modified by Michael Cahana in late Mar. 2019
# Verifies name matches determined echoed in address matches

#===========
# inputs: 
#===========
# name_matches tibble
# address_matches tibble
# output_file string

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
library(igraph)
library(tidyverse)
library(sf)
library(rpart)

#===========
# functions
#===========

# TC's random forest functions
source(file.path(root, "code", "functions", "random_forest_utils.R"))

# commonly used functions (alpha_order and create_edge)
source(file.path(root, "code", "functions", "utils.R"))

# given a cluster of names, enumerates all possible edges in a cluster
# such that the cluster is complete 
all_edges <- function(cluster) {
	edges <- 
		cluster %>% 
		pull(name) %>% 
		combn(2) %>% 
		t() %>% 
		as_tibble() %>% 
		setNames(c('name', 'match')) %>% 
		filter(name != match) 
	return (unlist(map2(edges$name, edges$match, create_edge)))
}

# takes in a list of edge pair vectors (vectors with a name and match)
# and converts it to a tibble with name and match columns 
list_to_df_edge <- function(l) {
	df <- 
		l %>% 
		t() %>% 
		as_tibble() %>% 
		setNames(c('name', 'match'))
	return (df)
}

# train random forest (RF) using sample data, and apply predictions onto df
rf_predict <- function(df, train_file_path) {
	train <- read_csv(train_file_path)
	func <- 
		paste("shared_words", "cosine_similarity", 
			"jw_distance", "human_jw_distance", 
			"word_count", "sum_n", sep = "+") %>%
		paste("keep", ., sep = "~") %>%
		as.formula()
	rf <-
		func %>%
  		regression_forest2(train)
  	df <- 
  		rf %>%
  		predict2(func, df) %>%
  		as_tibble() %>%
  		bind_cols(df, .) %>% 
  		rename(rf_prob = predictions) 

  	return(df)
}

# determines RF cutoff point by subsetting train data into train/validation sets
# applying trained RF onto validation set, and selecting cutoff point that 
# maximizes information gain (by applying a DT)
rf_cutoff <- function(train_file_path) {
	sample <- read_csv(train_file_path)
	train <- sample %>% sample_n(round((dim(sample)[1] * 0.8)))
	test <- sample %>% anti_join(train)
	func <- 
		paste("shared_words", "cosine_similarity", 
			"jw_distance", "human_jw_distance", 
			"word_count", "sum_n", sep = "+") %>%
		paste("keep", ., sep = "~") %>%
		as.formula()
	rf <-
		func %>%
  		regression_forest2(train)
  	test <- 
  		rf %>%
  		predict2(func, test) %>%
  		as_tibble() %>%
  		bind_cols(test, .) %>% 
  		rename(rf_prob = predictions) 

  	dt <- rpart(keep ~ rf_prob, 
  		data = test, method = 'class', control = list(maxdepth = 1))

  	cutoff <- dt$splits[,'index']

  	return(cutoff)
}

# determine Euclidean distance between a (max, min) point and 
# some specified importance cutoffs
calculate_distance <- function(df, max_threshold, min_threshold, 
	min_max_ratio = NA) {
	# create the rectangular polygon that represents the area in which
	# both the min and max threshold critera are met
	valid_polygon <- st_polygon(list(rbind(c(max_threshold, min_threshold),
		c(max_threshold, 1e10),
		c(1e10, 1e10), 
		c(1e10,min_threshold), 
		c(max_threshold, min_threshold))))
	if (!is.na(min_max_ratio)) {
		# create the traingular polygon that represents the area in which
		# the min max ratio is satisfied
		ratio <- st_polygon(list(rbind(c(0,0), 
			c(1e10,1e10*min_max_ratio), 
			c(0,1e10*min_max_ratio), c(0,0))))
		# declare the valid polygon to the be intersection of the previous
		# rectangular polygon and this triangular one
		valid_polygon <- st_intersection(valid_polygon, ratio)
	}

	df <-
		df %>% 
		rowwise() %>% 
		# determine importance distance as distance from (max, min) point to
		# the valid polygon 
		mutate(importance_dist = as.double(st_distance(
			st_point(c(max_n, min_n)), valid_polygon)))

	return (df)
}

pre_screen_names <- function(name_matches, address_matches, lease_count, 
	output_file, review_directory,
	human_jw_threshold = .6, human_cos_threshold = .6) {

	# flag matches where human name distance is high
	# fill in missing human match metrics
	name_matches <-
	  name_matches %>% 
	  mutate(keep = if_else(human_jw_distance > human_jw_threshold &
	  	human_cosine_similarity > human_cos_threshold & is.na(initials_match), 
	  	0, as.double(NA))) %>% 
	  replace_na(list(human_jw_distance = 1, human_cosine_similarity = 1, 
	  	initials_match = FALSE))

	# verify name matches that have an address match, add in lease counts, 
	# calculate closeness scores and minimum n's for each pair, adjust n's to 
	# avoid double counting 
	# also add in count of total number of words in match pair
	name_matches <- 
		name_matches %>% 
		left_join(address_matches, by = c('name','match')) %>% 
	    mutate(keep = if_else(!is.na(address), 1, keep)) %>% 
	    left_join(lease_count, by = 'name') %>% 
	    left_join(lease_count, by = c('match' = 'name')) %>%
	    rowwise() %>% 
	    mutate(min_n = min(n.x, n.y)) %>% 
	    mutate(max_n = max(n.x, n.y)) %>% 
	    mutate(sum_n = sum(n.x, n.y)) %>% 
	    mutate(actual_n.x = n.x) %>%
	    mutate(actual_n.y = n.y) %>%  
	    ungroup()  %>% 
	    mutate(n.x = ifelse(duplicated(name), 0, n.x)) %>% 
	    mutate(n.y = ifelse(duplicated(match), 0, n.y)) %>% 
	    mutate(n.y = ifelse(match %in% .$name, 0, n.y)) %>% 
	    replace_na(list(n.x = 0, n.y = 0)) %>% 
	    mutate(word_count = str_count(name,'\\w+') + str_count(match, '\\w+'))

	# mark match pairs we deem "important" based on their count coverage
	# note that the importance_dist variable will represent the distance 
	# an (x,y) pair is from being within both min and max thresholds 
	# (x here is max_n, and y min_n), and the min_max_ratio line, if specified
	count_deciles <- lease_count %>% pull() %>% quantile(probs = seq(.1,1,.1))
	seventieth_percentile <- count_deciles[7]
	ninetieth_percentile <- count_deciles[9]
	min_max_ratio = 0.10

	name_matches <- 
		name_matches %>% 
		calculate_distance(ninetieth_percentile, seventieth_percentile, 
			min_max_ratio) %>% 
		arrange(importance_dist)
	
	# verify name matches that have a low match probability according to a 
	# trained random forest (rf) model 
	if(file.exists(file.path(ddir, 'training', 'leases_sample.csv'))) {
	  cutoff <- rf_cutoff(file.path(ddir, 'training', 'leases_sample.csv'))
	  name_matches <- 
	    name_matches %>% 
	    mutate(human_jw_distance = if_else(is.na(human_jw_distance), 1, 
	                                       human_jw_distance)) %>% 
	    mutate(word_count = str_count(name, '\\w+') + 
	             str_count(match, '\\w+')) %>% 
	    rf_predict(file.path(ddir, 'training', 
	                         'leases_sample.csv')) %>% 
	    mutate(keep = ifelse((rf_prob<cutoff & is.na(keep)), 0, keep))
	}

	# if we already did some human review on these matches, incorporate it
	reviewed_files <- 
	  list.files(review_directory, full.names = TRUE, pattern = ".csv") %>%
	  .[!str_detect(., "TODO")]
	
	if (length(reviewed_files>0)) {
	  reviewed_pairs <- 
	    reviewed_files %>% 
	    map_df(read_csv) %>% 
	    dplyr::select(name, match, keep) %>%
	    filter(!is.na(keep))
	  
	  name_matches <- name_matches %>%
	    left_join(reviewed_pairs, by = c('name', 'match')) %>%
	    mutate(keep = if_else(is.na(keep.y), keep.x, keep.y)) %>%
	    select(-keep.y, -keep.x)
	  
	  need_review <-
	    name_matches %>% 
	    anti_join(reviewed_pairs, by = c('name', 'match'))
	} else {
	  need_review <- name_matches
	}
	write_csv(need_review, file.path(review_directory, 'TODO.csv'))
	
	
	# determine pairs that are now verified as correct but previously were not, 
	# if relevant 
	if (file.exists(output_file)) {
	  pre_existing_name_matches <-
	    read_csv(output_file) %>%
	    select(name, match, keep) %>%
	    filter(keep == 1)
	  previous_non_pairs <- 
	    name_matches %>% 
	    anti_join(pre_existing_name_matches, by = c('name', 'match')) %>%
	    filter(keep == 1) %>%
	    dplyr::select(name, match, shared_words, cosine_similarity, jw_distance,
	                  human_jw_distance, human_cosine_similarity, initials_match)
	} else {
	  previous_non_pairs <- 
	    name_matches %>%
	    filter(keep == 1) %>%
	    dplyr::select(name, match, shared_words, cosine_similarity, jw_distance,
	                  human_jw_distance, human_cosine_similarity, initials_match,
	                  address)
	}
	
	write_csv(previous_non_pairs, file.path(ddir, 'notifications', 'previous_non_pairs.csv'))
	write_csv(name_matches, output_file)
}