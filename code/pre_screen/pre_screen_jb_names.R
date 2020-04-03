# Created by Michael Cahana in mid Feb. 2019
# Verifies certain name matches given an address match

#===========
# inputs: 
#===========
# all_leases.Rds

#===========
# needed libraries
#===========
library(tidyverse)
library(sf)

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

#===========
# functions
#===========

source(file.path(root, 'code', 'functions', 'pre_screen_names.R'))

#===========
# data read in
#===========

all_leases <- readRDS(file.path(rdir, 'leases', 'leases_jb.Rds'))

#===========
# verify name matches with addresses 
#===========

# all 
name_matches <- read_csv(file.path(ddir, 'matches', 'names', 
	'jb_name_matches.csv'))
address_matches <- read_csv(file.path(ddir, 'matches', 'addresses', 
	'jb_address_matches.csv'))
output_file <- file.path(ddir, "matches", 'jb_matches.csv')
lease_count <- 
	all_leases %>% 
	rename(name = Lessee) %>% 
	mutate(name = str_replace_all(name, '\xc9', 'E')) %>% 
	count(name)

review_directory <- file.path(vdir, "leases_jb")

pre_screen_names(name_matches, address_matches, lease_count, output_file,
                 review_directory)
