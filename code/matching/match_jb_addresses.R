# Created by Michael Cahana in mid Feb. 2019
# Determines address matches for leases

#===========
# inputs: 
#===========
# coded_addresses.csv
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

source(file.path(root, 'code', 'functions', 'match_addresses.R'))

#===========
# data read in
#===========

all_leases <- 
	readRDS(file.path(rdir, 'leases', 'leases_jb.Rds'))
already_coded_addresses <- read_csv(file.path(ddir, 'address_backups', 
	'coded_addresses.csv')) 

#===========
# match addresses 
#===========

already_coded_addresses <-  pull(already_coded_addresses, address)

# all
df <- 
	all_leases %>% 
	mutate(address = str_c(LesseeAddr, LesseeCStZ, sep = " ")) %>%
	select(name = Lessee, address) 

output_file <- file.path(ddir, 'matches', 'addresses', 
	'jb_address_matches.csv')

match_addresses(df, already_coded_addresses, output_file)
