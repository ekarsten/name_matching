# Created by Eric Karsten Feb 2020
# Determines name matches for jb leases used in wellconfidentiality

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

source(file.path(root, 'code', 'functions', 'match_names.R'))

#===========
# data read in
#===========

all_leases <- 
	readRDS(file.path(rdir, 'leases', 'leases_jb.Rds'))

#===========
# match names 
#===========

# all
df <- 
    all_leases %>% 
    count(Lessee) %>% 
    rename(name = Lessee) 

output_file <- file.path(ddir, 'matches', 'names', 'jb_name_matches.csv')

match_names(df, output_file)


