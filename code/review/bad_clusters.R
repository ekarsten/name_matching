# Created by Eric Karsten April 2020
# eric.t.karsten@gmail.com
# Identifies Groups that contain an explicit zero from manual review

#===========
# standard setup
#===========
root <- getwd()
while(basename(root) != "name_matching") {
  root <- dirname(root)
}
source(file.path(root, "data.R"))

# ====================
# Libraries
# ===========

library(tidyverse)

# ===========
# data read-in
# ===========

df <- read_csv(file.path(ddir, 'grouped_matches', 'grouped_groups.csv'))

review_directory <- file.path(vdir)

# ==============================================================================
# Clusters That contain an explicit zero
# ==============================================================================

reviewed_files <- 
  list.files(review_directory, full.names = TRUE,
             pattern = ".csv", recursive = T) %>%
  .[!str_detect(., "TODO") & !str_detect(., "archive")]

special_read <- function(path) {
  read_csv(path) %>% mutate(file = path)
}

reviewed_pairs <- 
  reviewed_files %>% 
  map_df(special_read) %>% 
  select(name, match, keep, file) %>%
  filter(keep == 0)

bad_clusters <- reviewed_pairs %>%
  left_join(df) %>%
  left_join(df, by = c("match" = "name")) %>%
  filter(cluster.x == cluster.y)

write_csv(bad_clusters, file.path(ddir, "notifications", "bad_clusters.csv"))

