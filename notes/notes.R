

x <- downloadSpataObject(sample_name = "334_T")

library(tidyverse)

source_df <- 
  read.csv("data/source.csv", sep = ";") %>%
  as_tibble()


usethis::use_data(source_df)

