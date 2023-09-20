## give a short intro to R Studio, the different windows etc
## tidyverse, a package of packages (16)
## how to read raw data from github; careful not to read the html page

## load the tidyverse suite of packages
library(tidyverse)

## load the data
blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_wrangling/data/blp-trials-short.txt")

## look at the data
blp_df
## tibbles are more "effective" data frames;
## they do less (no change of variable names/types, no partial matching)
## and complain more (forcing you to deal with problems early on)