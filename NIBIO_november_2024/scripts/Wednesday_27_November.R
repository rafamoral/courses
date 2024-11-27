# to install the 'tidyverse' suite of packages, uncomment the code below and run it
#install.packages("tidyverse")

# by the way, if you use the # (hash) symbol, this is a comment!

# we will start the session by loading the packages we need
# to load a package, use the library() function
# let's load the tidyverse package
library(tidyverse)

# how do we create objects in R?
# we use an assignment operator: <-
x <- 2
x * 7

# R doesn't care too much about spaces
x*7
x                                       *7

# however, R is case-sensitive
X * 7
x * 7

# to clean the console output, click on it and use control + L
# but bear in mind this doesn't clean the memory
# to clean the memory (erase objects), use the following code
rm(list = ls())

# let's read in some data!
# read data from the British Lexicon Project (BLP)
blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/NIBIO_november_2024/data/blp-trials-short.txt")

blp_df # prints the first 10 rows

# I want to see the first 30 rows
print(blp_df, n = 30)

# I want to see everything
print(blp_df, n = Inf)

# I want to view the data in a more Excel-friendly way
view(blp_df)

# best-practice naming conventions for your objects
# snake_case
# camelCase

# reading in data from your local machine
# let's read the atibaia.csv dataset
getwd()
# setwd("my work directory") if you'd like to set it manually

# easiest procedure: to open R Studio by double-clicking the R script
# you saved in the same folder that you have your dataset in
atibaia_df <- read_csv("atibaia.csv")

# CTRL + SHIFT + R to create a comment identifying a new section, like the one below!

# getting started with dplyr verbs ----------------------------------------

# we will work with the blp_df dataset for this section

library(tidyverse)

blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/NIBIO_november_2024/data/blp-trials-short.txt")

# selecting columns with select()

# introducing the pipe: %>%
x <- 1:10
x

log(x)
mean(log(x))
sqrt(mean(log(x)))

x %>% log %>% mean %>% sqrt

# it is best practice to break the line after a pipe operator
# and indent the next line

x %>%
  log %>%
  mean %>%
  sqrt

blp_df %>%
  select(participant, lex, resp, rt)
# bear in mind this doesn't get rid of the columns
# to do so, you need to create a new object, like this
blp_df2 <- blp_df %>%
  select(participant, lex, resp, rt)
# best practice: give different names so you can recover if needed
# if you won't ever need to recover the columns you've excluded
# then you may overwrite the object

# you may also select and rename a column at the same time
blp_df %>%
  select(participant, lex, resp, reaction_time = rt)

# you can include spaces but that's non-advisable
blp_df %>%
  select(participant, lex, resp, `reaction time` = rt)

# this gives an error because there is no column called reaction_time
blp_df %>%
  select(participant, lex, resp, rt = reaction_time)

# you can also select by column
# e.g. imagine you want the first, second and fifth columns
blp_df %>%
  select(1, 2, 5)

# selecting chunks/blocks of columns
# imagine I want columns 2,3,4,5
blp_df %>%
  select(2, 3, 4, 5)
blp_df %>%
  select(2:5)
blp_df %>%
  select(lex:rt)

# selecting columns that match some expression
blp_df %>%
  select(starts_with("r"))
blp_df %>%
  select(starts_with("rt"))
blp_df %>%
  select(ends_with("t"))
blp_df %>%
  select(ends_with("rt"))

blp_df %>%
  select(ends_with("T", ignore.case = FALSE))

# RegEx matching
blp_df %>%
  select(matches("^rt")) # equivalent to starts_with
blp_df %>%
  select(matches("rt$")) # equivalent to ends_with
blp_df %>%
  select(matches("^rt|rt$")) # starts_with OR ends_with

# use the minus sign to drop columns
blp_df %>%
  select(- participant)
blp_df %>%
  select(- participant, - rt)
blp_df %>%
  select(- (participant:rt))

# selecting columns based on their characteristics
# select only numeric columns
blp_df %>%
  select(where(is.numeric))

blp_df %>%
  select(- where(is.numeric))

# Boolean logic: & = and, | = or, ! = not

not_numeric <- function(x) !is.numeric(x) # writing my own function

blp_df %>%
  select(where(not_numeric))

# let's say we only want numeric columns that have a mean higher than 500
has_high_mean <- function(x) {
  is.numeric(x) && mean(x, na.rm = TRUE) > 500
}

has_high_mean(1:10)
has_high_mean(c("rafael","zahra","alex")) # the logic gate && will check whether
                                          # it's numeric, and since it isn't it stops and returns FALSE

blp_df %>%
  select(where(has_high_mean))

# select everything
blp_df %>%
  select(everything())

blp_df %>%
  select(rt, everything())

# Reordering columns using relocate ---------------------------------------

blp_df %>%
  relocate(rt)

blp_df %>%
  relocate(starts_with("rt"))

blp_df %>%
  relocate(rt, .after = lex)
blp_df %>%
  relocate(rt, .before = spell)

blp_df %>%
  relocate(rt, .after = last_col())

blp_df %>%
  relocate(rt, where(is.numeric))

my_processed_data <- blp_df %>%
  select(where(has_high_mean)) %>%
  relocate(rt, .after = last_col())

write_csv2(my_processed_data, file = "high_mean.csv")

blp_df %>%
  select(where(has_high_mean)) %>%
  relocate(rt, .after = last_col()) %>%
  write_csv2(file = "high_mean2.csv")

# Renaming with rename ----------------------------------------------------

blp_df %>%
  select(reaction_time = rt, everything())

blp_df %>%
  rename(reaction_time = rt)

blp_df %>%
  rename(reaction_time = rt, lexical = lex)

# renaming with a function
# use the rename_with
blp_df %>%
  rename_with(toupper)

blp_df %>%
  rename_with(tolower)

# renaming only the numeric variables to upper-case

blp_df %>%
  rename_with(toupper, where(is.numeric))

# let's have a look at a function called str_replace
y <- c("hello","world")
y
str_replace(y, "he", "xx")

# purrr-style lambda function
# ~ str_replace(., "rt", "reaction_time")
# this is exactly the same as
# function(x) str_replace(x, "rt", "reaction_time")

blp_df %>%
  rename_with(~ str_replace(., "rt", "reaction_time"), matches("^rt|rt$"))

# Slicing with slice ------------------------------------------------------

# slice will select rows
# I want the 5th row
blp_df %>%
  slice(5)

# I want rows 10, 20, 30, 40, 50
blp_df %>%
  slice(c(10, 20, 30, 40, 50))
blp_df %>%
  slice(1:5 * 10)

# I want everything except row 10
blp_df %>%
  slice(- 10)

# give me the last 10 rows
blp_df %>%
  slice(991:1000)
blp_df %>%
  slice(991:n())
blp_df %>%
  slice((n() - 10):n())

# Filtering with filter ---------------------------------------------------

# imagine I want the data for participant 20
blp_df %>%
  filter(participant == 20)

# imagine I want the data for participants 20 or 29
blp_df %>%
  filter(participant == 20 | participant == 29)

blp_df %>%
  filter(participant %in% c(20, 29))

blp_df %>%
  filter(participant %in% c(20, 29)) %>%
  arrange(participant) ## ordering with arrange()

part20_29 <- blp_df %>%
  filter(participant %in% c(20, 29)) %>%
  arrange(participant) ## ordering with arrange()

write_csv2(part20_29, file = "participants_20_and_29.csv")

# we can use multiple conditions across different columns
blp_df %>%
  filter(lex == "W", resp == "W", rt <= 500)
# equivalent to...
blp_df %>%
  filter((lex == "W") & (resp == "W") & (rt <= 500))

# hack this to filter by lex = W, resp = W and reaction time greater than 500 without using '>'
blp_df %>%
  filter((lex == "W") & (resp == "W") & !(rt <= 500))

# filter where the response was correct (resp = lex)
blp_df %>%
  filter(resp == lex)

# find rows where there is at least one missing value
blp_df %>%
  filter(if_any(everything(), is.na))

blp_df %>%
  filter(if_all(everything(), is.na))

# use it to drop incomplete cases
blp_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

# good news is you can simply
blp_df %>%
  drop_na
blp_df %>%
  na.omit

# check only numeric columns for NAs and filter those rows if any of
# these columns have NAs ans save the result as a csv file
blp_df %>%
  filter(if_any(where(is.numeric), is.na)) %>%
  write.csv2(file = "incomplete_cases.csv")


# Creating new variables with mutate --------------------------------------

blp_df %>%
  mutate(accuracy = lex == resp,
         fast_response = rt < 500,
         rt_seconds = rt/1000)

blp_df %>%
  mutate(accuracy = lex == resp,
         rt_seconds = rt/1000,
         fast_response = rt_seconds < .5)

blp_df %>%
  mutate(participant = as.factor(participant))

blp_df %>%
  mutate(participant = as.factor(participant),
         spell = as.factor(spell),
         resp = as.factor(resp),
         lex = as.factor(lex))

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor))

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor),
         across(where(is.numeric), scale))

x <- rnorm(10, 50, 4)
scale(x) # returns a matrix with 1 column
as.vector(scale(x)) # returns a vector -- this is what we want

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor),
         across(where(is.numeric), ~ as.vector(scale(.))))

re_scale <- function(x) as.vector(scale(x))

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor),
         across(where(is.numeric), re_scale))

blp_df %>%
  mutate(lex = recode(lex, "W" = "word", "N" = "nonword"))

blp_df %>%
  mutate(lex = recode(lex, "W" = "word", "N" = "nonword"),
         resp = recode(resp, "W" = "word", "N" = "nonword"))

blp_df %>%
  mutate(across(c(lex, resp),
                ~ recode(., "W" = "word", "N" = "nonword")))

# we can use conditions as well
# one condition: if_else
# more than 1: case_when

blp_df %>%
  mutate(fast_response = if_else(rt < 500, "fast", "slow"))

blp_df %>%
  mutate(speed_category = case_when(
    rt > 900 ~ "slow",
    rt < 500 ~ "fast",
    (rt >= 500) & (rt <= 900) ~ "medium"
  ))

# for the "whatever else" category you can use TRUE
blp_df %>%
  mutate(speed_category = case_when(
    rt > 900 ~ "slow",
    rt < 500 ~ "fast",
    TRUE ~ "medium"
  ))


# Sorting with arrange ----------------------------------------------------

blp_df %>%
  arrange(rt)

blp_df %>%
  arrange(desc(rt))

blp_df %>%
  arrange(participant, rt)

blp_df %>%
  arrange(spell)


# Full data wrangling pipelines -------------------------------------------

## exercise: create a new variable called response_category that is "fast" when
## rt < 500 and "slow" otherwise, then filter by fast responses,
## then sort by participants and words in ascending alphabetical order within
## each participant, at the end remove all incomplete cases
## save is as a new object

final_dataset <- blp_df %>%
  # creating the response category variable
  mutate(response_category = if_else(rt < 500, "fast", "slow")) %>%
  # filtering by fast responses only
  filter(response_category == "fast") %>%
  # sorting by participant and words in alphabetical order
  arrange(participant, spell) %>%
  # removing incomplete cases
  drop_na

write.csv2(final_dataset, "final_dataset.csv")


# The group_by and summarise combo ----------------------------------------

atibaia_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/NIBIO_november_2024/data/atibaia.csv")

# I want the means and standard errors for the thrips response for each treatment

se <- function(x) sd(na.omit(x)) / length(na.omit(x))

atibaia_df %>%
  group_by(treatment) %>%
  summarise(means = mean(thrips, na.rm = TRUE),
            std_error = se(thrips))

atibaia_df %>%
  group_by(area, treatment) %>%
  summarise(means = mean(thrips, na.rm = TRUE),
            std_error = se(thrips))

letters_from_tukey_test <- c("a","ab","b")

atibaia_df %>%
  group_by(treatment) %>%
  summarise(means = mean(thrips, na.rm = TRUE),
            std_error = se(thrips)) %>%
  arrange(desc(means)) %>%
  mutate(letter = letters_from_tukey_test)

atibaia_df %>%
  drop_na %>%
  group_by(treatment) %>%
  summarise(across(thrips:beetles, mean))

atibaia_df %>%
  drop_na %>%
  group_by(treatment) %>%
  summarise(across(thrips:beetles, list(means = mean, std_errors = se)))