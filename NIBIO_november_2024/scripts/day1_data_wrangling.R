## give a short intro to R Studio, the different windows etc
## tidyverse, a package of packages
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

## British Lexicon Project
## blp_df: experiment in psychology with human participants
## lexical decision experiment
## strings of letters are shown, the person press a button to respond whether it is a word or not
## lex: N = not a word; W = it is a word
## resp: the participant's response
## rt: reaction time in miliseconds
## prev.rt: reaction time in the previous trial
## rt.raw: same as rt (can check using a dplyr pipeline)

## look at the first 50 rows of the data frame
print(blp_df, n = 50)

## look at ALL rows
print(blp_df, n = Inf)

## glimpsing
glimpse(blp_df)

## look at the data as a base R data frame
blp_df_base <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_wrangling/data/blp-trials-short.txt")
blp_df_base

## tidyverse vs. base R
## reference for the tidyverse (R4DS)

## dplyr verbs
## select: subsetting columns
## relocate: reordering columns
## rename: renaming columns
## slice: subsetting rows
## filter: subsetting rows according to a condition
## mutate: creating new variables/modifying variables
## transmute: minor variant of mutate
## arrange: sorting rows

## Selecting columns with select
select(blp_df, participant, lex, resp, rt)

## save the returned data frame as blp_df2
blp_df2 <- select(blp_df, participant, lex, resp, rt)
## overwrite the original data frame
#blp_df <- select(blp_df, participant, lex, resp, rt)

## select and rename at the same time
select(blp_df, participant, lex, resp, reaction_time = rt)
## base R
#blp_df <- blp_df[, c("participant", "lex", "resp", "rt")]
#names(blp_df)[5] <- "reaction_time"

## select by index
select(blp_df, 1, 2, 7)

## instead of this
select(blp_df, lex, spell, resp, rt)
## do this
select(blp_df, lex:rt)

select(blp_df, 2:5)
select(blp_df, participant:resp, rt.raw)
select(blp_df, 2:4, rt.raw)

select(blp_df, starts_with("r"))
select(blp_df, starts_with("rt"))
select(blp_df, ends_with("t"))
select(blp_df, contains("rt"))

select(blp_df, ends_with("T"))
select(blp_df, ends_with("T", ignore.case = FALSE)) ## case-sensitive

## RegEx matching (regexr.com)
select(blp_df, matches("^rt"))     ## equivalent to starts_with
select(blp_df, matches("rt$"))     ## equivalent to ends_with
select(blp_df, matches("^rt|rt$")) ## starts_with OR ends_with

## all but `participant`
select(blp_df, - participant)
select(blp_df, - participant, - rt)
select(blp_df, - c(participant, rt))

select(blp_df, lex:rt)
select(blp_df, - (lex:rt))

select(blp_df, - (2:5))

## select the numeric variables
select(blp_df, where(is.numeric))
select(blp_df, - where(is.numeric))

has_high_mean <- function(x) {
  is.numeric(x) && mean(x, na.rm = TRUE) > 500
}

select(blp_df, where(has_high_mean))

## anonymous function
select(blp_df, where(function(x) {
  is.numeric(x) && mean(x, na.rm = TRUE) > 500
}))

# purrr style lambda anonymous function
select(blp_df, where(~{
  is.numeric(.) && mean(., na.rm = TRUE) > 500
}))

# ~ <=> function(.)

## select "everything"
select(blp_df, everything())
## useful to reorder columns
select(blp_df, rt, everything())

## (Ctrl + Shit + R)
## Reordering columns with relocate --------------------------------------------

relocate(blp_df, rt)
relocate(blp_df, starts_with("r"))

relocate(blp_df, rt, .after = lex)
relocate(blp_df, rt, .before = lex)

relocate(blp_df, rt, .after = rt.raw)
relocate(blp_df, rt, .after = last_col())
relocate(blp_df, rt, .before = last_col())

relocate(blp_df, where(is.numeric))

relocate(blp_df, where(is.numeric), .after = last_col())

## Renaming with rename --------------------------------------------------------

select(blp_df, reaction_time = rt)
select(blp_df, reaction_time = rt, everything())

rename(blp_df, reaction_time = rt)

rename(blp_df, reaction_time = rt, lexical = lex)

## `rename_with` a function
rename_with(blp_df, toupper)
rename_with(blp_df, tolower)

## rename all numeric variables to uppercase
rename_with(blp_df, toupper, where(is.numeric))
rename_with(.data = blp_df, .fn = toupper, .cols = where(is.numeric))

## let's look at `str_replace`
y <- c("hello", "world")
str_replace(y, "he", "xx") # replace "he" with "xx"

# ~str_replace(., 'rt', 'reaction_time')

## what would happen here?
rename_with(.data = blp_df, 
            .fn = ~str_replace(., "rt", "reaction_time"))

rename_with(.data = blp_df,
            .fn = ~str_replace(., "rt", "reaction_time"),
            .cols = matches('^rt|rt$'))

## we can change the order of arguments if we declare them explicitly
rename_with(.fn = ~str_replace(., "rt", "reaction_time"),
            .data = blp_df,
            .cols = matches('^rt|rt$'))

## Slicing with slice ----------------------------------------------------------

slice(blp_df, 5)

slice(blp_df, 5:15)

slice(blp_df, c(10, 20, 50, 100, 500, 800))

slice(blp_df, seq(10, 1000, by = 10))

slice(blp_df, -10)

slice(blp_df, -(1:3))

slice(blp_df, 990:1000)
slice(blp_df, 990:n())
slice(blp_df, (n()-10):n())

## Filtering with filter -------------------------------------------------------

filter(blp_df, participant == 20)

filter(blp_df, lex == "W", resp == "W")
filter(blp_df, rt < 500)
filter(blp_df, rt <= 500)

## the following 
filter(blp_df, lex == "W", resp == "W", rt <= 500)
## is equivalent to this:
filter(blp_df, (lex == "W") & (resp == "W") & (rt <= 500))

## base R: blp_df[blp_df$lex == "W" & blp_df$resp == "W" & blp_df$rt <= 500, ] # (includes NAs!!)

## disjunction of conditions
filter(blp_df, lex == "W" | rt <= 500)

filter(blp_df, (lex == "W") & (resp == "W") | (rt <= 500))
filter(blp_df, (lex == "W") & (resp == "W") | !(rt <= 500))

## filter where their response is correct
filter(blp_df, lex == resp)

## find rows where there is at least one missing value
filter(blp_df, if_any(everything(), is.na))

filter(blp_df, if_all(everything(), is.na))

## dropping incomplete cases
filter(blp_df, if_all(everything(), ~!is.na(.))) # filter for rows where everything is not NA

## by the way, you can also do ....
drop_na(blp_df)
na.omit(blp_df)

# this 
# ~ . <= 500
# could be written as 
# function(x){
# x <= 500
# }

filter(blp_df, if_all(matches("^rt|rt$"), ~ . <= 500))
filter(blp_df, if_any(matches("^rt|rt$"), ~ . <= 500))

## the previous if_all command is equivalent to this:
filter(blp_df, rt <= 500, prev.rt <= 500, rt.raw <= 500)

## the previous if_any command is equivalent to this:
filter(blp_df, rt <= 500 | prev.rt <= 500 | rt.raw <= 500)

## filtering when all numeric values are less than their median
filter(blp_df, if_all(where(is.numeric), ~ . < median(., na.rm = TRUE)))

## Create new variables with mutate --------------------------------------------

mutate(blp_df, accuracy = lex == resp)

mutate(blp_df,
       accuracy = lex == resp,
       fast_response = rt < 500)

mutate(blp_df, rt = rt / 1000)

mutate(blp_df,
       accuracy = lex == resp,
       fast_response = rt < 500,
       rt = rt / 1000)

## the previous code is different to 
mutate(blp_df,
       accuracy = lex == resp,
       rt = rt / 1000,
       fast_response = rt < 500)
## we can use newly defined variables within a single mutate call
## not possible in base R
data.frame(blp_df,
           accuracy = blp_df$lex == blp_df$resp,
           rt2 = blp_df$rt * 2,
           fast_response = rt2 < 500)

mutate(blp_df,
       accuracy = lex == resp,
       rt2 = blp_df$rt * 2,
       fast_response = rt2 < 500)

## converting a variable to a factor
mutate(blp_df, lex = as.factor(lex))
       
## converting multiple columns to a factor
mutate(blp_df,
       lex = as.factor(lex),
       spell = as.factor(spell),
       resp = as.factor(resp))

## using across() with mutate()
## across("which_columns", "do_what")
mutate(blp_df, across(where(is.character), as.factor))

re_scale <- function(x) as.vector(scale(x))

mutate(blp_df, across(where(is.numeric), re_scale))
mutate(blp_df, across(where(is.numeric), ~ as.vector(scale(.)) ))


mutate(blp_df, 
       lex = recode(lex, "W" = "word", "N" = "nonword"))

mutate(blp_df,
       across(c(lex, resp), 
              ~ recode(., "W" = "word", "N" = "nonword")))

## one condition
mutate(blp_df,
       fast_response = if_else(rt < 500, "fast", "slow"))

## > 1 condition
mutate(blp_df,
       rt_speed = case_when(
         rt > 900 ~ "slow",
         rt < 500 ~ "fast",
         (rt >= 500) & (rt <= 900) ~ "medium"
       ))

## "otherwise / else", use the value TRUE
mutate(blp_df,
       rt_speed = case_when(
         rt > 900 ~ "slow",
         rt < 500 ~ "fast",
         TRUE ~ "medium"
       ))

## return only the mutated column (use transmute)
transmute(blp_df,
          rt_speed = case_when(
            rt > 900 ~ "slow",
            rt < 500 ~ "fast",
            TRUE ~ "medium"
          ))

## Sorting with arrange --------------------------------------------------------

arrange(blp_df, rt)
arrange(blp_df, desc(rt))
arrange(blp_df, spell)
arrange(blp_df, participant)
arrange(blp_df, participant, rt)

## Introducting the pipe -------------------------------------------------------

x <- c(2, 3, 5, 7, 11)

## we want the log of the sum of the sqrt of the log of x
log(x) # the log of x
sqrt(log(x)) # the sqrt of the log of x
sum(sqrt(log(x))) # sum of sqrt of log of x
log(sum(sqrt(log(x)))) # log of sum of sqrt of log of x

log(x) %>% sqrt() # equivalent to sqrt(log(x))

x %>% log() %>% sqrt() # same thing, equivalent to sqrt(log(x))

x %>% log() %>% sqrt() %>% sum() %>% log() # equiv to log(sum(sqrt(log(x))))

## A data wrangling pipeline ---------------------------------------------------

# take the dataset, and then...
blp_df %>% 
  # convert `participant` to a factor and create the accuracy variable, and then...
  mutate(participant = as.factor(participant),
         accuracy = lex == resp) %>% 
  # rename variables, and then...
  rename(subject = participant,
         stimulus = spell,
         reaction_time = rt) %>% 
  # select variables, and then...
  select(subject, stimulus, lex, accuracy, reaction_time) %>% 
  # filter according to some rules, and then...
  filter(lex == "W",
         accuracy, 
         reaction_time < 1500 & reaction_time > 500) %>% 
  # remove incomplete cases
  drop_na()

blp_df %>% 
  mutate(participant = as.factor(participant),
         accuracy = lex == resp) %>% 
  # select does renaming as well
  select(subject = participant, 
         stimulus = spell, 
         lex, 
         accuracy, 
         reaction_time = rt)

## long pipes aren't a problem, they are still readable
## and you can comment in between to explain what each step is doing


## Summary statistics with summarise -------------------------------------------

atibaia_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/NIBIO_november_2024/data/atibaia.csv")

summarise(atibaia_df, mean(thrips, na.rm = TRUE))

atibaia_df %>%
  drop_na() %>%
  summarise(avg_thrips = mean(thrips),
            avg_beetles = mean(beetles))

atibaia_df %>% 
  mutate(high_thrips = thrips > 30) %>%  
  drop_na() %>% 
  group_by(area, treatment) %>% 
  summarise(avg = mean(beetles))

atibaia_df %>% 
  drop_na() %>% 
  group_by(treatment) %>% 
  summarise(across(thrips:beetles, median))

atibaia_df %>% 
  mutate(area = factor(area)) %>%
  drop_na() %>% 
  group_by(treatment) %>% 
  summarise(across(where(is.numeric), median))

se <- function(x) sd(x) / sqrt(length(x))

atibaia_df %>% 
  mutate(area = factor(area)) %>%
  drop_na() %>% 
  group_by(treatment) %>% 
  summarise(across(where(is.numeric), list(mean = mean, se = se)))

## Merging data frames ---------------------------------------------------------

source("https://raw.githubusercontent.com/rafamoral/courses/main/data_wrangling/scripts/simple_df_eg.R")

bind_rows(Df_1, Df_2)
bind_rows(Df_2, Df_1)
bind_cols(Df_1, Df_3)

Df_a
Df_b

inner_join(Df_a, Df_b)
left_join(Df_a, Df_b)
right_join(Df_a, Df_b)
left_join(Df_b, Df_a)
full_join(Df_a, Df_b)

anti_join(Df_a, Df_b)
anti_join(Df_b, Df_a)
semi_join(Df_a, Df_b)

## let's get real
blp_stimuli <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_wrangling/data/blp_stimuli.csv")
## spell = word
## old20 = measure of similarity to other words
## bnc = frequency of the word usage per million
## subtlex = frequency of word usage in a large database of movie subtitles

left_join(blp_df, blp_stimuli)

filter(blp_stimuli, spell == "staud")

right_join(blp_df, blp_stimuli)
right_join(blp_df, blp_stimuli) %>% tail(20)

inner_join(blp_df, blp_stimuli)

all.equal(inner_join(blp_df, blp_stimuli),
          left_join(blp_df, blp_stimuli))

Df_9 <- tibble(spell = c("cat", "dog"),
               rt = c(100, 200))
Df_10 <- tibble(spell = c("cat", "cat", "dog"),
                old20 = c(5, 10, 20))

left_join(Df_9, Df_10)

Df_11 <- tibble(x = c(1, 2), y = c(42, 52))
Df_12 <- tibble(x = c("1", "2"), z = c("foo", "bar"))

left_join(Df_11, Df_12)

Df_13 <- tibble(x = as.factor(c(1, 2)), y = c(42, 52))
Df_14 <- tibble(x = c("1", "2"), z = c("foo", "bar"))

left_join(Df_13, Df_14)

inner_join(Df_4, Df_5)
inner_join(Df_4, rename(Df_5, x = a))
inner_join(Df_4, Df_5, by = c("x" = "a"))

## Reshaping with pivots -------------------------------------------------------

## environmental variables measured in a greenhouse
greenhouse_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/NIBIO_november_2024/data/greenhouse.csv")

greenhouse_df_long <- greenhouse_df %>%
  pivot_longer(- greenhouse,
               names_to = "measurement",
               values_to = "value")

greenhouse_df_long %>%
  pivot_wider(names_from = measurement,
              values_from = value)

greenhouse_df_b <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/NIBIO_november_2024/data/greenhouse_b.csv")

## this pipeline 
greenhouse_df_b %>%
  pivot_longer(- greenhouse,
               names_to = "measurement",
               values_to = "value") %>% 
  separate(col = measurement, 
           into = c("month", "environmental_variable"),
           sep = "_")

## is equivalent to this one command
greenhouse_df_b %>%
  pivot_longer(- greenhouse,
               names_to = c("month", "environmental_variable"),
               names_sep = "_",
               values_to = "value")

## and also it is equivalent to this
greenhouse_df_b %>%
  pivot_longer(- greenhouse,
               names_to = c("month", "environmental_variable"),
               names_pattern = "(.*)_(.*)",
               values_to = "value")
## . = any character
## * = any number of characters
## (.*) = any number of characters of any kind

blp_df_summ <- 
  blp_df %>% 
  drop_na() %>%
  summarise(across(rt:rt.raw,
                   list(median = median, mad = mad)))

## this pipeline
blp_df_summ %>%
  pivot_longer(everything(),
               names_to = "summary",
               values_to = "value") %>% 
  separate(summary,
           into = c("var", "descriptive"),
           sep = "_") %>% 
  pivot_wider(names_from = descriptive,
              values_from = value)

## is equivalent to this
blp_df_summ %>%
  pivot_longer(everything(),
               names_to = c("var", ".value"),
               names_sep = "_")
## .value = create two columns and pivot the second wide

## Nesting ---------------------------------------------------------------------

tidy_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_wrangling/data/example_1_tidy.csv")

tidy_df %>%
  group_by(subject) %>% 
  summarise(accuracy = mean(accuracy))

tidy_df %>%
  group_by(subject) %>%
  nest() %>%
  mutate(number_of_rows = map_int(data, nrow)) %>%
  select(- data)
## map() always returns a list; map_int() returns integers

## using `tidy_df` do a linear regression predicting `rt` from `delta`
## and return the slope coefficents (which is second value of coef)
coef(lm(rt ~ delta, data = tidy_df))[2]

# Now do that same thing for each subject in the data set
tidy_df %>% 
  group_by(subject) %>% 
  nest() %>% 
  mutate(slope = map_dbl(data, ~coef(lm(rt ~ delta, data = .))[2])) %>% 
  ungroup() %>%
  select(- data)