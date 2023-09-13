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
select(blp_df, ends_with("T", ignore.case = FALSE))

## RegEx matching
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

#has_high_mean(participant)
#has_high_mean(lex)

# anonymous function
select(blp_df, where(function(x) {
  is.numeric(x) && mean(x, na.rm = TRUE) > 500
}))

# purrr style lambda anonymous function
select(blp_df, where(~{
  is.numeric(.) && mean(., na.rm = TRUE) > 500
}))

# ~ <=> function(.)

# select "everything"
select(blp_df, everything())

select(blp_df, rt, everything())


# Reordering columns with relocate ----------------------------------------

relocate(blp_df, rt)
relocate(blp_df, starts_with('r'))

relocate(blp_df, rt, .after = lex)
relocate(blp_df, rt, .before = lex)

relocate(blp_df, rt, .after = rt.raw)
relocate(blp_df, rt, .after = last_col())
relocate(blp_df, rt, .before = last_col())

relocate(blp_df, where(is.numeric))

relocate(blp_df, where(is.numeric), .after = last_col())



# Renaming with rename ----------------------------------------------------

select(blp_df, reaction_time = rt)
select(blp_df, reaction_time = rt, everything())

rename(blp_df, reaction_time = rt)

rename(blp_df, reaction_time = rt, lexical = lex)

rename_with(blp_df, toupper)
rename_with(blp_df, tolower)

# rename all numeric variables to uppercase
rename_with(.data = blp_df, .fn = toupper, .cols = where(is.numeric))

# let's look at `str_replace`
y <- c('hello', 'world')
str_replace(y, 'he', 'xx')

# ~str_replace(., 'rt', 'reaction_time')

rename_with(.data = blp_df, 
            .fn = ~str_replace(., 'rt', 'reaction_time'),
            .cols = matches('^rt|rt$'))

rename_with(.fn = ~str_replace(., 'rt', 'reaction_time'),
            .data = blp_df,
            .cols = matches('^rt|rt$'))


# Slicing with slice ------------------------------------------------------

slice(blp_df, 5)

slice(blp_df, 5:15)

slice(blp_df, c(10, 20, 50, 100, 500, 800))

slice(blp_df, seq(10, 1000, by = 10))

slice(blp_df, -10)

slice(blp_df, -(1:3))

slice(blp_df, 990:1000)
slice(blp_df, 990:n())
slice(blp_df, (n()-10):n())


# Filtering with filter ---------------------------------------------------

filter(blp_df, participant == 20)

filter(blp_df, lex == 'W', resp == 'W')
filter(blp_df, rt < 500)
filter(blp_df, rt <= 500)

# the following 
filter(blp_df, lex == 'W', resp == 'W', rt <= 500)
# is equivalent to this:
filter(blp_df, (lex == 'W') & (resp == 'W') & (rt <= 500))

# disjunction of conditions
filter(blp_df, lex == 'W' | rt <= 500)

filter(blp_df, (lex == 'W') & (resp == 'W') | (rt <= 500))
filter(blp_df, (lex == 'W') & (resp == 'W') | !(rt <= 500))

# filter where their response correct
filter(blp_df, lex == resp)

# find rows where there is at least one missing value
filter(blp_df, if_any(everything(), is.na))

filter(blp_df, if_all(everything(), is.na))


filter(blp_df, if_all(everything(), ~!is.na(.)))

# by the way, you can also do ....
drop_na(blp_df)
na.omit(blp_df)

# this 
# ~ . <= 500
# could be written as 
# function(x){
# x <= 500
# }


filter(blp_df, if_all(matches('^rt|rt$'), ~ . <= 500))
filter(blp_df, if_any(matches('^rt|rt$'), ~ . <= 500))

# the previous if_all command is equivalent to this:
filter(blp_df, rt <= 500, prev.rt <= 500, rt.raw <= 500)

# the previous if_any command is equivalent to this:
filter(blp_df, rt <= 500 | prev.rt <= 500 | rt.raw <= 500)

filter(blp_df, if_all(where(is.numeric), ~ . < median(., na.rm = TRUE)))



# Create new variables with mutate ----------------------------------------

mutate(blp_df, accuracy = lex == resp)

mutate(blp_df,
       accuracy = lex == resp,
       fast_response = rt < 500)

mutate(blp_df, rt = rt / 1000)

mutate(blp_df,
       accuracy = lex == resp,
       fast_response = rt < 500,
       rt = rt /1000)

# the previous code is different to 
mutate(blp_df,
       accuracy = lex == resp,
       rt = rt / 1000,
       fast_response = rt < 500)


mutate(blp_df, 
       lex = as.factor(lex),
       spell = as.factor(spell),
       resp = as.factor(resp)
)

mutate(blp_df, across(where(is.character), as.factor))


re_scale <- function(x) as.vector(scale(x))

mutate(blp_df, across(where(is.numeric), re_scale))
mutate(blp_df, across(where(is.numeric), ~ as.vector(scale(.)) ))


mutate(blp_df, 
       lex = recode(lex, 'W' = 'word', 'N' = 'nonword'))

mutate(blp_df,
       across(c(lex, resp), 
              ~ recode(., 'W' = 'word', 'N' = 'nonword'))
)

mutate(blp_df,
       fast_response = if_else(rt < 500, 'fast', 'slow')
)

mutate(blp_df,
       rt_speed = case_when(
         rt > 900 ~ 'slow',
         rt < 500 ~ 'fast',
         (rt >= 500) & (rt <= 900) ~ 'medium'
       )
)

mutate(blp_df,
       rt_speed = case_when(
         rt > 900 ~ 'slow',
         rt < 500 ~ 'fast',
         TRUE ~ 'medium'
       )
)

transmute(blp_df,
          rt_speed = case_when(
            rt > 900 ~ 'slow',
            rt < 500 ~ 'fast',
            TRUE ~ 'medium'
          )
)


# Sorting with arrange ----------------------------------------------------

arrange(blp_df, rt)
arrange(blp_df, desc(rt))
arrange(blp_df, spell)
arrange(blp_df, participant)
arrange(blp_df, participant, rt)


# Introducting the pipe ---------------------------------------------------

x <- c(2, 3, 5, 7, 11)

# we want the log of the sum of the sqrt of the log of x
log(x) # the log of x
sqrt(log(x)) # the sqrt of the log of x
sum(sqrt(log(x))) # sum of sqrt of log of x
log(sum(sqrt(log(x)))) # log of sum of sqrt of log of x

log(x) %>% sqrt() # equivalent to sqrt(log(x))

x %>% log() %>% sqrt() # same thing, equivalent to sqrt(log(x))

x %>% log() %>% sqrt() %>% sum() %>% log() # equiv to log(sum(sqrt(log(x))))



# A data wrangling pipeline -----------------------------------------------

read_csv("https://raw.githubusercontent.com/mark-andrews/idwrt23/main/data/blp-trials-short.txt") %>% 
  mutate(participant = as.factor(participant),
         accuracy = lex == resp) %>% 
  # the next lines do renaming
  rename(subject = participant,
         stimulus = spell,
         reaction_time = rt) %>% 
  select(subject, stimulus, lex, accuracy, reaction_time) %>% 
  filter(lex == 'W', accuracy, 
         reaction_time < 1500 & reaction_time > 500) %>% 
  na.omit()


blp_df %>% 
  mutate(participant = as.factor(participant),
         accuracy = lex == resp) %>% 
  select(subject = participant, 
         stimulus = spell, 
         lex, 
         accuracy, 
         reaction_time = rt)