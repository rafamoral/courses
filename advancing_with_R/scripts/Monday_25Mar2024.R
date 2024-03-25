## tidyverse package (library) is a package of packages

## load the tidyverse package
library(tidyverse)

library(readxl)
ex1 <- read_xlsx("example1.xlsx")
ex1

## load a dataset
blp_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/blp-trials-short.txt")

## dbl: double precision (numeric)
## chr: character string (symbols, letters)

blp_df

## look at more than 10 rows
print(blp_df, n = 20)
print(blp_df, n = Inf)

## View
#View(blp_df)

## glimpse
glimpse(blp_df)

## base R data.frame
blp_df_base <- read.csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/blp-trials-short.txt")
blp_df_base

# (Cmd/Ctrl + Shift + R)
# dplyr verbs -------------------------------------------------------------


# selecting columns with "select" -----------------------------------------

## select: used to select columns
select(blp_df, participant, lex, resp, rt)

## using the pipe operator %>% (from magrittr) 
##                         |> (from base R)
## dataset %>% dplyr_verb (or any other function)

blp_df %>% select(participant, lex, resp, rt)

## typically it is more readable to always break the line
## and use indentation after "piping"
blp_df %>%
  select(participant, lex, resp, rt)

## save the returned tibble as blp_df2
blp_df2 <- blp_df %>%
  select(participant, lex, resp, rt)
## you can also overwrite blp_df (but you'd lose access to the extra columns,
## unless you loaded the original data again)

## select and rename at the same time
blp_df %>%
  select(participant, lex, resp, reaction_time = rt)

## base R
#blp_df2 <- blp_df[, c("participant", "lex", "resp", "rt")]
#names(blp_df2)[4] <- "reaction_time"

## select by index
## e.g. select first, second, and seventh columns
blp_df %>%
  select(1, 2, 7)
## renaming works with indices too!
blp_df %>%
  select(1, 2, raw_reaction_time = 7)

## this
blp_df %>%
  select(lex, spell, resp, rt)
## is equivalent to these
blp_df %>%
  select(lex:rt)
blp_df %>%
  select(2:5)

blp_df %>%
  select(participant:resp, rt.raw)
blp_df %>%
  select(1:4, rt.raw)
blp_df %>%
  select(- rt, - prev.rt)

## NB: whenever there is a conflicted function name (same function name from
## two or more different packages), you can choose which one to use by
## using the syntax "package_name::function_name"
## we will see this with dplyr::select later in the course

## matching character strings
blp_df %>%
  select(starts_with("rt"))

blp_df %>%
  select(starts_with("r"))

blp_df %>%
  select(ends_with("t"))

blp_df %>%
  select(ends_with("a")) ## empty tibble (no column names end with "a")

blp_df %>%
  select(contains("rt"))

## RegEx matching using matches()
blp_df %>%
  select(matches("^rt")) ## equivalent to starts_with
blp_df %>%
  select(matches("rt$")) ## equivalent to ends_with
blp_df %>%
  select(matches("^rt|rt$")) ## starts_with OR ends_with

## case-sensitivity
blp_df %>%
  select(ends_with("T", ignore.case = FALSE))
## be careful! starts_with, ends_with etc are case insensitive by default
## you can change this by setting the ignore.case argument to FALSE

## selecting all but a few columns
blp_df %>%
  select(- rt, - prev.rt)
blp_df %>%
  select(- c(rt, prev.rt))

blp_df %>%
  select(lex:rt)
blp_df %>%
  select(- (lex:rt))
blp_df %>%
  select(- (2:5))

## selecting based on variable type
blp_df %>%
  select(where(is.numeric))
blp_df %>%
  select(- where(is.numeric))

## where can be used with any function, including user-defined functions
is.numeric(5)
is.numeric("five")

## side info: snake_case
##            camelCase
## you should avoid using full stops "." to separate words within an object name

## let's create our own function to use with select(where(...))
has_high_mean <- function(x) {
  is.numeric(x) && mean(x, na.rm = TRUE) > 500
}

blp_df %>%
  select(where(has_high_mean))

## using an anonymous function
blp_df %>%
  select(where(function(x) {
    is.numeric(x) && mean(x, na.rm = TRUE) > 500
  }))

## purrr-style "lambda" anonymous function
blp_df %>%
  select(where(~ {
    is.numeric(.) && mean(., na.rm = TRUE) > 500
  }))

## "~" is equivalent to "function(x)"

## select "everything"
blp_df %>%
  select(everything())
## this is useful to reorder columns
blp_df %>%
  select(rt, everything())
blp_df %>%
  select(reaction_time = rt, everything())


# Reordering columns using "relocate" -------------------------------------

blp_df %>%
  relocate(rt)
blp_df %>%
  relocate(starts_with("r"))

blp_df %>%
  relocate(rt, .after = lex)
blp_df %>%
  relocate(rt, .before = spell)

blp_df %>%
  relocate(rt, .after = rt.raw)
blp_df %>%
  relocate(rt, .after = last_col())
blp_df %>%
  relocate(rt, .before = last_col())

## combining select with relocate within the same pipeline
## readable and useful!
blp_df %>%
  select(where(is.numeric)) %>%
  relocate(rt, .before = last_col())

blp_df %>%
  relocate(where(is.numeric))
blp_df %>%
  relocate(where(is.numeric), .after = last_col())


# Renaming with "rename" --------------------------------------------------

blp_df %>%
  select(reaction_time = rt)
blp_df %>%
  select(reaction_time = rt, everything())

blp_df %>%
  rename(reaction_time = rt)

blp_df %>%
  rename(reaction_time = rt, lexical = lex)

## "rename_with" a function
blp_df %>%
  rename_with(toupper)

toupper(c("a","b","march"))
tolower(c("A","B","March"))

blp_df %>%
  rename_with(tolower) ## here changes nothing because all column titles are
                       ## already lower-case

blp_df %>%
  rename_with(toupper, where(is.numeric))

##rename_with(.data, .fn, .cols = everything())
rename_with(.data = blp_df, .fn = toupper, .cols = where(is.numeric))

## a more verbose version of the code
## the full stop "." explicitly tells the function where the object
## is to be piped into
blp_df %>%
  rename_with(.data = ., .fn = toupper, .cols = where(is.numeric))

## let's have a look at the function "str_replace"
y <- c("hello", "world")
str_replace(y, "he", "xx")
str_replace(y, "world", "planet")

## this is looking for "rt" anywhere in the column names and replacing with
## "reation_time", including the word "participant"
blp_df %>%
  rename_with(~ str_replace(., "rt", "reaction_time"))

## to fix, tell R you want only whenever it starts or ends with "rt"
blp_df %>%
  rename_with(~ str_replace(., "rt", "reaction_time"),
              matches("^rt|rt$"))


# Slicing with "slice" ----------------------------------------------------

## looking at row number 5
blp_df %>%
  slice(5)

blp_df %>%
  slice(5:15)

blp_df %>%
  slice(c(10, 20, 50, 100, 500, 800))

blp_df %>%
  slice(seq(10, 1000, by = 10))

## remove 10th row
blp_df %>%
  slice(- 10)

blp_df %>%
  slice(- (1:3))

blp_df %>%
  slice(990:1000)

blp_df %>%
  slice(990:n())

blp_df %>%
  slice((n() - 10):n())


# Filtering with filter ---------------------------------------------------

blp_df %>%
  filter(participant == 20)

## NB: in logical syntax, we have
## > greater than
## < less than
## == equals
## >= greater than or equal
## <= less than or equal
## |  or
## &  and
## !  not
## != different

## participant 20 AND real words
blp_df %>%
  filter(participant == 20 & lex == "W")

## participant 20 (real or non-real word)
## OR any participant as long as the word is real
blp_df %>%
  filter(participant == 20 | lex == "W")

## multiple filters can be used without specifying "&"
blp_df %>%
  filter(participant == 20, lex == "W", rt < 600)

## in base R:
## blp_df[blp_df$participant == 20 & blp_df$lex == "W" & blp_df$rt < 800,]

## filter where the participant's response is correct
blp_df %>%
  filter(lex == resp)

## find rows where there is at least one missing value
blp_df %>%
  filter(if_any(everything(), is.na))

## returns an empty tibble because there is no row for which
## all observations are missing
blp_df %>%
  filter(if_all(everything(), is.na))

## dropping incomplete cases (where at least one observation is missing)
is_not_na <- function(x) {
  !is.na(x)
}

blp_df %>%
  filter(if_all(everything(), is_not_na))

## doing the same less explicitly, by using a lambda anonymous function
blp_df %>%
  filter(if_all(everything(), ~ !is.na(.)))

## in practice, this is easily achievable through
drop_na(blp_df)

## base R
na.omit(blp_df)

x <- 1:10
x[5] <- NA
na.omit(x)

## filtering where rt, prev.rt, and rt.raw are < 500
blp_df %>%
  filter(rt < 500, prev.rt < 500, rt.raw < 500)

## using RegEx matching and an anonymous function (function(x) x < 500)
blp_df %>%
  filter(if_all(matches("^rt|rt$"), ~ . < 500))

## filtering if any of the rt, prev.rt, and rt.raw are < 500
blp_df %>%
  filter(if_any(matches("^rt|rt$"), ~ . < 500))

## this previous if_any is equivalent to this
blp_df %>%
  filter(rt < 500 | prev.rt < 500 | rt.raw < 500)

## filtering when all numeric variables are less than their median
## and saving as a new object for further analyses, plotting, etc.
blp_df_fast_cohort <- blp_df %>%
  filter(if_all(where(is.numeric), ~ . < median(., na.rm = TRUE)))


# Creating new variables with "mutate" ------------------------------------

## creating the "accuracy" variable which is TRUE if resp = lex and FALSE otherwise
blp_df %>%
  mutate(accuracy = lex == resp)

## filtering by incorrect answers
blp_df %>%
  mutate(accuracy = lex == resp) %>%
  filter(accuracy == FALSE)

blp_df %>%
  mutate(accuracy = lex == resp) %>%
  filter(!accuracy) ## this is because "accuracy" is a logical (Boolean) column

## mutate to create more than one variable
blp_df %>%
  mutate(accuracy = lex == resp,
         fast_response = rt < 500)

## the base R equivalent to a "tibble" is a "data.frame"
## see this data.frame
data.frame(blp_df,
           fast_response = rt < 500)
## gives error

mutate(blp_df,
       fast_response = rt < 500)
## works fine

## with tidyverse you can define new variables and immediately use them
## within the same function call
blp_df %>%
  mutate(scaled_rt = as.numeric(scale(rt)),
         fast_rt = scaled_rt < 0)

## but a variable has to be created before it is used
## the following code generates an error because "scaled_rt"
## is created after "fast_rt"
blp_df %>%
  mutate(fast_rt = scaled_rt < 0,
         scaled_rt = as.numeric(scale(rt)))

## you can do mathematical operations / transformations
## bear in mind that if you use the same variable name, it will
## be overwritten
blp_df %>%
  mutate(accuracy = lex == resp,
         rt = rt / 1000)

## careful when using overwritten variables!
blp_df %>%
  mutate(accuracy = lex == resp,
         rt = rt / 1000,
         fast_response = rt < 500) ## fast_response will be TRUE for all, when we didn't mean it to

## converting multiple columns to a factor
blp_df %>%
  mutate(lex = as.factor(lex),
         spell = as.factor(spell),
         resp = as.factor(resp))

as.factor(letters[1:5]) ## e.g.

## using across() with mutate()
## across("which_columns", "do_what")
blp_df %>%
  mutate(across(where(is.character), as.factor))

## scaling all numeric columns to have a mean of zero and variance of 1
re_scale <- function(x) as.numeric(scale(x))

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor),
         across(where(is.numeric), re_scale))

blp_df %>%
  mutate(participant = as.factor(participant),
         across(where(is.character), as.factor),
         across(where(is.numeric), ~ as.numeric(scale(.))))

## recoding levels of a factor or character column
blp_df %>%
  mutate(lex = recode(lex, "W" = "word",
                           "N" = "nonword"))

## recoding two columns in one go
blp_df %>%
  mutate(across(c(lex, resp),
                ~ recode(., "W" = "word",
                            "N" = "nonword")))

## conditional statements
## if_else("condition", "what to do if condition is true", "what to do otherwise")
blp_df %>%
  mutate(fast_response = if_else(rt < 500, "fast", "slow"))

## more than 1 condition
## use case_when() instead of if_else()
blp_df %>%
  mutate(rt_speed = case_when(
    rt > 900 ~ "slow",
    rt < 500 ~ "fast",
    rt >= 500 & rt <= 900 ~ "medium"
  ))

## "otherwise / else" condition: use the value TRUE
blp_df %>%
  mutate(rt_speed = case_when(
    rt > 900 ~ "slow",
    rt < 500 ~ "fast",
    TRUE ~ "medium"
  ))

## transmute function: returns only the mutated column
blp_df %>%
  transmute(rt_seconds = rt / 1000,
            participant = as.factor(participant),
            rt_speed = case_when(
              rt > 900 ~ "slow",
              rt < 500 ~ "fast",
              TRUE ~ "medium"
            ))

blp_df %>%
  mutate(rt_seconds = rt / 1000,
         participant = as.factor(participant),
         rt_speed = case_when(
           rt > 900 ~ "slow",
           rt < 500 ~ "fast",
           TRUE ~ "medium"
         ),
         .keep = "none")


# Sorting with arrange ----------------------------------------------------

blp_df %>%
  arrange(rt)

blp_df %>%
  arrange(desc(rt))

blp_df %>%
  arrange(spell)

blp_df %>%
  arrange(desc(spell))

blp_df %>%
  arrange(participant, rt)


# Pipes -------------------------------------------------------------------

x <- c(2, 3, 5, 7, 11)

## we want the log of the sum of the sqrt of the log of x
log(x) # the log of x
sqrt(log(x)) # the sqrt of the log of x
sum(sqrt(log(x))) # the sum of the sqrt of the log of x
log(sum(sqrt(log(x)))) # what we want

x %>% log ## equivalent to log(x)

x %>% log %>% sqrt %>% sum %>% log

x %>%
  log %>%
  sqrt %>%
  sum %>%
  log


# A data wrangling pipeline -----------------------------------------------

## take the dataset, and then...
blp_df %>%
  ## convert "participant" to a factor, create the accuracy variable, and then...
  mutate(participant = as.factor(participant),
         accuracy = lex == resp) %>%
  ## rename variables, and then...
  rename(subject = participant,
         stimulus = spell,
         reaction_time = rt) %>%
  ## select variables, and then...
  select(subject, stimulus, lex, accuracy, reaction_time) %>%
  ## filter according to some rules, and then...
  filter(lex == "W",
         accuracy,
         reaction_time < 1500 & reaction_time > 500) %>%
  ## remove incomplete cases
  drop_na()

## long pipes aren't a problem, they are still readable
## you can comment in between and explain what each step is doing


# Grouping and summarising with group_by and summarise --------------------

blp_df %>%
  summarise(mean(rt, na.rm = TRUE))

## NB: you can use pull() to pull a variable and work with it
blp_df %>%
  pull(rt) %>%
  na.omit() %>%
  mean

## getting more than 1 summary in one go
blp_df %>%
  drop_na() %>%
  summarize(avg = mean(rt),
            med_rt_raw = median(rt.raw))

## combining group_by() and summarise()
blp_df %>%
  drop_na() %>%
  group_by(lex) %>%
  summarise(avg = mean(rt),
            std_dev = sd(rt))

blp_df %>%
  mutate(fast_rt = rt < 500) %>%
  drop_na() %>%
  group_by(lex, fast_rt) %>%
  summarise(avg = mean(rt),
            std_dev = sd(rt))

## summarising across multiple columns
blp_df %>%
  drop_na() %>%
  group_by(lex) %>%
  summarise(across(rt:rt.raw, median))

## more than one summary in one go
blp_df %>%
  drop_na() %>%
  group_by(lex) %>%
  summarise(across(rt:rt.raw, list(median = median,
                                   mad = mad)))


# Merging datasets --------------------------------------------------------

source("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/scripts/simple_df_eg.R")

Df_1
Df_2

bind_rows(Df_1, Df_2)
bind_rows(Df_2, Df_1)
bind_rows(Df_1, Df_3)

Df_a
Df_b

left_join(Df_a, Df_b)
right_join(Df_a, Df_b)
inner_join(Df_a, Df_b)
full_join(Df_a, Df_b)

anti_join(Df_a, Df_b)
semi_join(Df_a, Df_b)

## an example with real data
blp_stimuli <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/blp_stimuli.csv")

left_join(blp_df, blp_stimuli)

blp_stimuli %>%
  filter(spell == "staud")

right_join(blp_df, blp_stimuli)

inner_join(blp_df, blp_stimuli)

all.equal(left_join(blp_df, blp_stimuli),
          inner_join(blp_df, blp_stimuli))

## joining by 2 variables
d1 <- tibble(replicate_id = c(1,1,2,2),
             year = c(2019,2020,2019,2020),
             response1 = rnorm(4))

d2 <- tibble(replicate_id = c(1,1,2,2),
             year = c(2019,2020,2019,2020),
             response2 = rnorm(4))

left_join(d1, d2) ## will do automatically if names are the same

d3 <- tibble(id = c(1,1,2,2),
             year = c(2019,2020,2019,2020),
             response2 = rnorm(4))

left_join(d1, d3)
left_join(d1, d3, by = join_by(replicate_id == id, year))


# Reshaping with pivot_long and pivot_wider -------------------------------

recall_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/repeated_measured_a.csv")
recall_df_b <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/repeated_measured_b.csv")

recall_df_long <- recall_df %>%
  pivot_longer(- Subject,
               names_to = "emotion",
               values_to = "memory")

recall_df_long %>%
  pivot_wider(names_from = emotion,
              values_from = memory)

recall_df_b %>%
  pivot_longer(- Subject,
               names_to = "treatment",
               values_to = "memory") %>%
  separate(col = treatment,
           into = c("cue", "emotion"),
           sep = "_")

## you can do the above in one go
recall_df_b %>%
  pivot_longer(- Subject,
               names_to = c("cue", "emotion"),
               names_sep = "_",
               values_to = "memory")

recall_df_b %>%
  pivot_longer(- Subject,
               names_to = c("cue", "emotion"),
               names_pattern = "(.*)_(.*)",
               values_to = "memory")
## . = any character
## * = any number of characters
## (.*) = any number of characters of any kind

blp_summary <- blp_df %>%
  drop_na() %>%
  group_by(lex) %>%
  summarise(across(rt:rt.raw, list(median = median,
                                   mad = mad)))

## make it into a tidy table
## every variable has its own column
## every observation has its own row
## every cell contains one single value
blp_summary_final <- blp_summary %>%
  pivot_longer(- lex,
               names_to = "summary",
               values_to = "value") %>%
  separate(col = summary,
           into = c("variable", "desc_stat"),
           sep = "_") %>%
  pivot_wider(names_from = desc_stat,
              values_from = value)

blp_summary_final


# Nesting -----------------------------------------------------------------

tidy_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/example_1_tidy.csv")

## how many subjects?
tidy_df %>%
  pull(subject) %>%
  unique %>%
  length

## mean accuracy for each subject
tidy_df %>%
  group_by(subject) %>%
  summarise(mean_acc = mean(accuracy)) %>%
  arrange(desc(mean_acc))

## nesting the dataset
tidy_df %>%
  group_by(subject) %>%
  nest()

## nesting allows us to apply functions to each nested dataset
tidy_df %>%
  group_by(subject) %>%
  nest() %>%
  mutate(number_of_rows = map_int(data, nrow)) %>%
  select(- data)

## equivalent to...
tidy_df %>%
  group_by(subject) %>%
  summarise(number_of_rows = n())

## let's use tidy_df to do a linear regression predicting
## "rt" using "delta" and return the slope coefficients
## for each subject
extract_slope <- function(x) {
  coef(lm(rt ~ delta, data = x))[2]
}

tidy_df %>%
  group_by(subject) %>%
  nest() %>%
  mutate(slope = map_dbl(data, extract_slope)) %>%
  select(- data)