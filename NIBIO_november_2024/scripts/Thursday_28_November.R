# load the tidyverse suite of packages
library(tidyverse)

# installing extra packages we will use in this session
# install.packages(c("ggExtra", "cowplot", "ggrepel", "ggpubr", "maps"))??


# Introducing ggplot2 -----------------------------------------------------
# gg = "grammar of graphics" (Leland Wilkinson)

# let's create a fictitious dataset (very small)
data_df <- tibble(var1 = c(1, 3, 7, 11),
                  var2 = c(5, 7, 10, 12),
                  var3 = c("a", "a", "b", "b"))

data_df

# A scatter plot of points (x vs y)

# In Base R, a simple scatter plot is very direct to code
plot(var2 ~ var1, data = data_df)

# in ggplot2, simple graphics are more verbose
# however, it is much simpler to add complexity when compared
# to base R plots
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point()

# a line plot
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_line()

# I want points AND lines
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point() +
  geom_line()

# Add a colour mapping
data_df %>%
  ggplot(aes(x = var1, y = var2, colour = var3)) +
  geom_point()

# Add a shape mapping, also increase size of points and paint them in blue
data_df %>%
  ggplot(aes(x = var1, y = var2, shape = var3)) +
  geom_point(size = 5, col = 4)

# what happens here?
data_df %>%
  ggplot(aes(x = var1, y = var2, colour = var3)) +
  geom_point(col = "red")
# setting up a colour within geom_point overrides the aesthetic mapping

# colour mapping to a line plot
data_df %>%
  ggplot(aes(x = var1, y = var2, colour = var3)) +
  geom_point(size = 5) +
  geom_line()
# by adding a colour mapping to the overall aesthetics, the lines
# won't all be connected any longer
# if you wish to only add colour to a specific geom and not all of them
# you may specify aes() within that specific geom

# e.g. I want to only colour the points, but I want the line to connect
# through all of them
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point(aes(colour = var3),
             size = 5) +
  geom_line()

data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point(size = 5) +
  geom_line(aes(colour = var3),
            linewidth = 5)


# Histograms --------------------------------------------------------------

# load the weight data
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/NIBIO_november_2024/data/weight.csv")
weight_df

# default histogram in ggplot
weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram()

# let's change the binwidth to 5 kg
weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth = 5)

weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram(col = "white",
                 binwidth = 5)

weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram(col = 1,
                 fill = "white",
                 binwidth = 5)

# let's add a rug
weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram(col = 1,
                 fill = "white",
                 binwidth = 5) +
  geom_rug()

# decrease rug width and add some transparency ("alpha-blending")
weight_df %>%
  ggplot(aes(x = weight)) +
  geom_histogram(col = 1,
                 fill = "white",
                 binwidth = 5) +
  geom_rug(size = 0.1, alpha = .2)

# let's have separate histograms for males vs females
# and use the height variable
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5)
# by default, ggplot will stack the histograms
# it would be better to place them beside each other
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5,
                 position = "dodge")

weight_df %>%
  ggplot(aes(x = height, fill = gender, colour = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5,
                 
                 position = "dodge") +
  geom_rug(size = .1, alpha = .2)

# calculate the mean and standard deviation of the height
# variable, grouped by gender
weight_df %>%
  group_by(gender) %>%
  summarise(means = mean(height),
            std_deviation = sd(height))

# experiment changing from "dodge" to "identity" or "fill"
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5,
                 position = "identity",
                 alpha = .6)

weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5,
                 position = "fill")


# Bar plots ---------------------------------------------------------------

# loading the Titanic data
titanic_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/refs/heads/main/NIBIO_november_2024/data/TitanicSurvival.csv")
titanic_df

### aside -- to read a csv file we use read_csv / read_csv2
### to read an xlsx file, you need to load a package called readxl and use the read_excel function
library(readxl)
test <- read_excel("Bok1.xlsx")
test2 <- read_csv2("Bok1_csv_version.csv")
### close aside

titanic_df %>%
  ggplot(aes(x = passengerClass)) +
  geom_bar()
  
titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar()

titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "fill")

titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "dodge")

titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "identity", alpha = .7)
# the above is awful, don't use it!

## barbarplots.github.io


# Box plots ---------------------------------------------------------------

swiss_df <- swiss %>%
  rownames_to_column("province") %>%
  mutate(is_catholic = Catholic > 50)

swiss_df %>%
  ggplot(aes(x = Fertility)) +
  geom_boxplot()

swiss_df %>%
  ggplot(aes(y = Fertility)) +
  geom_boxplot()

swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot(width = .2)

# adding points to the boxplot
swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot(width = .4,
               outlier.shape = NA) +
  geom_jitter(alpha = .2,
              height = 0,
              width = .1)

# violin variant
swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_violin() +
  geom_jitter(alpha = .2,
              height = 0,
              width = .1)

# hack the code below to produce boxplots side by side
# by the is_catholic variable
swiss_df %>%
  ggplot(aes(x = is_catholic, y = Fertility)) +
  geom_boxplot(width = .4,
               outlier.shape = NA) +
  geom_jitter(alpha = .2,
              height = 0,
              width = .1)

# Box plots with two grouping variables
ToothGrowth %>%
  mutate(dose = as.factor(dose)) %>%
  ggplot(aes(x = supp, y = len, fill = dose)) +
  geom_boxplot()

# this is equivalent (passing the data to ggplot rather than piping into it)
ggplot(ToothGrowth %>%
         mutate(dose = as.factor(dose)),
       aes(x = supp, y = len, fill = dose)) +
  geom_boxplot()

# bar plot version of a grouped box plot
ToothGrowth %>%
  mutate(dose = as.factor(dose)) %>%
  ggplot(aes(x = supp, y = len, fill = dose)) +
  geom_col()

## adding error bars (mean +/- std. error)
se <- function(x) sd(x) / sqrt(length(x))

ToothGrowth %>%
  mutate(dose = as_factor(dose)) %>%
  group_by(supp, dose) %>%
  summarise(avg = mean(len),
            lower = mean(len) - se(len),
            upper = mean(len) + se(len)) %>%
  ggplot(aes(x = dose,
             y = avg,
             fill = supp,
             ymin = lower,
             ymax = upper)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(position = position_dodge(width = .85),
                width = .25)

# suppose I carried out an analysis and ended up with
# letters denoting statistical significance within doses

ToothGrowth %>%
  mutate(dose = as_factor(dose)) %>%
  group_by(supp, dose) %>%
  summarise(avg = mean(len),
            lower = mean(len) - se(len),
            upper = mean(len) + se(len)) %>%
  ungroup() %>%
  mutate(letters = c("a","a","a","b","b","a")) %>%
  ggplot(aes(x = dose,
             y = avg,
             fill = supp,
             ymin = lower,
             ymax = upper,
             label = letters)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(position = position_dodge(width = .85),
                width = .25) +
  geom_text(aes(y = upper + 1),
            position = position_dodge(width = .85))

# the plot already looks "almost nice"
# let's make it ready for publication by tweaking some final bits

# we'll start by saving our ggplot to tweak it later
my_plot <- ToothGrowth %>%
  mutate(dose = as_factor(dose)) %>%
  group_by(supp, dose) %>%
  summarise(avg = mean(len),
            lower = mean(len) - se(len),
            upper = mean(len) + se(len)) %>%
  ungroup() %>%
  mutate(letters = c("a","a","a","b","b","a")) %>%
  ggplot(aes(x = dose,
             y = avg,
             fill = supp,
             ymin = lower,
             ymax = upper,
             label = letters)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(position = position_dodge(width = .85),
                width = .25) +
  geom_text(aes(y = upper + 1),
            position = position_dodge(width = .85))

# let's add nicer axis names
my_plot +
  xlab("Dose (mg/mL)")

# install.packages("egg")
library(egg) # to be able to use theme_article

my_plot +
  xlab(expression(Dose~(mg.mL^-1))) +
  ylab("Length (cm)") +
  scale_fill_discrete(name = "Supplement\ntype") +
  theme_article() +
  labs(title = "Tooth growth using different supplements",
       caption = "OJ: orange juice; VC = vitamin C pill") +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(hjust = .5))

# saving as a high resolution png
my_final_plot <- my_plot +
  xlab(expression(Dose~(mg.mL^-1))) +
  ylab("Length (cm)") +
  scale_fill_discrete(name = "Supplement\ntype") +
  theme_article() +
  labs(title = "Tooth growth using different supplements",
       caption = "OJ: orange juice; VC = vitamin C pill") +
  theme(axis.text = element_text(size = 10),
        plot.title = element_text(hjust = .5))

# use ggsave and specify the features you need
ggsave(filename = "figure1.png",
       plot = my_final_plot,
       width = 6,
       height = 4,
       dpi = 800)


# Scatter plots -----------------------------------------------------------

# basic scatter plot
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  theme_article() +
  geom_point(size = .8,
             alpha = .7)

# colour-coding by gender
weight_df %>%
  ggplot(aes(x = height, y = weight, colour = gender)) +
  theme_article() +
  geom_point(size = .8,
             alpha = .7)

# adding a marginal plot
library(ggExtra)

p1 <- weight_df %>%
  ggplot(aes(x = height, y = weight, colour = gender)) +
  theme_article() +
  geom_point(size = .8,
             alpha = .7) +
  theme(legend.position = "bottom")

ggMarginal(p1)

ggMarginal(p1,
           type = "histogram",
           groupFill = TRUE,
           position = "identity",
           alpha = .5)

# adding a smoother to the plot
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  theme_article() +
  geom_point(size = .8,
             alpha = .7) +
  geom_smooth(se = FALSE)


# Faceting ----------------------------------------------------------------

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  facet_wrap(~ gender, nrow = 1) +
  geom_smooth(col = 2)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  facet_grid(race ~ gender)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  facet_wrap(race ~ gender)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  facet_wrap(~ gender, scales = "free")

# plotting text
swiss_df %>%
  ggplot(aes(x = Examination, y = Fertility,
             label = province)) +
  geom_point() +
  geom_text()

swiss_df %>%
  ggplot(aes(x = Examination, y = Fertility,
             label = province)) +
  geom_point() +
  geom_label()

# to fix the overlapping labels, we can use ggrepel
library(ggrepel)

swiss_df %>%
  ggplot(aes(x = Examination, y = Fertility,
             label = province)) +
  geom_point() +
  geom_label_repel(size = 2, max.overlaps = 40)


# Maps --------------------------------------------------------------------

library(maps)

norway <- map_data("world", "norway")

norway %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", col = "black") +
  theme_bw() +
  coord_quickmap()


# https://www.color-hex.com/color-palettes/

norway %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#1274b950", col = "black") +
  theme_bw() +
  coord_quickmap()


# Creating multi-panel figures using ggarrange ----------------------------

panel_a <- my_final_plot +
  ggtitle("(a) Tooth growth data")

panel_b <- norway %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#1274b950", col = "black") +
  theme_bw() +
  coord_quickmap() +
  ggtitle("(b) A map of Norway in\na nice colour")

panel_c <- weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point() +
  facet_wrap(~ gender, scales = "free") +
  ggtitle("(c) A plot with facets")

panel_d <- swiss_df %>%
  ggplot(aes(x = Examination, y = Fertility,
             label = province)) +
  geom_point() +
  geom_label_repel(size = 2, max.overlaps = 40) +
  ggtitle("(d) Swiss fertility by province\nbecause we needed a 4th panel")

library(ggpubr)
ggarrange(panel_a, panel_b, panel_c, panel_d,
          nrow = 2, ncol = 2)

# save this multi-panel figure in high resolution (800 dpi)

last_plot_the_day <- ggarrange(panel_a, panel_b, panel_c, panel_d,
                               nrow = 2, ncol = 2)

ggsave(filename = "figure2.png",
       plot = last_plot_the_day,
       width = 10,
       height = 10,
       dpi = 800)
