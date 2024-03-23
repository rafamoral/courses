## load the tidyverse suite of packages
library(tidyverse)

## Introducing ggplot2 ---------------------------------------------------------
## "Grammar of Graphics" (Leland Wilkinson)

data_df <- tibble(var1 = c(1, 3, 7, 11),
                  var2 = c(5, 7, 10, 12),
                  var3 = c("a", "b", "a", "b"))
## tibbles are more "effective" data frames;
## they do less (no change of variable names/types, no partial matching)
## and complain more (forcing you to deal with problems early on)

## A scatterplot of points
ggplot(data_df,
       mapping = aes(x = var1, y = var2)) +
  geom_point()

## A line plot
ggplot(data_df,
       mapping = aes(x = var1, y = var2)) +
  geom_line()

## A line plot with points
ggplot(data_df,
       mapping = aes(x = var1, y = var2)) +
  geom_line() +
  geom_point()

## Add a colour mapping as well
ggplot(data_df,
       mapping = aes(x = var1, y = var2, colour = var3)) +
  geom_point()

## Add a shape mapping as well
ggplot(data_df,
       mapping = aes(x = var1, y = var2, shape = var3)) +
  geom_point()

## Add a shape mapping as well and change size of points
ggplot(data_df,
       mapping = aes(x = var1, y = var2, shape = var3)) +
  geom_point(size = 5)

## override a mapping with a setting
ggplot(data_df,
       mapping = aes(x = var1, y = var2, colour = var3)) +
  geom_point(colour = "red")

## change shape and colour mapping
ggplot(data_df,
       mapping = aes(x = var1, y = var2, shape = var3, colour = var3)) +
  geom_point(size = 2.5)

##  colour mapping with a line plot
## Add a colour mapping as well
ggplot(data_df,
       mapping = aes(x = var1, y = var2, colour = var3)) +
  geom_line()

## the same code as previous, but less verbose, less explicit
ggplot(data_df,
       aes(var1, var2, colour = var3)) +
  geom_line()

## Histograms ------------------------------------------------------------------

## how to read raw data from github; careful not to read the html page
## load the data
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/weight.csv")

## histogram where binwidth is 5cm
ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_histogram()

ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_histogram(colour = "white", binwidth = 5)

## histogram where binwidth is 2.5cm
ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_histogram(colour = "white", binwidth = 2.5)

## histogram where we have 50 bins
ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_histogram(colour = "white", bins = 50)

## plus a rug
ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_histogram(colour = "white", bins = 50) +
  geom_rug(size = 0.1, alpha = 0.25)

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Stacked histogram: the default
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)) +
  geom_histogram(colour = "white", binwidth = 2.5)

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Dodged histogram
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)) +
  geom_histogram(colour = "white", 
                 binwidth = 2.5, 
                 position = "dodge")

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Identity histogram
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)) +
  geom_histogram(colour = "white", 
                 binwidth = 2.5, 
                 position = "identity")

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Identity histogram
## No fill; different colours
ggplot(weight_df,
       mapping = aes(x = height, colour = gender)) +
  geom_histogram(binwidth = 2.5, 
                 fill = NA,
                 position = "identity")

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Identity histogram
## with transparent fill colour
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)) +
  geom_histogram(colour = "white", 
                 binwidth = 2.5, 
                 position = "identity",
                 alpha = 0.75)

## histogram with binwidth 2.5, and separate histogram
## for Male and Female
## Filled histogram
ggplot(weight_df,
       mapping = aes(x = height, fill = gender)) +
  geom_histogram(colour = "white", 
                 binwidth = 2.5, 
                 position = "fill")

## Bar plots -------------------------------------------------------------------

## Get Titanic data
titanic_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/TitanicSurvival.csv")
titanic_df

## bar plot
ggplot(titanic_df,
       mapping = aes(x = passengerClass)) +
  geom_bar()

## stacked bar plot
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "stack")

## filled bar plot
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "fill")

## dodged bar plot
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "dodge")

## identity bar plot
ggplot(titanic_df,
       mapping = aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "identity", alpha = 0.75)

## barbarplots.github.io

## Tukey boxplot ---------------------------------------------------------------

swiss_df <- swiss %>%
  rownames_to_column("province") %>%
  mutate(is_catholic = Catholic > 50)

## boxplots
ggplot(swiss_df,
       mapping = aes(x = Fertility)) +
  geom_boxplot()

ggplot(swiss_df,
       mapping = aes(x = "", y = Fertility)) +
  geom_boxplot()

ggplot(swiss_df,
       mapping = aes(x = "", y = Fertility)) +
  geom_boxplot(width = 0.25)

## box and jitter plot
ggplot(swiss_df,
       mapping = aes(x = "", y = Fertility)) +
  geom_boxplot(width = 0.25) + 
  geom_jitter(width = 0.05, size = 0.5, alpha = 0.5)

## box and jitter plot
ggplot(swiss_df,
       mapping = aes(x = "", y = Fertility)) +
  geom_boxplot(width = 0.25, outlier.shape = NA) + 
  geom_jitter(width = 0.05, size = 0.75, alpha = 0.75)

## separate boxplots for is_catholic true and false
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility)) +
  geom_boxplot(width = 0.25, outlier.shape = NA) + 
  geom_jitter(width = 0.05, size = 0.75, alpha = 0.75)

## same boxplot with notches (95% CI for median)
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility)) +
  geom_boxplot(width = 0.25, outlier.shape = NA, notch = TRUE) + 
  geom_jitter(width = 0.05, size = 0.75, alpha = 0.75)

## violin variant
ggplot(swiss_df,
       mapping = aes(x = is_catholic, y = Fertility)) +
  geom_violin() + 
  geom_jitter(width = 0.05, size = 0.75, alpha = 0.75)

## Boxplots with two grouping variables

## VC = vitamin C pill; OJ = orange juice
## for starters, just `supp` on x axis
ggplot(ToothGrowth,
       mapping = aes(x = supp, y = len)) +
  geom_boxplot()

## use `dose` as x-axis
ggplot(ToothGrowth,
       mapping = aes(x = dose, y = len, group = dose)) +
  geom_boxplot()

## let's use both `supp` and `dose`
ggplot(ToothGrowth,
       mapping = aes(x = dose, y = len, 
                     group = interaction(dose,supp), 
                     colour = supp)) +
  geom_boxplot()

## barplot versions of grouped boxplot -----------------------------------------

## note the use of geom_col not geom_bar
ggplot(ToothGrowth,
       mapping = aes(x = dose, y = len, fill = supp) ) +
  geom_col(position = "dodge")

ggplot(ToothGrowth,
       mapping = aes(x = dose, y = len, fill = supp) ) +
  geom_bar(position = "dodge", stat = "identity")
#?geom_bar

## add error bars?

## first calculate mean and upper and lower lims
ToothGrowth %>%
  group_by(supp, dose) %>% 
  summarise(avg = mean(len),
            upper = mean(len) + sd(len)/sqrt(n()),
            lower = mean(len) - sd(len)/sqrt(n())) %>%
  ggplot(mapping = aes(x = factor(dose), 
                       y = avg, 
                       fill = supp,
                       ymin = lower,
                       ymax = upper)) +
  geom_col(position = "dodge") +
  geom_errorbar(width = .2, position = position_dodge(width = .9))

## Scatterplots ----------------------------------------------------------------

## basic scatterplot
ggplot(weight_df,
       mapping = aes(x = height, y = weight)) +
  geom_point(size = 0.5, alpha = 0.75)

## scatterplot with colour coding
ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)) +
  geom_point(size = 0.5, alpha = 0.75)

## scatterplot with colour coding
## and a rug plot
ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  geom_rug(size = 0.1, alpha = 0.5)

## marginal plots on scatterplots
library(ggExtra)

p1 <- ggplot(weight_df,
             mapping = aes(x = height, 
                           y = weight, 
                           colour = gender)) +
  geom_point(size = 0.5, alpha  = 0.75)

ggMarginal(p1)

p2 <- ggplot(weight_df,
             mapping = aes(x = height, 
                           y = weight, 
                           colour = gender)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  geom_rug(size = 0.1, alpha = 0.25) + 
  theme_classic() +
  theme(legend.position = "bottom")


ggMarginal(p2, 
           type = "histogram",
           groupFill = TRUE,
           groupColour = TRUE,
           position = 'identity',
           bins = 50,
           alpha = 0.5)


## add scatterplot smoother; loess smoother
ggplot(swiss_df,
       mapping = aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth()

## add scatterplot smoother; lm smoother
ggplot(swiss_df,
       mapping = aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm")

## add scatterplot smoother; lm smoother, no conf int
ggplot(swiss_df,
       mapping = aes(x = Examination, y = Fertility)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## using a larger data set
ggplot(weight_df,
       mapping = aes(x = height, y = weight)) +
  geom_point() +
  geom_smooth(method = "gam")

## using colours for gender
ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)) +
  geom_point(size = 0.1, alpha = 0.25) + 
  geom_smooth(method = "lm")

ggplot(weight_df,
       mapping = aes(x = height, y = weight, colour = gender)) +
  geom_point(size = 0.1, alpha = 0.25) + 
  geom_smooth(method = "lm", fullrange = TRUE)

## labelling the points in the scatterplot
ggplot(swiss_df,
       mapping = aes(x = Examination, 
                     y = Fertility, 
                     label = province)) +
  geom_text()

## using ggrepel
library(ggrepel)

ggplot(swiss_df,
       mapping = aes(x = Examination, 
                     y = Fertility, 
                     label = province)) +
  geom_text_repel()

ggplot(swiss_df,
       mapping = aes(x = Examination, 
                     y = Fertility, 
                     label = province)) +
  geom_label_repel()

ggplot(swiss_df,
       mapping = aes(x = Examination, 
                     y = Fertility, 
                     colour = is_catholic,
                     label = province)) +
  geom_point() +
  geom_label_repel()

## facet plots
ggplot(weight_df,
       mapping = aes(x = height, 
                     y = weight)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  facet_wrap(~ gender, scales = "fixed")

ggplot(weight_df,
       mapping = aes(x = height, 
                     y = weight)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  facet_wrap(~ race, scales = "fixed")

ggplot(weight_df,
       mapping = aes(x = height, 
                     y = weight)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  facet_wrap(gender ~ race, scales = "fixed")

## facet grid
ggplot(weight_df,
       mapping = aes(x = height, 
                     y = weight)) +
  geom_point(size = 0.5, alpha  = 0.75) +
  facet_grid(gender ~ race, scales = "fixed")

## Line plots ------------------------------------------------------------------

nott_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/nottingham_temp.csv")

ggplot(nott_df,
       mapping = aes(x = month, y = value)) +
  geom_point()

## join the lines
## this is not what we want
ggplot(nott_df,
       mapping = aes(x = month, y = value)) +
  geom_point() +
  geom_line()

## join the lines
ggplot(nott_df,
       mapping = aes(x = month, 
                     y = value, 
                     group = year)) +
  geom_point() +
  geom_line()

ggplot(nott_df,
       mapping = aes(x = month, 
                     y = value, 
                     colour = year)) +
  geom_point() +
  geom_line()

ggplot(nott_df,
       mapping = aes(x = month, 
                     y = value, 
                     colour = factor(year))) +
  geom_point() +
  geom_line()

ggplot(nott_df,
       mapping = aes(x = month, 
                     y = value, 
                     group = year,
                     colour = year)) +
  geom_point() +
  geom_line()

## density plots ---------------------------------------------------------------

ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_density()

## use wider bandwidth of KDE 
ggplot(weight_df,
       mapping = aes(x = height)) +
  geom_density(adjust = 2) + 
  geom_rug(size = 0.1, alpha = 0.5)

ggplot(weight_df,
       mapping = aes(x = height, colour = gender, fill = gender)) +
  geom_density(adjust = 2, alpha = .5) + 
  geom_rug(size = 0.1, alpha = 0.5)

## Heatmap ---------------------------------------------------------------------

ggplot(nott_df,
       mapping = aes(x = year, y = month, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red")

ggplot(nott_df,
       mapping = aes(x = year, y = month, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "blue", high = "red")

## Geospatial map --------------------------------------------------------------

map_data <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/local_authority_map_data.csv")
eu_ref <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/EU-referendum-result-data.csv")

## join up the two data sets
ref_mapdata <- inner_join(map_data, eu_ref, 
                          by = c("id" = "Area_Code"))

ggplot(ref_mapdata,
       mapping = aes(x = long, 
                     y = lat)) +
  geom_polygon(colour = "white", size = 0.1)

ggplot(ref_mapdata,
       mapping = aes(x = long, 
                     y = lat,
                     group = group)) +
  geom_polygon(colour = "white", size = 0.1)

ggplot(ref_mapdata,
       mapping = aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Pct_Leave)) +
  geom_polygon(colour = "white", size = 0.1)

## experiment with scale_fill_distiller, types, palettes, limits...

ggplot(ref_mapdata,
       mapping = aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Pct_Leave)) +
  geom_polygon(colour = "white", size = 0.1) + 
  coord_equal() +
  theme_minimal() +
  scale_fill_distiller(type = "div", 
                       palette = 5, 
                       limits = c(20, 80))
## brewer = discrete
## distiller = continuous interpolation
## fermenter = binned

## colorbrewer2.org
## knowledge from centuries of making maps, well though-out pallettes

## Fine tuning -----------------------------------------------------------------

p1 <- ggplot(weight_df,
             mapping = aes(x = height, y = weight, colour = gender)) +
  geom_point()

p1 + theme_classic()
p1 + theme_minimal()
p1 + theme_bw()
p1 + theme_dark()
p1 + theme_light()
p1 + theme_gray()

library(ggthemes)

p1 + theme_wsj()
p1 + theme_economist()
p1 + theme_economist_white()
p1 + theme_stata()
p1 + theme_excel()
p1 + theme_excel_new()
p1 + theme_tufte()
p1 + theme_calc()

## set the default theme to be classic
theme_set(theme_classic())

## Change labels
p1 + labs(x = "Height (cm)",
          y = "Weight (kg)",
          colour = "Gender")

p1 + xlab("Height (cm)") + ylab("Weight (kg)")

p1 + ggtitle("A scatterplot of weight vs. height")

## Changes limits
p1 + lims(x = c(0, 250),
          y = c(0, 150))
p1 + xlim(0, 200)

## colours
p1 + scale_colour_manual(values = c("HoneyDew", "brown"))
## HTML colour names: https://www.w3schools.com/tags/ref_colornames.asp
## explain RGB 0-256, written in hex #000000 (black) to #FFFFFF (white)
## rgb color picker (google)

## brewer palettes (see ?scale_colour_brewer's #Palettes subsection)
p1 + scale_colour_brewer(palette = "Accent")
p1 + scale_colour_brewer(palette = "Dark2")
p1 + scale_colour_brewer(palette = "Paired")
p1 + scale_colour_brewer(palette = "Set1")
p1 + scale_colour_brewer(palette = "Greens")
p1 + scale_colour_brewer(type = "seq", palette = 5)

p1 + scale_x_continuous(breaks = c(150, 160, 175, 200))
p1 + scale_y_continuous(breaks = c(50, 100, 120))

p2 <- ggplot(weight_df, aes(x = gender, y = height)) +
  geom_boxplot()

p2 + scale_x_discrete(label = c("Group A", "Group B"))

p1 + theme(legend.position = "top")
p1 + theme(legend.position = "bottom")
p1 + theme(legend.position = "none")

p1 + theme(axis.title.x = element_text(size = 20))
p1 + theme(axis.title.y = element_text(size = 20))
p1 + theme(axis.title = element_text(size = 20, family = "mono"))

## ggsave() --------------------------------------------------------------------

## saving as an image / pdf
## "Export" button, or
## ggsave(filename = "myplot.png", width = 6, height = 8, dpi = 300)

## Combining ggplot objects as panels within a figure --------------------------

library(ggpubr)

panel1 <- p1 +
  ggtitle("(a)")

panel2 <- p2 +
  geom_jitter(aes(color = gender),
              alpha = .2) +
  ggtitle("(b)")

ggarrange(panel1, panel2)
ggarrange(panel1, panel2, common.legend = TRUE)
ggarrange(panel1, panel2, ncol = 1)
ggarrange(panel1, panel2, ncol = 1, common.legend = TRUE)

panel1 <- panel1 +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

panel2 <- panel2 +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggarrange(panel1, panel2, ncol = 1, common.legend = TRUE)

## R Markdown tutorial ---------------------------------------------------------