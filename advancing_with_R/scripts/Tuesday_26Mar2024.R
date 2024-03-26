## load the tidyverse
library(tidyverse)


# Introducing ggplot2 -----------------------------------------------------
## gg = grammar of graphics (Leland Wilkinson)

data_df <- tibble(var1 = c(1, 3, 7, 11),
                  var2 = c(5, 7, 10, 12),
                  var3 = c("a", "b", "a", "b"))

plot(data_df$var1, data_df$var2) ## base R: plot(x, y)

## with ggplot2, a bit more verbose for simple plots
## but way easier to construct more complex plots
ggplot(data_df) ## empty panel

## the plot is dictated by a mapping of aesthetic elements
ggplot(data_df,
       mapping = aes(x = var1, y = var2))
## now ggplot draws an empty panel but it includes
## var1 in the x-axis and var2 in the y-axis, with their ranges

## now we need to *add* geometric objects to our plot
ggplot(data_df,
       mapping = aes(x = var1, y = var2)) +
  geom_point()

## piping the data into ggplot
data_df %>%
  ggplot(mapping = aes(x = var1, y = var2)) +
  geom_point()

## function_name(argument1 = ..., argument2 = ...)
## `+` is a function

`+`(2, 3) ## standard way of evaluating a function
2 + 3     ## non-standard evaluation

## a line plot
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_line()

## points and a line
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point() +
  geom_line()

## changing the colour of the points
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_point(col = "red") +
  geom_line()

## the order of inclusion of geoms affects the end result
## in this case, points are drawn on top of the line
## whereas above, the line was drawn on top of the points
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_line() +
  geom_point(col = "red")
  
## change the colour of the line as well
data_df %>%
  ggplot(aes(x = var1, y = var2)) +
  geom_line(col = "blue") +
  geom_point(col = "red")

## setting up colour as an aesthetic mapping
data_df %>%
  ggplot(aes(x = var1, y = var2, col = var3)) +
  geom_point()

data_df %>%
  ggplot(aes(x = var1, y = var2, col = var3)) +
  geom_point() +
  geom_line()

## override a mapping using a setting within a geom
data_df %>%
  ggplot(aes(x = var1, y = var2, col = var3)) +
  geom_point(col = "red")

## add a shape mapping as well
data_df %>%
  ggplot(aes(x = var1, y = var2, shape = var3)) +
  geom_point(size = 5)

## colour and shape mappings
data_df %>%
  ggplot(aes(x = var1, y = var2, shape = var3, col = var3)) +
  geom_point(size = 5)

## less explicit code (within aes, x and y come first and second, respectively)
data_df %>%
  ggplot(aes(var1, var2, col = var3)) +
  geom_point() +
  geom_line()


# Histograms --------------------------------------------------------------
## for continuous variables

## loading some data from github
weight_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/weight.csv")

## do a default histogram
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram()

## playing with binwidth
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 5)

## too coarse
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 20)

## too much resolution
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = .2)

## we can also pick the number of bins rather than the binwidth
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram(col = "white", bins = 50)

## adding a rug (also adding transparency = alpha blending)
weight_df %>%
  ggplot(aes(x = height)) +
  geom_histogram(col = "white", bins = 50, linewidth = .1) +
  geom_rug(linewidth = .1, alpha = .2)

## creating multiple histograms according to a variable
## default "stacked" version
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(col = "white",
                 bins = 50,
                 linewidth = .1)

## separate the two using a "dodged" histogram
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(bins = 50,
                 linewidth = .1,
                 position = "dodge")

## overlaid histograms (added transparency for better visualisation)
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(bins = 50,
                 linewidth = .1,
                 position = "identity",
                 alpha = .7)

## histogram with binwidth of 2.5
## separate histograms for Male and Female
## identity histogram (overlaid)
## no fill, but different colours according to gender
weight_df %>%
  ggplot(aes(x = height, col = gender)) +
  geom_histogram(binwidth = 2.5,
                 linewidth = .5,
                 position = "identity",
                 fill = NA)

## "filled" histogram
weight_df %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(colour = "white",
                 binwidth = 2.5,
                 position = "fill")


# Bar plots ---------------------------------------------------------------

## loading the Titanic dataset
titanic_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/TitanicSurvival.csv")

## simple bar plot
titanic_df %>%
  ggplot(aes(x = passengerClass)) +
  geom_bar()

## stacked bar plots (the default)
titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "stack")

## filled bar plots (easier to examine proportions for each class)
titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "fill")

## aside: a data wrangling pipeline to look at percentage of survival
titanic_df %>%
  mutate(survival_flag = survived == "yes") %>%
  group_by(passengerClass) %>%
  summarise(prop_surv = mean(survival_flag))

## dodged bar plots
titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "dodge")

## identity bar plots (looks terrible!)
titanic_df %>%
  ggplot(aes(x = passengerClass, fill = survived)) +
  geom_bar(position = "identity",
           alpha = .6)

## piping wrangled data into ggplot for points and bars
titanic_df %>%
  mutate(survival_flag = survived == "yes") %>%
  group_by(passengerClass) %>%
  summarise(prop_surv = mean(survival_flag)) %>%
  ggplot(aes(x = passengerClass, y = prop_surv)) +
  geom_point()

titanic_df %>%
  mutate(survival_flag = survived == "yes") %>%
  group_by(passengerClass) %>%
  summarise(prop_surv = mean(survival_flag)) %>%
  ggplot(aes(x = passengerClass, y = prop_surv)) +
  geom_bar(stat = "identity")

## careful when using barplots to represent means +/- standard errors
## barbarplots.github.io


# Box plots ---------------------------------------------------------------

## box and whiskers
## Tukey's box plot

swiss_df <- swiss %>%
  rownames_to_column("province") %>%
  mutate(is_catholic = Catholic > 50) %>%
  as_tibble()

## boxplots
swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot()

swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot(width = .25)

## making a boxplot more informative by adding the raw data points
swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot(width = .25,
               outlier.shape = NA) +
  geom_point(alpha = .3)

## now jittering the points horizontally
swiss_df %>%
  ggplot(aes(x = "", y = Fertility)) +
  geom_boxplot(width = .25,
               outlier.shape = NA) +
  geom_jitter(height = 0,
              width = .1,
              alpha = .3)

## separate boxplots for is_catholice TRUE vs FALSE
swiss_df %>%
  ggplot(aes(x = is_catholic, y = Fertility)) +
  geom_boxplot(width = .25,
               outlier.shape = NA) +
  geom_jitter(height = 0,
              width = .1,
              alpha = .3)

## adding notches (the notches represent a 95% CI for the median)
swiss_df %>%
  ggplot(aes(x = is_catholic, y = Fertility)) +
  geom_boxplot(width = .25,
               outlier.shape = NA,
               notch = TRUE) +
  geom_jitter(height = 0,
              width = .1,
              alpha = .3)

## violin plot variant
swiss_df %>%
  ggplot(aes(x = is_catholic, y = Fertility)) +
  geom_violin() +
  geom_jitter(height = 0,
              width = .1,
              alpha = .3)

## boxplots with two grouping variables

## VC = vitamin C pill; OJ = orange juice
ToothGrowth %>%
  ggplot(aes(x = supp, y = len)) +
  geom_boxplot()

## use `dose` as the x-axis
ToothGrowth %>%
  ggplot(aes(x = factor(dose), y = len)) +
  geom_boxplot()

ToothGrowth %>%
  ggplot(aes(x = dose, y = len, group = dose)) +
  geom_boxplot()

## let's use both `supp` and `dose`
ToothGrowth %>%
  ggplot(aes(x = dose, y = len,
             group = interaction(dose, supp),
             col = supp)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0,
              width = .1,
              alpha = .4)

ToothGrowth %>%
  ggplot(aes(x = dose, y = len,
             group = interaction(dose, supp),
             col = supp)) +
  geom_violin() +
  geom_jitter(height = 0,
              width = .1,
              alpha = .4)

## bar plot version of the grouped box plot
ToothGrowth %>%
  ggplot(aes(x = dose, y = len, fill = supp)) +
  geom_bar(stat = "identity",
           position = "dodge")
## be careful! this bar plot is showing the maximum values!

## if we want bar plots showing the mean and error bars for the
## standard error, we need to compute those!
ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(avg = mean(len),
            upper = mean(len) + sd(len) / sqrt(n()),
            lower = mean(len) - sd(len) / sqrt(n())) %>%
  ggplot(aes(x = factor(dose),
             y = avg,
             fill = supp,
             ymin = lower,
             ymax = upper)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(width = .3,
                position = position_dodge(width = .9))

## error bars representing 95% CI for the mean
## (using t quantile because n is small)
t_quantile <- qt(p = 0.975, df = 9)

ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(avg = mean(len),
            upper = mean(len) + t_quantile * sd(len) / sqrt(n()),
            lower = mean(len) - t_quantile * sd(len) / sqrt(n())) %>%
  ggplot(aes(x = factor(dose),
             y = avg,
             fill = supp,
             ymin = lower,
             ymax = upper)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_errorbar(width = .3,
                position = position_dodge(width = .9))


# Scatterplots ------------------------------------------------------------

## basic scatterplot
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .5, alpha = .3)

## scatterplot with colour coding by gender
weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3)

## add a linear regression line
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth(method = lm,
              linewidth = .5,
              se = FALSE)

weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth(method = lm,
              linewidth = .5,
              se = FALSE)

## adding a nonlinear smooth
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth()

weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth()

## adding a rug to your scatterplot
weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth(method = lm) +
  geom_rug(size = .1, alpha = .3)

## in ggplot2 you can create plot objects and add layers / geoms to them
plot1 <- weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3)

plot1 + geom_smooth()

## adding a theme
plot1 + theme_bw()
plot1 + theme_minimal()
plot1 + theme_light()
plot1 + theme_void()

## setting up your custom theme
plot1 + theme(axis.text.x = element_text(size = 20,
                                         color = "green",
                                         angle = 45,
                                         hjust = 1),
              panel.grid = element_blank())

## theme_bw removing the grid lines
plot1 + theme_bw() + theme(panel.grid = element_blank())

## adding marginal plots to a scatterplot
library(ggExtra)

p1 <- weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  theme_bw()

ggMarginal(p1)

ggMarginal(p1,
           type = "boxplot")

ggMarginal(p1,
           type = "densigram")

ggMarginal(p1,
           type = "boxplot",
           groupColour = TRUE)

ggMarginal(p1,
           type = "histogram",
           groupFill = TRUE,
           position = "identity",
           alpha = .4,
           bins = 40)

p2 <- weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth(method = lm) +
  theme_bw() +
  theme(legend.position = "bottom")

ggMarginal(p2,
           type = "histogram",
           groupFill = TRUE,
           position = "identity",
           alpha = .4,
           bins = 40)

## editing the range of the geom_smooth()
## careful, this is extrapolation!
weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point(size = .5, alpha = .3) +
  geom_smooth(method = lm,
              fullrange = TRUE)

## fitting a model separately and adding predictions
## we fit a quadratic polynomial for each gender
## be careful, this is not a sensible model, we are using it just to illustrate
fit <- lm(weight ~ poly(height, 2) * gender,
          data = weight_df)

library(modelr)

weight_df %>%
  add_predictions(model = fit) %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  theme_bw() +
  geom_point(size = .5, alpha = .3) +
  geom_line(aes(y = pred))

## labelling the points in a scatterplot
swiss_df %>%
  ggplot(aes(x = Examination,
             y = Fertility,
             label = province)) +
  geom_text()

swiss_df %>%
  ggplot(aes(x = Examination,
             y = Fertility,
             label = province)) +
  geom_label(size = 1.5)

## repel the labels using ggrepel
library(ggrepel)

swiss_df %>%
  ggplot(aes(x = Examination,
             y = Fertility,
             label = province)) +
  geom_text_repel(size = 2,
                  force = 2)

swiss_df %>%
  ggplot(aes(x = Examination,
             y = Fertility,
             label = province)) +
  geom_label_repel(size = 1.5)

swiss_df %>%
  mutate(Religion = recode(factor(is_catholic),
                           "TRUE" = "Catholic",
                           "FALSE" = "Protestant")) %>%
  ggplot(aes(x = Examination,
             y = Fertility,
             color = Religion,
             label = province)) +
  geom_point(size = .7) +
  geom_label_repel(size = 1.5) +
  theme_bw() +
  ggtitle("Fertility indices of Swiss provinces in 1888") +
  xlab("Examination (% draftees)") +
  ylab(expression(Fertility~(I[g])))

## expression() uses its own "language" to write Greek letters, symbols,
## superscripts (^), subscripts ([]), spaces (~)
## see ?plotmath for more
## e.g. expression(Richness~of~italic(Arabidopsis)~sp.)

## splitting the graphical window into different panels / facets
## facet_wrap and facet_grid
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ gender)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ gender,
             scales = "free_x")

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ gender,
             scales = "free_y")

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ gender,
             scales = "free")

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ gender, ncol = 1)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ race)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(~ race, ncol = 4, nrow = 2)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_wrap(gender ~ race)

## using facet_grid
weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_grid(gender ~ race)

weight_df %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(size = .3, alpha = .3) +
  facet_grid(race ~ gender) +
  theme(strip.text.x = element_text(face = "italic"))


# Line plots --------------------------------------------------------------

nott_df <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/advancing_with_R/data/nottingham_temp.csv")

nott_df %>%
  ggplot(aes(x = month, y = value)) +
  geom_point()

## joining the lines
## this is not what we want
nott_df %>%
  ggplot(aes(x = month, y = value)) +
  geom_point() +
  geom_line()

## we want to join the lines grouping per year
nott_df %>%
  ggplot(aes(x = month, y = value, group = year)) +
  geom_point() +
  geom_line()

nott_df %>%
  ggplot(aes(x = month, y = value, col = year)) +
  geom_point() +
  geom_line()

nott_df %>%
  ggplot(aes(x = month, y = value,
             group = year,
             col = year)) +
  geom_point() +
  geom_line()


# Density plots -----------------------------------------------------------

weight_df %>%
  ggplot(aes(x = height)) +
  geom_density()

## tweaking the plot: wider bandwidth, adding rug
weight_df %>%
  ggplot(aes(x = height)) +
  geom_density(adjust = 1.5) +
  geom_rug(size = .1, alpha = .3)

## overlay density plots according to gender
weight_df %>%
  ggplot(aes(x = height, fill = gender, col = gender)) +
  theme_bw() +
  geom_density(adjust = 1.5,
               alpha = .7) +
  geom_rug(size = .1, alpha = .3) +
  xlab("Height (cm)") +
  ylab("Estimated density")

## saving your ggplot
## you can click on "export", but I don't recommend it

## 1. using ggsave
## store your graph as an object
my_plot <- weight_df %>%
  ggplot(aes(x = height, fill = gender, col = gender)) +
  theme_bw() +
  geom_density(adjust = 1.5,
               alpha = .7) +
  geom_rug(size = .1, alpha = .3) +
  xlab("Height (cm)") +
  ylab("Estimated density") +
  theme(legend.position = "top")

## use the ggsave() function
ggsave(filename = "plot1.png",
       plot = my_plot,
       width = 5,
       height = 4,
       dpi = 800)

## 2. using png()
png(filename = "plot1_alt.png",
    width = 6,
    height = 4,
    res = 800,
    units = "in")
print(my_plot)
dev.off()


# Heatmaps ----------------------------------------------------------------

nott_df %>%
  ggplot(aes(x = year, y = month, fill = value)) +
  geom_tile()

nott_df %>%
  ggplot(aes(x = year, y = month, fill = value)) +
  geom_raster()

nott_df %>%
  ggplot(aes(x = year, y = month, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red")

months <- c("Jan","Feb","Mar","Apr","May","Jun",
            "Jul","Aug","Sep","Oct","Nov","Dec")

nott_df %>%
  ggplot(aes(x = year, y = month, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red",
                      name = "Temperature (°F)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "top") +
  xlab("Year") +
  ylab("Month") +
  scale_y_continuous(breaks = 1:12,
                     labels = months)


# Alluvial plots ----------------------------------------------------------

library(ggalluvial)

Titanic %>%
  as_tibble %>%
ggplot(aes(y = n,
           axis1 = Class, axis2 = Sex, axis3 = Age,
           fill = Survived)) +
  geom_alluvium() +
  scale_x_discrete(limits = c("Class", "Sex", "Age"))


# Geospatial maps ---------------------------------------------------------

map_data <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/local_authority_map_data.csv")
eu_ref <- read_csv("https://raw.githubusercontent.com/rafamoral/courses/main/data_visualisation/data/EU-referendum-result-data.csv")

## join the two datasets
ref_mapdata <- inner_join(map_data, eu_ref,
                          by = c("id" = "Area_Code"))

ref_mapdata %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon()

ref_mapdata %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(col = "white", size = .1)

ref_mapdata %>%
  ggplot(aes(x = long, y = lat,
             group = group,
             fill = Pct_Leave)) +
  geom_polygon(col = "white", size = .1)

## brewer = discrete
## distiller = continuous interpolation
## fermenter = binned

ref_mapdata %>%
  ggplot(aes(x = long, y = lat,
             group = group,
             fill = Pct_Leave)) +
  geom_polygon(col = "black", size = .1) +
  coord_equal() +
  theme_minimal() +
  scale_fill_distiller(type = "div", palette = "PuOr")

## find out more about colorblind safe / print safe / photocopy safe
## sequential/diverging/qualitative scales at colorbrewer2.org


# Fine tuning -------------------------------------------------------------

p1 <- weight_df %>%
  ggplot(aes(x = height, y = weight, col = gender)) +
  geom_point()

p1 + theme_gray()
p1 + theme_classic()
p1 + theme_dark()
p1 + theme_light()

library(ggthemes)

p1 + theme_wsj()
p1 + theme_economist()
p1 + theme_economist_white()
p1 + theme_stata()
p1 + theme_excel()
p1 + theme_excel_new()
p1 + theme_tufte() + geom_rangeframe()
p1 + theme_calc()
p1 + theme_clean()
p1 + theme_par()

ref_mapdata %>%
  ggplot(aes(x = long, y = lat,
             group = group,
             fill = Pct_Leave)) +
  geom_polygon(col = "black", size = .1) +
  coord_equal() +
  theme_map() +
  scale_fill_distiller(type = "div", palette = "PuOr",
                       name = "% Leave")

## set a default theme for your session
theme_set(theme_bw())

p1

## changing labels
p1 +
  xlab("Height (cm)") +
  ylab("Weight (kg)")

p1 +
  labs(x = "Height (cm)",
       y = "Weight (kg)",
       colour = "Gender")

## changing limits for axes
p1 +
  xlim(0, 200) +
  ylim(0, 200)

p1 + 
  lims(x = c(0, 200),
       y = c(0, 150))

## changing the colours
p1 +
  scale_color_manual(values = c("yellow","hotpink"))
p1 +
  scale_color_manual(values = c("chocolate","darkslategrey"))

## HTML colour names: https://www.w3schools.com/tags/ref_colornames.asp
## RGB color picker (google), pick a colour in hexadecimal

p1 +
  scale_color_manual(values = c("#8020DD","#41f072"))

## brewer palettes
p1 +
  scale_color_brewer(palette = "Accent")
p1 +
  scale_color_brewer(palette = "Spectral")

## changing boxplot labels
p2 <- weight_df %>%
  ggplot(aes(x = gender, y = height)) +
  geom_boxplot()

p2 +
  scale_x_discrete(label = c("Group A", "Group B"))


# Combining ggplot objects as panels within a figure ----------------------

library(ggpubr)

panel1 <- p1 +
  ggtitle("(a)") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5))

panel2 <- p2 +
  geom_jitter(aes(color = gender),
              alpha = .2) +
  ggtitle("(b)") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5))

ggarrange(panel1, panel2)

ggarrange(panel1, panel2,
          common.legend = TRUE,
          legend = "bottom")

ggarrange(panel1, panel2,
          common.legend = TRUE,
          legend = "right",
          ncol = 1)

final_plot <- ggarrange(panel1, panel2,
                        common.legend = TRUE,
                        legend = "bottom")

ggsave(filename = "figure1.png",
       plot = final_plot,
       dpi = 800,
       width = 10,
       height = 5)