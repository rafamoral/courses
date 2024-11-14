## Day 1: Data wrangling

*Level: From beginner to intermediate*

For data wrangling, we focus on tools provided by R's tidyverse. Data wrangling is the art of taking raw and messy data and formatting and cleaning it so that data analysis and visualization may be performed on it. Done poorly, it can be a time consuming, laborious, and error-prone. Fortunately, the tools provided by R's tidyverse allow us to do data wrangling in a fast, efficient, and high-level manner, which can have dramatic consequence for ease and speed with which we analyse data. We start with how to read data of different types into R, we then cover in detail all the dplyr tools such as select, filter, mutate, and others. Here, we will also cover the pipe operator (`%>%`) to create data wrangling pipelines that take raw messy data on the one end and return cleaned tidy data on the other. We then cover how to perform descriptive or summary statistics on our data using dplyr’s group_by and summarise functions. We then turn to combining and merging data. Here, we will consider how to concatenate data frames, including concatenating all data files in a folder, as well as cover the powerful SQL-like join operations that allow us to merge information in different data frames. The final topic we will consider is how to “pivot” data from a “wide” to “long” format and back using tidyr’s pivot_longer and pivot_wider functions.

**Topic 1:** Reading in data. We will begin by reading in data into R using tools such as `readr` and `readxl`. Almost all types of data can be read into R, and here we will consider many of the main types, such as csv, xlsx, sav, etc. Here, we will also consider how to control how data are parsed, e.g., so that they are read as dates, numbers, strings, etc.

**Topic 2:** Wrangling with dplyr. We will next cover the very powerful dplyr R package. This package supplies a number of so-called "verbs" — select, rename, slice, filter, mutate, arrange, etc. — each of which focuses on a key data manipulation tools, such as selecting or changing variables. All of these verbs can be chained together using "pipes" (represented by `%>%`). Together, these create powerful data wrangling pipelines that take raw data as input and return cleaned data as output. Here, we will also learn about the key concept of "tidy data", which is roughly where each row of a data frame is an observation and each column is a variable.

**Topic 3:** Summarizing data. The summarize and group_by tools in dplyr can be used with great effect to summarize data using descriptive statistics.

**Topic 4:** Merging and joining data frames. There are multiple ways to combine data frames, with the simplest being "bind" operations, which are effectively horizontal or vertical concatenations. Much more powerful are the SQL-like "join" operations. Here, we will consider the inner_join, left_join, right_join, full_join operations. In this section, we will also consider how to use purrr to read in and automatically merge large sets of files.

**Topic 5:** Pivoting data. Sometimes we need to change data frames from "long" to "wide" formats. The R package tidyr provides the tools pivot_longer and pivot_wider for doing this.

## Day 2: Data visualisation

*Level: From beginner to intermediate*

For visualisation, we focus on the ggplot2 package. We begin by providing a brief overview of the general principles data visualization, and an overview of the general principles behind ggplot. We then proceed to cover the major types of plots for visualizing distributions of univariate data: histograms, density plots, barplots, and Tukey boxplots. In all of these cases, we will consider how to visualize multiple distributions simultaneously on the same plot using different colours and "facet" plots. We then turn to the visualization of bivariate data using scatterplots. Here, we will explore how to apply linear and nonlinear smoothing functions to the data, how to add marginal histograms to the scatterplot, add labels to points, and scale each point by the value of a third variable. We then cover some additional plot types that are often related but not identical to those major types covered during the beginning of the course: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. We then consider more fine grained control of the plot by changing axis scales, axis labels, axis tick points, colour palettes, and ggplot "themes". Finally, we consider how to make plots for presentations and publications. Here, we will introduce how to insert plots into documents using RMarkdown, and also how to create labelled grids of subplots of the kind seen in many published articles.

**Topic 1:** What is data visualization. Data visualization is a means to explore and understand our data and should be a major part of any data analysis. Here, we briefly discuss why data visualization is so important and what the major principles behind it are.

**Topic 2:** Introducing ggplot. Though there are many options for visualization in R, ggplot is simply the best. Here, we briefly introduce the major principles behind how ggplot works, namely how it is a layered grammar of graphics.

**Topic 3:** Visualizing univariate data. Here, we cover a set of major tools for visualizing distributions over single variables: histograms, density plots, barplots, Tukey boxplots. In each case, we will explore how to plot multiple groups of data simultaneously using different colours and also using facet plots.

**Topic 4:** Scatterplots. Scatterplots and their variants are used to visualize bivariate data. Here, in addition to covering how to visualize multiple groups using colours and facets, we will also cover how to provide marginal plots on the scatterplots, labels to points, and how to obtain linear and nonlinear smoothing of the plots.

**Topic 5:** More plot types. Having already covered the most widely used general purpose plots, we now turn to cover a range of other major plot types: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. Each of these are important and widely used types of plots, and knowing them will expand your repertoire.

**Topic 6:** Fine control of plots. Thus far, we will have mostly used the default for the plot styles and layouts. Here, we will introduce how to modify things like the limits and scales on the axes, the positions and nature of the axis ticks, the colour palettes that are used, and the different types of ggplot themes that are available.

**Topic 7:** Plots for publications and presentations. Thus far, we have primarily focused on data visualization as a means of interactively exploring data. Often, however, we also want to present our plots in, for example, published articles or in slide presentations. It is simple to save a plot in different file formats, and then insert them into a document. However, a much more efficient way of doing this is to use RMarkdown to run the R code and automatically insert the resulting figure into a, for example, Word document, pdf document, html page, etc. In addition, here we will also cover how to make labelled grids of subplots like those found in many scientific articles.

## Day 3: Machine Learning

*Level: Intermediate (requires previous knowledge of R)*

We provide a practical and theoretical introduction to statistical machine learning using R.

**Topic 1:** Introductory concepts in statistical machine learning. Unsupervised vs. supervised learning. Useful plots in classification and clustering tasks. Unsupervised learning methods: hierarchical clustering and the k-means method.

**Topic 2:** Classification tasks. Supervised learning methods: logistic regression. Cross-validation techniques.

**Topic 3:** Tree-based methods. Classification and regression trees (CART), random forests. Extensions to tree-based methods, such as Boruta and Bayesian additive regression trees (BART).

**Topic 4:** Combining tree-based methods and neural networks within a regression framework through GAMLSS (generalized additive models for location, scale and shape).