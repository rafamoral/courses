## Advancing with R: An Introduction to Statistical Modelling

This course is designed to provide attendees with a comprehensive understanding of statistical modelling and its applications in various fields, such as ecology, biology, sociology, agriculture, and health. We cover all foundational aspects of modelling, including all coding aspects, ranging from data wrangling, visualisation and exploratory data analysis, to generalized linear mixed models, assessing goodness-of-fit and carrying out model comparison.

### Data wrangling

For data wrangling, we focus on tools provided by R's tidyverse. Data wrangling is the art of taking raw and messy data and formatting and cleaning it so that data analysis and visualization may be performed on it. Done poorly, it can be a time consuming, laborious, and error-prone. Fortunately, the tools provided by R's tidyverse allow us to do data wrangling in a fast, efficient, and high-level manner, which can have dramatic consequence for ease and speed with which we analyse data. We start with how to read data of different types into R, we then cover in detail all the dplyr tools such as select, filter, mutate, and others. Here, we will also cover the pipe operator (%>%) to create data wrangling pipelines that take raw messy data on the one end and return cleaned tidy data on the other. We then cover how to perform descriptive or summary statistics on our data using dplyr’s group_by and summarise functions. We then turn to combining and merging data. Here, we will consider how to concatenate data frames, including concatenating all data files in a folder, as well as cover the powerful SQL-like join operations that allow us to merge information in different data frames. The final topic we will consider is how to “pivot” data from a “wide” to “long” format and back using tidyr’s pivot_longer and pivot_wider functions.

### Data visualisation

For visualisation, we focus on the ggplot2 package. We begin by providing a brief overview of the general principles data visualization, and an overview of the general principles behind ggplot. We then proceed to cover the major types of plots for visualizing distributions of univariate data: histograms, density plots, barplots, and Tukey boxplots. In all of these cases, we will consider how to visualize multiple distributions simultaneously on the same plot using different colours and "facet" plots. We then turn to the visualization of bivariate data using scatterplots. Here, we will explore how to apply linear and nonlinear smoothing functions to the data, how to add marginal histograms to the scatterplot, add labels to points, and scale each point by the value of a third variable. We then cover some additional plot types that are often related but not identical to those major types covered during the beginning of the course: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. We then consider more fine grained control of the plot by changing axis scales, axis labels, axis tick points, colour palettes, and ggplot "themes". Finally, we consider how to make plots for presentations and publications. Here, we will introduce how to insert plots into documents using RMarkdown, and also how to create labelled grids of subplots of the kind seen in many published articles.

### Generalized linear models

Generalized linear models are generalizations of linear regression models for situations where the outcome variable is, for example, a binary, or ordinal, or count variable, etc. The specific models we cover include binary, binomial, and categorical logistic regression, Poisson and negative binomial regression for count variables, as well as extensions for overdispersed and zero-inflated data. We begin by providing a brief overview of the normal general linear model. Understanding this model is vital for the proper understanding of how it is generalized in generalized linear models. Next, we introduce the widely used binary logistic regression model, which is is a regression model for when the outcome variable is binary. Next, we cover the binomial logistic regression, and the multinomial case, which is for modelling outcomes variables that are polychotomous, i.e., have more than two categorically distinct values. We will then cover Poisson regression, which is widely used for modelling outcome variables that are counts (i.e the number of times something has happened). We then cover extensions to accommodate overdispersion, starting with the quasi-likelihood approach, then covering the negative binomial and beta-binomial models for counts and discrete proportions, respectively. Finally, we will cover zero-inflated Poisson and negative binomial models, which are for count data with excessive numbers of zero observations.

### Mixed models

We will focus primarily on multilevel linear models, but also cover multilevel generalized linear models. Likewise, we will also describe Bayesian approaches to multilevel modelling. We will begin by focusing on random effects multilevel models. These models make it clear how multilevel models are in fact models of models. In addition, random effects models serve as a solid basis for understanding mixed effects, i.e. fixed and random effects, models. In this coverage of random effects, we will also cover the important concepts of statistical shrinkage in the estimation of effects, as well as intraclass correlation. We then proceed to cover linear mixed effects models, particularly focusing on varying intercept and/or varying slopes regression models. We will then cover further aspects of linear mixed effects models, including multilevel models for nested and crossed data data, and group level predictor variables. Towards the end of the course we also cover generalized linear mixed models (GLMMs), how to accommodate overdispersion through individual-level random effects, as well as Bayesian approaches to multilevel levels using the brms R package.

### Model selection and model simplification

Throughout the course we consider the fundamental issue of how to measure model fit and a model’s predictive performance, and discuss a wide range of other major model fit measurement concepts like likelihood, log likelihood, deviance, and residual sums of squares. We thoroughly explore nested model comparison, particularly in general and generalized linear models, and their mixed effects counterparts. We discuss out-of-sample generalization, and introduce leave-one-out cross-validation and the Akaike Information Criterion (AIC). We also cover general concepts and methods related to variable selection, including stepwise regression, ridge regression, Lasso, and elastic nets. Finally, we turn to model averaging, which may represent a preferable alternative to model selection.

## Topics covered

_Day 1_

**Topic 1:** Reading in data. We will begin by reading in data into R using tools such as readr and readxl. Almost all types of data can be read into R, and here we will consider many of the main types, such as csv, xlsx, sav, etc. Here, we will also consider how to control how data are parsed, e.g., so that they are read as dates, numbers, strings, etc.

**Topic 2:** Wrangling with dplyr. We will next cover the very powerful dplyr R package. This package supplies a number of so-called "verbs" — select, rename, slice, filter, mutate, arrange, etc. — each of which focuses on a key data manipulation tools, such as selecting or changing variables. All of these verbs can be chained together using "pipes" (represented by %>%). Together, these create powerful data wrangling pipelines that take raw data as input and return cleaned data as output. Here, we will also learn about the key concept of "tidy data", which is roughly where each row of a data frame is an observation and each column is a variable.

**Topic 3:** Summarizing data. The summarize and group_by tools in dplyr can be used with great effect to summarize data using descriptive statistics.

**Topic 4:** Merging and joining data frames. There are multiple ways to combine data frames, with the simplest being "bind" operations, which are effectively horizontal or vertical concatenations. Much more powerful are the SQL-like "join" operations. Here, we will consider the inner_join, left_join, right_join, full_join operations. In this section, we will also consider how to use purrr to read in and automatically merge large sets of files.

**Topic 5:** Pivoting data. Sometimes we need to change data frames from "long" to "wide" formats. The R package tidyr provides the tools pivot_longer and pivot_wider for doing this.

_Day 2_

**Topic 1:** What is data visualization. Data visualization is a means to explore and understand our data and should be a major part of any data analysis. Here, we briefly discuss why data visualization is so important and what the major principles behind it are.

**Topic 2:** Introducing ggplot. Though there are many options for visualization in R, ggplot is simply the best. Here, we briefly introduce the major principles behind how ggplot works, namely how it is a layered grammar of graphics.

**Topic 3:** Visualizing univariate data. Here, we cover a set of major tools for visualizing distributions over single variables: histograms, density plots, barplots, Tukey boxplots. In each case, we will explore how to plot multiple groups of data simultaneously using different colours and also using facet plots.

**Topic 4:** Scatterplots. Scatterplots and their variants are used to visualize bivariate data. Here, in addition to covering how to visualize multiple groups using colours and facets, we will also cover how to provide marginal plots on the scatterplots, labels to points, and how to obtain linear and nonlinear smoothing of the plots.

**Topic 5:** More plot types. Having already covered the most widely used general purpose plots, we now turn to cover a range of other major plot types: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. Each of these are important and widely used types of plots, and knowing them will expand your repertoire.

**Topic 6:** Fine control of plots. Thus far, we will have mostly used the default for the plot styles and layouts. Here, we will introduce how to modify things like the limits and scales on the axes, the positions and nature of the axis ticks, the colour palettes that are used, and the different types of ggplot themes that are available.

**Topic 7:** Plots for publications and presentations. Thus far, we have primarily focused on data visualization as a means of interactively exploring data. Often, however, we also want to present our plots in, for example, published articles or in slide presentations. It is simple to save a plot in different file formats, and then insert them into a document. However, a much more efficient way of doing this is to use RMarkdown to run the R code and automatically insert the resulting figure into a, for example, Word document, pdf document, html page, etc. In addition, here we will also cover how to make labelled grids of subplots like those found in many scientific articles.

_Day 3_

**Topic 1:** The general linear model. We begin by providing an overview of the normal, as in normal distribution, general linear model, including using categorical predictor variables. Although this model is not the focus of the course, it is the foundation on which generalized linear models are based and so must be understood to understand generalized linear models.

**Topic 2:** Binary logistic regression. Our first generalized linear model is the binary logistic regression model, for use when modelling binary outcome data. We will present the assumed theoretical model behind logistic regression, implement it using R’s glm, and then show how to interpret its results, perform predictions, and (nested) model comparisons.

**Topic 3:** Binomial logistic regression. Here, we show how the binary logistic regression can be extended to deal with data on discrete proportions. We will also present alternative link functions to the logit, such as the probit and complementary log-log links.

**Topic 4:** Categorical logistic regression. Categorical logistic regression, also known as multinomial logistic regression, is for modelling polychotomous data, i.e. data taking more than two categorically distinct values. Categorical logistic regression is based on an extension of the binary logistic regression case.

**Topic 5:** Poisson regression. Poisson regression is a widely used technique for modelling count data, i.e., data where the variable denotes the number of times an event has occurred.

_Day 4_

**Topic 1:** Measuring model fit. Here, the concept of conditional probability of the observed data, or of future data, is of vital importance. This is intimately related, though distinct, to concept of likelihood and the likelihood function, which is in turn related to the concept of the log likelihood or deviance of a model. Here, we also show how these concepts are related to concepts of residual sums of squares, root mean square error (rmse), and deviance residuals.

**Topic 2:** Nested model comparison. In this section, we cover how to do nested model comparison in general linear models, generalized linear models, and their mixed effects (multilevel) counterparts. First, we precisely define what is meant by a nested model. Then we show how nested model comparison can be accomplished in general linear models with F tests, which we will also discuss in relation to R^2 and adjusted R^2. In generalized linear models, we can accomplish nested model comparison using deviance based chi-square tests via Wilks’s theorem.

**Topic 3:** Overdispersion models. The quasi-likelihood approach for both the Poisson and binomial models. Negative binomial regression. The negative binomial model is, like the Poisson regression model, used for unbounded count data, but it is less restrictive than Poisson regression, specifically by dealing with overdispersed data. Beta-binomial regression. The beta-binomial model is an overdispersed alternative to the binomial.

**Topic 4:** Zero inflated models. Zero inflated count data is where there are excessive numbers of zero counts that can be modelled using either a Poisson or negative binomial model. Zero inflated Poisson or negative binomial models are types of latent variable models.

**Topic 5:** Random effects models. The defining feature of multilevel models is that they are models of models. We begin by using a binomial random effects model to illustrate this. Specifically, we show how multilevel models are models of the variability in models of different clusters or groups of data.

**Topic 6:** Normal random effects models. Normal, as in normal distribution, random effects models are the key to understanding the more general and widely used linear mixed effects models. Here, we also cover the key concepts of statistical shrinkage and intraclass correlation.

_Day 5_

**Topic 1:** Out of sample predictive performance: cross validation and information criteria. Here, we describe how to measure out of sample predictive performance, which measures how well a model can generalize to new data. This is arguably the gold-standard for evaluating any statistical models. A practical means to measure out of sample predictive performance is cross-validation, especially leave-one-out cross-validation. Leave-one-out cross-validation can, in relatively simple models, be approximated by Akaike Information Criterion (AIC), which can be exceptionally simple to calculate. We will discuss how to interpret AIC values, and describe other related information criteria, some of which will be used in more detail in later sections.

**Topic 2:** Linear mixed effects models. Next, we turn to multilevel linear models, also known as linear mixed effects models. We specifically deal with the cases of varying intercept and/or varying slope linear regression models.

**Topic 3:** Multilevel models for nested data. Here, we will consider multilevel linear models for nested, as in groups of groups, data. As an example, we will look at multilevel linear models applied to data from students within classes that are themselves within different schools, and where we model the variability of effects across the classes and across the schools.

**Topic 4:** Multilevel models for crossed data. In some multilevel models, each observation occurs in multiple groups, but these groups are not nested. For example, animals may be members of different species and in different locations, but the species are not subsets of locations, nor vice versa. These are known as crossed or multiclass data structures.

**Topic 5:** Group level predictors. In some multilevel regression models, predictor variable are sometimes associated with individuals, and sometimes associated with their groups. In this section, we consider how to handle these two situations.

**Topic 6:** Generalized linear mixed models (GLMMs). Here, we extend the linear mixed model to the exponential family of distributions and showcase an example using the Poisson GLMM. We also cover how to accommodate overdispersion through individual-level random effects.

**Topic 7:** Bayesian multilevel models. All of the models that we have considered can be handled, often more easily, using Bayesian models. Here, we provide an brief introduction to Bayesian models and how to perform examples of the models that we have considered using Bayesian methods and the brms R package.

**Topic 8:** Variable selection. Variable selection is a type of nested model comparison. It is also one of the most widely used model selection methods, and variable selection of some kind is almost always done routinely in all data analysis. In particular, we cover stepwise regression (and its limitations), all subsets methods, ridge regression, Lasso, and elastic nets.

**Topic 9:** Model averaging. Rather than selecting one model from a set of candidates, it is arguably always better perform model averaging, using all the candidates models, weighted by the predictive performance. We show how to perform model average using information criteria.
