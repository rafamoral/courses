## Introduction to Mixed Models using R

This course provides a comprehensive practical and theoretical introduction to multilevel models, also known as hierarchical or mixed effects models. We will focus primarily on multilevel linear models, but also cover multilevel generalized linear models. Likewise, we will also describe Bayesian approaches to multilevel modelling. We will begin by focusing on random effects multilevel models. These models make it clear how multilevel models are in fact models of models. In addition, random effects models serve as a solid basis for understanding mixed effects, i.e. fixed and random effects, models. In this coverage of random effects, we will also cover the important concepts of statistical shrinkage in the estimation of effects, as well as intraclass correlation. We then proceed to cover linear mixed effects models, particularly focusing on varying intercept and/or varying slopes regression models. We will then cover further aspects of linear mixed effects models, including multilevel models for nested and crossed data data, and group level predictor variables. Towards the end of the course we also cover generalized linear mixed models (GLMMs), how to accommodate overdispersion through individual-level random effects, as well as Bayesian approaches to multilevel levels using the `brms` R package.

## Topics Covered

**Topic 1:** Random effects models. The defining feature of multilevel models is that they are models of models. We begin by using a binomial random effects model to illustrate this. Specifically, we show how multilevel models are models of the variability in models of different clusters or groups of data.

**Topic 2:** Normal random effects models. Normal, as in normal distribution, random effects models are the key to understanding the more general and widely used linear mixed effects models. Here, we also cover the key concepts of statistical shrinkage and intraclass correlation.

**Topic 3:** Linear mixed effects models. Next, we turn to multilevel linear models, also known as linear mixed effects models. We specifically deal with the cases of varying intercept and/or varying slope linear regression models.

**Topic 4:** Multilevel models for nested data. Here, we will consider multilevel linear models for nested, as in groups of groups, data. As an example, we will look at multilevel linear models applied to data from students within classes that are themselves within different schools, and where we model the variability of effects across the classes and across the schools.

**Topic 5:** Multilevel models for crossed data. In some multilevel models, each observation occurs in multiple groups, but these groups are not nested. For example, animals may be members of different species and in different locations, but the species are not subsets of locations, nor vice versa. These are known as crossed or multiclass data structures.

**Topic 6:** Group level predictors. In some multilevel regression models, predictor variable are sometimes associated with individuals, and sometimes associated with their groups. In this section, we consider how to handle these two situations.

**Topic 7:** Generalized linear mixed models (GLMMs). Here, we extend the linear mixed model to the exponential family of distributions and showcase an example using the Poisson GLMM. We also cover how to accommodate overdispersion through individual-level random effects.

**Topic 8:** Bayesian multilevel models. All of the models that we have considered can be handled, often more easily, using Bayesian models. Here, we provide an brief introduction to Bayesian models and how to perform examples of the models that we have considered using Bayesian methods and the `brms` R package.