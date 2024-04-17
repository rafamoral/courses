## Introduction to Regression Modelling in Immunology

This course provides a comprehensive practical and theoretical introduction to regression modelling using R.

## Topics Covered

**Topic 1:** The normal linear model. We begin by providing an overview of the normal, as in normal distribution, general linear model. This is the foundation on which extended approaches are based.

**Topic 2:** Generalized linear models. We provide an introduction to the history of GLMs and the foundational aspects of this class of models. We implement them using R, and then show how to interpret the results, and carry out model comparisons.

**Topic 3:** Generalized additive models. We introduce the use of (penalised) smooth functions to accommodate nonlinear relationships. We show how to fit these types of models using `mgcv` and how to evaluate goodness-of-fit.

**Topic 4:** Distributional regression and generalized additive models for location, scale and shape (GAMLSS). We show how to relax assumptions on the variance (or dispersion) parameter of a distribution, allowing it to be modelled with covariates using `gamlss`. We also introduce the use of machine learning approaches, such as regression trees, within a parametric regression context.

**Topic 5:** Mixed models. The last topic we cover is the introduction of random effects to accommodate dependencies in the data. We introduce the general theory behind mixed models, how to fit and carry out inference, and implement them using `lme4`.