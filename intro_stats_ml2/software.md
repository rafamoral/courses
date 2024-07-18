The required software for this workshop is all free and open source
and will run identically on Windows, Mac OS X, and Linux platforms.

There are a few pieces of software to install:

-   [R](https://www.r-project.org/): An environment for statistical
    computing.
-   [Rstudio](https://www.rstudio.com/): An integrated development
    environment for using R.
-   [tidyverse](https://www.tidyverse.org/): A bundle of R packages to
    use R the modern way.
-   Various R packages including `gamlss`, `glmnet`, etc
-   [Stan](http://mc-stan.org/): A Bayesian probabilistic modelling
    language. This will be used briefly when discussing including BART within a mixed-effects modelling framework.
    
All of the above installation should be easy and painless except
possibly for the installation of [Stan](http://mc-stan.org/), which can
possibly be tricky because it is an external program and requires
addition programming tools like c++ libraries and compilers etc.
However, in the instructions below there are links to pages that provide
ample detail on how to install and test [Stan](http://mc-stan.org/) and
all its dependencies.


## Installing R

Go to the [R](https://www.r-project.org/) website and follow the links
for downloading. On Windows, this should lead you to

-   <https://cran.r-project.org/bin/windows/base/>.

Downloading this and following the usual Windows installation process,
you\'ll then have a full working version of R.

On Macs, the installation procedure is essentially identical. The latest
Mac installer should be available at

-   <https://cran.r-project.org/bin/macosx/>.

Download this and follow the usual Mac installation process to get a
full working version of R for Macs.

## Installing Rstudio

Using Rstudio is not strictly necessary. You can do all you need to do
with R without using Rstudio. However, many people have found that using
R is more convenient and pleasant when working through Rstudio. To
install it, go to the [Rstudio](https://www.rstudio.com/) website,
specifically to

-   <https://www.rstudio.com/products/rstudio/download/>

which will list all the available installers. Note that you just want
the Rstudio *desktop* program. The Rstudio *server* is something else
(basically it is for providing remote access to Rstudio hosted on Linux
servers).

Again, you\'ll just follow the usual installation process for Windows or
Macs to install Rstudio using these installers.

## Installing the tidyverse packages

The so-called [tidyverse](https://www.tidyverse.org/) is a collection of
interrelated R packages that implement essentially a new standard
library for R. In other words, the
[tidyverse](https://www.tidyverse.org/) gives us a bundle tools for
doing commonplace data manipulation and visualization and programming.
It represents the modern way to use R, and in my opinion, it\'s the best
way to use R. All the [tidyverse](https://www.tidyverse.org/) packages
can be installed by typing the following command in R:

```r
install.packages("tidyverse")
```

The main packages that are contained within the
[tidyverse](https://www.tidyverse.org/) bundle are listed
[here](https://www.tidyverse.org/packages/).

## Installing various R packages

We need a number of extra R packages, which can be installed individually via the R package installer dialog, or else with the following commands:
```r
install.packages(c("ISLR", "glmnet", "GGally", "BiocManager", "dendextend", "splines", "class", "caret", "arm", "MLeval", "pROC", "gclus", "seriation", "DendSer", "leaps", "gridExtra", "gam", "rpart", "rpart.plot", "kernlab", "randomForest", "hnp", "dbarts", "gamlss", "gamlss.add", "Boruta", "lme4"))

BiocManager::install("graph")

install.packages("PairViz")

install.packages("devtools")
devtools::install_github("andrew-mcinerney/selectnn")
devtools::install_github("andrew-mcinerney/interpretnn")
```

## Installing Stan

Stan is a probabilistic programming language. Using the Stan language,
you can define arbitrary probabilistic models and then perform Bayesian
inference on them using
[MCMC](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo),
specifically using [Hamiltonian Monte
Carlo](https://en.wikipedia.org/wiki/Hamiltonian_Monte_Carlo).

In general, Stan is a external program to R; it does not need to be used
with R. However, one of the most common ways of using Stan is by using
it through R and that is what we will be doing in this workshop.

To use Stan with R, you need to install an R package called
[rstan](http://mc-stan.org/users/interfaces/rstan). However, you also
need additional external tools installed in order for
[rstan](http://mc-stan.org/users/interfaces/rstan) to work.

Instructions for installing rstan on can be found here:

- <https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>

Specific instructions for different platforms can be found by following links from this page.

## Installing stan4bart

If the installation of R, Rstudio and Stan seemed to go fine, you can
get the `stan4bart` R package, which allows for the inclusion of BART
terms within a mixed modelling framework.

To install it, run

``` {.R}
install.packages("stan4bart")
```

You can test that it worked by running the following code.

```r
library(stan4bart)

example(stan4bart)
```
