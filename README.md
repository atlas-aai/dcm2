
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcm2

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R package
version](https://www.r-pkg.org/badges/version/dcm2)](https://cran.r-project.org/package=dcm2)
[![Package
downloads](https://cranlogs.r-pkg.org/badges/grand-total/dcm2)](https://cran.r-project.org/package=dcm2)
[![R-CMD-check](https://github.com/atlas-aai/dcm2/workflows/R-CMD-check/badge.svg)](https://github.com/atlas-aai/dcm2/actions)</br>
[![Codecov test
coverage](https://codecov.io/gh/atlas-aai/dcm2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/atlas-aai/dcm2?branch=main)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/b82caf01-0611-4f8b-bbca-5b89b5a80791/deploy-status)](https://app.netlify.com/sites/dcm2/deploys)
[![Signed
by](https://img.shields.io/badge/Keybase-Verified-brightgreen.svg)](https://keybase.io/jeffreychoover)
![License](https://img.shields.io/badge/License-GPL_v3-blue.svg)
<!-- badges: end -->

The goal of dcm2 is to provide a generic function for calculating the
$M_2$ statistic described by [Liu et
al. (2016)](https://doi.org/10.3102/1076998615621293). The package
provides native support for models estimated with GDINA, but package
authors can create methods for different classes of models.

## Installation

You can install the release version of dcm2 from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("dcm2")
```

To install the development version from [GitHub](https://github.com/)
use:

``` r
# install.packages("remotes")
remotes::install_github("atlas-aai/dcm2")
```

## Usage

Once dcm2 has been installed, we can estimate a DCM and apply the $M_2$
statistic to estimate the evidence of model fit.

``` r
library(dcm2)
library(tidyverse)
library(GDINA)
```

We included simulated data in the dcm2 package to demonstrate how the
package can be used. We can load in this data using:

``` r
fullData <- dcm2::sample_data
q_matrix <- fullData$q_matrix
data <- fullData$data
```

Then, we want to estimate a DCM to fit this data. We will use the GDINA
package to estimate a log-linear cognitive diagnosis model \[LCDM;
@henson_2009\]. However, we need to format the data prior to fitting the
LCDM.

``` r
fit_dat <- data %>%
             tidyr::pivot_wider(names_from = "item_id",
                                values_from = "score") %>%
             dplyr::select(-"resp_id") %>%
             as.matrix() %>%
             unname()
```

Now that the data is formatted, we can fit the model using:

``` r
gdina_mod <- GDINA::GDINA(dat = fit_dat,
                          Q = data.frame(q_matrix),
                          model = "logitGDINA",
                          control = list(conv.type = "neg2LL"))
```

Finally, we use the `fit_m2()` function to estimate the model fit using
the $M_2$ fit statistic for the model estimated from the GDINA package.
This function produces a tibble with all of the output for the $M_2$ fit
statistic.

``` r
fit_m2(gdina_mod, ci = 0.9)
```

Contributions are welcome. To ensure a smooth process, please review the
[Contributing Guide](https://dcm2.info/dev/CONTRIBUTING.html). Please
note that the dcm2 project is released with a [Contributor Code of
Conduct](https://dcm2.info/CODE_OF_CONDUCT.html). By contributing to
this project, you agree to abide by its terms.
