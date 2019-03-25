[![Travis build status](https://travis-ci.org/kang-yu/visa.svg?branch=master)](https://travis-ci.org/kang-yu/visa)
[![Build status](https://ci.appveyor.com/api/projects/status/8rxdgcr4ro8ga0s4?svg=true)](https://ci.appveyor.com/project/kang-yu/visa)
[![codecov](https://codecov.io/gh/kang-yu/visa/branch/master/graph/badge.svg)](https://codecov.io/gh/kang-yu/visa)
[![HitCount](http://hits.dwyl.io/kang-yu/visa.svg)](http://hits.dwyl.io/kang-yu/visa)

# visa
This R package is to facilitate Vegetation Imaging Spectroscopy Analysis (VISA). For more details of this package, you can  `browseVignettes("visa")`. 


## Installation

### install the released version from CRAN:

``` r
# install.packages() # does not work currently
``` 

You might encounter problem of package dependencies, such as some functions depend on `ggplot2`, `ggpmisc`. In such cases, you would have to install the all the dependencies.


### install the dev-version from GitHub:

``` r
if (!require("devtools")) install.packages("devtools")
# devtools::install_github("kang-yu/visa") # install without vignettes
devtools::install_github("kang-yu/visa", build_opts = c("--no-resave-data", "--no-manual")
``` 

You might encounter [this problem of install_github()](https://github.com/r-lib/devtools/issues/1978), and in this case, you could try update the {remotes} `devtools::install_github("r-lib/remotes")`.


## Usage

```
library(visa)
data(NSpec.DF)
x <- NSpec.DF$N # nitrogen
S <- NSpec.DF$spectra[, seq(1, ncol(NSpec.DF$spectra), 10)] # resampled to 10 nm steps
cm.nsr(S, x, cm.plot = TRUE)
```

## Bug report [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/kang-yu/visa/issues)

You can report your [issues of using visa here](https://github.com/kang-yu/visa/issues). Don't know how to report a bug? Check RStudio's Instructions on [Writing Good Bug Reports](https://github.com/rstudio/rstudio/wiki/Writing-Good-Bug-Reports).

You might find useful information by checking the [issues reported by others](https://github.com/kang-yu/visa/issues?q=is%3Aissue).

