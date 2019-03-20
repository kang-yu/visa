[![Travis build status](https://travis-ci.org/kang-yu/visa.svg?branch=master)](https://travis-ci.org/kang-yu/visa)

# visa
This R package is to facilitate Vegetation Imaging Spectroscopy Analysis (VISA). For more details of this package, you can  `browseVignettes("visa")`. 


# Installation

install the released version from [CRAN]

``` r
# install.packages() # does not work currently
``` 

You might encounter problem of package dependencies, such as some functions depend on `ggplot`, `ggpmisc`. In such cases, you would have to install the all the dependencies.


install the dev-version of visa from github with:

``` r
# devtools::install_github(build_vignettes = TRUE) # to also install vignettes and suggested packages
# install.packages("devtools")
devtools::install_github("kang-yu/visa")
``` 
You might encounter [this problem of install_github()](https://github.com/r-lib/devtools/issues/1978), and in this case, you could try update the {remotes} `devtools::install_github("r-lib/remotes")`.


# Usage

```
library(visa)
x <- NSpec.DF$N
s <- NSpec.DF$spectra
sr1 <- sr(s, 440, 445)
plot(sr1)
```

# Bug report

You can report your [issues of using visa here](https://github.com/kang-yu/visa/issues).

RStudio's Instructions on [Writing Good Bug Reports](https://github.com/rstudio/rstudio/wiki/Writing-Good-Bug-Reports).

