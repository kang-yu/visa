# visa
This R package is to facilitate Vegetation Imaging Spectroscopy Analysis (VISA). For more details of this package, you can  `browseVignettes("visa")`. 


# Installation

install the released version from [CRAN]

``` r
# install.packages() # does not work currently
``` 

install the dev-version of visa from github with:

``` r
# install.packages("devtools")
devtools::install_github("kang-yu/visa")
``` 

You might encounter problem of package dependencies, such as some functions depend on `ggplot`, `ggpmisc`. In such cases, you would have to install the all the dependencies.

# Usage

```
library(visa)
x <- NSpec.DF$N
s <- NSpec.DF$spectra
sr1 <- sr(s, 440, 445)
plot(sr1)
```

# Bug report

RStudio's Instructions on [Writing Good Bug Reports](https://github.com/rstudio/rstudio/wiki/Writing-Good-Bug-Reports)
