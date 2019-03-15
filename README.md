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

# Usage

```
library(visa)
x <- NSpec.DF$N
s <- NSpec.DF$spectra
sr1 <- sr(s, 440, 445)

```


# Bug report

