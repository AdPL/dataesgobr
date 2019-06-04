
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataesgobr

This package intended to work with the [government’s
API](https://datos.gob.es) that allow you to download, analyze and
generate information through datasets obtained from the API.

## Installation

You can install it using [remotes](https://github.com/r-lib/remotes)
package.

``` r
# install.packages("remotes")
remotes::install_github("adpl/dataesgobr")
```

## Example

Dataesgobr package provides a web GUI able to download, load, visualize
an generate information using the Government’s Spain API. You can launch
it using the R console:

``` r
library(dataesgobr)
runGUI()
```

It is possible to use the package in the R console too, for example
obtaining datasets and loading their information:

``` r
library(dataesgobr)
datasets <- search_by_title("consumo")
dataset <- load_dataset(datasets, 1)
print(dataset)
```

## Information

Dataesgobr package is in development, so you can install, execute and
test it, but it is very probable that you can find bugs while you are
using it.
