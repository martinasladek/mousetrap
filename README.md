
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mousetRap

<!-- badges: start -->
<!-- badges: end -->

## Installation

Install the development version of `mousetRap` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("martinasladek/mousetrap")
```

## Shiny App

To install the `shiny` R package for checking `mousetRap` output, visit
the [GitHub page for
`shinymousetRap`](https://github.com/martinasladek/shinymousetrap), or
run:

``` r
if(!require(mousetrap)){
  devtools::install_github("martinasladek/mousetrap")
}

devtools::install_github("martinasladek/shinymousetrap")
```

## Example

Loading `mousetRap`:

``` r
library(mousetrap)
```

To generate data, run:

``` r
mousetrap::get_data(cand_no = 12345)
#> # A tibble: 47 × 4
#>    ID     Condition response Reward
#>    <chr>  <fct>        <dbl>  <dbl>
#>  1 F46VIU 3               21    160
#>  2 6N7J4A 0              288     48
#>  3 Q0D6R3 2               38     34
#>  4 2H49L6 0              384     64
#>  5 918Q0J 3               56     35
#>  6 4CEXQF 0              336     56
#>  7 UYSOFW 0              294     49
#>  8 9FNZGD 3              175     44
#>  9 K3J59A 2               43     40
#> 10 RBZ7DH 0              240     40
#> # … with 37 more rows
```
