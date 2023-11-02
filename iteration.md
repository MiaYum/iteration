Writing Functions
================

Load key packages.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

Set seed for reproducibility.

``` r
set.seed(1235)
```

### Z score function

Z scores subtract the mean and divide by the sd.

``` r
x_vec = rnorm(20, mean = 5, sd = .3)
```

Compute Z scores for `x_vec`.

``` r
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.94728322 -1.65396104  1.08526738  0.02779827  0.03072659  1.93809547
    ##  [7] -0.04918364  0.68175896  1.53721663  0.37796324 -1.23914425  1.19966803
    ## [13] -1.01010280 -0.61458088 -1.09802904  0.39015817  0.63170844 -0.65341058
    ## [19] -0.90903975  0.27437403

Write a function to do this!

``` r
z_score = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }
  
  z = (x - mean(x)) / sd(x) 
  # if there use a varible don't exist in input, then R will go to gobal enviroument to find
  z
}
```

Check that this works.

``` r
z_score(x = x_vec)
```

    ##  [1] -0.94728322 -1.65396104  1.08526738  0.02779827  0.03072659  1.93809547
    ##  [7] -0.04918364  0.68175896  1.53721663  0.37796324 -1.23914425  1.19966803
    ## [13] -1.01010280 -0.61458088 -1.09802904  0.39015817  0.63170844 -0.65341058
    ## [19] -0.90903975  0.27437403

``` r
z_score(x = rnorm(10, mean = 5))
```

    ##  [1]  0.2860333 -1.9417125  1.6376085 -0.2455081  0.9068033 -0.1007777
    ##  [7] -0.3066805  0.2710474 -1.0526265  0.5458128

Keep checking.

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): You need at least 2 numbers to get z scores

``` r
z_score(c("my", "name", "is", "jeff"))
```

    ## Error in z_score(c("my", "name", "is", "jeff")): Argument should be numbers

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Argument should be numbers

``` r
z_score(iris)
```

    ## Error in z_score(iris): Argument should be numbers
