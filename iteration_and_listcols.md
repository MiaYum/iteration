Iteration and listcols
================

Load key packages.

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

Set seed for reproducibility.

``` r
set.seed(1235)
```

### Lists

``` r
vec_numeric = 1:4
vec_char = c("my", "name", "is", "jeff")

tibble(
  num = vec_numeric,
  char = vec_char
)
```

    ## # A tibble: 4 × 2
    ##     num char 
    ##   <int> <chr>
    ## 1     1 my   
    ## 2     2 name 
    ## 3     3 is   
    ## 4     4 jeff

Different stuff with different lengths

``` r
l = 
  list(
    vec_numeric = 1:5,
    vec_char = LETTERS,
    matrix = matrix(1:10, nrow = 5, ncol = 2),
    summary = summary(rnorm(100))
  )
```

Accessing lists

``` r
l$vec_char
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[[2]]
```

    ##  [1] "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
    ## [20] "T" "U" "V" "W" "X" "Y" "Z"

``` r
l[["summary"]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.49510 -0.50677  0.13938  0.07803  0.60054  3.34618

### loops

``` r
list_norm_samples = 
  list(
    a = rnorm(20, 1, 5),
    b = rnorm(20, 0, 7),
    c = rnorm(20, 20, 1),
    d = rnorm(20, -45, 13)
  )
```

mean and sd function

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at least 2 numbers to get z scores")
  }

  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

``` r
mean_and_sd(list_norm_samples$a)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.50  5.53

``` r
mean_and_sd(list_norm_samples$b)
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0234  8.00

``` r
mean_and_sd(list_norm_samples$c)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.1  1.12

``` r
mean_and_sd(list_norm_samples$d)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -49.7  16.3

``` r
output_mean_sd = vector("list", length = 4)
output_median = vector("list", length = 4)

for (i in 1:4) {
  
  output_mean_sd[[i]] = mean_and_sd(list_norm_samples[[i]])
  output_median[[i]] = median(list_norm_samples[[i]])
  
}
```

### use `map`

``` r
output_mean_sd = map(list_norm_samples, mean_and_sd)
output_summary = map(list_norm_samples, summary)
```

### create DF

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm_samples
  )
```

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.50  5.53

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0234  8.00

``` r
mean_and_sd(listcol_df$samp[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.1  1.12

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.50  5.53
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0234  8.00
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.1  1.12
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -49.7  16.3

``` r
listcol_df |> 
  mutate(
    mean_sd = map(samp, mean_and_sd),
    median =  map(samp, median)) |> 
  select(name, mean_sd) |> 
  unnest(mean_sd)
```

    ## # A tibble: 4 × 3
    ##   name      mean    sd
    ##   <chr>    <dbl> <dbl>
    ## 1 a       1.50    5.53
    ## 2 b       0.0234  8.00
    ## 3 c      20.1     1.12
    ## 4 d     -49.7    16.3

### NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

Import function.

``` r
nsduh_import = function(html, table_number, outcome_name){
  
  html |> 
  html_table() |> 
  nth(table_number) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent),
    outcome = outcome_name) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
}

nsduh_import(nsduh_html, 1, "marj")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    9.98 marj   
    ##  2 Alabama 12+   2014-2015    9.6  marj   
    ##  3 Alabama 12-17 2013-2014    9.9  marj   
    ##  4 Alabama 12-17 2014-2015    9.71 marj   
    ##  5 Alabama 18-25 2013-2014   27.0  marj   
    ##  6 Alabama 18-25 2014-2015   26.1  marj   
    ##  7 Alabama 26+   2013-2014    7.1  marj   
    ##  8 Alabama 26+   2014-2015    6.81 marj   
    ##  9 Alabama 18+   2013-2014    9.99 marj   
    ## 10 Alabama 18+   2014-2015    9.59 marj   
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, 4, "cocaine")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    1.23 cocaine
    ##  2 Alabama 12+   2014-2015    1.22 cocaine
    ##  3 Alabama 12-17 2013-2014    0.42 cocaine
    ##  4 Alabama 12-17 2014-2015    0.41 cocaine
    ##  5 Alabama 18-25 2013-2014    3.09 cocaine
    ##  6 Alabama 18-25 2014-2015    3.2  cocaine
    ##  7 Alabama 26+   2013-2014    1.01 cocaine
    ##  8 Alabama 26+   2014-2015    0.99 cocaine
    ##  9 Alabama 18+   2013-2014    1.31 cocaine
    ## 10 Alabama 18+   2014-2015    1.31 cocaine
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html, 5, "heroin")
```

    ## # A tibble: 510 × 5
    ##    State   age   year      percent outcome
    ##    <chr>   <chr> <chr>       <dbl> <chr>  
    ##  1 Alabama 12+   2013-2014    0.22 heroin 
    ##  2 Alabama 12+   2014-2015    0.27 heroin 
    ##  3 Alabama 12-17 2013-2014    0.1  heroin 
    ##  4 Alabama 12-17 2014-2015    0.08 heroin 
    ##  5 Alabama 18-25 2013-2014    0.45 heroin 
    ##  6 Alabama 18-25 2014-2015    0.64 heroin 
    ##  7 Alabama 26+   2013-2014    0.19 heroin 
    ##  8 Alabama 26+   2014-2015    0.23 heroin 
    ##  9 Alabama 18+   2013-2014    0.23 heroin 
    ## 10 Alabama 18+   2014-2015    0.29 heroin 
    ## # ℹ 500 more rows

import data using a for loop.

``` r
table_input = list(1, 4, 5)
name_input = list("marj", "cocaine", "heroin")

output = vector("list", length = 3)

for (i in 1:3) {
  
  output[[i]] = nsduh_import(nsduh_html, table_input[[i]], name_input[[i]])
  
}

nsduh_df = bind_rows(output)
```
