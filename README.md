
# spicy <a href="https://amaltawfik.github.io/spicy/"><img src="man/figures/logo.png" align="left" height="139" alt="spicy website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/amaltawfik/spicy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/amaltawfik/spicy/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
<!-- badges: end -->

spicy adds a dash of heat to data analysis, giving insights a whole new
flavour! It is designed to make variable exploration and descriptive
statistics fast, expressive, and easy to use.

## What is spicy?

spicy is an R package for quick, consistent, and elegant exploration of
data frames. It helps you:

- Extract variable metadata and display compact summaries of dataset
  variables, including names, labels, values, classes, number of
  distinct non-missing values, number of valid observations, number of
  missing observations (`varlist()`). Similar to the “Variable View” in
  SPSS or the “Variables Manager” in Stata.
- Compute frequency tables (`freq()`), row means (`mean_n()`), row sums
  (`sum_n()`), and row count of specific values (`count_n()`) with
  automatic handling of missing data.
- Explore relationships between variables via cross-tabulations
  (`cross_tab()`), and Cramer’s V for categorical associations
  (`cramer_v()`).
- Copy data or results directly to the clipboard (`copy_clipboard()`)
  for quick export.
- Handle `labelled`, `factor`, `Date`, `POSIXct`, and other common
  types.

All with intuitive functions that produce clean, structured outputs.

------------------------------------------------------------------------

## Installation

You can install the development version of spicy from GitHub with:

``` r
# install.packages("pak")
pak::pak("amaltawfik/spicy")
```

------------------------------------------------------------------------

## Example usage

Here are some quick examples using built-in datasets:

``` r
library(spicy)

# Get a summary of all variables
varlist(iris, tbl = TRUE)

# Tabulate frequencies
freq(iris$Species)

# Cross-tab with row percentages
cross_tab(mtcars, cyl, gear, percent = "row")

# Compute row-wise mean/sum (all values must be valid by default)
df <- data.frame(
      var1 = c(10, NA, 30, 40, 50),
      var2 = c(5, NA, 15, NA, 25),
      var3 = c(NA, 30, 20, 50, 10)
      )
mean_n(df)
sum_n(df)
```

> All functions can be directly used in pipelines.

------------------------------------------------------------------------

## Why use `spicy`?

- Clean, expressive output  
- Works well with labelled survey data  
- Handles weights, percentages, NA counts  
- Great for exploring data and variables, teaching, or reporting

------------------------------------------------------------------------

## Citation

If you use `spicy` in a publication or teaching material, please cite it
as:

> Tawfik, A. (2025). *spicy: Tools for Data Management and Variable
> Exploration*. R package version 0.1.0.
> <https://github.com/amaltawfik/spicy>

You can also get the citation in R format by typing:

``` r
citation("spicy")
```

------------------------------------------------------------------------

## License

This package is licensed under the MIT license. See [`LICENSE`](LICENSE)
for details.
