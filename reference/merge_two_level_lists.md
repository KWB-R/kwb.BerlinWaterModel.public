# Merge nested two-level lists while preserving structure

This function merges two nested lists of the form
`list(top_id = list(sub_id = tibble))`, as used in the BerlinWaterModel
package. The structure (top-level and sub-level keys) is preserved, and
missing elements are added. If the same element exists in both lists,
the preferred side can be chosen.

## Usage

``` r
merge_two_level_lists(x, y, prefer = c("left", "right"))
```

## Arguments

- x:

  A nested list with structure `list(top_id = list(sub_id = tibble))`.

- y:

  A nested list with the same structure as `x`.

- prefer:

  Character string, either `"left"` (default) or `"right"`. If `"left"`,
  values from `x` are kept in case of duplicates. If `"right"`, values
  from `y` overwrite those in `x`.

## Value

A nested list with the same two-level structure containing the union of
all elements from `x` and `y`.

## Examples

``` r
library(tibble)

a <- list("S03" = list("S21" = tibble(a = 1, b = 2)))
b <- list("S03" = list("S07" = tibble(a = 3, b = 4)))
c <- list("S01" = list("S02" = tibble(a = 5, b = 6)))

# Default: prefer = "left"
merge_two_level_lists(a, b)
#> $S03
#> $S03$S21
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     1     2
#> 
#> $S03$S07
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     3     4
#> 
#> 

# Right side overwrites in case of duplicates
merge_two_level_lists(a, b, prefer = "right")
#> $S03
#> $S03$S21
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     1     2
#> 
#> $S03$S07
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     3     4
#> 
#> 

# Merge more than two lists using purrr::reduce
purrr::reduce(list(a, b, c), merge_two_level_lists)
#> $S03
#> $S03$S21
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     1     2
#> 
#> $S03$S07
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     3     4
#> 
#> 
#> $S01
#> $S01$S02
#> # A tibble: 1 × 2
#>       a     b
#>   <dbl> <dbl>
#> 1     5     6
#> 
#> 
```
