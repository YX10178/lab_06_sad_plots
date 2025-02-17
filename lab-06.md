Lab 06 - Ugly charts and Simpson’s paradox
================
Yuxin
2/17/25

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
staff <- read_csv("data/instructional-staff.csv")
##Each row in this dataset represents a faculty type, and the columns are the years for which we have data. The values are percentage of hires of that type of faculty for each year.
```

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))

staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(x = year, y = value, color = faculty_type)) +
  geom_line()
```

    ## `geom_line()`: Each group consists of only one observation.
    ## ℹ Do you need to adjust the group aesthetic?

![](lab-06_files/figure-gfm/a%20line%20graph-1.png)<!-- -->

``` r
## `geom_line()`: Each group consists of only one observation.
## ℹ Do you need to adjust the group aesthetic?
## To address this, we can use the group aesthetic in the following code.
```

### Exercise 1

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line() + labs (
    title = "Faculty Trends Over Time", x = "Year", y = "Percentage of Hires") +   theme_minimal()
```

![](lab-06_files/figure-gfm/group,%20then%20a%20line%20graph-1.png)<!-- -->

### Exercise 2

``` r
## Suppose the objective of this plot was to show that the proportion of part-time faculty have gone up over time compared to other instructional staff types.
## the goal is to compare proportions, we should normalize the data so that each year sums to 100%.
staff_long <- staff_long %>%
  group_by(year) %>%
  mutate(proportion = value / sum(value))

staff_long %>%
  group_by(faculty_type) %>%
  ggplot(aes(
    x = year,
    y = proportion,
    fill = faculty_type))+
    geom_bar(stat = "identity", position = "fill")+
    labs(
    title = "Proportion of Part-Time Faculty Over Time",
    x = "Year",
    y = "Proportion of Faculty",
    fill = "Faculty Type"
    ) +
    coord_flip()+
    theme_minimal()
```

![](lab-06_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

### Exercise 3

### Exercise 4

### Exercise 5

### Exercise 6

### Exercise 7
