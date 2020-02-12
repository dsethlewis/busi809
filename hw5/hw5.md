HW \#5: Difference Scores and Profile Similarity Indices
================
Daniel Lewis
2/12/2020

# Set up working environment

As usual, I load the R packages I will be using first.

``` r
# Tidyverse
library(readxl)
library(tidyverse)

# R Markdown
library(knitr)

# Project
library(here)

# Statistics
library(psych)
```

Next, I will read in the data. I will also glance at the data to see
what I’m working with.

``` r
tbl.2 <- read_excel(here('data', 'DIF.xlsx'))

tbl.2[1:10, ] %>%
  kable
```

|        ALTO |        ALTI |        PAYO |      PAYI |   JOBSAT |    IDENT |
| ----------: | ----------: | ----------: | --------: | -------: | -------: |
| \-0.3333333 |   2.0000000 |   1.0000000 | 0.6666667 | 3.666667 | 4.166667 |
|   1.0000000 |   1.0000000 |   1.6666667 | 2.0000000 | 3.333333 | 2.166667 |
| \-1.3333333 |   0.3333333 | \-0.6666667 | 1.3333333 | 3.000000 | 1.833333 |
|   2.0000000 |   2.0000000 |   1.0000000 | 1.6666667 | 3.666667 | 2.166667 |
|   0.6666667 |   1.0000000 |   1.0000000 | 1.0000000 | 4.000000 | 3.666667 |
|   1.3333333 |   2.0000000 |   0.3333333 | 2.0000000 | 4.000000 | 4.333333 |
|   0.0000000 |   2.0000000 |   0.0000000 | 2.0000000 | 3.000000 | 3.666667 |
| \-1.3333333 | \-0.6666667 |   0.3333333 | 2.0000000 | 3.666667 | 2.333333 |
| \-0.3333333 |   1.6666667 | \-0.3333333 | 2.0000000 | 5.000000 | 3.833333 |
|   0.3333333 |   1.3333333 |   0.0000000 | 1.6666667 | 3.000000 | 3.000000 |

``` r
describe(tbl.2)[c('n', 'mean', 'sd',
                  'min', 'max')] %>%
  kable
```

|        |   n |      mean |        sd |         min | max |
| ------ | --: | --------: | --------: | ----------: | --: |
| ALTO   | 970 | 0.5020619 | 0.9185661 | \-2.0000000 |   2 |
| ALTI   | 970 | 1.0805842 | 0.6897499 | \-2.0000000 |   2 |
| PAYO   | 969 | 0.4791882 | 0.8725597 | \-2.0000000 |   2 |
| PAYI   | 969 | 1.2707258 | 0.6024815 | \-0.6666667 |   2 |
| JOBSAT | 967 | 3.6856256 | 0.9173250 |   1.0000000 |   5 |
| IDENT  | 965 | 3.5260794 | 0.8160464 |   1.0000000 |   5 |

It looks like there may be a small number of missing data. While ALTO
and ALTI have data for 970 observations each, the other four variables
have fewer than 970 observations. Although I am sure there is an
appropriate way to handle these missing data
\[@newmanMissingDataFive2014\], I do not have enough information about
how the data were collected to be sure of what to do. So let’s take a
quick look at the observations with NA values and see if any corrections
are needed.

``` r
tbl.2 %>%
  filter_all(any_vars(is.na(.))) %>%
  kable
```

|        ALTO |        ALTI |        PAYO |     PAYI |   JOBSAT |    IDENT |
| ----------: | ----------: | ----------: | -------: | -------: | -------: |
|   0.6666667 |   0.6666667 |   0.6666667 | 2.000000 | 4.666667 |       NA |
|   1.0000000 |   2.0000000 |   0.6666667 | 1.000000 |       NA |       NA |
| \-1.0000000 |   0.0000000 | \-1.0000000 | 1.000000 | 2.666667 |       NA |
| \-1.5000000 |   0.0000000 |          NA | 2.000000 | 2.666667 | 1.400000 |
|   0.0000000 | \-0.5000000 |   0.5000000 |       NA | 4.000000 | 3.333333 |
|   0.0000000 |   2.0000000 | \-0.3333333 | 2.000000 | 5.000000 |       NA |
|   0.6666667 |   1.0000000 |   0.0000000 | 1.333333 |       NA |       NA |
|   1.3333333 |   1.3333333 |   1.0000000 | 1.000000 |       NA | 4.166667 |

It looks like the main problem here is that if I take the difference of
PAYO and PAYI, I will lose two data points. It is also a problem that
two observations have no data on either outcome variable, essentially
causing a loss of eight data points for independent variables. R will
drop those observations when computing correlations and regressions, and
I don’t think there is anything to be done about that, so I will simply
move on.

Returning to the table of descriptive statistics above, the mean
organizational scores on both altruism and pay are lower than their
respective mean individual scores. This suggests that individuals tended
to report valuing altruism and pay more than their organizations. (The
standard deviations for individual scores were lower than for
organizations, but I’m not sure what the substantive interpretation of
that is.)

Interestingly, nobody reported valuing pay less than -0.67. That fact
makes me wonder if perhaps respondents were more worried about future
negotiations with employers than about presenting socially desirable
responses to researchers.

# a. Construct difference scores and compute correlations

My understanding of algebraic and squared difference scores is that, for
two variables, ![X](https://latex.codecogs.com/png.latex?X "X") and
![Y](https://latex.codecogs.com/png.latex?Y "Y"), their algebraic
difference is ![X - Y](https://latex.codecogs.com/png.latex?X%20-%20Y
"X - Y") and their squared difference is
![(X-Y)^2](https://latex.codecogs.com/png.latex?%28X-Y%29%5E2 "(X-Y)^2")
\[@edwards2002maabioaimada\].

``` r
tbl.2.a <- tbl.2 %>%
  mutate(
    alt.df = ALTO - ALTI,
    alt.sq = alt.df ^ 2,
    pay.df = PAYO - PAYI,
    pay.sq = pay.df ^2
  )

tbl.2.a[1:10, ] %>%
  kable
```

|        ALTO |        ALTI |        PAYO |      PAYI |   JOBSAT |    IDENT |      alt.df |    alt.sq |      pay.df |    pay.sq |
| ----------: | ----------: | ----------: | --------: | -------: | -------: | ----------: | --------: | ----------: | --------: |
| \-0.3333333 |   2.0000000 |   1.0000000 | 0.6666667 | 3.666667 | 4.166667 | \-2.3333333 | 5.4444444 |   0.3333333 | 0.1111111 |
|   1.0000000 |   1.0000000 |   1.6666667 | 2.0000000 | 3.333333 | 2.166667 |   0.0000000 | 0.0000000 | \-0.3333333 | 0.1111111 |
| \-1.3333333 |   0.3333333 | \-0.6666667 | 1.3333333 | 3.000000 | 1.833333 | \-1.6666667 | 2.7777778 | \-2.0000000 | 4.0000000 |
|   2.0000000 |   2.0000000 |   1.0000000 | 1.6666667 | 3.666667 | 2.166667 |   0.0000000 | 0.0000000 | \-0.6666667 | 0.4444444 |
|   0.6666667 |   1.0000000 |   1.0000000 | 1.0000000 | 4.000000 | 3.666667 | \-0.3333333 | 0.1111111 |   0.0000000 | 0.0000000 |
|   1.3333333 |   2.0000000 |   0.3333333 | 2.0000000 | 4.000000 | 4.333333 | \-0.6666667 | 0.4444444 | \-1.6666667 | 2.7777778 |
|   0.0000000 |   2.0000000 |   0.0000000 | 2.0000000 | 3.000000 | 3.666667 | \-2.0000000 | 4.0000000 | \-2.0000000 | 4.0000000 |
| \-1.3333333 | \-0.6666667 |   0.3333333 | 2.0000000 | 3.666667 | 2.333333 | \-0.6666667 | 0.4444444 | \-1.6666667 | 2.7777778 |
| \-0.3333333 |   1.6666667 | \-0.3333333 | 2.0000000 | 5.000000 | 3.833333 | \-2.0000000 | 4.0000000 | \-2.3333333 | 5.4444444 |
|   0.3333333 |   1.3333333 |   0.0000000 | 1.6666667 | 3.000000 | 3.000000 | \-1.0000000 | 1.0000000 | \-1.6666667 | 2.7777778 |

As I would expect, the squared differences are all positive (or zero),
and are larger or smaller in magnitude than the algebraic differences
according to whether the magnitudes of the algebraic differences are
greater or less than one.

Now, I will compute the correlations.

``` r
vec.2.a.1 <- tbl.2.a[7:10] %>%
  map_dbl(~ cor(., tbl.2.a$JOBSAT, use = "pairwise.complete.obs"))

vec.2.a.2 <- tbl.2.a[7:10] %>%
  map_dbl(~ cor(., tbl.2.a$IDENT, use = "pairwise.complete.obs"))

tibble(var = names(vec.2.a.1), job = vec.2.a.1, ide = vec.2.a.2) %>%
  kable
```

| var    |         job |         ide |
| :----- | ----------: | ----------: |
| alt.df |   0.2921289 |   0.1359600 |
| alt.sq | \-0.3870969 | \-0.3013596 |
| pay.df |   0.1393014 |   0.1258576 |
| pay.sq | \-0.2877371 | \-0.2360829 |

Overall, the correlations are fairly weak.
