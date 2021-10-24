Mini Data-Analysis Deliverable 3
================
John Gray

# Welcome to your last milestone in your mini data analysis project!

In Milestone 1, you explored your data and came up with research
questions. In Milestone 2, you obtained some results by making summary
tables and graphs.

In this (3rd) milestone, you’ll be sharpening some of the results you
obtained from your previous milestone by:

-   Manipulating special data types in R: factors and/or dates and
    times.
-   Fitting a model object to your data, and extract a result.
-   Reading and writing data as separate files.

**NOTE**: The main purpose of the mini data analysis is to integrate
what you learn in class in an analysis. Although each milestone provides
a framework for you to conduct your analysis, it’s possible that you
might find the instructions too rigid for your data set. If this is the
case, you may deviate from the instructions – just make sure you’re
demonstrating a wide range of tools and techniques taught in this class.

## Instructions

**To complete this milestone**, edit [this very `.Rmd`
file](https://raw.githubusercontent.com/UBC-STAT/stat545.stat.ubc.ca/master/content/mini-project/mini-project-3.Rmd)
directly. Fill in the sections that are tagged with
`<!--- start your work here--->`.

**To submit this milestone**, make sure to knit this `.Rmd` file to an
`.md` file by changing the YAML output settings from
`output: html_document` to `output: github_document`. Commit and push
all of your work to your mini-analysis GitHub repository, and tag a
release on GitHub. Then, submit a link to your tagged release on canvas.

**Points**: This milestone is worth 40 points (compared to the usual 30
points): 30 for your analysis, and 10 for your entire mini-analysis
GitHub repository. Details follow.

**Research Questions**: In Milestone 2, you chose two research questions
to focus on. Wherever realistic, your work in this milestone should
relate to these research questions whenever we ask for justification
behind your work. In the case that some tasks in this milestone don’t
align well with one of your research questions, feel free to discuss
your results in the context of a different research question.

# Setup

Begin by loading your data and the tidyverse package below:

``` r
library(datateachr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)
library(lubridate)
library(tsibble)
library(broom)
fish_sampling<-read_csv("fish_sampling.csv")
```

From Milestone 2, you chose two research questions. What were they? Put
them here.

<!-------------------------- Start your work below ---------------------------->

1.  *How do populations of fish differ between Pepin Creek and Bertrand
    Creek?*
2.  *How does the length to weight ratio differ between species in Pepin
    Creek and Bertrand Creek?*
    <!----------------------------------------------------------------------------->

# Exercise 1: Special Data Types (10)

For this exercise, you’ll be choosing two of the three tasks below –
both tasks that you choose are worth 5 points each.

But first, tasks 1 and 2 below ask you to modify a plot you made in a
previous milestone. The plot you choose should involve plotting across
at least three groups (whether by facetting, or using an aesthetic like
colour). Place this plot below (you’re allowed to modify the plot if
you’d like). If you don’t have such a plot, you’ll need to make one.
Place the code for your plot below.

<!-------------------------- Start your work below ---------------------------->
<!----------------------------------------------------------------------------->

Now, choose two of the following tasks.

1.  Produce a new plot that reorders a factor in your original plot,
    using the `forcats` package (3 points). Then, in a sentence or two,
    briefly explain why you chose this ordering (1 point here for
    demonstrating understanding of the reordering, and 1 point for
    demonstrating some justification for the reordering, which could be
    subtle or speculative.)

2.  Produce a new plot that groups some factor levels together into an
    “other” category (or something similar), using the `forcats` package
    (3 points). Then, in a sentence or two, briefly explain why you
    chose this grouping (1 point here for demonstrating understanding of
    the grouping, and 1 point for demonstrating some justification for
    the grouping, which could be subtle or speculative.)

3.  If your data has some sort of time-based column like a date (but
    something more granular than just a year):

    1.  Make a new column that uses a function from the `lubridate` or
        `tsibble` package to modify your original time-based column. (3
        points)
        -   Note that you might first have to *make* a time-based column
            using a function like `ymd()`, but this doesn’t count.
        -   Examples of something you might do here: extract the day of
            the year from a date, or extract the weekday, or let 24
            hours elapse on your dates.
    2.  Then, in a sentence or two, explain how your new column might be
        useful in exploring a research question. (1 point for
        demonstrating understanding of the function you used, and 1
        point for your justification, which could be subtle or
        speculative).
        -   For example, you could say something like “Investigating the
            day of the week might be insightful because penguins don’t
            work on weekends, and so may respond differently”.

<!-------------------------- Start your work below ---------------------------->

**Task Number**: 1

I chose to alter a plot from my Milestone 1 of the Mini-Data-Analysis
Project. The plot visually represents the length to weight ratio of fish
species within both Bertrand Creek and Pepin Creek. The original plot,
though slightly modified aesthetically, is seen below.

``` r
fish_sampling<-fish_sampling%>%
  transform(weight_g=as.numeric(weight_g),length_cm=as.numeric(length_cm))%>%
  mutate(length_weight_ratio=length_cm/weight_g)
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
fish_sampling%>%
  ggplot(aes(species,length_weight_ratio))+
  geom_jitter(alpha=0.4,width=0.3,aes(color=system))+
  scale_y_log10()+
  labs(y="Fish Length to Weight Ratio",x="Species", title = "The length to weight ratio of fish species within Bertrand Creek and Pepin Creek")+
  scale_x_discrete(labels=c("CO"="Coho", "CRAYFISH"="Crayfish","CUT"="Cutthroat","RBT"="Steelhead", "LAMP"= "Lamprey","STK"="Stickleback","DAC"="Dace","TROUT"="Trout sp."))
```

    ## Warning: Removed 191 rows containing missing values (geom_point).

![](Milestone-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

I used the ‘fct\_reorder()’ function of the ‘forcats’ package to reorder
the above plot. I had the values along the x-axis reordered; the fish
species, according to values on the y-axis; the length to weight ratio
of the different species. I used this function as it orders the fish
species along the axis based on their length to weight ratios in
ascending order. While I could estimate the order in which the species
should be ordered based on the above plot or values, it would not be as
accurate. the ‘fct\_reorder()’ function takes into account values such
as outliers which would be difficult to account for when trying to
organize the plot visually. Additionally, the use of this function is
far more efficient then me spending the time to guess the order that
fish species should be ordered along the x-axis based on the length tot
weight ratio.

``` r
fish_sampling%>%
  mutate(species = fct_reorder(species,length_weight_ratio,na.rm=TRUE))%>%
  ggplot(aes(species,length_weight_ratio))+
  geom_jitter(alpha=0.4,width=0.3,aes(color=system))+
  scale_y_log10()+
  labs(y="Fish Length to Weight Ratio",x="Species", title = "The length to weight ratio of fish species within Bertrand Creek and Pepin Creek")+
  scale_x_discrete(labels=c("CO"="Coho", "CRAYFISH"="Crayfish","CUT"="Cutthroat","RBT"="Steelhead", "LAMP"= "Lamprey","STK"="Stickleback","DAC"="Dace","TROUT"="Trout sp."))
```

    ## Warning: Removed 191 rows containing missing values (geom_point).

![](Milestone-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

<!----------------------------------------------------------------------------->
<!-------------------------- Start your work below ---------------------------->

**Task Number**: 3

In this task I modified the “Date” column of my dataframe
“fish\_sampling”.

``` r
fish_sampling%>%
  mutate(Date=mdy(Date))%>%
  mutate(Date=yearweek(Date))
```

    ##       system riffle pass  species weight_g length_cm     Date shocking_effort_s
    ## 1      Pepin  14.00 1.00     LAMP     6.80     15.00 2021 W33                 -
    ## 2      Pepin  14.00 1.00     LAMP     4.80     15.00 2021 W33                 -
    ## 3      Pepin  14.00 1.00     LAMP     1.05     10.00 2021 W33                 -
    ## 4      Pepin  14.00 1.00     LAMP     0.90      7.00 2021 W33                 -
    ## 5      Pepin  14.00 1.00     LAMP     0.90      6.00 2021 W33                 -
    ## 6      Pepin  14.00 1.00     LAMP     2.25     10.00 2021 W33                 -
    ## 7      Pepin  14.00 1.00      CUT     3.10      6.60 2021 W33                 -
    ## 8      Pepin  14.00 1.00       CO     2.70      5.70 2021 W33                 -
    ## 9      Pepin  14.00 1.00      CUT     6.40      7.20 2021 W33                 -
    ## 10     Pepin  14.00 1.00      RBT     2.50      5.80 2021 W33                 -
    ## 11     Pepin  14.00 1.00      CUT     3.40      6.60 2021 W33                 -
    ## 12     Pepin  14.00 1.00      CUT     3.40      6.70 2021 W33                 -
    ## 13     Pepin  14.00 1.00      STK     2.00      5.60 2021 W33                 -
    ## 14     Pepin  14.00 1.00      CUT     2.20      5.90 2021 W33                 -
    ## 15     Pepin  14.00 1.00     LAMP     3.60     15.00 2021 W33                 -
    ## 16     Pepin  14.00 1.00       CO     1.10      4.60 2021 W33                 -
    ## 17     Pepin  14.00 1.00      STK     0.60      3.80 2021 W33                 -
    ## 18     Pepin  14.00 1.00      RBT     0.04      3.60 2021 W33                 -
    ## 19     Pepin  14.00 1.00      CUT     1.60      4.80 2021 W33                 -
    ## 20     Pepin  14.00 1.00     LAMP     1.20      8.00 2021 W33                 -
    ## 21     Pepin  14.00 1.00     LAMP     1.50      8.00 2021 W33                 -
    ## 22     Pepin  14.00 1.00     LAMP     3.60     12.00 2021 W33                 -
    ## 23     Pepin  14.00 1.00     LAMP     0.50      5.00 2021 W33                 -
    ## 24     Pepin  14.00 1.00     LAMP     1.50      7.00 2021 W33                 -
    ## 25     Pepin  14.00 1.00     LAMP     1.80      8.00 2021 W33                 -
    ## 26     Pepin  14.00 2.00     LAMP     1.10      8.00 2021 W33                 -
    ## 27     Pepin  14.00 2.00     LAMP     0.90      8.00 2021 W33                 -
    ## 28     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33                 -
    ## 29     Pepin  14.00 2.00     LAMP     1.00      9.00 2021 W33                 -
    ## 30     Pepin  14.00 2.00     LAMP     1.00      7.00 2021 W33                 -
    ## 31     Pepin  14.00 2.00     LAMP     1.40      9.00 2021 W33                 -
    ## 32     Pepin  14.00 2.00     LAMP     2.60     14.00 2021 W33                 -
    ## 33     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33                 -
    ## 34     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33                 -
    ## 35     Pepin  14.00 2.00     LAMP     3.20     11.00 2021 W33                 -
    ## 36     Pepin  14.00 2.00     LAMP     1.30      9.00 2021 W33                 -
    ## 37     Pepin  14.00 2.00     LAMP     1.20      9.00 2021 W33                 -
    ## 38     Pepin  14.00 2.00     LAMP     1.00      7.00 2021 W33                 -
    ## 39     Pepin  14.00 2.00     LAMP     1.30      7.00 2021 W33                 -
    ## 40     Pepin  14.00 2.00     LAMP     2.30     11.00 2021 W33                 -
    ## 41     Pepin  14.00 2.00     LAMP     1.60      8.00 2021 W33                 -
    ## 42     Pepin  14.00 2.00     LAMP     1.00      8.00 2021 W33                 -
    ## 43     Pepin  14.00 2.00     LAMP     1.90     10.00 2021 W33                 -
    ## 44     Pepin  14.00 2.00     LAMP     0.70      7.00 2021 W33                 -
    ## 45     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33                 -
    ## 46     Pepin  14.00 2.00      RBT     3.30      7.00 2021 W33                 -
    ## 47     Pepin  14.00 2.00       CO     4.80      8.00 2021 W33                 -
    ## 48     Pepin  14.00 2.00       CO     2.90      6.50 2021 W33                 -
    ## 49     Pepin  14.00 2.00      STK     2.10      5.50 2021 W33                 -
    ## 50     Pepin  14.00 2.00     LAMP     0.70      5.00 2021 W33                 -
    ## 51     Pepin  14.00 3.00     LAMP     0.60      7.00 2021 W33                 -
    ## 52     Pepin  14.00 3.00     LAMP     1.50      9.00 2021 W33                 -
    ## 53     Pepin  14.00 3.00     LAMP     2.90     11.00 2021 W33                 -
    ## 54     Pepin  14.00 3.00     LAMP     3.30     11.00 2021 W33                 -
    ## 55     Pepin  14.00 3.00     LAMP     3.10     12.00 2021 W33                 -
    ## 56     Pepin  14.00 3.00     LAMP     1.60      9.00 2021 W33                 -
    ## 57     Pepin  14.00 3.00     LAMP     0.80      6.00 2021 W33                 -
    ## 58     Pepin  14.00 3.00     LAMP     4.40     11.00 2021 W33                 -
    ## 59     Pepin  14.00 3.00     LAMP     3.50     10.00 2021 W33                 -
    ## 60     Pepin  14.00 3.00     LAMP     4.20     12.00 2021 W33                 -
    ## 61     Pepin  14.00 3.00       CO     1.90      6.00 2021 W33                 -
    ## 62     Pepin  14.00 3.00      RBT     3.10      6.50 2021 W33                 -
    ## 63     Pepin  14.00 3.00      DAC     8.40      9.50 2021 W33                 -
    ## 64     Pepin  14.00 3.00      RBT     2.20      6.00 2021 W33                 -
    ## 65     Pepin  14.00 3.00      RBT     2.20      6.00 2021 W33                 -
    ## 66     Pepin  12.00 1.00      CUT     3.40      6.50 2021 W33                 -
    ## 67     Pepin  12.00 1.00      CUT     7.70      9.50 2021 W33                 -
    ## 68     Pepin  12.00 1.00      CUT     1.50      6.50 2021 W33                 -
    ## 69     Pepin  12.00 1.00      STK     1.30      4.50 2021 W33                 -
    ## 70     Pepin  12.00 1.00     LAMP     1.90     10.00 2021 W33                 -
    ## 71     Pepin  12.00 1.00     LAMP     3.60     15.00 2021 W33                 -
    ## 72     Pepin  12.00 1.00     LAMP     3.60     14.00 2021 W33                 -
    ## 73     Pepin  12.00 1.00     LAMP     0.70      7.00 2021 W33                 -
    ## 74     Pepin  12.00 1.00     LAMP     1.10      7.00 2021 W33                 -
    ## 75     Pepin  12.00 1.00     LAMP     0.40      6.00 2021 W33                 -
    ## 76     Pepin  12.00 2.00      RBT     1.20      4.50 2021 W33                 -
    ## 77     Pepin  12.00 2.00     LAMP     1.30     10.00 2021 W33                 -
    ## 78     Pepin  12.00 2.00     LAMP     3.50     11.00 2021 W33                 -
    ## 79     Pepin  12.00 2.00     LAMP     0.60      8.00 2021 W33                 -
    ## 80     Pepin  12.00 2.00     LAMP     1.20     10.00 2021 W33                 -
    ## 81     Pepin  12.00 2.00     LAMP     0.70      8.00 2021 W33                 -
    ## 82     Pepin  12.00 3.00 CRAYFISH       NA        NA 2021 W33                 -
    ## 83     Pepin   6.00 1.00      STK     0.40      3.40 2021 W33                 -
    ## 84     Pepin   6.00 1.00      CUT    33.00     15.40 2021 W33                 -
    ## 85     Pepin   6.00 1.00       CO     4.10      7.10 2021 W33                 -
    ## 86     Pepin   6.00 1.00      RBT     0.80      4.30 2021 W33                 -
    ## 87     Pepin   6.00 1.00      STK     0.70      4.10 2021 W33                 -
    ## 88     Pepin   6.00 1.00       CO     3.50      6.70 2021 W33                 -
    ## 89     Pepin   6.00 1.00      STK     0.60      3.80 2021 W33                 -
    ## 90     Pepin   6.00 1.00       CO     3.70      6.80 2021 W33                 -
    ## 91     Pepin   6.00 1.00      RBT     1.60      5.30 2021 W33                 -
    ## 92     Pepin   6.00 1.00       CO     1.50      5.20 2021 W33                 -
    ## 93     Pepin   6.00 1.00      CUT     5.70      8.30 2021 W33                 -
    ## 94     Pepin   6.00 1.00      CUT     1.80      5.40 2021 W33                 -
    ## 95     Pepin   6.00 1.00      CUT     2.20      6.10 2021 W33                 -
    ## 96     Pepin   6.00 1.00      CUT    14.30     11.20 2021 W33                 -
    ## 97     Pepin   6.00 1.00      CUT     1.70      5.30 2021 W33                 -
    ## 98     Pepin   6.00 1.00      CUT    18.40     12.50 2021 W33                 -
    ## 99     Pepin   6.00 1.00      CUT     2.60      6.20 2021 W33                 -
    ## 100    Pepin   6.00 1.00       CO     3.70      6.80 2021 W33                 -
    ## 101    Pepin   6.00 1.00       CO     2.00      5.20 2021 W33                 -
    ## 102    Pepin   6.00 1.00      RBT     0.60      3.30 2021 W33                 -
    ## 103    Pepin   6.00 1.00       CO     2.70      6.20 2021 W33                 -
    ## 104    Pepin   6.00 1.00      CUT     1.50      5.20 2021 W33                 -
    ## 105    Pepin   6.00 1.00      CUT    15.20     11.90 2021 W33                 -
    ## 106    Pepin   6.00 1.00      RBT     0.90      4.20 2021 W33                 -
    ## 107    Pepin   6.00 1.00      DAC     3.90      6.90 2021 W33                 -
    ## 108    Pepin   6.00 1.00      DAC     7.90      8.90 2021 W33                 -
    ## 109    Pepin   6.00 1.00       CO     3.30      6.60 2021 W33                 -
    ## 110    Pepin   6.00 1.00     LAMP     0.60      6.70 2021 W33                 -
    ## 111    Pepin   6.00 1.00       CO     2.50      6.00 2021 W33                 -
    ## 112    Pepin   6.00 1.00      CUT     4.80      7.60 2021 W33                 -
    ## 113    Pepin   6.00 1.00      RBT     1.80      4.20 2021 W33                 -
    ## 114    Pepin   6.00 1.00      RBT     3.00      6.70 2021 W33                 -
    ## 115    Pepin   6.00 1.00      DAC     9.30      9.50 2021 W33                 -
    ## 116    Pepin   6.00 1.00       CO     2.90      5.40 2021 W33                 -
    ## 117    Pepin   6.00 1.00      STK     1.00      3.90 2021 W33                 -
    ## 118    Pepin   6.00 1.00      CUT     7.80      9.80 2021 W33                 -
    ## 119    Pepin   6.00 1.00       CO     1.40      4.60 2021 W33                 -
    ## 120    Pepin   6.00 1.00      STK     1.20      4.60 2021 W33                 -
    ## 121    Pepin   6.00 1.00      STK     0.80      3.70 2021 W33                 -
    ## 122    Pepin   6.00 1.00      STK     3.00      6.00 2021 W33                 -
    ## 123    Pepin   6.00 1.00      DAC     4.20      7.20 2021 W33                 -
    ## 124    Pepin   6.00 1.00     LAMP     1.10      9.00 2021 W33                 -
    ## 125    Pepin   6.00 1.00      DAC     7.30      8.50 2021 W33                 -
    ## 126    Pepin   6.00 2.00       CO     1.20      4.70 2021 W33                 -
    ## 127    Pepin   6.00 2.00      CUT     1.80      5.70 2021 W33                 -
    ## 128    Pepin   6.00 2.00       CO     4.10      7.10 2021 W33                 -
    ## 129    Pepin   6.00 2.00      RBT     3.00      6.50 2021 W33                 -
    ## 130    Pepin   6.00 2.00      STK     4.30      6.80 2021 W33                 -
    ## 131    Pepin   6.00 2.00      CUT     1.30      4.90 2021 W33                 -
    ## 132    Pepin   6.00 2.00       CO     2.40      6.20 2021 W33                 -
    ## 133    Pepin   6.00 2.00      CUT     1.10      4.10 2021 W33                 -
    ## 134    Pepin   6.00 2.00      CUT     2.50      5.50 2021 W33                 -
    ## 135    Pepin   6.00 2.00       CO     3.00      6.30 2021 W33                 -
    ## 136    Pepin   6.00 2.00      STK     1.50      5.50 2021 W33                 -
    ## 137    Pepin   6.00 2.00      CUT     1.50      5.40 2021 W33                 -
    ## 138    Pepin   6.00 2.00      STK     0.80      4.00 2021 W33                 -
    ## 139    Pepin   6.00 2.00       CO     1.30      4.90 2021 W33                 -
    ## 140    Pepin   6.00 2.00       CO     2.00      5.50 2021 W33                 -
    ## 141    Pepin   6.00 2.00     LAMP     0.90      6.20 2021 W33                 -
    ## 142    Pepin   6.00 2.00     LAMP     1.60      8.50 2021 W33                 -
    ## 143    Pepin   6.00 3.00      RBT     0.90      4.40 2021 W33                 -
    ## 144    Pepin   6.00 3.00      RBT     0.60      3.30 2021 W33                 -
    ## 145    Pepin   6.00 3.00     LAMP     0.70      8.00 2021 W33                 -
    ## 146    Pepin  11.00 1.00      STK     0.70      4.50 2021 W33                 -
    ## 147    Pepin  11.00 1.00       CO     1.60      5.50 2021 W33                 -
    ## 148    Pepin  11.00 1.00       CO     1.90      5.40 2021 W33                 -
    ## 149    Pepin  11.00 1.00      STK     1.60      5.20 2021 W33                 -
    ## 150    Pepin  11.00 1.00      CUT     1.10      4.60 2021 W33                 -
    ## 151    Pepin  11.00 1.00      STK     0.50      3.40 2021 W33                 -
    ## 152    Pepin  11.00 1.00      STK     1.20      5.40 2021 W33                 -
    ## 153    Pepin  11.00 1.00     LAMP     3.40      9.80 2021 W33                 -
    ## 154    Pepin  11.00 2.00 CRAYFISH       NA        NA 2021 W33                 -
    ## 155    Pepin  11.00 3.00      CUT     2.20      6.10 2021 W33                 -
    ## 156    Pepin  11.00 3.00      CUT     2.80      6.70 2021 W33                 -
    ## 157    Pepin  11.00 3.00       CO     0.90      4.60 2021 W33                 -
    ## 158    Pepin   9.50 1.00      CUT       NA      6.10 2021 W34                 -
    ## 159    Pepin   9.50 1.00      STK       NA      5.00 2021 W34                 -
    ## 160    Pepin   9.50 1.00      STK       NA      2.70 2021 W34                 -
    ## 161    Pepin   9.50 1.00      RBT       NA      4.30 2021 W34                 -
    ## 162    Pepin   9.50 1.00      CUT       NA      8.20 2021 W34                 -
    ## 163    Pepin   9.50 1.00      RBT       NA      4.80 2021 W34                 -
    ## 164    Pepin   9.50 1.00      CUT       NA      7.70 2021 W34                 -
    ## 165    Pepin   9.50 1.00       CO       NA      6.60 2021 W34                 -
    ## 166    Pepin   9.50 1.00       CO       NA      5.50 2021 W34                 -
    ## 167    Pepin   9.50 1.00      RBT       NA      5.00 2021 W34                 -
    ## 168    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 169    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 170    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 171    Pepin   9.50 2.00      CUT       NA      8.30 2021 W34                 -
    ## 172    Pepin   9.50 2.00      STK       NA      5.70 2021 W34                 -
    ## 173    Pepin   9.50 2.00      STK       NA      3.70 2021 W34                 -
    ## 174    Pepin   9.50 3.00      CUT       NA      6.80 2021 W34                 -
    ## 175    Pepin   9.50 3.00       CO       NA      4.90 2021 W34                 -
    ## 176    Pepin   9.50 3.00      RBT       NA      5.40 2021 W34                 -
    ## 177    Pepin   9.50 4.00       CO       NA      5.90 2021 W34                 -
    ## 178    Pepin   9.00 1.00      CUT       NA      9.10 2021 W34                 -
    ## 179    Pepin   9.00 1.00      CUT       NA      8.30 2021 W34                 -
    ## 180    Pepin   9.00 1.00       CO       NA      5.80 2021 W34                 -
    ## 181    Pepin   9.00 1.00     LAMP       NA      7.00 2021 W34                 -
    ## 182    Pepin   9.00 1.00       CO       NA      4.90 2021 W34                 -
    ## 183    Pepin   9.00 1.00      CUT       NA     10.90 2021 W34                 -
    ## 184    Pepin   9.00 1.00      STK       NA      5.30 2021 W34                 -
    ## 185    Pepin   9.00 1.00     LAMP       NA      7.00 2021 W34                 -
    ## 186    Pepin   9.00 1.00       CO       NA      5.20 2021 W34                 -
    ## 187    Pepin   9.00 1.00     LAMP       NA      6.00 2021 W34                 -
    ## 188    Pepin   9.00 1.00       CO       NA      5.20 2021 W34                 -
    ## 189    Pepin   9.00 1.00       CO       NA      5.90 2021 W34                 -
    ## 190    Pepin   9.00 1.00      DAC       NA      8.50 2021 W34                 -
    ## 191    Pepin   9.00 1.00      STK       NA      5.40 2021 W34                 -
    ## 192    Pepin   9.00 1.00     LAMP       NA      5.00 2021 W34                 -
    ## 193    Pepin   9.00 1.00      STK       NA      5.40 2021 W34                 -
    ## 194    Pepin   9.00 1.00      RBT       NA      5.10 2021 W34                 -
    ## 195    Pepin   9.00 1.00      STK       NA      5.10 2021 W34                 -
    ## 196    Pepin   9.00 1.00      RBT       NA      3.90 2021 W34                 -
    ## 197    Pepin   9.00 1.00      STK       NA      5.40 2021 W34                 -
    ## 198    Pepin   9.00 1.00     LAMP       NA      8.00 2021 W34                 -
    ## 199    Pepin   9.00 1.00      CUT       NA      8.20 2021 W34                 -
    ## 200    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 201    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 202    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 203    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 204    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 205    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 206    Pepin   9.00 2.00       CO       NA      5.40 2021 W34                 -
    ## 207    Pepin   9.00 2.00      CUT       NA     10.20 2021 W34                 -
    ## 208    Pepin   9.00 2.00       CO       NA      7.30 2021 W34                 -
    ## 209    Pepin   9.00 2.00      CUT       NA     12.10 2021 W34                 -
    ## 210    Pepin   9.00 2.00       CO       NA      7.00 2021 W34                 -
    ## 211    Pepin   9.00 2.00      STK       NA      5.60 2021 W34                 -
    ## 212    Pepin   9.00 2.00      STK       NA      2.60 2021 W34                 -
    ## 213    Pepin   9.00 2.00       CO       NA      5.40 2021 W34                 -
    ## 214    Pepin   9.00 2.00       CO       NA      6.40 2021 W34                 -
    ## 215    Pepin   9.00 2.00      STK       NA      4.60 2021 W34                 -
    ## 216    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 217    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 218    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 219    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 220    Pepin   9.00 3.00      STK       NA      5.10 2021 W34               263
    ## 221    Pepin   9.00 3.00      STK       NA      2.80 2021 W34                 -
    ## 222    Pepin   9.00 3.00      STK       NA      3.30 2021 W34                 -
    ## 223    Pepin   9.00 3.00       CO       NA      5.40 2021 W34                 -
    ## 224    Pepin   9.00 3.00      STK       NA      5.10 2021 W34                 -
    ## 225    Pepin   9.00 3.00      RBT       NA      3.30 2021 W34                 -
    ## 226    Pepin   9.00 3.00       CO       NA      5.20 2021 W34                 -
    ## 227    Pepin   9.00 3.00      STK       NA      3.40 2021 W34                 -
    ## 228    Pepin   9.00 3.00       CO       NA      6.30 2021 W34                 -
    ## 229    Pepin   9.00 3.00      DAC       NA      9.70 2021 W34                 -
    ## 230    Pepin   9.00 3.00       CO       NA      7.20 2021 W34                 -
    ## 231    Pepin   9.00 3.00     LAMP       NA      9.00 2021 W34                 -
    ## 232    Pepin   9.00 3.00       CO       NA      6.50 2021 W34                 -
    ## 233    Pepin   9.00 3.00      DAC       NA      8.80 2021 W34                 -
    ## 234    Pepin   9.00 3.00      STK       NA      3.60 2021 W34                 -
    ## 235    Pepin   9.00 3.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 236    Pepin   9.00 3.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 237    Pepin   9.00 4.00      CUT       NA     11.80 2021 W34               264
    ## 238    Pepin   9.00 4.00      RBT       NA      5.10 2021 W34                 -
    ## 239    Pepin   9.00 4.00      STK       NA      5.30 2021 W34                 -
    ## 240    Pepin   9.00 4.00      STK       NA      5.40 2021 W34                 -
    ## 241    Pepin   9.00 4.00      CUT       NA      6.10 2021 W34                 -
    ## 242    Pepin   9.00 4.00     LAMP       NA     12.00 2021 W34                 -
    ## 243    Pepin   9.00 4.00       CO       NA      6.00 2021 W34                 -
    ## 244    Pepin   9.00 4.00       CO       NA      6.70 2021 W34                 -
    ## 245    Pepin   9.00 4.00      STK       NA      4.80 2021 W34                 -
    ## 246    Pepin   9.00 4.00       CO       NA      5.00 2021 W34                 -
    ## 247    Pepin   9.00 4.00      STK       NA      3.10 2021 W34                 -
    ## 248    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 249    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 250    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 251    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 252    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 253    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 254    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 255    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 256    Pepin  10.00 1.00      RBT       NA      4.40 2021 W34               433
    ## 257    Pepin  10.00 1.00       CO       NA      4.40 2021 W34                 -
    ## 258    Pepin  10.00 1.00      RBT       NA      3.30 2021 W34                 -
    ## 259    Pepin  10.00 1.00       CO       NA      5.10 2021 W34                 -
    ## 260    Pepin  10.00 1.00       CO       NA      6.80 2021 W34                 -
    ## 261    Pepin  10.00 1.00       CO       NA      4.50 2021 W34                 -
    ## 262    Pepin  10.00 1.00       CO       NA      4.70 2021 W34                 -
    ## 263    Pepin  10.00 1.00      STK       NA      3.10 2021 W34                 -
    ## 264    Pepin  10.00 1.00      RBT       NA      5.70 2021 W34                 -
    ## 265    Pepin  10.00 1.00     LAMP       NA      8.00 2021 W34                 -
    ## 266    Pepin  10.00 1.00     LAMP       NA      9.00 2021 W34                 -
    ## 267    Pepin  10.00 1.00     LAMP       NA      9.00 2021 W34                 -
    ## 268    Pepin  10.00 1.00      CUT       NA      5.30 2021 W34                 -
    ## 269    Pepin  10.00 1.00       CO       NA      5.70 2021 W34                 -
    ## 270    Pepin  10.00 1.00       CO       NA      6.00 2021 W34                 -
    ## 271    Pepin  10.00 1.00     LAMP       NA      7.00 2021 W34                 -
    ## 272    Pepin  10.00 1.00     LAMP       NA      6.00 2021 W34                 -
    ## 273    Pepin  10.00 1.00       CO       NA      5.40 2021 W34                 -
    ## 274    Pepin  10.00 1.00     LAMP       NA      5.00 2021 W34                 -
    ## 275    Pepin  10.00 1.00       CO       NA      5.30 2021 W34                 -
    ## 276    Pepin  10.00 1.00     LAMP       NA      3.00 2021 W34                 -
    ## 277    Pepin  10.00 1.00     LAMP       NA      6.00 2021 W34                 -
    ## 278    Pepin  10.00 1.00      RBT       NA      5.90 2021 W34                 -
    ## 279    Pepin  10.00 1.00      CUT       NA      7.80 2021 W34                 -
    ## 280    Pepin  10.00 1.00      RBT       NA      4.20 2021 W34                 -
    ## 281    Pepin  10.00 1.00      STK       NA      5.50 2021 W34                 -
    ## 282    Pepin  10.00 1.00       CO       NA      6.10 2021 W34                 -
    ## 283    Pepin  10.00 1.00      DAC       NA     10.20 2021 W34                 -
    ## 284    Pepin  10.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 285    Pepin  10.00 2.00       CO       NA      4.30 2021 W34               462
    ## 286    Pepin  10.00 2.00     LAMP       NA      9.00 2021 W34                 -
    ## 287    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34                 -
    ## 288    Pepin  10.00 2.00     LAMP       NA      7.00 2021 W34                 -
    ## 289    Pepin  10.00 2.00      STK       NA      5.60 2021 W34                 -
    ## 290    Pepin  10.00 2.00      STK       NA      5.50 2021 W34                 -
    ## 291    Pepin  10.00 2.00     LAMP       NA      4.00 2021 W34                 -
    ## 292    Pepin  10.00 2.00      RBT       NA      5.10 2021 W34                 -
    ## 293    Pepin  10.00 2.00     LAMP       NA      7.00 2021 W34                 -
    ## 294    Pepin  10.00 2.00       CO       NA      6.10 2021 W34                 -
    ## 295    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34                 -
    ## 296    Pepin  10.00 2.00      RBT       NA      3.40 2021 W34                 -
    ## 297    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34                 -
    ## 298    Pepin  10.00 2.00      STK       NA      3.00 2021 W34                 -
    ## 299    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34                 -
    ## 300    Pepin  10.00 2.00       CO       NA      5.20 2021 W34                 -
    ## 301    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34                 -
    ## 302    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34                 -
    ## 303    Pepin  10.00 2.00      STK       NA      4.90 2021 W34                 -
    ## 304    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 305    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 306    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 307    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 308    Pepin  10.00 3.00       CO       NA      6.50 2021 W34               431
    ## 309    Pepin  10.00 3.00      RBT       NA      3.60 2021 W34                 -
    ## 310    Pepin  10.00 3.00     LAMP       NA      5.00 2021 W34                 -
    ## 311    Pepin  10.00 3.00       CO       NA      5.80 2021 W34                 -
    ## 312    Pepin  10.00 3.00      STK       NA      5.40 2021 W34                 -
    ## 313    Pepin  10.00 3.00     LAMP       NA      7.00 2021 W34                 -
    ## 314    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34                 -
    ## 315    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34                 -
    ## 316    Pepin  10.00 3.00      STK       NA      5.50 2021 W34                 -
    ## 317    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34                 -
    ## 318    Pepin  10.00 3.00     LAMP       NA      9.00 2021 W34                 -
    ## 319    Pepin  10.00 3.00     LAMP       NA     11.00 2021 W34                 -
    ## 320    Pepin   3.00 1.00      CUT    12.20     10.50 2021 W34               447
    ## 321    Pepin   3.00 1.00      STK     0.40      3.10 2021 W34                 -
    ## 322    Pepin   3.00 1.00       CO     3.10      6.60 2021 W34                 -
    ## 323    Pepin   3.00 1.00     LAMP     0.90      7.00 2021 W34                 -
    ## 324    Pepin   3.00 1.00      RBT     2.00      5.50 2021 W34                 -
    ## 325    Pepin   3.00 1.00     LAMP     0.70      6.50 2021 W34                 -
    ## 326    Pepin   3.00 1.00      STK     0.70      3.90 2021 W34                 -
    ## 327    Pepin   3.00 1.00      RBT     1.10      4.60 2021 W34                 -
    ## 328    Pepin   3.00 1.00      STK     0.80      4.10 2021 W34                 -
    ## 329    Pepin   3.00 1.00      STK     0.10      2.50 2021 W34                 -
    ## 330    Pepin   3.00 1.00      STK     0.80      4.20 2021 W34                 -
    ## 331    Pepin   3.00 1.00     LAMP     0.70      7.00 2021 W34                 -
    ## 332    Pepin   3.00 1.00      STK     0.80      4.00 2021 W34                 -
    ## 333    Pepin   3.00 1.00     LAMP     0.40      6.50 2021 W34                 -
    ## 334    Pepin   3.00 1.00     LAMP     1.30      8.00 2021 W34                 -
    ## 335    Pepin   3.00 1.00     LAMP     0.50      7.00 2021 W34                 -
    ## 336    Pepin   3.00 1.00     LAMP     0.70      7.00 2021 W34                 -
    ## 337    Pepin   3.00 1.00      STK     0.80      4.10 2021 W34                 -
    ## 338    Pepin   3.00 1.00      STK     2.30      5.80 2021 W34                 -
    ## 339    Pepin   3.00 1.00     LAMP     1.00      8.00 2021 W34                 -
    ## 340    Pepin   3.00 1.00      STK     0.30      3.20 2021 W34                 -
    ## 341    Pepin   3.00 1.00      STK     0.70      3.70 2021 W34                 -
    ## 342    Pepin   3.00 1.00      RBT     0.90      4.40 2021 W34                 -
    ## 343    Pepin   3.00 1.00      STK     0.30      3.10 2021 W34                 -
    ## 344    Pepin   3.00 1.00      CUT     2.00      5.70 2021 W34                 -
    ## 345    Pepin   3.00 1.00     LAMP     0.70      6.00 2021 W34                 -
    ## 346    Pepin   3.00 1.00     LAMP     0.50      6.00 2021 W34                 -
    ## 347    Pepin   3.00 1.00     LAMP     0.90      7.50 2021 W34                 -
    ## 348    Pepin   3.00 2.00     LAMP     1.00      8.00 2021 W34               440
    ## 349    Pepin   3.00 2.00     LAMP     0.50      6.50 2021 W34                 -
    ## 350    Pepin   3.00 2.00     LAMP     0.60      8.00 2021 W34                 -
    ## 351    Pepin   3.00 2.00      RBT     0.90      4.50 2021 W34                 -
    ## 352    Pepin   3.00 2.00     LAMP     1.60      9.00 2021 W34                 -
    ## 353    Pepin   3.00 2.00     LAMP     1.20      8.00 2021 W34                 -
    ## 354    Pepin   3.00 2.00     LAMP     1.30      9.00 2021 W34                 -
    ## 355    Pepin   3.00 2.00     LAMP     1.20      7.50 2021 W34                 -
    ## 356    Pepin   3.00 2.00     LAMP     1.00      7.50 2021 W34                 -
    ## 357    Pepin   3.00 2.00     LAMP     1.20     10.00 2021 W34                 -
    ## 358    Pepin   3.00 2.00     LAMP     0.40      6.00 2021 W34                 -
    ## 359    Pepin   3.00 2.00     LAMP     1.20      8.00 2021 W34                 -
    ## 360    Pepin   3.00 2.00     LAMP     0.60      7.00 2021 W34                 -
    ## 361    Pepin   3.00 2.00     LAMP     0.30      4.50 2021 W34                 -
    ## 362    Pepin   3.00 2.00     LAMP     1.20      9.00 2021 W34                 -
    ## 363    Pepin   3.00 2.00      RBT     2.00      5.50 2021 W34                 -
    ## 364    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 365    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 366    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 367    Pepin   3.00 3.00      STK     0.40      3.50 2021 W34               474
    ## 368    Pepin   3.00 3.00      STK     0.90      4.50 2021 W34                 -
    ## 369    Pepin   3.00 3.00      STK     0.40      3.00 2021 W34                 -
    ## 370    Pepin   3.00 3.00      STK     0.60      4.00 2021 W34                 -
    ## 371    Pepin   3.00 3.00     LAMP     0.03      3.00 2021 W34                 -
    ## 372    Pepin   3.00 3.00     LAMP     0.70      7.00 2021 W34                 -
    ## 373    Pepin   3.00 3.00     LAMP     0.40      6.00 2021 W34                 -
    ## 374    Pepin   3.00 3.00      RBT     0.70      4.50 2021 W34                 -
    ## 375    Pepin   3.00 3.00     LAMP     0.90      7.00 2021 W34                 -
    ## 376    Pepin   3.00 3.00     LAMP     1.20      8.00 2021 W34                 -
    ## 377    Pepin   3.00 3.00      STK     0.50      3.50 2021 W34                 -
    ## 378    Pepin   3.00 3.00     LAMP     0.50      7.00 2021 W34                 -
    ## 379    Pepin   3.00 3.00      STK     0.50      3.50 2021 W34                 -
    ## 380    Pepin   3.00 3.00     LAMP     0.90      7.50 2021 W34                 -
    ## 381    Pepin   3.00 3.00     LAMP     0.30      4.00 2021 W34                 -
    ## 382    Pepin   3.00 3.00     LAMP     1.20      9.00 2021 W34                 -
    ## 383    Pepin   7.00 1.00      STK     2.10      5.50 2021 W34               441
    ## 384    Pepin   7.00 1.00       CO     2.50      6.50 2021 W34                 -
    ## 385    Pepin   7.00 1.00      RBT     2.90      7.00 2021 W34                 -
    ## 386    Pepin   7.00 1.00      CUT     1.10      4.50 2021 W34                 -
    ## 387    Pepin   7.00 1.00      DAC     8.60      9.50 2021 W34                 -
    ## 388    Pepin   7.00 1.00       CO     4.40      7.50 2021 W34                 -
    ## 389    Pepin   7.00 1.00      STK     1.20      4.00 2021 W34                 -
    ## 390    Pepin   7.00 1.00      CUT    11.68     11.00 2021 W34                 -
    ## 391    Pepin   7.00 1.00      RBT     2.30      6.00 2021 W34                 -
    ## 392    Pepin   7.00 1.00       CO     2.70      6.50 2021 W34                 -
    ## 393    Pepin   7.00 1.00      CUT    23.80     14.50 2021 W34                 -
    ## 394    Pepin   7.00 1.00       CO     4.50      7.50 2021 W34                 -
    ## 395    Pepin   7.00 1.00       CO     4.90      7.50 2021 W34                 -
    ## 396    Pepin   7.00 1.00      CUT     9.10     10.00 2021 W34                 -
    ## 397    Pepin   7.00 1.00     LAMP     0.50      6.00 2021 W34                 -
    ## 398    Pepin   7.00 1.00      CUT    13.80     11.00 2021 W34                 -
    ## 399    Pepin   7.00 1.00      CUT     1.50      5.50 2021 W34                 -
    ## 400    Pepin   7.00 1.00      CUT    17.80     12.00 2021 W34                 -
    ## 401    Pepin   7.00 1.00       CO     2.20      5.80 2021 W34                 -
    ## 402    Pepin   7.00 1.00      STK     0.20      3.00 2021 W34                 -
    ## 403    Pepin   7.00 1.00      RBT     2.80      6.50 2021 W34                 -
    ## 404    Pepin   7.00 1.00       CO     2.80      6.00 2021 W34                 -
    ## 405    Pepin   7.00 1.00      CUT     9.10      9.80 2021 W34                 -
    ## 406    Pepin   7.00 1.00       CO     2.90      7.00 2021 W34                 -
    ## 407    Pepin   7.00 1.00     LAMP     4.90     12.00 2021 W34                 -
    ## 408    Pepin   7.00 1.00       CO     0.70      4.50 2021 W34                 -
    ## 409    Pepin   7.00 1.00       CO     2.00      5.50 2021 W34                 -
    ## 410    Pepin   7.00 1.00      RBT     0.90      4.50 2021 W34                 -
    ## 411    Pepin   7.00 1.00      STK     1.40      5.00 2021 W34                 -
    ## 412    Pepin   7.00 1.00       CO     4.00      7.00 2021 W34                 -
    ## 413    Pepin   7.00 1.00       CO     1.80      5.30 2021 W34                 -
    ## 414    Pepin   7.00 1.00      CUT    49.50     17.20 2021 W34                 -
    ## 415    Pepin   7.00 1.00      CUT     8.20      9.50 2021 W34                 -
    ## 416    Pepin   7.00 1.00       CO     1.80      5.50 2021 W34                 -
    ## 417    Pepin   7.00 1.00      RBT     0.90      4.40 2021 W34                 -
    ## 418    Pepin   7.00 1.00       CO     2.50      6.00 2021 W34                 -
    ## 419    Pepin   7.00 1.00      RBT     1.30      4.60 2021 W34                 -
    ## 420    Pepin   7.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 421    Pepin   7.00 2.00      RBT     0.70      4.00 2021 W34               441
    ## 422    Pepin   7.00 2.00      RBT     2.30      6.50 2021 W34                 -
    ## 423    Pepin   7.00 2.00      CUT     1.70      5.50 2021 W34                 -
    ## 424    Pepin   7.00 2.00       CO     1.80      5.50 2021 W34                 -
    ## 425    Pepin   7.00 2.00      CUT     7.60      9.00 2021 W34                 -
    ## 426    Pepin   7.00 2.00     LAMP     0.90      8.00 2021 W34                 -
    ## 427    Pepin   7.00 2.00      CUT     2.80      6.00 2021 W34                 -
    ## 428    Pepin   7.00 2.00     LAMP     0.70      7.00 2021 W34                 -
    ## 429    Pepin   7.00 2.00      DAC     8.80      9.50 2021 W34                 -
    ## 430    Pepin   7.00 2.00     LAMP     0.70      7.00 2021 W34                 -
    ## 431    Pepin   7.00 2.00     LAMP     0.40      6.00 2021 W34                 -
    ## 432    Pepin   7.00 2.00     LAMP     1.90     10.00 2021 W34                 -
    ## 433    Pepin   7.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 434    Pepin   7.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 435    Pepin   7.00 3.00     LAMP     0.20      4.00 2021 W34               454
    ## 436    Pepin   7.00 3.00     LAMP     1.00      8.00 2021 W34                 -
    ## 437    Pepin   7.00 3.00      STK     0.70      4.00 2021 W34                 -
    ## 438    Pepin   7.00 3.00       CO     3.40      6.50 2021 W34                 -
    ## 439    Pepin   7.00 3.00      CUT     7.00      9.00 2021 W34                 -
    ## 440    Pepin   7.00 3.00     LAMP     0.50      6.00 2021 W34                 -
    ## 441    Pepin   7.00 3.00     LAMP     2.10     10.00 2021 W34                 -
    ## 442    Pepin   7.00 3.00     LAMP     1.90     10.00 2021 W34                 -
    ## 443    Pepin   7.00 3.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 444    Pepin   5.00 1.00      STK     1.70      5.50 2021 W34               397
    ## 445    Pepin   5.00 1.00       CO     3.60      7.00 2021 W34                 -
    ## 446    Pepin   5.00 1.00      CUT     4.70      8.00 2021 W34                 -
    ## 447    Pepin   5.00 1.00      STK     0.70      4.00 2021 W34                 -
    ## 448    Pepin   5.00 1.00      STK     0.20      2.50 2021 W34                 -
    ## 449    Pepin   5.00 1.00      DAC     7.30      9.00 2021 W34                 -
    ## 450    Pepin   5.00 1.00      CUT     5.00      7.50 2021 W34                 -
    ## 451    Pepin   5.00 1.00      STK     1.80      5.00 2021 W34                 -
    ## 452    Pepin   5.00 1.00       CO     2.30      6.00 2021 W34                 -
    ## 453    Pepin   5.00 1.00       CO     3.20      6.50 2021 W34                 -
    ## 454    Pepin   5.00 1.00      CUT     1.70      5.50 2021 W34                 -
    ## 455    Pepin   5.00 1.00     LAMP     0.60      6.00 2021 W34                 -
    ## 456    Pepin   5.00 1.00      RBT     0.90      4.50 2021 W34                 -
    ## 457    Pepin   5.00 1.00     LAMP     2.30     10.00 2021 W34                 -
    ## 458    Pepin   5.00 1.00     LAMP     2.80     12.00 2021 W34                 -
    ## 459    Pepin   5.00 1.00     LAMP     0.90      8.00 2021 W34                 -
    ## 460    Pepin   5.00 1.00      STK     2.90      5.50 2021 W34                 -
    ## 461    Pepin   5.00 1.00     LAMP     0.60      6.00 2021 W34                 -
    ## 462    Pepin   5.00 1.00     LAMP     0.40      6.00 2021 W34                 -
    ## 463    Pepin   5.00 1.00      STK     1.90      5.00 2021 W34                 -
    ## 464    Pepin   5.00 1.00      RBT     1.00      4.50 2021 W34                 -
    ## 465    Pepin   5.00 1.00      STK     0.80      4.00 2021 W34                 -
    ## 466    Pepin   5.00 1.00     LAMP     1.00      8.00 2021 W34                 -
    ## 467    Pepin   5.00 1.00     LAMP     0.40      6.00 2021 W34                 -
    ## 468    Pepin   5.00 1.00     LAMP     2.10     11.00 2021 W34                 -
    ## 469    Pepin   5.00 1.00     LAMP     2.90     10.00 2021 W34                 -
    ## 470    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 471    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 472    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 473    Pepin   5.00 2.00      RBT     1.70      5.00 2021 W34               401
    ## 474    Pepin   5.00 2.00     LAMP     1.40      9.00 2021 W34                 -
    ## 475    Pepin   5.00 2.00     LAMP     0.40      6.00 2021 W34                 -
    ## 476    Pepin   5.00 2.00     LAMP     0.70      6.50 2021 W34                 -
    ## 477    Pepin   5.00 2.00      RBT     2.30      6.00 2021 W34                 -
    ## 478    Pepin   5.00 2.00       CO     2.20      6.00 2021 W34                 -
    ## 479    Pepin   5.00 2.00     LAMP     0.40      5.00 2021 W34                 -
    ## 480    Pepin   5.00 2.00     LAMP     0.80      7.00 2021 W34                 -
    ## 481    Pepin   5.00 2.00     LAMP     0.40      5.00 2021 W34                 -
    ## 482    Pepin   5.00 2.00     LAMP     0.50      6.50 2021 W34                 -
    ## 483    Pepin   5.00 2.00     LAMP     0.60      6.50 2021 W34                 -
    ## 484    Pepin   5.00 2.00     LAMP     2.20     10.00 2021 W34                 -
    ## 485    Pepin   5.00 2.00      STK     0.30      2.50 2021 W34                 -
    ## 486    Pepin   5.00 3.00     LAMP     1.30     10.00 2021 W34               395
    ## 487    Pepin   5.00 3.00     LAMP     0.60      6.50 2021 W34                 -
    ## 488    Pepin   5.00 3.00     LAMP     2.40     10.00 2021 W34                 -
    ## 489    Pepin   5.00 3.00     LAMP     1.80      8.00 2021 W34                 -
    ## 490    Pepin   5.00 3.00     LAMP     0.80      6.00 2021 W34                 -
    ## 491    Pepin   5.00 3.00      RBT     1.40      5.50 2021 W34                 -
    ## 492    Pepin   5.00 3.00      CUT     2.40      6.00 2021 W34                 -
    ## 493    Pepin   5.00 3.00     LAMP     0.60      6.00 2021 W34                 -
    ## 494    Pepin   5.00 3.00     LAMP     0.70      6.00 2021 W34                 -
    ## 495    Pepin   5.00 3.00     LAMP     0.60      7.00 2021 W34                 -
    ## 496    Pepin   5.00 0.03     LAMP     1.50      9.00 2021 W34                 -
    ## 497    Pepin   5.00 3.00     LAMP     0.60      5.50 2021 W34                 -
    ## 498    Pepin   5.00 3.00     LAMP     1.10      8.00 2021 W34                 -
    ## 499    Pepin   5.00 3.00     LAMP     1.20      9.00 2021 W34                 -
    ## 500    Pepin   5.00 3.00      CUT    10.50     12.00 2021 W34                 -
    ## 501    Pepin   5.75 1.00      CUT     3.63      6.60 2021 W34                 -
    ## 502    Pepin   5.75 1.00      CUT    14.66     11.30 2021 W34                 -
    ## 503    Pepin   5.75 1.00      CUT     9.87      9.80 2021 W34                 -
    ## 504    Pepin   5.75 1.00       CO     4.56      7.40 2021 W34                 -
    ## 505    Pepin   5.75 1.00       CO     3.26      6.50 2021 W34                 -
    ## 506    Pepin   5.75 1.00       CO     3.50      6.70 2021 W34                 -
    ## 507    Pepin   5.75 1.00       CO     0.93      4.60 2021 W34                 -
    ## 508    Pepin   5.75 1.00       CO     2.93      6.80 2021 W34                 -
    ## 509    Pepin   5.75 1.00     LAMP     1.55     10.10 2021 W34                 -
    ## 510    Pepin   5.75 1.00     LAMP     2.15     10.00 2021 W34                 -
    ## 511    Pepin   5.75 1.00      RBT    27.02     13.70 2021 W34                 -
    ## 512    Pepin   5.75 1.00      STK     2.06      5.70 2021 W34                 -
    ## 513    Pepin   5.75 1.00      STK     1.58      5.20 2021 W34                 -
    ## 514    Pepin   5.75 1.00      STK     0.95      4.10 2021 W34                 -
    ## 515    Pepin   5.75 1.00      STK     0.44      3.50 2021 W34                 -
    ## 516    Pepin   5.75 1.00      STK     0.43      2.90 2021 W34                 -
    ## 517    Pepin   5.75 1.00      STK     0.27      2.50 2021 W34                 -
    ## 518    Pepin   5.75 1.00    TROUT     2.11      5.60 2021 W34                 -
    ## 519    Pepin   5.75 1.00    TROUT     1.60      5.20 2021 W34                 -
    ## 520    Pepin   5.75 1.00    TROUT     1.67      5.90 2021 W34                 -
    ## 521    Pepin   5.75 1.00    TROUT     0.98      4.50 2021 W34                 -
    ## 522    Pepin   5.75 1.00    TROUT     0.93      4.50 2021 W34                 -
    ## 523    Pepin   5.75 1.00    TROUT     1.80      6.00 2021 W34                 -
    ## 524    Pepin   5.75 1.00    TROUT     0.71      4.00 2021 W34                 -
    ## 525    Pepin   5.75 1.00    TROUT     0.94      4.80 2021 W34                 -
    ## 526    Pepin   5.75 1.00    TROUT     0.58      3.90 2021 W34                 -
    ## 527    Pepin   5.75 1.00    TROUT     0.74      4.10 2021 W34                 -
    ## 528    Pepin   5.75 2.00      RBT     2.33      6.20 2021 W34                 -
    ## 529    Pepin   5.75 2.00       CO     1.72      5.50 2021 W34                 -
    ## 530    Pepin   5.75 2.00       CO     3.31      6.70 2021 W34                 -
    ## 531    Pepin   5.75 2.00      STK     0.45      3.70 2021 W34                 -
    ## 532    Pepin   5.75 2.00       CO     1.75      5.30 2021 W34                 -
    ## 533    Pepin   5.75 2.00      RBT     1.73      5.70 2021 W34                 -
    ## 534    Pepin   5.75 2.00      STK     0.11      3.60 2021 W34                 -
    ## 535    Pepin   5.75 2.00     LAMP     1.80      9.50 2021 W34                 -
    ## 536    Pepin   5.75 2.00     LAMP     1.81     10.00 2021 W34                 -
    ## 537    Pepin   5.75 2.00     LAMP     0.74      8.00 2021 W34                 -
    ## 538    Pepin   5.75 2.00     LAMP     1.35      9.00 2021 W34                 -
    ## 539    Pepin   5.75 3.00      STK     6.38      5.00 2021 W34                 -
    ## 540    Pepin   5.75 3.00     LAMP     2.79     11.00 2021 W34                 -
    ## 541    Pepin   5.75 3.00     LAMP     2.50     11.00 2021 W34                 -
    ## 542    Pepin   5.75 3.00     LAMP     1.21      8.00 2021 W34                 -
    ## 543    Pepin   5.75 3.00     LAMP     1.53     10.00 2021 W34                 -
    ## 544    Pepin   5.75 3.00     LAMP     1.15      8.50 2021 W34                 -
    ## 545    Pepin   5.75 3.00       CO     3.18      6.70 2021 W34                 -
    ## 546    Pepin   5.75 3.00      STK     1.50      4.90 2021 W34                 -
    ## 547    Pepin   5.75 3.00      RBT     1.19      5.10 2021 W34                 -
    ## 548    Pepin   5.90 1.00      RBT    75.40     19.10 2021 W34                 -
    ## 549    Pepin   5.90 1.00    TROUT     0.70      4.00 2021 W34                 -
    ## 550    Pepin   5.90 1.00    TROUT     1.60      5.30 2021 W34                 -
    ## 551    Pepin   5.90 1.00    TROUT     0.70      4.30 2021 W34                 -
    ## 552    Pepin   5.90 1.00    TROUT     3.00      6.40 2021 W34                 -
    ## 553    Pepin   5.90 1.00    TROUT     1.00      4.40 2021 W34                 -
    ## 554    Pepin   5.90 2.00     LAMP     1.26      8.00 2021 W34                 -
    ## 555    Pepin   5.90 2.00     LAMP     1.47      8.50 2021 W34                 -
    ## 556    Pepin   5.90 2.00     LAMP     1.73      9.00 2021 W34                 -
    ## 557    Pepin   5.90 2.00    TROUT     0.40      3.50 2021 W34                 -
    ## 558    Pepin   5.90 2.00    TROUT     0.95      4.40 2021 W34                 -
    ## 559    Pepin   5.90 2.00    TROUT       NA      4.50 2021 W34                 -
    ## 560    Pepin   5.90 2.00    TROUT     0.55      3.90 2021 W34                 -
    ## 561    Pepin   8.00 1.00      CUT     6.64      8.50 2021 W34                 -
    ## 562    Pepin   8.00 1.00      CUT    10.67     10.10 2021 W34                 -
    ## 563    Pepin   8.00 1.00      CUT    14.97     11.30 2021 W34                 -
    ## 564    Pepin   8.00 1.00      CUT    10.13      9.50 2021 W34                 -
    ## 565    Pepin   8.00 1.00      CUT     7.04      9.40 2021 W34                 -
    ## 566    Pepin   8.00 1.00      CUT     2.60      5.60 2021 W34                 -
    ## 567    Pepin   8.00 1.00      CUT     0.71      3.90 2021 W34                 -
    ## 568    Pepin   8.00 1.00      CUT     1.46      5.40 2021 W34                 -
    ## 569    Pepin   8.00 1.00      CUT     2.77      6.20 2021 W34                 -
    ## 570    Pepin   8.00 1.00      CUT     2.46      6.00 2021 W34                 -
    ## 571    Pepin   8.00 1.00       CO     4.59      8.40 2021 W34                 -
    ## 572    Pepin   8.00 1.00       CO     3.89      7.00 2021 W34                 -
    ## 573    Pepin   8.00 1.00       CO     3.78      6.50 2021 W34                 -
    ## 574    Pepin   8.00 1.00       CO     4.12      6.80 2021 W34                 -
    ## 575    Pepin   8.00 1.00       CO     4.12      6.90 2021 W34                 -
    ## 576    Pepin   8.00 1.00       CO     2.92      6.20 2021 W34                 -
    ## 577    Pepin   8.00 1.00       CO     2.45      5.60 2021 W34                 -
    ## 578    Pepin   8.00 1.00       CO     1.48      5.40 2021 W34                 -
    ## 579    Pepin   8.00 1.00       CO       NA      5.20 2021 W34                 -
    ## 580    Pepin   8.00 1.00       CO     1.28      4.40 2021 W34                 -
    ## 581    Pepin   8.00 1.00     LAMP     3.33     10.00 2021 W34                 -
    ## 582    Pepin   8.00 1.00     LAMP     3.40     13.00 2021 W34                 -
    ## 583    Pepin   8.00 1.00     LAMP     3.76     12.00 2021 W34                 -
    ## 584    Pepin   8.00 1.00     LAMP     2.00      9.00 2021 W34                 -
    ## 585    Pepin   8.00 1.00     LAMP     1.12      6.00 2021 W34                 -
    ## 586    Pepin   8.00 1.00     LAMP     0.94      7.00 2021 W34                 -
    ## 587    Pepin   8.00 1.00     LAMP     0.47      6.50 2021 W34                 -
    ## 588    Pepin   8.00 1.00     LAMP     0.35      6.00 2021 W34                 -
    ## 589    Pepin   8.00 1.00     LAMP     0.81      6.00 2021 W34                 -
    ## 590    Pepin   8.00 1.00     LAMP     0.77      7.50 2021 W34                 -
    ## 591    Pepin   8.00 1.00     LAMP     0.15      5.00 2021 W34                 -
    ## 592    Pepin   8.00 1.00      DAC     8.15      9.00 2021 W34                 -
    ## 593    Pepin   8.00 1.00      STK     2.21      5.80 2021 W34                 -
    ## 594    Pepin   8.00 1.00      STK     1.55      5.50 2021 W34                 -
    ## 595    Pepin   8.00 1.00      STK     3.48      5.50 2021 W34                 -
    ## 596    Pepin   8.00 1.00      STK     1.27      4.00 2021 W34                 -
    ## 597    Pepin   8.00 1.00    TROUT     2.01      5.30 2021 W34                 -
    ## 598    Pepin   8.00 1.00    TROUT     2.06      5.60 2021 W34                 -
    ## 599    Pepin   8.00 1.00    TROUT     1.89      5.30 2021 W34                 -
    ## 600    Pepin   8.00 1.00    TROUT     1.65      5.60 2021 W34                 -
    ## 601    Pepin   8.00 1.00    TROUT     1.82      4.90 2021 W34                 -
    ## 602    Pepin   8.00 1.00    TROUT     0.95      3.90 2021 W34                 -
    ## 603    Pepin   8.00 1.00    TROUT     1.73      5.50 2021 W34                 -
    ## 604    Pepin   8.00 1.00    TROUT     0.94      4.30 2021 W34                 -
    ## 605    Pepin   8.00 1.00    TROUT     1.41      4.40 2021 W34                 -
    ## 606    Pepin   8.00 1.00    TROUT     2.15      5.40 2021 W34                 -
    ## 607    Pepin   8.00 1.00    TROUT     0.87      4.50 2021 W34                 -
    ## 608    Pepin   8.00 2.00     LAMP     1.31      9.00 2021 W34                 -
    ## 609    Pepin   8.00 2.00     LAMP     1.20      7.00 2021 W34                 -
    ## 610    Pepin   8.00 2.00     LAMP     0.66      6.00 2021 W34                 -
    ## 611    Pepin   8.00 2.00      RBT     9.60      9.80 2021 W34                 -
    ## 612    Pepin   8.00 2.00      STK     0.63      3.50 2021 W34                 -
    ## 613    Pepin   8.00 2.00      STK     1.49      5.90 2021 W34                 -
    ## 614    Pepin   8.00 2.00    TROUT     1.55      5.40 2021 W34                 -
    ## 615    Pepin   8.00 3.00      CUT     6.54      8.80 2021 W34                 -
    ## 616    Pepin   8.00 3.00      RBT     0.68      4.20 2021 W34                 -
    ## 617    Pepin   8.00 3.00      DAC     6.15      8.70 2021 W34                 -
    ## 618    Pepin   8.00 3.00     LAMP     2.68     11.00 2021 W34                 -
    ## 619 Bertrand   7.00 1.00      DAC     1.10      4.25 2021 W34               209
    ## 620 Bertrand   7.00 1.00      CUT     2.40      7.00 2021 W34                 -
    ## 621 Bertrand   7.00 1.00      DAC     0.70      4.00 2021 W34                 -
    ## 622 Bertrand   7.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 623 Bertrand   7.00 2.00      DAC     2.90      5.50 2021 W34               197
    ## 624 Bertrand   7.00 3.00      DAC     0.90      3.70 2021 W34                 -
    ## 625 Bertrand   6.00 1.00      DAC       NA        NA 2021 W34               292
    ## 626 Bertrand   6.00 1.00      DAC     2.70      6.00 2021 W34                 -
    ## 627 Bertrand   6.00 1.00      DAC     3.80      7.00 2021 W34                 -
    ## 628 Bertrand   6.00 1.00     LAMP     3.90     12.00 2021 W34                 -
    ## 629 Bertrand   6.00 1.00      DAC     4.60      7.50 2021 W34                 -
    ## 630 Bertrand   6.00 1.00      CUT     3.30      6.50 2021 W34                 -
    ## 631 Bertrand   6.00 1.00      CUT     3.40      6.50 2021 W34                 -
    ## 632 Bertrand   6.00 1.00      CUT     4.90      7.50 2021 W34                 -
    ## 633 Bertrand   6.00 1.00      RBT     3.60      6.50 2021 W34                 -
    ## 634 Bertrand   6.00 1.00      DAC     4.00      7.25 2021 W34                 -
    ## 635 Bertrand   6.00 1.00      DAC     2.40      6.00 2021 W34                 -
    ## 636 Bertrand   6.00 1.00      DAC     3.60      7.00 2021 W34                 -
    ## 637 Bertrand   6.00 1.00      DAC     3.10      6.50 2021 W34                 -
    ## 638 Bertrand   6.00 1.00      DAC     3.10      7.00 2021 W34                 -
    ## 639 Bertrand   6.00 1.00      DAC     5.30      7.50 2021 W34                 -
    ## 640 Bertrand   6.00 1.00      DAC     3.00      6.50 2021 W34                 -
    ## 641 Bertrand   6.00 1.00      DAC     2.40      6.00 2021 W34                 -
    ## 642 Bertrand   6.00 1.00      DAC     4.00      7.00 2021 W34                 -
    ## 643 Bertrand   6.00 1.00      CUT     3.40      6.50 2021 W34                 -
    ## 644 Bertrand   6.00 1.00      RBT     4.40      7.00 2021 W34                 -
    ## 645 Bertrand   6.00 1.00      CUT     4.50      7.25 2021 W34                 -
    ## 646 Bertrand   6.00 1.00      DAC     2.60      6.50 2021 W34                 -
    ## 647 Bertrand   6.00 1.00      DAC     4.20      7.00 2021 W34                 -
    ## 648 Bertrand   6.00 1.00      DAC     4.10      7.50 2021 W34                 -
    ## 649 Bertrand   6.00 1.00      CUT     2.80      6.00 2021 W34                 -
    ## 650 Bertrand   6.00 1.00      RBT     1.40      5.00 2021 W34                 -
    ## 651 Bertrand   6.00 1.00      CUT     5.30      7.50 2021 W34                 -
    ## 652 Bertrand   6.00 1.00      DAC     2.90      6.00 2021 W34                 -
    ## 653 Bertrand   6.00 1.00      DAC     2.60      6.00 2021 W34                 -
    ## 654 Bertrand   6.00 1.00      DAC     4.00      6.50 2021 W34                 -
    ## 655 Bertrand   6.00 1.00      DAC     2.40      5.50 2021 W34                 -
    ## 656 Bertrand   6.00 1.00      DAC     3.70      7.00 2021 W34                 -
    ## 657 Bertrand   6.00 1.00      DAC     1.00      4.00 2021 W34                 -
    ## 658 Bertrand   6.00 1.00      DAC     2.60      6.50 2021 W34                 -
    ## 659 Bertrand   6.00 1.00      DAC     1.60      5.50 2021 W34                 -
    ## 660 Bertrand   6.00 1.00      DAC     0.90      3.50 2021 W34                 -
    ## 661 Bertrand   6.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 662 Bertrand   6.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 663 Bertrand   6.00 2.00      CUT     4.80      7.00 2021 W34               364
    ## 664 Bertrand   6.00 2.00      DAC     2.70      6.00 2021 W34                 -
    ## 665 Bertrand   6.00 2.00      DAC     3.40      7.00 2021 W34                 -
    ## 666 Bertrand   6.00 2.00      CUT     3.80      7.00 2021 W34                 -
    ## 667 Bertrand   6.00 2.00      DAC     3.10      7.50 2021 W34                 -
    ## 668 Bertrand   6.00 2.00      DAC     3.90      7.50 2021 W34                 -
    ## 669 Bertrand   6.00 2.00      RBT     2.00      5.50 2021 W34                 -
    ## 670 Bertrand   6.00 2.00      CUT     3.20      6.50 2021 W34                 -
    ## 671 Bertrand   6.00 2.00      CUT     3.50      6.50 2021 W34                 -
    ## 672 Bertrand   6.00 2.00      DAC     3.50      7.00 2021 W34                 -
    ## 673 Bertrand   6.00 2.00      DAC     3.70      7.00 2021 W34                 -
    ## 674 Bertrand   6.00 2.00      CUT     3.20      6.00 2021 W34                 -
    ## 675 Bertrand   6.00 2.00      RBT     2.10      5.50 2021 W34                 -
    ## 676 Bertrand   6.00 2.00      DAC     3.20      7.00 2021 W34                 -
    ## 677 Bertrand   6.00 2.00      DAC     2.50      6.00 2021 W34                 -
    ## 678 Bertrand   6.00 2.00      DAC     3.40      7.00 2021 W34                 -
    ## 679 Bertrand   6.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 680 Bertrand   6.00 3.00      DAC     3.50      6.90 2021 W34                 -
    ## 681 Bertrand   6.00 3.00      DAC     3.30      7.00 2021 W34                 -
    ## 682 Bertrand   6.00 3.00      DAC     4.20      7.50 2021 W34                 -
    ## 683 Bertrand   6.00 3.00      DAC     3.90      7.20 2021 W34                 -
    ## 684 Bertrand   6.00 3.00      DAC     2.50      6.40 2021 W34                 -
    ## 685 Bertrand   6.00 3.00      DAC     3.40      6.70 2021 W34                 -
    ## 686 Bertrand   6.00 3.00      CUT     3.80      6.50 2021 W34                 -
    ## 687 Bertrand   6.00 3.00      DAC     5.10      7.00 2021 W34                 -
    ## 688 Bertrand   6.00 3.00      DAC     5.50      6.90 2021 W34                 -
    ## 689 Bertrand   6.00 3.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 690 Bertrand   4.00 1.00      STK     1.18      4.50 2021 W34               194
    ## 691 Bertrand   4.00 1.00      DAC     0.92      4.20 2021 W34                 -
    ## 692 Bertrand   4.00 1.00      STK     0.93      4.70 2021 W34                 -
    ## 693 Bertrand   4.00 1.00      CUT     3.20      6.90 2021 W34                 -
    ## 694 Bertrand   4.00 1.00      DAC     2.50      6.50 2021 W34                 -
    ## 695 Bertrand   4.00 1.00      CUT     2.30      5.90 2021 W34                 -
    ## 696 Bertrand   4.00 1.00      DAC     0.93      4.30 2021 W34                 -
    ## 697 Bertrand   4.00 1.00      DAC     1.57      5.40 2021 W34                 -
    ## 698 Bertrand   4.00 1.00      DAC     0.82      4.40 2021 W34                 -
    ## 699 Bertrand   4.00 1.00      DAC     0.80      4.20 2021 W34                 -
    ## 700 Bertrand   4.00 1.00      DAC     3.44      7.30 2021 W34                 -
    ## 701 Bertrand   4.00 1.00      DAC     3.72      7.50 2021 W34                 -
    ## 702 Bertrand   4.00 1.00      DAC     4.35      7.70 2021 W34                 -
    ## 703 Bertrand   4.00 1.00      DAC     2.24      6.10 2021 W34                 -
    ## 704 Bertrand   4.00 1.00      RBT     4.13      7.30 2021 W34                 -
    ## 705 Bertrand   4.00 1.00      STK     1.65      5.00 2021 W34                 -
    ## 706 Bertrand   4.00 1.00      DAC     2.26      6.10 2021 W34                 -
    ## 707 Bertrand   4.00 1.00      STK     1.36      4.80 2021 W34                 -
    ## 708 Bertrand   4.00 1.00      STK     1.21      4.70 2021 W34                 -
    ## 709 Bertrand   4.00 1.00      DAC     2.87      6.70 2021 W34                 -
    ## 710 Bertrand   4.00 1.00      DAC     2.98      6.50 2021 W34                 -
    ## 711 Bertrand   4.00 1.00      STK     0.39      3.80 2021 W34                 -
    ## 712 Bertrand   4.00 1.00      STK     1.32      4.80 2021 W34                 -
    ## 713 Bertrand   4.00 1.00      STK     0.54      3.90 2021 W34                 -
    ## 714 Bertrand   4.00 1.00      DAC     2.21      6.00 2021 W34                 -
    ## 715 Bertrand   4.00 1.00      STK     0.77      4.20 2021 W34                 -
    ## 716 Bertrand   4.00 1.00      DAC     1.73      5.80 2021 W34                 -
    ## 717 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 718 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 719 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 720 Bertrand   4.00 2.00      RBT     3.22      6.40 2021 W34               233
    ## 721 Bertrand   4.00 2.00      RBT     3.94      6.90 2021 W34                 -
    ## 722 Bertrand   4.00 2.00      DAC     2.20      5.80 2021 W34                 -
    ## 723 Bertrand   4.00 2.00      RBT     3.30      6.60 2021 W34                 -
    ## 724 Bertrand   4.00 2.00      STK     1.40      5.00 2021 W34                 -
    ## 725 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34                 -
    ## 726 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34                 -
    ## 727 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34                 -
    ## 728 Bertrand   4.00 2.00     LAMP     4.90     14.10 2021 W34                 -
    ## 729 Bertrand   4.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 730 Bertrand   4.00 3.00      DAC     3.30      7.30 2021 W34               236
    ## 731 Bertrand   4.00 3.00      DAC     2.00      6.10 2021 W34                 -
    ## 732 Bertrand   4.00 3.00      STK     1.40      4.90 2021 W34                 -
    ## 733 Bertrand   4.00 3.00      STK     1.00      5.50 2021 W34                 -
    ## 734 Bertrand   4.00 3.00      DAC     5.90      8.90 2021 W34                 -
    ## 735 Bertrand   4.00 3.00      RBT     4.00      7.40 2021 W34                 -
    ## 736 Bertrand   4.00 3.00      DAC     2.00      5.50 2021 W34                 -
    ## 737 Bertrand   3.50 1.00      RBT    23.00     13.00 2021 W34               209
    ## 738 Bertrand   3.50 1.00      STK     1.30      4.50 2021 W34                 -
    ## 739 Bertrand   3.50 1.00      DAC     1.50      3.40 2021 W34                 -
    ## 740 Bertrand   3.50 1.00      DAC     2.20      5.50 2021 W34                 -
    ## 741 Bertrand   3.50 2.00      DAC     0.70      4.00 2021 W34               211
    ## 742 Bertrand   3.50 3.00      STK     0.70      3.80 2021 W34               196
    ## 743 Bertrand   3.00 1.00      STK     1.20      4.00 2021 W34               217
    ## 744 Bertrand   3.00 1.00      DAC     4.20      7.00 2021 W34                 -
    ## 745 Bertrand   3.00 1.00      DAC     3.30      7.00 2021 W34                 -
    ## 746 Bertrand   3.00 1.00      DAC     1.00      4.50 2021 W34                 -
    ## 747 Bertrand   3.00 1.00      DAC     0.90      4.50 2021 W34                 -
    ## 748 Bertrand   3.00 1.00      RBT     3.10      6.50 2021 W34                 -
    ## 749 Bertrand   3.00 1.00      CUT     5.30      7.50 2021 W34                 -
    ## 750 Bertrand   3.00 1.00      DAC     4.72      7.40 2021 W34                 -
    ## 751 Bertrand   3.00 1.00      DAC     4.35      6.20 2021 W34                 -
    ## 752 Bertrand   3.00 1.00      STK     2.25      4.20 2021 W34                 -
    ## 753 Bertrand   3.00 1.00      STK     1.75      4.50 2021 W34                 -
    ## 754 Bertrand   3.00 1.00      DAC     3.60      6.30 2021 W34                 -
    ## 755 Bertrand   3.00 1.00      DAC     3.50      6.50 2021 W34                 -
    ## 756 Bertrand   3.00 1.00      DAC     2.35      4.60 2021 W34                 -
    ## 757 Bertrand   3.00 2.00      CUT     3.30      6.50 2021 W34               207
    ## 758 Bertrand   3.00 2.00      DAC     0.90      4.00 2021 W34                 -
    ## 759 Bertrand   3.00 2.00      CUT     4.70      7.50 2021 W34                 -
    ## 760 Bertrand   3.00 2.00      DAC     1.00      4.00 2021 W34                 -
    ## 761 Bertrand   3.00 2.00      DAC     2.30      6.00 2021 W34                 -
    ## 762 Bertrand   3.00 2.00      DAC     0.80      3.50 2021 W34                 -
    ## 763 Bertrand   3.00 2.00      DAC     2.91      6.50 2021 W34                 -
    ## 764 Bertrand   3.00 2.00      CUT     3.64      7.50 2021 W34                 -
    ## 765 Bertrand   3.00 2.00      DAC     5.05      8.00 2021 W34                 -
    ## 766 Bertrand   3.00 2.00      DAC     2.22      5.50 2021 W34                 -
    ## 767 Bertrand   3.00 3.00      DAC     5.60      8.00 2021 W34               202
    ## 768 Bertrand   3.00 3.00      DAC     0.73      4.00 2021 W34                 -
    ## 769 Bertrand   2.00 1.00      DAC     5.88      8.00 2021 W34               292
    ## 770 Bertrand   2.00 1.00      DAC     3.53      7.50 2021 W34                 -
    ## 771 Bertrand   2.00 1.00      DAC     3.76      7.00 2021 W34                 -
    ## 772 Bertrand   2.00 1.00      DAC     2.45      5.80 2021 W34                 -
    ## 773 Bertrand   2.00 1.00      DAC     2.77      6.50 2021 W34                 -
    ## 774 Bertrand   2.00 1.00      DAC     4.91      8.00 2021 W34                 -
    ## 775 Bertrand   2.00 1.00      DAC     0.74      3.40 2021 W34                 -
    ## 776 Bertrand   2.00 1.00      DAC     2.26      5.50 2021 W34                 -
    ## 777 Bertrand   2.00 1.00      DAC     3.71      7.00 2021 W34                 -
    ## 778 Bertrand   2.00 1.00      DAC     3.80      7.30 2021 W34                 -
    ## 779 Bertrand   2.00 1.00      DAC     8.40      9.00 2021 W34                 -
    ## 780 Bertrand   2.00 1.00      DAC     4.53      7.75 2021 W34                 -
    ## 781 Bertrand   2.00 1.00      DAC     4.12      7.50 2021 W34                 -
    ## 782 Bertrand   2.00 1.00      DAC     2.15      5.80 2021 W34                 -
    ## 783 Bertrand   2.00 2.00      DAC     4.65      5.75 2021 W34               330
    ## 784 Bertrand   2.00 2.00      DAC     1.56      5.25 2021 W34                 -
    ## 785 Bertrand   2.00 2.00      DAC     1.10      4.00 2021 W34                 -
    ## 786 Bertrand   2.00 2.00      DAC     4.59      8.00 2021 W34                 -
    ## 787 Bertrand   2.00 2.00      DAC     1.98      5.75 2021 W34                 -
    ## 788 Bertrand   2.00 2.00      DAC     2.66      6.50 2021 W34                 -
    ## 789 Bertrand   2.00 2.00      DAC     2.40      6.00 2021 W34                 -
    ## 790 Bertrand   2.00 2.00      DAC     1.92      6.00 2021 W34                 -
    ## 791 Bertrand   2.00 2.00      DAC     0.58      3.50 2021 W34                 -
    ## 792 Bertrand   2.00 2.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 793 Bertrand   2.00 3.00      DAC     2.80      6.00 2021 W34               282
    ## 794 Bertrand   2.00 3.00      CUT     3.55      6.50 2021 W34                 -
    ## 795 Bertrand   2.00 3.00      DAC     8.12      8.50 2021 W34                 -
    ## 796 Bertrand   2.00 3.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 797 Bertrand   2.00 4.00      DAC     3.89      6.40 2021 W34               274
    ## 798 Bertrand   2.00 4.00      DAC     1.74      6.20 2021 W34                 -
    ## 799 Bertrand   1.00 1.00      STK     0.59      3.80 2021 W34               200
    ## 800 Bertrand   1.00 1.00      CUT     2.23      6.50 2021 W34                 -
    ## 801 Bertrand   1.00 1.00      STK     1.42      4.50 2021 W34                 -
    ## 802 Bertrand   1.00 1.00      STK     1.52      4.50 2021 W34                 -
    ## 803 Bertrand   1.00 1.00      STK     1.46      4.50 2021 W34                 -
    ## 804 Bertrand   1.00 1.00      STK     1.50      4.75 2021 W34                 -
    ## 805 Bertrand   1.00 1.00      STK     1.00      4.40 2021 W34                 -
    ## 806 Bertrand   1.00 1.00      STK     1.34      4.50 2021 W34                 -
    ## 807 Bertrand   1.00 1.00      DAC     2.02      6.50 2021 W34                 -
    ## 808 Bertrand   1.00 1.00      DAC     3.14      6.50 2021 W34                 -
    ## 809 Bertrand   1.00 1.00      STK     1.33      4.50 2021 W34                 -
    ## 810 Bertrand   1.00 1.00      DAC     2.16      5.70 2021 W34                 -
    ## 811 Bertrand   1.00 1.00      STK     1.91      4.00 2021 W34                 -
    ## 812 Bertrand   1.00 1.00      STK     1.34      4.00 2021 W34                 -
    ## 813 Bertrand   1.00 1.00      CUT     3.24      6.40 2021 W34                 -
    ## 814 Bertrand   1.00 1.00       CO     3.51      6.50 2021 W34                 -
    ## 815 Bertrand   1.00 1.00      CUT     4.29      6.90 2021 W34                 -
    ## 816 Bertrand   1.00 1.00      RBT     1.81      5.00 2021 W34                 -
    ## 817 Bertrand   1.00 1.00      STK     1.64      4.00 2021 W34                 -
    ## 818 Bertrand   1.00 1.00      STK     0.99      4.10 2021 W34                 -
    ## 819 Bertrand   1.00 1.00      STK     1.26      3.90 2021 W34                 -
    ## 820 Bertrand   1.00 1.00      STK     1.33      4.50 2021 W34                 -
    ## 821 Bertrand   1.00 1.00      RBT     1.99      5.30 2021 W34                 -
    ## 822 Bertrand   1.00 1.00      DAC     1.08      4.40 2021 W34                 -
    ## 823 Bertrand   1.00 1.00      DAC     4.40      7.50 2021 W34                 -
    ## 824 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 825 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 826 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34                 -
    ## 827 Bertrand   1.00 2.00      STK     1.73      4.00 2021 W34               173
    ## 828 Bertrand   1.00 2.00      STK     1.64      4.00 2021 W34                 -
    ## 829 Bertrand   1.00 2.00      DAC     3.24      6.90 2021 W34                 -
    ## 830 Bertrand   1.00 2.00      CUT     2.62      6.00 2021 W34                 -
    ## 831 Bertrand   1.00 2.00      STK     1.44      4.50 2021 W34                 -
    ## 832 Bertrand   1.00 2.00      RBT     2.23      5.70 2021 W34                 -
    ## 833 Bertrand   1.00 2.00      CUT     3.11      6.40 2021 W34                 -
    ## 834 Bertrand   1.00 2.00      STK     1.08      4.00 2021 W34                 -
    ## 835 Bertrand   1.00 2.00      STK     1.17      4.50 2021 W34                 -
    ## 836 Bertrand   1.00 2.00      RBT     2.46      5.50 2021 W34                 -
    ## 837 Bertrand   1.00 2.00      CUT     3.56      6.60 2021 W34                 -
    ## 838 Bertrand   1.00 3.00      STK     1.25      4.60 2021 W34               194
    ## 839 Bertrand   1.00 3.00      STK     1.34      5.10 2021 W34                 -
    ## 840 Bertrand   0.50 1.00      DAC     2.65      5.80 2021 W34               135
    ## 841 Bertrand   0.50 1.00      DAC     2.51      5.50 2021 W34                 -
    ## 842 Bertrand   0.50 1.00      DAC     1.17      4.00 2021 W34                 -
    ## 843 Bertrand   0.50 1.00      DAC     1.15      4.50 2021 W34                 -
    ## 844 Bertrand   0.50 1.00      CUT     3.48      6.50 2021 W34                 -
    ## 845 Bertrand   0.50 1.00      CUT     5.06      7.70 2021 W34                 -
    ## 846 Bertrand   0.50 1.00      CUT     4.15      7.20 2021 W34                 -
    ## 847 Bertrand   0.50 2.00      DAC     2.07      6.00 2021 W34               130
    ## 848 Bertrand   0.50 2.00       CO     3.65      7.00 2021 W34                 -
    ## 849 Bertrand   0.50 2.00      RBT     1.24      4.40 2021 W34                 -
    ## 850 Bertrand   0.50 3.00      DAC     1.45      5.40 2021 W34               126
    ##     sampled_riffle_length_m
    ## 1                       6.4
    ## 2                         -
    ## 3                         -
    ## 4                         -
    ## 5                         -
    ## 6                         -
    ## 7                         -
    ## 8                         -
    ## 9                         -
    ## 10                        -
    ## 11                        -
    ## 12                        -
    ## 13                        -
    ## 14                        -
    ## 15                        -
    ## 16                        -
    ## 17                        -
    ## 18                        -
    ## 19                        -
    ## 20                        -
    ## 21                        -
    ## 22                        -
    ## 23                        -
    ## 24                        -
    ## 25                        -
    ## 26                        -
    ## 27                        -
    ## 28                        -
    ## 29                        -
    ## 30                        -
    ## 31                        -
    ## 32                        -
    ## 33                        -
    ## 34                        -
    ## 35                        -
    ## 36                        -
    ## 37                        -
    ## 38                        -
    ## 39                        -
    ## 40                        -
    ## 41                        -
    ## 42                        -
    ## 43                        -
    ## 44                        -
    ## 45                        -
    ## 46                        -
    ## 47                        -
    ## 48                        -
    ## 49                        -
    ## 50                        -
    ## 51                        -
    ## 52                        -
    ## 53                        -
    ## 54                        -
    ## 55                        -
    ## 56                        -
    ## 57                        -
    ## 58                        -
    ## 59                        -
    ## 60                        -
    ## 61                        -
    ## 62                        -
    ## 63                        -
    ## 64                        -
    ## 65                        -
    ## 66                      3.5
    ## 67                        -
    ## 68                        -
    ## 69                        -
    ## 70                        -
    ## 71                        -
    ## 72                        -
    ## 73                        -
    ## 74                        -
    ## 75                        -
    ## 76                        -
    ## 77                        -
    ## 78                        -
    ## 79                        -
    ## 80                        -
    ## 81                        -
    ## 82                        -
    ## 83                     10.6
    ## 84                        -
    ## 85                        -
    ## 86                        -
    ## 87                        -
    ## 88                        -
    ## 89                        -
    ## 90                        -
    ## 91                        -
    ## 92                        -
    ## 93                        -
    ## 94                        -
    ## 95                        -
    ## 96                        -
    ## 97                        -
    ## 98                        -
    ## 99                        -
    ## 100                       -
    ## 101                       -
    ## 102                       -
    ## 103                       -
    ## 104                       -
    ## 105                       -
    ## 106                       -
    ## 107                       -
    ## 108                       -
    ## 109                       -
    ## 110                       -
    ## 111                       -
    ## 112                       -
    ## 113                       -
    ## 114                       -
    ## 115                       -
    ## 116                       -
    ## 117                       -
    ## 118                       -
    ## 119                       -
    ## 120                       -
    ## 121                       -
    ## 122                       -
    ## 123                       -
    ## 124                       -
    ## 125                       -
    ## 126                       -
    ## 127                       -
    ## 128                       -
    ## 129                       -
    ## 130                       -
    ## 131                       -
    ## 132                       -
    ## 133                       -
    ## 134                       -
    ## 135                       -
    ## 136                       -
    ## 137                       -
    ## 138                       -
    ## 139                       -
    ## 140                       -
    ## 141                       -
    ## 142                       -
    ## 143                       -
    ## 144                       -
    ## 145                       -
    ## 146                     5.4
    ## 147                       -
    ## 148                       -
    ## 149                       -
    ## 150                       -
    ## 151                       -
    ## 152                       -
    ## 153                       -
    ## 154                       -
    ## 155                       -
    ## 156                       -
    ## 157                       -
    ## 158                     4.2
    ## 159                       -
    ## 160                       -
    ## 161                       -
    ## 162                       -
    ## 163                       -
    ## 164                       -
    ## 165                       -
    ## 166                       -
    ## 167                       -
    ## 168                       -
    ## 169                       -
    ## 170                       -
    ## 171                       -
    ## 172                       -
    ## 173                       -
    ## 174                       -
    ## 175                       -
    ## 176                       -
    ## 177                       -
    ## 178                     9.1
    ## 179                       -
    ## 180                       -
    ## 181                       -
    ## 182                       -
    ## 183                       -
    ## 184                       -
    ## 185                       -
    ## 186                       -
    ## 187                       -
    ## 188                       -
    ## 189                       -
    ## 190                       -
    ## 191                       -
    ## 192                       -
    ## 193                       -
    ## 194                       -
    ## 195                       -
    ## 196                       -
    ## 197                       -
    ## 198                       -
    ## 199                       -
    ## 200                       -
    ## 201                       -
    ## 202                       -
    ## 203                       -
    ## 204                       -
    ## 205                       -
    ## 206                       -
    ## 207                       -
    ## 208                       -
    ## 209                       -
    ## 210                       -
    ## 211                       -
    ## 212                       -
    ## 213                       -
    ## 214                       -
    ## 215                       -
    ## 216                       -
    ## 217                       -
    ## 218                       -
    ## 219                       -
    ## 220                       -
    ## 221                       -
    ## 222                       -
    ## 223                       -
    ## 224                       -
    ## 225                       -
    ## 226                       -
    ## 227                       -
    ## 228                       -
    ## 229                       -
    ## 230                       -
    ## 231                       -
    ## 232                       -
    ## 233                       -
    ## 234                       -
    ## 235                       -
    ## 236                       -
    ## 237                       -
    ## 238                       -
    ## 239                       -
    ## 240                       -
    ## 241                       -
    ## 242                       -
    ## 243                       -
    ## 244                       -
    ## 245                       -
    ## 246                       -
    ## 247                       -
    ## 248                       -
    ## 249                       -
    ## 250                       -
    ## 251                       -
    ## 252                       -
    ## 253                       -
    ## 254                       -
    ## 255                       -
    ## 256                     6.6
    ## 257                       -
    ## 258                       -
    ## 259                       -
    ## 260                       -
    ## 261                       -
    ## 262                       -
    ## 263                       -
    ## 264                       -
    ## 265                       -
    ## 266                       -
    ## 267                       -
    ## 268                       -
    ## 269                       -
    ## 270                       -
    ## 271                       -
    ## 272                       -
    ## 273                       -
    ## 274                       -
    ## 275                       -
    ## 276                       -
    ## 277                       -
    ## 278                       -
    ## 279                       -
    ## 280                       -
    ## 281                       -
    ## 282                       -
    ## 283                       -
    ## 284                       -
    ## 285                       -
    ## 286                       -
    ## 287                       -
    ## 288                       -
    ## 289                       -
    ## 290                       -
    ## 291                       -
    ## 292                       -
    ## 293                       -
    ## 294                       -
    ## 295                       -
    ## 296                       -
    ## 297                       -
    ## 298                       -
    ## 299                       -
    ## 300                       -
    ## 301                       -
    ## 302                       -
    ## 303                       -
    ## 304                       -
    ## 305                       -
    ## 306                       -
    ## 307                       -
    ## 308                       -
    ## 309                       -
    ## 310                       -
    ## 311                       -
    ## 312                       -
    ## 313                       -
    ## 314                       -
    ## 315                       -
    ## 316                       -
    ## 317                       -
    ## 318                       -
    ## 319                       -
    ## 320                     5.6
    ## 321                       -
    ## 322                       -
    ## 323                       -
    ## 324                       -
    ## 325                       -
    ## 326                       -
    ## 327                       -
    ## 328                       -
    ## 329                       -
    ## 330                       -
    ## 331                       -
    ## 332                       -
    ## 333                       -
    ## 334                       -
    ## 335                       -
    ## 336                       -
    ## 337                       -
    ## 338                       -
    ## 339                       -
    ## 340                       -
    ## 341                       -
    ## 342                       -
    ## 343                       -
    ## 344                       -
    ## 345                       -
    ## 346                       -
    ## 347                       -
    ## 348                       -
    ## 349                       -
    ## 350                       -
    ## 351                       -
    ## 352                       -
    ## 353                       -
    ## 354                       -
    ## 355                       -
    ## 356                       -
    ## 357                       -
    ## 358                       -
    ## 359                       -
    ## 360                       -
    ## 361                       -
    ## 362                       -
    ## 363                       -
    ## 364                       -
    ## 365                       -
    ## 366                       -
    ## 367                       -
    ## 368                       -
    ## 369                       -
    ## 370                       -
    ## 371                       -
    ## 372                       -
    ## 373                       -
    ## 374                       -
    ## 375                       -
    ## 376                       -
    ## 377                       -
    ## 378                       -
    ## 379                       -
    ## 380                       -
    ## 381                       -
    ## 382                       -
    ## 383                       6
    ## 384                       -
    ## 385                       -
    ## 386                       -
    ## 387                       -
    ## 388                       -
    ## 389                       -
    ## 390                       -
    ## 391                       -
    ## 392                       -
    ## 393                       -
    ## 394                       -
    ## 395                       -
    ## 396                       -
    ## 397                       -
    ## 398                       -
    ## 399                       -
    ## 400                       -
    ## 401                       -
    ## 402                       -
    ## 403                       -
    ## 404                       -
    ## 405                       -
    ## 406                       -
    ## 407                       -
    ## 408                       -
    ## 409                       -
    ## 410                       -
    ## 411                       -
    ## 412                       -
    ## 413                       -
    ## 414                       -
    ## 415                       -
    ## 416                       -
    ## 417                       -
    ## 418                       -
    ## 419                       -
    ## 420                       -
    ## 421                       -
    ## 422                       -
    ## 423                       -
    ## 424                       -
    ## 425                       -
    ## 426                       -
    ## 427                       -
    ## 428                       -
    ## 429                       -
    ## 430                       -
    ## 431                       -
    ## 432                       -
    ## 433                       -
    ## 434                       -
    ## 435                       -
    ## 436                       -
    ## 437                       -
    ## 438                       -
    ## 439                       -
    ## 440                       -
    ## 441                       -
    ## 442                       -
    ## 443                       -
    ## 444                     8.2
    ## 445                       -
    ## 446                       -
    ## 447                       -
    ## 448                       -
    ## 449                       -
    ## 450                       -
    ## 451                       -
    ## 452                       -
    ## 453                       -
    ## 454                       -
    ## 455                       -
    ## 456                       -
    ## 457                       -
    ## 458                       -
    ## 459                       -
    ## 460                       -
    ## 461                       -
    ## 462                       -
    ## 463                       -
    ## 464                       -
    ## 465                       -
    ## 466                       -
    ## 467                       -
    ## 468                       -
    ## 469                       -
    ## 470                       -
    ## 471                       -
    ## 472                       -
    ## 473                       -
    ## 474                       -
    ## 475                       -
    ## 476                       -
    ## 477                       -
    ## 478                       -
    ## 479                       -
    ## 480                       -
    ## 481                       -
    ## 482                       -
    ## 483                       -
    ## 484                       -
    ## 485                       -
    ## 486                       -
    ## 487                       -
    ## 488                       -
    ## 489                       -
    ## 490                       -
    ## 491                       -
    ## 492                       -
    ## 493                       -
    ## 494                       -
    ## 495                       -
    ## 496                       -
    ## 497                       -
    ## 498                       -
    ## 499                       -
    ## 500                       -
    ## 501                     2.9
    ## 502                       -
    ## 503                       -
    ## 504                       -
    ## 505                       -
    ## 506                       -
    ## 507                       -
    ## 508                       -
    ## 509                       -
    ## 510                       -
    ## 511                       -
    ## 512                       -
    ## 513                       -
    ## 514                       -
    ## 515                       -
    ## 516                       -
    ## 517                       -
    ## 518                       -
    ## 519                       -
    ## 520                       -
    ## 521                       -
    ## 522                       -
    ## 523                       -
    ## 524                       -
    ## 525                       -
    ## 526                       -
    ## 527                       -
    ## 528                       -
    ## 529                       -
    ## 530                       -
    ## 531                       -
    ## 532                       -
    ## 533                       -
    ## 534                       -
    ## 535                       -
    ## 536                       -
    ## 537                       -
    ## 538                       -
    ## 539                       -
    ## 540                       -
    ## 541                       -
    ## 542                       -
    ## 543                       -
    ## 544                       -
    ## 545                       -
    ## 546                       -
    ## 547                       -
    ## 548                     2.1
    ## 549                       -
    ## 550                       -
    ## 551                       -
    ## 552                       -
    ## 553                       -
    ## 554                       -
    ## 555                       -
    ## 556                       -
    ## 557                       -
    ## 558                       -
    ## 559                       -
    ## 560                       -
    ## 561                     4.2
    ## 562                       -
    ## 563                       -
    ## 564                       -
    ## 565                       -
    ## 566                       -
    ## 567                       -
    ## 568                       -
    ## 569                       -
    ## 570                       -
    ## 571                       -
    ## 572                       -
    ## 573                       -
    ## 574                       -
    ## 575                       -
    ## 576                       -
    ## 577                       -
    ## 578                       -
    ## 579                       -
    ## 580                       -
    ## 581                       -
    ## 582                       -
    ## 583                       -
    ## 584                       -
    ## 585                       -
    ## 586                       -
    ## 587                       -
    ## 588                       -
    ## 589                       -
    ## 590                       -
    ## 591                       -
    ## 592                       -
    ## 593                       -
    ## 594                       -
    ## 595                       -
    ## 596                       -
    ## 597                       -
    ## 598                       -
    ## 599                       -
    ## 600                       -
    ## 601                       -
    ## 602                       -
    ## 603                       -
    ## 604                       -
    ## 605                       -
    ## 606                       -
    ## 607                       -
    ## 608                       -
    ## 609                       -
    ## 610                       -
    ## 611                       -
    ## 612                       -
    ## 613                       -
    ## 614                       -
    ## 615                       -
    ## 616                       -
    ## 617                       -
    ## 618                       -
    ## 619                     3.7
    ## 620                       -
    ## 621                       -
    ## 622                       -
    ## 623                       -
    ## 624                       -
    ## 625                    13.6
    ## 626                       -
    ## 627                       -
    ## 628                       -
    ## 629                       -
    ## 630                       -
    ## 631                       -
    ## 632                       -
    ## 633                       -
    ## 634                       -
    ## 635                       -
    ## 636                       -
    ## 637                       -
    ## 638                       -
    ## 639                       -
    ## 640                       -
    ## 641                       -
    ## 642                       -
    ## 643                       -
    ## 644                       -
    ## 645                       -
    ## 646                       -
    ## 647                       -
    ## 648                       -
    ## 649                       -
    ## 650                       -
    ## 651                       -
    ## 652                       -
    ## 653                       -
    ## 654                       -
    ## 655                       -
    ## 656                       -
    ## 657                       -
    ## 658                       -
    ## 659                       -
    ## 660                       -
    ## 661                       -
    ## 662                       -
    ## 663                       -
    ## 664                       -
    ## 665                       -
    ## 666                       -
    ## 667                       -
    ## 668                       -
    ## 669                       -
    ## 670                       -
    ## 671                       -
    ## 672                       -
    ## 673                       -
    ## 674                       -
    ## 675                       -
    ## 676                       -
    ## 677                       -
    ## 678                       -
    ## 679                       -
    ## 680                       -
    ## 681                       -
    ## 682                       -
    ## 683                       -
    ## 684                       -
    ## 685                       -
    ## 686                       -
    ## 687                       -
    ## 688                       -
    ## 689                       -
    ## 690                     5.9
    ## 691                       -
    ## 692                       -
    ## 693                       -
    ## 694                       -
    ## 695                       -
    ## 696                       -
    ## 697                       -
    ## 698                       -
    ## 699                       -
    ## 700                       -
    ## 701                       -
    ## 702                       -
    ## 703                       -
    ## 704                       -
    ## 705                       -
    ## 706                       -
    ## 707                       -
    ## 708                       -
    ## 709                       -
    ## 710                       -
    ## 711                       -
    ## 712                       -
    ## 713                       -
    ## 714                       -
    ## 715                       -
    ## 716                       -
    ## 717                       -
    ## 718                       -
    ## 719                       -
    ## 720                       -
    ## 721                       -
    ## 722                       -
    ## 723                       -
    ## 724                       -
    ## 725                       -
    ## 726                       -
    ## 727                       -
    ## 728                       -
    ## 729                       -
    ## 730                       -
    ## 731                       -
    ## 732                       -
    ## 733                       -
    ## 734                       -
    ## 735                       -
    ## 736                       -
    ## 737                     5.1
    ## 738                       -
    ## 739                       -
    ## 740                       -
    ## 741                       -
    ## 742                       -
    ## 743                     5.2
    ## 744                       -
    ## 745                       -
    ## 746                       -
    ## 747                       -
    ## 748                       -
    ## 749                       -
    ## 750                       -
    ## 751                       -
    ## 752                       -
    ## 753                       -
    ## 754                       -
    ## 755                       -
    ## 756                       -
    ## 757                       -
    ## 758                       -
    ## 759                       -
    ## 760                       -
    ## 761                       -
    ## 762                       -
    ## 763                       -
    ## 764                       -
    ## 765                       -
    ## 766                       -
    ## 767                       -
    ## 768                       -
    ## 769                       3
    ## 770                       -
    ## 771                       -
    ## 772                       -
    ## 773                       -
    ## 774                       -
    ## 775                       -
    ## 776                       -
    ## 777                       -
    ## 778                       -
    ## 779                       -
    ## 780                       -
    ## 781                       -
    ## 782                       -
    ## 783                       -
    ## 784                       -
    ## 785                       -
    ## 786                       -
    ## 787                       -
    ## 788                       -
    ## 789                       -
    ## 790                       -
    ## 791                       -
    ## 792                       -
    ## 793                       -
    ## 794                       -
    ## 795                       -
    ## 796                       -
    ## 797                       -
    ## 798                       -
    ## 799                     8.6
    ## 800                       -
    ## 801                       -
    ## 802                       -
    ## 803                       -
    ## 804                       -
    ## 805                       -
    ## 806                       -
    ## 807                       -
    ## 808                       -
    ## 809                       -
    ## 810                       -
    ## 811                       -
    ## 812                       -
    ## 813                       -
    ## 814                       -
    ## 815                       -
    ## 816                       -
    ## 817                       -
    ## 818                       -
    ## 819                       -
    ## 820                       -
    ## 821                       -
    ## 822                       -
    ## 823                       -
    ## 824                       -
    ## 825                       -
    ## 826                       -
    ## 827                       -
    ## 828                       -
    ## 829                       -
    ## 830                       -
    ## 831                       -
    ## 832                       -
    ## 833                       -
    ## 834                       -
    ## 835                       -
    ## 836                       -
    ## 837                       -
    ## 838                       -
    ## 839                       -
    ## 840                     5.2
    ## 841                       -
    ## 842                       -
    ## 843                       -
    ## 844                       -
    ## 845                       -
    ## 846                       -
    ## 847                       -
    ## 848                       -
    ## 849                       -
    ## 850                       -
    ##                                                                                                                                                                                                                                                                                                                                                                                                                            Comments
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                Water temp = 14.6C
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                           Pass 1: 200V, 30Hz, 12%
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 7                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 8                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 9                                                                                                                                                                                                                                                                                                                                                                                                                              <NA>
    ## 10                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 11                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 12                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 13                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 14                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 15                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 16                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 17                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 18                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 19                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 20                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 21                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 22                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 23                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 24                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 25                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 26                                                                                                                                                                                                                                                                                                                                                                                                          Pass 2: 220V, 40Hz, 12%
    ## 27                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 28                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 29                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 30                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 31                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 32                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 33                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 34                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 35                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 36                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 37                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 38                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 39                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 40                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 41                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 42                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 43                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 44                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 45                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 46                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 47                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 48                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 49                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 50                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 51                                                                                                                                                                                                                                                                                                                                                                                                          Pass 3: 220V, 40Hz, 12%
    ## 52                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 53                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 54                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 55                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 56                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 57                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 58                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 59                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 60                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 61                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 62                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 63                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 64                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 65                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 66                                                                                                                                                                                                                                                                                                                                                                                                          Pass 1: 220V, 40Hz, 12%
    ## 67                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 68                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 69                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 70                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 71                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 72                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 73                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 74                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 75                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 76                                                                                                                                                                                                                                                                                                                                                                                                          Pass 2: 220V, 40Hz, 12%
    ## 77                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 78                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 79                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 80                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 81                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 82                                                                                                                                                                                                                                                                                                                                                                                                          Pass 3: 220V, 40Hz, 12%
    ## 83                                                                                                                                                                                                                                                                                                                                                                                                               Water temp = 14.6C
    ## 84                                                                                                                                                                                                                                                                                                                                                                                                          Pass 1: 220V, 40Hz, 12%
    ## 85                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 86                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 87                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 88                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 89                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 90                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 91                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 92                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 93                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 94                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 95                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 96                                                                                                                                                                                                                                                                                                                                                                                                                         BRUISING
    ## 97                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 98                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 99                                                                                                                                                                                                                                                                                                                                                                                                                             <NA>
    ## 100                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 101                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 102                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 103                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 104                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 105                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 106                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 107                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 108                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 109                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 110                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 111                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 112                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 113                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 114                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 115                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 116                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 117                                                                                                                                                                                                                                                                                                                                                                                                                        BRUISING
    ## 118                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 119                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 120                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 121                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 122                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 123                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 124                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 125                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 126                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 220V, 40Hz, 12%
    ## 127                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 128                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 129                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 130                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 131                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 132                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 133                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 134                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 135                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 136                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 137                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 138                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 139                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 140                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 141                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 142                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 143                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 220V, 40Hz, 12% (FISH BRUISING)
    ## 144                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 145                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 146                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 220V, 40Hz, 12%
    ## 147                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 148                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 149                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 150                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 151                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 152                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 153                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 154                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 220V, 40Hz, 12%
    ## 155                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 156                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 157                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 158                                                                                                                                                                                                                                                                                                                                                                               Pass 1: 220V, 40Hz, 12%; (Note: no scale brought)
    ## 159                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 160                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 161                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 162                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 163                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 164                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 165                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 166                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 167                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 168                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 169                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 170                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 171                                                                                                                                                                                                                                                                                                                                                                                Pass 2: 220V, 40Hz, 12% (Note: No scale brought)
    ## 172                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 173                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 174                                                                                                                                                                                                                                                                                                                                                                                Pass 3: 220V, 40Hz, 12% (Note: No scale brought)
    ## 175                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 176                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 177                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 178                                                                                                                                                                                                                                                                                                                                                                                Pass 1: 220V, 40Hz, 12% (Note: No scale brought)
    ## 179                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 180                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 181                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 182                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 183                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 184                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 185                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 186                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 187                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 188                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 189                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 190                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 191                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 192                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 193                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 194                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 195                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 196                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 197                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 198                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 199                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 200                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 201                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 202                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 203                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 204                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 205                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 206                                                                                                                                                                                                                                                                                                                                                                                Pass 2: 240V, 50Hz, 12% (Note: No scale brought)
    ## 207                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 208                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 209                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 210                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 211                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 212                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 213                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 214                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 215                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 216                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 217                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 218                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 219                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 220                                                                                                                                                                                                                                                                                                                                                                               Pass 3: 240V, 50Hz, 12%; (Note: No scale brought)
    ## 221                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 222                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 223                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 224                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 225                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 226                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 227                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 228                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 229                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 230                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 231                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 232                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 233                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 234                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 235                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 236                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 237                                                                                                                                                                                                                                                                                                                                                                              Pass (4: 240v, 50Hz, 12%; (Note: No scale brought)
    ## 238                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 239                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 240                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 241                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 242                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 243                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 244                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 245                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 246                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 247                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 248                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 249                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 250                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 251                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 252                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 253                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 254                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 255                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 256                                                                                                                                                                                                                                                                                                                                                                                Pass 1: 240V, 50Hz, 12% (Note: No scale brought)
    ## 257                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 258                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 259                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 260                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 261                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 262                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 263                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 264                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 265                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 266                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 267                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 268                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 269                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 270                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 271                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 272                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 273                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 274                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 275                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 276                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 277                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 278                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 279                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 280                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 281                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 282                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 283                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 284                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 285                                                                                                                                                                                                                                                                                                                                                                                Pass 2: 240V, 50Hz, 12% (Note: No scale brought)
    ## 286                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 287                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 288                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 289                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 290                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 291                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 292                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 293                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 294                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 295                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 296                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 297                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 298                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 299                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 300                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 301                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 302                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 303                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 304                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 305                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 306                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 307                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 308                                                                                                                                                                                                                                                                                                                                                                                Pass 3: 240V, 50Hz, 12% (Note: No scale brought)
    ## 309                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 310                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 311                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 312                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 313                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 314                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 315                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 316                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 317                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 318                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 319                                                                                                                                                                                                                                                                                                                                                                                                        (Note: no scale brought)
    ## 320                                                                                                                                                                                                                                                                                                                                                                                Pass 1: 240V, 50Hz, 12% (Note: No scale brought)
    ## 321                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 322                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 323                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 324                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 325                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 326                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 327                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 328                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 329                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 330                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 331                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 332                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 333                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 334                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 335                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 336                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 337                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 338                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 339                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 340                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 341                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 342                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 343                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 344                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 345                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 346                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 347                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 348                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 240V, 50Hz, 12%
    ## 349                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 350                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 351                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 352                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 353                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 354                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 355                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 356                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 357                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 358                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 359                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 360                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 361                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 362                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 363                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 364                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 365                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 366                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 367                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 240V, 50Hz, 12%
    ## 368                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 369                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 370                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 371                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 372                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 373                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 374                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 375                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 376                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 377                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 378                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 379                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 380                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 381                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 382                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 383                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 240V, 50Hz, 12%
    ## 384                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 385                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 386                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 387                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 388                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 389                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 390                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 391                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 392                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 393                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 394                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 395                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 396                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 397                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 398                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 399                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 400                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 401                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 402                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 403                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 404                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 405                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 406                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 407                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 408                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 409                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 410                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 411                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 412                                                                                                                                                                                                                                                                                                                                                                                                                 "SLOW RECOVERY"
    ## 413                                                                                                                                                                                                                                                                                                                                                                                                                           BURNS
    ## 414                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 415                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 416                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 417                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 418                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 419                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 420                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 421                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 240V, 50Hz, 12%
    ## 422                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 423                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 424                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 425                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 426                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 427                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 428                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 429                                                                                                                                                                                                                                                                                                                                                                                                                   HIT VERY HARD
    ## 430                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 431                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 432                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 433                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 434                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 435                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 240V, 50Hz, 12%
    ## 436                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 437                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 438                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 439                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 440                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 441                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 442                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 443                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 444                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 240V, 50Hz, 12%
    ## 445                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 446                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 447                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 448                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 449                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 450                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 451                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 452                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 453                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 454                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 455                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 456                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 457                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 458                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 459                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 460                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 461                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 462                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 463                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 464                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 465                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 466                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 467                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 468                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 469                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 470                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 471                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 472                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 473                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 240V, 50Hz, 12%
    ## 474                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 475                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 476                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 477                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 478                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 479                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 480                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 481                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 482                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 483                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 484                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 485                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 486                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 240V, 50Hz, 12%
    ## 487                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 488                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 489                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 490                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 491                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 492                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 493                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 494                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 495                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 496                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 497                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 498                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 499                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 500                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 501                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 240V, 50Hz, 12%
    ## 502                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 503                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 504                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 505                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 506                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 507                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 508                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 509                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 510                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 511                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 512                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 513                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 514                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 515                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 516                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 517                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 518                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 519                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 520                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 521                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 522                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 523                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 524                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 525                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 526                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 527                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 528                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 240V, 50Hz, 12%
    ## 529                                                                                                                                                                                                                                                                                                                                                                                                                            MORT
    ## 530                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 531                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 532                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 533                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 534                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 535                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 536                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 537                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 538                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 539                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 240V, 50Hz, 12%
    ## 540                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 541                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 542                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 543                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 544                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 545                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 546                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 547                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 548                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 549                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 550                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 551                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 552                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 553                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 554                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 555                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 556                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 557                                                                                                                                                                                                                                                                                                                                                                                                        MORT-placed on hot scale
    ## 558                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 559                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 560                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 561                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 240V, 50Hz, 12%
    ## 562                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 563                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 564                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 565                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 566                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 567                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 568                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 569                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 570                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 571                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 572                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 573                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 574                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 575                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 576                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 577                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 578                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 579                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 580                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 581                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 582                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 583                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 584                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 585                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 586                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 587                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 588                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 589                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 590                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 591                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 592                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 593                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 594                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 595                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 596                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 597                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 598                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 599                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 600                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 601                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 602                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 603                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 604                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 605                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 606                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 607                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 608                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 240V, 50Hz, 12%
    ## 609                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 610                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 611                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 612                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 613                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 614                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 615                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 240V, 50Hz, 12%
    ## 616                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 617                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 618                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 619                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 200V, 50Hz, 12%
    ## 620                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 621                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 622                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 623                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 200V, 50Hz, 12%
    ## 624                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 200V, 50Hz, 12%
    ## 625                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 200V, 50Hz, 12%
    ## 626                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 627                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 628                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 629                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 630                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 631                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 632                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 633                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 634                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 635                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 636                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 637                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 638                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 639                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 640                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 641                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 642                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 643                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 644                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 645                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 646                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 647                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 648                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 649                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 650                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 651                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 652                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 653                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 654                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 655                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 656                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 657                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 658                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 659                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 660                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 661                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 662                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 663                                                                                                                                                                                                                                                                                                                                                                        Pass 2: 150V, 50Hz, 12% (in response to the mortalities)
    ## 664                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 665                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 666                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 667                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 668                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 669                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 670                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 671                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 672                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 673                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 674                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 675                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 676                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 677                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 678                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 679                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 680                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 140V, 40Hz, 12%
    ## 681                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 682                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 683                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 684                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 685                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 686                                                                                                                                                                                                                                                                                                                                                                                                              DEAD - OVERSHOCKED
    ## 687                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 688                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 689                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 690                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 140V, 40Hz, 12%
    ## 691                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 692                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 693                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 694                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 695                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 696                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 697                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 698                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 699                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 700                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 701                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 702                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 703                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 704                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 705                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 706                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 707                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 708                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 709                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 710                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 711                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 712                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 713                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 714                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 715                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 716                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 717                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 718                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 719                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 720                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 140V, 40Hz, 12%
    ## 721                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 722                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 723                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 724                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 725                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 726                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 727                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 728                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 729                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 730                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 140V, 40Hz, 12%
    ## 731                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 732                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 733                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 734                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 735                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 736                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 737                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 140V, 40Hz, 12%
    ## 738                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 739                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 740                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 741                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 140V, 40Hz, 12%
    ## 742                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 140V, 40Hz, 12%
    ## 743                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 140V, 40Hz, 12%
    ## 744                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 745                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 746                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 747                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 748                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 749                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 750                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 751                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 752                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 753                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 754                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 755                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 756                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 757                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 140V, 40Hz, 12%
    ## 758                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 759                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 760                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 761                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 762                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 763                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 764                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 765                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 766                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 767                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 140V, 40Hz, 12%
    ## 768                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 769                                                                                                                                                                           Pass 1: 100V, 30Hz, 12% (adapted from 140V, 40Hz, 12% as a result of mortalities -  THE RIFFLE WAS COMPOSED OF LARGE COBBLE WITH LARGE INTERSTIAL SPACES SO FISH WERE GETTING STUCK WITHIN THE SUBSTRATE AND THUS HAMMERED HARD BY THE ELECTROFISHER)
    ## 770                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 771                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 772                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 773                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 774                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 775                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 776                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 777                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 778                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 779                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 780                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 781                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 782                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 783                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 100V, 30Hz, 12%
    ## 784                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 785                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 786                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 787                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 788                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 789                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 790                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 791                                                                                                                                                                                                                                                                                                                                                                                                              NO BLACK PARASITES
    ## 792                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 793                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 100V, 30Hz, 12%
    ## 794                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 795                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 796                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 797 Pass 4: 100V, 30Hz, 12% (Being that the substrate had a lot of large cobble with large interstitial spaces, the surface layer of substrate was removed and then the treatment was shocked to assess how many fishes were removed from the riffle and how many were actually being caught in the rocks and missed from the counts - based on the low numbers in the 4th pass it seems as though the electrofishing was efficient
    ## 798                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 799                                                                                                                                                               Pass 1: 135V, 40Hz, 12%; Fish dead prior to sampling; (Being that the substrate was smaller (i.e. lower chances of fishes getting stuck in the substrate and shocked for extended periods), the power of the shocker was increased to maximize capture efficiency
    ## 800                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 801                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 802                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 803                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 804                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 805                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 806                                                                                                                                                                                                                                                                                                                                                                                                    FOUND DEAD PRIOR TO SHOCKING
    ## 807                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 808                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 809                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 810                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 811                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 812                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 813                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 814                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 815                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 816                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 817                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 818                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 819                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 820                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 821                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 822                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 823                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 824                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 825                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 826                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 827                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 135V, 40Hz, 12%
    ## 828                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 829                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 830                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 831                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 832                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 833                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 834                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 835                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 836                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 837                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 838                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 135V, 40Hz, 12%
    ## 839                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 840                                                                                                                                                                                                                                                                                                                                                                                                         Pass 1: 135V, 40Hz, 12%
    ## 841                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 842                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 843                                                                                                                                                                                                                                                                                                                                                                                                                            DEAD
    ## 844                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 845                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 846                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 847                                                                                                                                                                                                                                                                                                                                                                                                         Pass 2: 135V, 40Hz, 12%
    ## 848                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 849                                                                                                                                                                                                                                                                                                                                                                                                                            <NA>
    ## 850                                                                                                                                                                                                                                                                                                                                                                                                         Pass 3: 135V, 40Hz, 12%
    ##                                                                                                                                                                     ...11
    ## 1                                                                                                                                                                    <NA>
    ## 2                                                                                                                                                                    <NA>
    ## 3                                                                                                                                                                    <NA>
    ## 4                                                                                                                                                                    <NA>
    ## 5                                                                                                                                                                    <NA>
    ## 6                                                                                                                                                                    <NA>
    ## 7                                                                                                                                                                    <NA>
    ## 8                                                                                                                                                                    <NA>
    ## 9                                                                                                                                                                    <NA>
    ## 10                                                                                                                                                                   <NA>
    ## 11                                                                                                                                                                   <NA>
    ## 12                                                                                                                                                                   <NA>
    ## 13                                                                                                                                                                   <NA>
    ## 14                                                                                                                                                                   <NA>
    ## 15                                                                                                                                                                   <NA>
    ## 16                                                                                                                                                                   <NA>
    ## 17                                                                                                                                                                   <NA>
    ## 18                                                                                                                                                                   <NA>
    ## 19                                                                                                                                                                   <NA>
    ## 20                                                                                                                                                                   <NA>
    ## 21                                                                                                                                                                   <NA>
    ## 22                                                                                                                                                                   <NA>
    ## 23                                                                                                                                                                   <NA>
    ## 24                                                                                                                                                                   <NA>
    ## 25                                                                                                                                                                   <NA>
    ## 26                                                                                                                                                                   <NA>
    ## 27                                                                                                                                                                   <NA>
    ## 28                                                                                                                                                                   <NA>
    ## 29                                                                                                                                                                   <NA>
    ## 30                                                                                                                                                                   <NA>
    ## 31                                                                                                                                                                   <NA>
    ## 32                                                                                                                                                                   <NA>
    ## 33                                                                                                                                                                   <NA>
    ## 34                                                                                                                                                                   <NA>
    ## 35                                                                                                                                                                   <NA>
    ## 36                                                                                                                                                                   <NA>
    ## 37                                                                                                                                                                   <NA>
    ## 38                                                                                                                                                                   <NA>
    ## 39                                                                                                                                                                   <NA>
    ## 40                                                                                                                                                                   <NA>
    ## 41                                                                                                                                                                   <NA>
    ## 42                                                                                                                                                                   <NA>
    ## 43                                                                                                                                                                   <NA>
    ## 44                                                                                                                                                                   <NA>
    ## 45                                                                                                                                                                   <NA>
    ## 46                                                                                                                                                                   <NA>
    ## 47                                                                                                                                                                   <NA>
    ## 48                                                                                                                                                                   <NA>
    ## 49                                                                                                                                                                   <NA>
    ## 50                                                                                                                                                                   <NA>
    ## 51                                                                                                                                                                   <NA>
    ## 52                                                                                                                                                                   <NA>
    ## 53                                                                                                                                                                   <NA>
    ## 54                                                                                                                                                                   <NA>
    ## 55                                                                                                                                                                   <NA>
    ## 56                                                                                                                                                                   <NA>
    ## 57                                                                                                                                                                   <NA>
    ## 58                                                                                                                                                                   <NA>
    ## 59                                                                                                                                                                   <NA>
    ## 60                                                                                                                                                                   <NA>
    ## 61                                                                                                                                                                   <NA>
    ## 62                                                                                                                                                                   <NA>
    ## 63                                                                                                                                                                   <NA>
    ## 64                                                                                                                                                                   <NA>
    ## 65                                                                                                                                                                   <NA>
    ## 66                                                                                                                                                                   <NA>
    ## 67                                                                                                                                                                   <NA>
    ## 68                                                                                                                                                                   <NA>
    ## 69                                                                                                                                                                   <NA>
    ## 70                                                                                                                                                                   <NA>
    ## 71                                                                                                                                                                   <NA>
    ## 72                                                                                                                                                                   <NA>
    ## 73                                                                                                                                                                   <NA>
    ## 74                                                                                                                                                                   <NA>
    ## 75                                                                                                                                                                   <NA>
    ## 76                                                                                                                                                                   <NA>
    ## 77                                                                                                                                                                   <NA>
    ## 78                                                                                                                                                                   <NA>
    ## 79                                                                                                                                                                   <NA>
    ## 80                                                                                                                                                                   <NA>
    ## 81                                                                                                                                                                   <NA>
    ## 82                                                                                                                                                                   <NA>
    ## 83                                                                                                                                                                   <NA>
    ## 84                                                                                                                                                                   <NA>
    ## 85                                                                                                                                                                   <NA>
    ## 86                                                                                                                                                                   <NA>
    ## 87                                                                                                                                                                   <NA>
    ## 88                                                                                                                                                                   <NA>
    ## 89                                                                                                                                                                   <NA>
    ## 90                                                                                                                                                                   <NA>
    ## 91                                                                                                                                                                   <NA>
    ## 92                                                                                                                                                                   <NA>
    ## 93                                                                                                                                                                   <NA>
    ## 94                                                                                                                                                                   <NA>
    ## 95                                                                                                                                                                   <NA>
    ## 96                                                                                                                                                                   <NA>
    ## 97                                                                                                                                                                   <NA>
    ## 98                                                                                                                                                                   <NA>
    ## 99                                                                                                                                                                   <NA>
    ## 100                                                                                                                                                                  <NA>
    ## 101                                                                                                                                                                  <NA>
    ## 102                                                                                                                                                                  <NA>
    ## 103                                                                                                                                                                  <NA>
    ## 104                                                                                                                                                                  <NA>
    ## 105                                                                                                                                                                  <NA>
    ## 106                                                                                                                                                                  <NA>
    ## 107                                                                                                                                                                  <NA>
    ## 108                                                                                                                                                                  <NA>
    ## 109                                                                                                                                                                  <NA>
    ## 110                                                                                                                                                                  <NA>
    ## 111                                                                                                                                                                  <NA>
    ## 112                                                                                                                                                                  <NA>
    ## 113                                                                                                                                                                  <NA>
    ## 114                                                                                                                                                                  <NA>
    ## 115                                                                                                                                                                  <NA>
    ## 116                                                                                                                                                                  <NA>
    ## 117                                                                                                                                                                  <NA>
    ## 118                                                                                                                                                                  <NA>
    ## 119                                                                                                                                                                  <NA>
    ## 120                                                                                                                                                                  <NA>
    ## 121                                                                                                                                                                  <NA>
    ## 122                                                                                                                                                                  <NA>
    ## 123                                                                                                                                                                  <NA>
    ## 124                                                                                                                                                                  <NA>
    ## 125                                                                                                                                                                  <NA>
    ## 126                                                                                                                                                                  <NA>
    ## 127                                                                                                                                                                  <NA>
    ## 128                                                                                                                                                                  <NA>
    ## 129                                                                                                                                                                  <NA>
    ## 130                                                                                                                                                                  <NA>
    ## 131                                                                                                                                                                  <NA>
    ## 132                                                                                                                                                                  <NA>
    ## 133                                                                                                                                                                  <NA>
    ## 134                                                                                                                                                                  <NA>
    ## 135                                                                                                                                                                  <NA>
    ## 136                                                                                                                                                                  <NA>
    ## 137                                                                                                                                                                  <NA>
    ## 138                                                                                                                                                                  <NA>
    ## 139                                                                                                                                                                  <NA>
    ## 140                                                                                                                                                                  <NA>
    ## 141                                                                                                                                                                  <NA>
    ## 142                                                                                                                                                                  <NA>
    ## 143                                                                                                                                                                  <NA>
    ## 144                                                                                                                                                                  <NA>
    ## 145                                                                                                                                                                  <NA>
    ## 146                                                                                                                                                                  <NA>
    ## 147                                                                                                                                                                  <NA>
    ## 148                                                                                                                                                                  <NA>
    ## 149                                                                                                                                                                  <NA>
    ## 150                                                                                                                                                                  <NA>
    ## 151                                                                                                                                                                  <NA>
    ## 152                                                                                                                                                                  <NA>
    ## 153                                                                                                                                                                  <NA>
    ## 154                                                                                                                                                                  <NA>
    ## 155                                                                                                                                                                  <NA>
    ## 156                                                                                                                                                                  <NA>
    ## 157                                                                                                                                                                  <NA>
    ## 158                                                                                                                                                                  <NA>
    ## 159                                                                                                                                                                  <NA>
    ## 160                                                                                                                                                                  <NA>
    ## 161                                                                                                                                                                  <NA>
    ## 162                                                                                                                                                                  <NA>
    ## 163                                                                                                                                                                  <NA>
    ## 164                                                                                                                                                                  <NA>
    ## 165                                                                                                                                                                  <NA>
    ## 166                                                                                                                                                                  <NA>
    ## 167                                                                                                                                                                  <NA>
    ## 168                                                                                                                                                                  <NA>
    ## 169                                                                                                                                                                  <NA>
    ## 170                                                                                                                                                                  <NA>
    ## 171                                                                                                                                                                  <NA>
    ## 172                                                                                                                                                                  <NA>
    ## 173                                                                                                                                                                  <NA>
    ## 174                                                                                                                                                                  <NA>
    ## 175                                                                                                                                                                  <NA>
    ## 176                                                                                                                                                                  <NA>
    ## 177                                                                                                                                                                  <NA>
    ## 178                                                                                                                                                                  <NA>
    ## 179                                                                                                                                                                  <NA>
    ## 180                                                                                                                                                                  <NA>
    ## 181                                                                                                                                                                  <NA>
    ## 182                                                                                                                                                                  <NA>
    ## 183                                                                                                                                                                  <NA>
    ## 184                                                                                                                                                                  <NA>
    ## 185                                                                                                                                                                  <NA>
    ## 186                                                                                                                                                                  <NA>
    ## 187                                                                                                                                                                  <NA>
    ## 188                                                                                                                                                                  <NA>
    ## 189                                                                                                                                                                  <NA>
    ## 190                                                                                                                                                                  <NA>
    ## 191                                                                                                                                                                  <NA>
    ## 192                                                                                                                                                                  <NA>
    ## 193                                                                                                                                                                  <NA>
    ## 194                                                                                                                                                                  <NA>
    ## 195                                                                                                                                                                  <NA>
    ## 196                                                                                                                                                                  <NA>
    ## 197                                                                                                                                                                  <NA>
    ## 198                                                                                                                                                                  <NA>
    ## 199                                                                                                                                                                  <NA>
    ## 200                                                                                                                                                                  <NA>
    ## 201                                                                                                                                                                  <NA>
    ## 202                                                                                                                                                                  <NA>
    ## 203                                                                                                                                                                  <NA>
    ## 204                                                                                                                                                                  <NA>
    ## 205                                                                                                                                                                  <NA>
    ## 206                                                                                                                                                                  <NA>
    ## 207                                                                                                                                                                  <NA>
    ## 208                                                                                                                                                                  <NA>
    ## 209                                                                                                                                                                  <NA>
    ## 210                                                                                                                                                                  <NA>
    ## 211                                                                                                                                                                  <NA>
    ## 212                                                                                                                                                                  <NA>
    ## 213                                                                                                                                                                  <NA>
    ## 214                                                                                                                                                                  <NA>
    ## 215                                                                                                                                                                  <NA>
    ## 216                                                                                                                                                                  <NA>
    ## 217                                                                                                                                                                  <NA>
    ## 218                                                                                                                                                                  <NA>
    ## 219                                                                                                                                                                  <NA>
    ## 220                                                                                                                                                                  <NA>
    ## 221                                                                                                                                                                  <NA>
    ## 222                                                                                                                                                                  <NA>
    ## 223                                                                                                                                                                  <NA>
    ## 224                                                                                                                                                                  <NA>
    ## 225                                                                                                                                                                  <NA>
    ## 226                                                                                                                                                                  <NA>
    ## 227                                                                                                                                                                  <NA>
    ## 228                                                                                                                                                                  <NA>
    ## 229                                                                                                                                                                  <NA>
    ## 230                                                                                                                                                                  <NA>
    ## 231                                                                                                                                                                  <NA>
    ## 232                                                                                                                                                                  <NA>
    ## 233                                                                                                                                                                  <NA>
    ## 234                                                                                                                                                                  <NA>
    ## 235                                                                                                                                                                  <NA>
    ## 236                                                                                                                                                                  <NA>
    ## 237                                                                                                                                                                  <NA>
    ## 238                                                                                                                                                                  <NA>
    ## 239                                                                                                                                                                  <NA>
    ## 240                                                                                                                                                                  <NA>
    ## 241                                                                                                                                                                  <NA>
    ## 242                                                                                                                                                                  <NA>
    ## 243                                                                                                                                                                  <NA>
    ## 244                                                                                                                                                                  <NA>
    ## 245                                                                                                                                                                  <NA>
    ## 246                                                                                                                                                                  <NA>
    ## 247                                                                                                                                                                  <NA>
    ## 248                                                                                                                                                                  <NA>
    ## 249                                                                                                                                                                  <NA>
    ## 250                                                                                                                                                                  <NA>
    ## 251                                                                                                                                                                  <NA>
    ## 252                                                                                                                                                                  <NA>
    ## 253                                                                                                                                                                  <NA>
    ## 254                                                                                                                                                                  <NA>
    ## 255                                                                                                                                                                  <NA>
    ## 256                                                                                                                                                                  <NA>
    ## 257                                                                                                                                                                  <NA>
    ## 258                                                                                                                                                                  <NA>
    ## 259                                                                                                                                                                  <NA>
    ## 260                                                                                                                                                                  <NA>
    ## 261                                                                                                                                                                  <NA>
    ## 262                                                                                                                                                                  <NA>
    ## 263                                                                                                                                                                  <NA>
    ## 264                                                                                                                                                                  <NA>
    ## 265                                                                                                                                                                  <NA>
    ## 266                                                                                                                                                                  <NA>
    ## 267                                                                                                                                                                  <NA>
    ## 268                                                                                                                                                                  <NA>
    ## 269                                                                                                                                                                  <NA>
    ## 270                                                                                                                                                                  <NA>
    ## 271                                                                                                                                                                  <NA>
    ## 272                                                                                                                                                                  <NA>
    ## 273                                                                                                                                                                  <NA>
    ## 274                                                                                                                                                                  <NA>
    ## 275                                                                                                                                                                  <NA>
    ## 276                                                                                                                                                                  <NA>
    ## 277                                                                                                                                                                  <NA>
    ## 278                                                                                                                                                                  <NA>
    ## 279                                                                                                                                                                  <NA>
    ## 280                                                                                                                                                                  <NA>
    ## 281                                                                                                                                                                  <NA>
    ## 282                                                                                                                                                                  <NA>
    ## 283                                                                                                                                                                  <NA>
    ## 284                                                                                                                                                                  <NA>
    ## 285                                                                                                                                                                  <NA>
    ## 286                                                                                                                                                                  <NA>
    ## 287                                                                                                                                                                  <NA>
    ## 288                                                                                                                                                                  <NA>
    ## 289                                                                                                                                                                  <NA>
    ## 290                                                                                                                                                                  <NA>
    ## 291                                                                                                                                                                  <NA>
    ## 292                                                                                                                                                                  <NA>
    ## 293                                                                                                                                                                  <NA>
    ## 294                                                                                                                                                                  <NA>
    ## 295                                                                                                                                                                  <NA>
    ## 296                                                                                                                                                                  <NA>
    ## 297                                                                                                                                                                  <NA>
    ## 298                                                                                                                                                                  <NA>
    ## 299                                                                                                                                                                  <NA>
    ## 300                                                                                                                                                                  <NA>
    ## 301                                                                                                                                                                  <NA>
    ## 302                                                                                                                                                                  <NA>
    ## 303                                                                                                                                                                  <NA>
    ## 304                                                                                                                                                                  <NA>
    ## 305                                                                                                                                                                  <NA>
    ## 306                                                                                                                                                                  <NA>
    ## 307                                                                                                                                                                  <NA>
    ## 308                                                                                                                                                                  <NA>
    ## 309                                                                                                                                                                  <NA>
    ## 310                                                                                                                                                                  <NA>
    ## 311                                                                                                                                                                  <NA>
    ## 312                                                                                                                                                                  <NA>
    ## 313                                                                                                                                                                  <NA>
    ## 314                                                                                                                                                                  <NA>
    ## 315                                                                                                                                                                  <NA>
    ## 316                                                                                                                                                                  <NA>
    ## 317                                                                                                                                                                  <NA>
    ## 318                                                                                                                                                                  <NA>
    ## 319                                                                                                                                                                  <NA>
    ## 320                                                                                                                                                                  <NA>
    ## 321                                                                                                                                                                  <NA>
    ## 322                                                                                                                                                                  <NA>
    ## 323                                                                                                                                                                  <NA>
    ## 324                                                                                                                                                                  <NA>
    ## 325                                                                                                                                                                  <NA>
    ## 326                                                                                                                                                                  <NA>
    ## 327                                                                                                                                                                  <NA>
    ## 328                                                                                                                                                                  <NA>
    ## 329                                                                                                                                                                  <NA>
    ## 330                                                                                                                                                                  <NA>
    ## 331                                                                                                                                                                  <NA>
    ## 332                                                                                                                                                                  <NA>
    ## 333                                                                                                                                                                  <NA>
    ## 334                                                                                                                                                                  <NA>
    ## 335                                                                                                                                                                  <NA>
    ## 336                                                                                                                                                                  <NA>
    ## 337                                                                                                                                                                  <NA>
    ## 338                                                                                                                                                                  <NA>
    ## 339                                                                                                                                                                  <NA>
    ## 340                                                                                                                                                                  <NA>
    ## 341                                                                                                                                                                  <NA>
    ## 342                                                                                                                                                                  <NA>
    ## 343                                                                                                                                                                  <NA>
    ## 344                                                                                                                                                                  <NA>
    ## 345                                                                                                                                                                  <NA>
    ## 346                                                                                                                                                                  <NA>
    ## 347                                                                                                                                                                  <NA>
    ## 348                                                                                                                                                                  <NA>
    ## 349                                                                                                                                                                  <NA>
    ## 350                                                                                                                                                                  <NA>
    ## 351                                                                                                                                                                  <NA>
    ## 352                                                                                                                                                                  <NA>
    ## 353                                                                                                                                                                  <NA>
    ## 354                                                                                                                                                                  <NA>
    ## 355                                                                                                                                                                  <NA>
    ## 356                                                                                                                                                                  <NA>
    ## 357                                                                                                                                                                  <NA>
    ## 358                                                                                                                                                                  <NA>
    ## 359                                                                                                                                                                  <NA>
    ## 360                                                                                                                                                                  <NA>
    ## 361                                                                                                                                                                  <NA>
    ## 362                                                                                                                                                                  <NA>
    ## 363                                                                                                                                                                  <NA>
    ## 364                                                                                                                                                                  <NA>
    ## 365                                                                                                                                                                  <NA>
    ## 366                                                                                                                                                                  <NA>
    ## 367                                                                                                                                                                  <NA>
    ## 368                                                                                                                                                                  <NA>
    ## 369                                                                                                                                                                  <NA>
    ## 370                                                                                                                                                                  <NA>
    ## 371                                                                                                                                                                  <NA>
    ## 372                                                                                                                                                                  <NA>
    ## 373                                                                                                                                                                  <NA>
    ## 374                                                                                                                                                                  <NA>
    ## 375                                                                                                                                                                  <NA>
    ## 376                                                                                                                                                                  <NA>
    ## 377                                                                                                                                                                  <NA>
    ## 378                                                                                                                                                                  <NA>
    ## 379                                                                                                                                                                  <NA>
    ## 380                                                                                                                                                                  <NA>
    ## 381                                                                                                                                                                  <NA>
    ## 382                                                                                                                                                                  <NA>
    ## 383                                                                                                                                                                  <NA>
    ## 384                                                                                                                                                                  <NA>
    ## 385                                                                                                                                                                  <NA>
    ## 386                                                                                                                                                                  <NA>
    ## 387                                                                                                                                                                  <NA>
    ## 388                                                                                                                                                                  <NA>
    ## 389                                                                                                                                                                  <NA>
    ## 390                                                                                                                                                                  <NA>
    ## 391                                                                                                                                                                  <NA>
    ## 392                                                                                                                                                                  <NA>
    ## 393                                                                                                                                                                  <NA>
    ## 394                                                                                                                                                                  <NA>
    ## 395                                                                                                                                                                  <NA>
    ## 396                                                                                                                                                                  <NA>
    ## 397                                                                                                                                                                  <NA>
    ## 398                                                                                                                                                                  <NA>
    ## 399                                                                                                                                                                  <NA>
    ## 400                                                                                                                                                                  <NA>
    ## 401                                                                                                                                                                  <NA>
    ## 402                                                                                                                                                                  <NA>
    ## 403                                                                                                                                                                  <NA>
    ## 404                                                                                                                                                                  <NA>
    ## 405                                                                                                                                                                  <NA>
    ## 406                                                                                                                                                                  <NA>
    ## 407                                                                                                                                                                  <NA>
    ## 408                                                                                                                                                                  <NA>
    ## 409                                                                                                                                                                  <NA>
    ## 410                                                                                                                                                                  <NA>
    ## 411                                                                                                                                                                  <NA>
    ## 412                                                                                                                                                                  <NA>
    ## 413                                                                                                                                                                  <NA>
    ## 414                                                                                                                                                                  <NA>
    ## 415                                                                                                                                                                  <NA>
    ## 416                                                                                                                                                                  <NA>
    ## 417                                                                                                                                                                  <NA>
    ## 418                                                                                                                                                                  <NA>
    ## 419                                                                                                                                                                  <NA>
    ## 420                                                                                                                                                                  <NA>
    ## 421                                                                                                                                                                  <NA>
    ## 422                                                                                                                                                                  <NA>
    ## 423                                                                                                                                                                  <NA>
    ## 424                                                                                                                                                                  <NA>
    ## 425                                                                                                                                                                  <NA>
    ## 426                                                                                                                                                                  <NA>
    ## 427                                                                                                                                                                  <NA>
    ## 428                                                                                                                                                                  <NA>
    ## 429                                                                                                                                                                  <NA>
    ## 430                                                                                                                                                                  <NA>
    ## 431                                                                                                                                                                  <NA>
    ## 432                                                                                                                                                                  <NA>
    ## 433                                                                                                                                                                  <NA>
    ## 434                                                                                                                                                                  <NA>
    ## 435                                                                                                                                                                  <NA>
    ## 436                                                                                                                                                                  <NA>
    ## 437                                                                                                                                                                  <NA>
    ## 438                                                                                                                                                                  <NA>
    ## 439                                                                                                                                                                  <NA>
    ## 440                                                                                                                                                                  <NA>
    ## 441                                                                                                                                                                  <NA>
    ## 442                                                                                                                                                                  <NA>
    ## 443                                                                                                                                                                  <NA>
    ## 444                                                                                                                                                                  <NA>
    ## 445                                                                                                                                                                  <NA>
    ## 446                                                                                                                                                                  <NA>
    ## 447                                                                                                                                                                  <NA>
    ## 448                                                                                                                                                                  <NA>
    ## 449                                                                                                                                                                  <NA>
    ## 450                                                                                                                                                                  <NA>
    ## 451                                                                                                                                                                  <NA>
    ## 452                                                                                                                                                                  <NA>
    ## 453                                                                                                                                                                  <NA>
    ## 454                                                                                                                                                                  <NA>
    ## 455                                                                                                                                                                  <NA>
    ## 456                                                                                                                                                                  <NA>
    ## 457                                                                                                                                                                  <NA>
    ## 458                                                                                                                                                                  <NA>
    ## 459                                                                                                                                                                  <NA>
    ## 460                                                                                                                                                                  <NA>
    ## 461                                                                                                                                                                  <NA>
    ## 462                                                                                                                                                                  <NA>
    ## 463                                                                                                                                                                  <NA>
    ## 464                                                                                                                                                                  <NA>
    ## 465                                                                                                                                                                  <NA>
    ## 466                                                                                                                                                                  <NA>
    ## 467                                                                                                                                                                  <NA>
    ## 468                                                                                                                                                                  <NA>
    ## 469                                                                                                                                                                  <NA>
    ## 470                                                                                                                                                                  <NA>
    ## 471                                                                                                                                                                  <NA>
    ## 472                                                                                                                                                                  <NA>
    ## 473                                                                                                                                                                  <NA>
    ## 474                                                                                                                                                                  <NA>
    ## 475                                                                                                                                                                  <NA>
    ## 476                                                                                                                                                                  <NA>
    ## 477                                                                                                                                                                  <NA>
    ## 478                                                                                                                                                                  <NA>
    ## 479                                                                                                                                                                  <NA>
    ## 480                                                                                                                                                                  <NA>
    ## 481                                                                                                                                                                  <NA>
    ## 482                                                                                                                                                                  <NA>
    ## 483                                                                                                                                                                  <NA>
    ## 484                                                                                                                                                                  <NA>
    ## 485                                                                                                                                                                  <NA>
    ## 486                                                                                                                                                                  <NA>
    ## 487                                                                                                                                                                  <NA>
    ## 488                                                                                                                                                                  <NA>
    ## 489                                                                                                                                                                  <NA>
    ## 490                                                                                                                                                                  <NA>
    ## 491                                                                                                                                                                  <NA>
    ## 492                                                                                                                                                                  <NA>
    ## 493                                                                                                                                                                  <NA>
    ## 494                                                                                                                                                                  <NA>
    ## 495                                                                                                                                                                  <NA>
    ## 496                                                                                                                                                                  <NA>
    ## 497                                                                                                                                                                  <NA>
    ## 498                                                                                                                                                                  <NA>
    ## 499                                                                                                                                                                  <NA>
    ## 500                                                                                                                                                                  <NA>
    ## 501                                                                                                                                                                  <NA>
    ## 502                                                                                                                                                                  <NA>
    ## 503                                                                                                                                                                  <NA>
    ## 504                                                                                                                                                                  <NA>
    ## 505                                                                                                                                                                  <NA>
    ## 506                                                                                                                                                                  <NA>
    ## 507                                                                                                                                                                  <NA>
    ## 508 ALL DACE CAUGHT IN BERTRAND HAD BLACK PARASITES (ABOUT THE SIZE OF THE HEAD OF A PEN (I.E. 7MM) ALONG THEIR BODIES. THEY WERE ABUNDANT ON THEIR BODIES; EVEN COVERING
    ## 509                                                                                                                                                                  <NA>
    ## 510                                                                                                                                                                  <NA>
    ## 511                                                                                                                                                                  <NA>
    ## 512                                                                                                                                                                  <NA>
    ## 513                                                                                                                                                                  <NA>
    ## 514                                                                                                                                                                  <NA>
    ## 515                                                                                                                                                                  <NA>
    ## 516                                                                                                                                                                  <NA>
    ## 517                                                                                                                                                                  <NA>
    ## 518                                                                                                                                                                  <NA>
    ## 519                                                                                                                                                                  <NA>
    ## 520                                                                                                                                                                  <NA>
    ## 521                                                                                                                                                                  <NA>
    ## 522                                                                                                                                                                  <NA>
    ## 523                                                                                                                                                                  <NA>
    ## 524                                                                                                                                                                  <NA>
    ## 525                                                                                                                                                                  <NA>
    ## 526                                                                                                                                                                  <NA>
    ## 527                                                                                                                                                                  <NA>
    ## 528                                                                                                                                                                  <NA>
    ## 529                                                                                                                                                                  <NA>
    ## 530                                                                                                                                                                  <NA>
    ## 531                                                                                                                                                                  <NA>
    ## 532                                                                                                                                                                  <NA>
    ## 533                                                                                                                                                                  <NA>
    ## 534                                                                                                                                                                  <NA>
    ## 535                                                                                                                                                                  <NA>
    ## 536                                                                                                                                                                  <NA>
    ## 537                                                                                                                                                                  <NA>
    ## 538                                                                                                                                                                  <NA>
    ## 539                                                                                                                                                                  <NA>
    ## 540                                                                                                                                                                  <NA>
    ## 541                                                                                                                                                                  <NA>
    ## 542                                                                                                                                                                  <NA>
    ## 543                                                                                                                                                                  <NA>
    ## 544                                                                                                                                                                  <NA>
    ## 545                                                                                                                                                                  <NA>
    ## 546                                                                                                                                                                  <NA>
    ## 547                                                                                                                                                                  <NA>
    ## 548                                                                                                                                                                  <NA>
    ## 549                                                                                                                                                                  <NA>
    ## 550                                                                                                                                                                  <NA>
    ## 551                                                                                                                                                                  <NA>
    ## 552                                                                                                                                                                  <NA>
    ## 553                                                                                                                                                                  <NA>
    ## 554                                                                                                                                                                  <NA>
    ## 555                                                                                                                                                                  <NA>
    ## 556                                                                                                                                                                  <NA>
    ## 557                                                                                                                                                                  <NA>
    ## 558                                                                                                                                                                  <NA>
    ## 559                                                                                                                                                                  <NA>
    ## 560                                                                                                                                                                  <NA>
    ## 561                                                                                                                                                                  <NA>
    ## 562                                                                                                                                                                  <NA>
    ## 563                                                                                                                                                                  <NA>
    ## 564                                                                                                                                                                  <NA>
    ## 565                                                                                                                                                                  <NA>
    ## 566                                                                                                                                                                  <NA>
    ## 567                                                                                                                                                                  <NA>
    ## 568                                                                                                                                                                  <NA>
    ## 569                                                                                                                                                                  <NA>
    ## 570                                                                                                                                                                  <NA>
    ## 571                                                                                                                                                                  <NA>
    ## 572                                                                                                                                                                  <NA>
    ## 573                                                                                                                                                                  <NA>
    ## 574                                                                                                                                                                  <NA>
    ## 575                                                                                                                                                                  <NA>
    ## 576                                                                                                                                                                  <NA>
    ## 577                                                                                                                                                                  <NA>
    ## 578                                                                                                                                                                  <NA>
    ## 579                                                                                                                                                                  <NA>
    ## 580                                                                                                                                                                  <NA>
    ## 581                                                                                                                                                                  <NA>
    ## 582                                                                                                                                                                  <NA>
    ## 583                                                                                                                                                                  <NA>
    ## 584                                                                                                                                                                  <NA>
    ## 585                                                                                                                                                                  <NA>
    ## 586                                                                                                                                                                  <NA>
    ## 587                                                                                                                                                                  <NA>
    ## 588                                                                                                                                                                  <NA>
    ## 589                                                                                                                                                                  <NA>
    ## 590                                                                                                                                                                  <NA>
    ## 591                                                                                                                                                                  <NA>
    ## 592                                                                                                                                                                  <NA>
    ## 593                                                                                                                                                                  <NA>
    ## 594                                                                                                                                                                  <NA>
    ## 595                                                                                                                                                                  <NA>
    ## 596                                                                                                                                                                  <NA>
    ## 597                                                                                                                                                                  <NA>
    ## 598                                                                                                                                                                  <NA>
    ## 599                                                                                                                                                                  <NA>
    ## 600                                                                                                                                                                  <NA>
    ## 601                                                                                                                                                                  <NA>
    ## 602                                                                                                                                                                  <NA>
    ## 603                                                                                                                                                                  <NA>
    ## 604                                                                                                                                                                  <NA>
    ## 605                                                                                                                                                                  <NA>
    ## 606                                                                                                                                                                  <NA>
    ## 607                                                                                                                                                                  <NA>
    ## 608                                                                                                                                                                  <NA>
    ## 609                                                                                                                                                                  <NA>
    ## 610                                                                                                                                                                  <NA>
    ## 611                                                                                                                                                                  <NA>
    ## 612                                                                                                                                                                  <NA>
    ## 613                                                                                                                                                                  <NA>
    ## 614                                                                                                                                                                  <NA>
    ## 615                                                                                                                                                                  <NA>
    ## 616                                                                                                                                                                  <NA>
    ## 617                                                                                                                                                                  <NA>
    ## 618                                                                                                                                                                  <NA>
    ## 619                                                                                                                                                                  <NA>
    ## 620                                                                                                                                                                  <NA>
    ## 621                                                                                                                                                                  <NA>
    ## 622                                                                                                                                                                  <NA>
    ## 623                                                                                                                                                                  <NA>
    ## 624                                                                                                                                                                  <NA>
    ## 625                                                                                                                                                                  <NA>
    ## 626                                                                                                                                                                  <NA>
    ## 627                                                                                                                                                                  <NA>
    ## 628                                                                                                                                                                  <NA>
    ## 629                                                                                                                                                                  <NA>
    ## 630                                                                                                                                                                  <NA>
    ## 631                                                                                                                                                                  <NA>
    ## 632                                                                                                                                                                  <NA>
    ## 633                                                                                                                                                                  <NA>
    ## 634                                                                                                                                                                  <NA>
    ## 635                                                                                                                                                                  <NA>
    ## 636                                                                                                                                                                  <NA>
    ## 637                                                                                                                                                                  <NA>
    ## 638                                                                                                                                                                  <NA>
    ## 639                                                                                                                                                                  <NA>
    ## 640                                                                                                                                                                  <NA>
    ## 641                                                                                                                                                                  <NA>
    ## 642                                                                                                                                                                  <NA>
    ## 643                                                                                                                                                                  <NA>
    ## 644                                                                                                                                                                  <NA>
    ## 645                                                                                                                                                                  <NA>
    ## 646                                                                                                                                                                  <NA>
    ## 647                                                                                                                                                                  <NA>
    ## 648                                                                                                                                                                  <NA>
    ## 649                                                                                                                                                                  <NA>
    ## 650                                                                                                                                                                  <NA>
    ## 651                                                                                                                                                                  <NA>
    ## 652                                                                                                                                                                  <NA>
    ## 653                                                                                                                                                                  <NA>
    ## 654                                                                                                                                                                  <NA>
    ## 655                                                                                                                                                                  <NA>
    ## 656                                                                                                                                                                  <NA>
    ## 657                                                                                                                                                                  <NA>
    ## 658                                                                                                                                                                  <NA>
    ## 659                                                                                                                                                                  <NA>
    ## 660                                                                                                                                                                  <NA>
    ## 661                                                                                                                                                                  <NA>
    ## 662                                                                                                                                                                  <NA>
    ## 663                                                                                                                                                                  <NA>
    ## 664                                                                                                                                                                  <NA>
    ## 665                                                                                                                                                                  <NA>
    ## 666                                                                                                                                                                  <NA>
    ## 667                                                                                                                                                                  <NA>
    ## 668                                                                                                                                                                  <NA>
    ## 669                                                                                                                                                                  <NA>
    ## 670                                                                                                                                                                  <NA>
    ## 671                                                                                                                                                                  <NA>
    ## 672                                                                                                                                                                  <NA>
    ## 673                                                                                                                                                                  <NA>
    ## 674                                                                                                                                                                  <NA>
    ## 675                                                                                                                                                                  <NA>
    ## 676                                                                                                                                                                  <NA>
    ## 677                                                                                                                                                                  <NA>
    ## 678                                                                                                                                                                  <NA>
    ## 679                                                                                                                                                                  <NA>
    ## 680                                                                                                                                                                  <NA>
    ## 681                                                                                                                                                                  <NA>
    ## 682                                                                                                                                                                  <NA>
    ## 683                                                                                                                                                                  <NA>
    ## 684                                                                                                                                                                  <NA>
    ## 685                                                                                                                                                                  <NA>
    ## 686                                                                                                                                                                  <NA>
    ## 687                                                                                                                                                                  <NA>
    ## 688                                                                                                                                                                  <NA>
    ## 689                                                                                                                                                                  <NA>
    ## 690                                                                                                                                                                  <NA>
    ## 691                                                                                                                                                                  <NA>
    ## 692                                                                                                                                                                  <NA>
    ## 693                                                                                                                                                                  <NA>
    ## 694                                                                                                                                                                  <NA>
    ## 695                                                                                                                                                                  <NA>
    ## 696                                                                                                                                                                  <NA>
    ## 697                                                                                                                                                                  <NA>
    ## 698                                                                                                                                                                  <NA>
    ## 699                                                                                                                                                                  <NA>
    ## 700                                                                                                                                                                  <NA>
    ## 701                                                                                                                                                                  <NA>
    ## 702                                                                                                                                                                  <NA>
    ## 703                                                                                                                                                                  <NA>
    ## 704                                                                                                                                                                  <NA>
    ## 705                                                                                                                                                                  <NA>
    ## 706                                                                                                                                                                  <NA>
    ## 707                                                                                                                                                                  <NA>
    ## 708                                                                                                                                                                  <NA>
    ## 709                                                                                                                                                                  <NA>
    ## 710                                                                                                                                                                  <NA>
    ## 711                                                                                                                                                                  <NA>
    ## 712                                                                                                                                                                  <NA>
    ## 713                                                                                                                                                                  <NA>
    ## 714                                                                                                                                                                  <NA>
    ## 715                                                                                                                                                                  <NA>
    ## 716                                                                                                                                                                  <NA>
    ## 717                                                                                                                                                                  <NA>
    ## 718                                                                                                                                                                  <NA>
    ## 719                                                                                                                                                                  <NA>
    ## 720                                                                                                                                                                  <NA>
    ## 721                                                                                                                                                                  <NA>
    ## 722                                                                                                                                                                  <NA>
    ## 723                                                                                                                                                                  <NA>
    ## 724                                                                                                                                                                  <NA>
    ## 725                                                                                                                                                                  <NA>
    ## 726                                                                                                                                                                  <NA>
    ## 727                                                                                                                                                                  <NA>
    ## 728                                                                                                                                                                  <NA>
    ## 729                                                                                                                                                                  <NA>
    ## 730                                                                                                                                                                  <NA>
    ## 731                                                                                                                                                                  <NA>
    ## 732                                                                                                                                                                  <NA>
    ## 733                                                                                                                                                                  <NA>
    ## 734                                                                                                                                                                  <NA>
    ## 735                                                                                                                                                                  <NA>
    ## 736                                                                                                                                                                  <NA>
    ## 737                                                                                                                                                                  <NA>
    ## 738                                                                                                                                                                  <NA>
    ## 739                                                                                                                                                                  <NA>
    ## 740                                                                                                                                                                  <NA>
    ## 741                                                                                                                                                                  <NA>
    ## 742                                                                                                                                                                  <NA>
    ## 743                                                                                                                                                                  <NA>
    ## 744                                                                                                                                                                  <NA>
    ## 745                                                                                                                                                                  <NA>
    ## 746                                                                                                                                                                  <NA>
    ## 747                                                                                                                                                                  <NA>
    ## 748                                                                                                                                                                  <NA>
    ## 749                                                                                                                                                                  <NA>
    ## 750                                                                                                                                                                  <NA>
    ## 751                                                                                                                                                                  <NA>
    ## 752                                                                                                                                                                  <NA>
    ## 753                                                                                                                                                                  <NA>
    ## 754                                                                                                                                                                  <NA>
    ## 755                                                                                                                                                                  <NA>
    ## 756                                                                                                                                                                  <NA>
    ## 757                                                                                                                                                                  <NA>
    ## 758                                                                                                                                                                  <NA>
    ## 759                                                                                                                                                                  <NA>
    ## 760                                                                                                                                                                  <NA>
    ## 761                                                                                                                                                                  <NA>
    ## 762                                                                                                                                                                  <NA>
    ## 763                                                                                                                                                                  <NA>
    ## 764                                                                                                                                                                  <NA>
    ## 765                                                                                                                                                                  <NA>
    ## 766                                                                                                                                                                  <NA>
    ## 767                                                                                                                                                                  <NA>
    ## 768                                                                                                                                                                  <NA>
    ## 769                                                                                                                                                                  <NA>
    ## 770                                                                                                                                                                  <NA>
    ## 771                                                                                                                                                                  <NA>
    ## 772                                                                                                                                                                  <NA>
    ## 773                                                                                                                                                                  <NA>
    ## 774                                                                                                                                                                  <NA>
    ## 775                                                                                                                                                                  <NA>
    ## 776                                                                                                                                                                  <NA>
    ## 777                                                                                                                                                                  <NA>
    ## 778                                                                                                                                                                  <NA>
    ## 779                                                                                                                                                                  <NA>
    ## 780                                                                                                                                                                  <NA>
    ## 781                                                                                                                                                                  <NA>
    ## 782                                                                                                                                                                  <NA>
    ## 783                                                                                                                                                                  <NA>
    ## 784                                                                                                                                                                  <NA>
    ## 785                                                                                                                                                                  <NA>
    ## 786                                                                                                                                                                  <NA>
    ## 787                                                                                                                                                                  <NA>
    ## 788                                                                                                                                                                  <NA>
    ## 789                                                                                                                                                                  <NA>
    ## 790                                                                                                                                                                  <NA>
    ## 791                                                                                                                                                                  <NA>
    ## 792                                                                                                                                                                  <NA>
    ## 793                                                                                                                                                                  <NA>
    ## 794                                                                                                                                                                  <NA>
    ## 795                                                                                                                                                                  <NA>
    ## 796                                                                                                                                                                  <NA>
    ## 797                                                                                                                                                                  <NA>
    ## 798                                                                                                                                                                  <NA>
    ## 799                                                                                                                                                                  <NA>
    ## 800                                                                                                                                                                  <NA>
    ## 801                                                                                                                                                                  <NA>
    ## 802                                                                                                                                                                  <NA>
    ## 803                                                                                                                                                                  <NA>
    ## 804                                                                                                                                                                  <NA>
    ## 805                                                                                                                                                                  <NA>
    ## 806                                                                                                                                                                  <NA>
    ## 807                                                                                                                                                                  <NA>
    ## 808                                                                                                                                                                  <NA>
    ## 809                                                                                                                                                                  <NA>
    ## 810                                                                                                                                                                  <NA>
    ## 811                                                                                                                                                                  <NA>
    ## 812                                                                                                                                                                  <NA>
    ## 813                                                                                                                                                                  <NA>
    ## 814                                                                                                                                                                  <NA>
    ## 815                                                                                                                                                                  <NA>
    ## 816                                                                                                                                                                  <NA>
    ## 817                                                                                                                                                                  <NA>
    ## 818                                                                                                                                                                  <NA>
    ## 819                                                                                                                                                                  <NA>
    ## 820                                                                                                                                                                  <NA>
    ## 821                                                                                                                                                                  <NA>
    ## 822                                                                                                                                                                  <NA>
    ## 823                                                                                                                                                                  <NA>
    ## 824                                                                                                                                                                  <NA>
    ## 825                                                                                                                                                                  <NA>
    ## 826                                                                                                                                                                  <NA>
    ## 827                                                                                                                                                                  <NA>
    ## 828                                                                                                                                                                  <NA>
    ## 829                                                                                                                                                                  <NA>
    ## 830                                                                                                                                                                  <NA>
    ## 831                                                                                                                                                                  <NA>
    ## 832                                                                                                                                                                  <NA>
    ## 833                                                                                                                                                                  <NA>
    ## 834                                                                                                                                                                  <NA>
    ## 835                                                                                                                                                                  <NA>
    ## 836                                                                                                                                                                  <NA>
    ## 837                                                                                                                                                                  <NA>
    ## 838                                                                                                                                                                  <NA>
    ## 839                                                                                                                                                                  <NA>
    ## 840                                                                                                                                                                  <NA>
    ## 841                                                                                                                                                                  <NA>
    ## 842                                                                                                                                                                  <NA>
    ## 843                                                                                                                                                                  <NA>
    ## 844                                                                                                                                                                  <NA>
    ## 845                                                                                                                                                                  <NA>
    ## 846                                                                                                                                                                  <NA>
    ## 847                                                                                                                                                                  <NA>
    ## 848                                                                                                                                                                  <NA>
    ## 849                                                                                                                                                                  <NA>
    ## 850                                                                                                                                                                  <NA>
    ##     length_weight_ratio
    ## 1             2.2058824
    ## 2             3.1250000
    ## 3             9.5238095
    ## 4             7.7777778
    ## 5             6.6666667
    ## 6             4.4444444
    ## 7             2.1290323
    ## 8             2.1111111
    ## 9             1.1250000
    ## 10            2.3200000
    ## 11            1.9411765
    ## 12            1.9705882
    ## 13            2.8000000
    ## 14            2.6818182
    ## 15            4.1666667
    ## 16            4.1818182
    ## 17            6.3333333
    ## 18           90.0000000
    ## 19            3.0000000
    ## 20            6.6666667
    ## 21            5.3333333
    ## 22            3.3333333
    ## 23           10.0000000
    ## 24            4.6666667
    ## 25            4.4444444
    ## 26            7.2727273
    ## 27            8.8888889
    ## 28           12.0000000
    ## 29            9.0000000
    ## 30            7.0000000
    ## 31            6.4285714
    ## 32            5.3846154
    ## 33           12.0000000
    ## 34           12.0000000
    ## 35            3.4375000
    ## 36            6.9230769
    ## 37            7.5000000
    ## 38            7.0000000
    ## 39            5.3846154
    ## 40            4.7826087
    ## 41            5.0000000
    ## 42            8.0000000
    ## 43            5.2631579
    ## 44           10.0000000
    ## 45           12.0000000
    ## 46            2.1212121
    ## 47            1.6666667
    ## 48            2.2413793
    ## 49            2.6190476
    ## 50            7.1428571
    ## 51           11.6666667
    ## 52            6.0000000
    ## 53            3.7931034
    ## 54            3.3333333
    ## 55            3.8709677
    ## 56            5.6250000
    ## 57            7.5000000
    ## 58            2.5000000
    ## 59            2.8571429
    ## 60            2.8571429
    ## 61            3.1578947
    ## 62            2.0967742
    ## 63            1.1309524
    ## 64            2.7272727
    ## 65            2.7272727
    ## 66            1.9117647
    ## 67            1.2337662
    ## 68            4.3333333
    ## 69            3.4615385
    ## 70            5.2631579
    ## 71            4.1666667
    ## 72            3.8888889
    ## 73           10.0000000
    ## 74            6.3636364
    ## 75           15.0000000
    ## 76            3.7500000
    ## 77            7.6923077
    ## 78            3.1428571
    ## 79           13.3333333
    ## 80            8.3333333
    ## 81           11.4285714
    ## 82                   NA
    ## 83            8.5000000
    ## 84            0.4666667
    ## 85            1.7317073
    ## 86            5.3750000
    ## 87            5.8571429
    ## 88            1.9142857
    ## 89            6.3333333
    ## 90            1.8378378
    ## 91            3.3125000
    ## 92            3.4666667
    ## 93            1.4561404
    ## 94            3.0000000
    ## 95            2.7727273
    ## 96            0.7832168
    ## 97            3.1176471
    ## 98            0.6793478
    ## 99            2.3846154
    ## 100           1.8378378
    ## 101           2.6000000
    ## 102           5.5000000
    ## 103           2.2962963
    ## 104           3.4666667
    ## 105           0.7828947
    ## 106           4.6666667
    ## 107           1.7692308
    ## 108           1.1265823
    ## 109           2.0000000
    ## 110          11.1666667
    ## 111           2.4000000
    ## 112           1.5833333
    ## 113           2.3333333
    ## 114           2.2333333
    ## 115           1.0215054
    ## 116           1.8620690
    ## 117           3.9000000
    ## 118           1.2564103
    ## 119           3.2857143
    ## 120           3.8333333
    ## 121           4.6250000
    ## 122           2.0000000
    ## 123           1.7142857
    ## 124           8.1818182
    ## 125           1.1643836
    ## 126           3.9166667
    ## 127           3.1666667
    ## 128           1.7317073
    ## 129           2.1666667
    ## 130           1.5813953
    ## 131           3.7692308
    ## 132           2.5833333
    ## 133           3.7272727
    ## 134           2.2000000
    ## 135           2.1000000
    ## 136           3.6666667
    ## 137           3.6000000
    ## 138           5.0000000
    ## 139           3.7692308
    ## 140           2.7500000
    ## 141           6.8888889
    ## 142           5.3125000
    ## 143           4.8888889
    ## 144           5.5000000
    ## 145          11.4285714
    ## 146           6.4285714
    ## 147           3.4375000
    ## 148           2.8421053
    ## 149           3.2500000
    ## 150           4.1818182
    ## 151           6.8000000
    ## 152           4.5000000
    ## 153           2.8823529
    ## 154                  NA
    ## 155           2.7727273
    ## 156           2.3928571
    ## 157           5.1111111
    ## 158                  NA
    ## 159                  NA
    ## 160                  NA
    ## 161                  NA
    ## 162                  NA
    ## 163                  NA
    ## 164                  NA
    ## 165                  NA
    ## 166                  NA
    ## 167                  NA
    ## 168                  NA
    ## 169                  NA
    ## 170                  NA
    ## 171                  NA
    ## 172                  NA
    ## 173                  NA
    ## 174                  NA
    ## 175                  NA
    ## 176                  NA
    ## 177                  NA
    ## 178                  NA
    ## 179                  NA
    ## 180                  NA
    ## 181                  NA
    ## 182                  NA
    ## 183                  NA
    ## 184                  NA
    ## 185                  NA
    ## 186                  NA
    ## 187                  NA
    ## 188                  NA
    ## 189                  NA
    ## 190                  NA
    ## 191                  NA
    ## 192                  NA
    ## 193                  NA
    ## 194                  NA
    ## 195                  NA
    ## 196                  NA
    ## 197                  NA
    ## 198                  NA
    ## 199                  NA
    ## 200                  NA
    ## 201                  NA
    ## 202                  NA
    ## 203                  NA
    ## 204                  NA
    ## 205                  NA
    ## 206                  NA
    ## 207                  NA
    ## 208                  NA
    ## 209                  NA
    ## 210                  NA
    ## 211                  NA
    ## 212                  NA
    ## 213                  NA
    ## 214                  NA
    ## 215                  NA
    ## 216                  NA
    ## 217                  NA
    ## 218                  NA
    ## 219                  NA
    ## 220                  NA
    ## 221                  NA
    ## 222                  NA
    ## 223                  NA
    ## 224                  NA
    ## 225                  NA
    ## 226                  NA
    ## 227                  NA
    ## 228                  NA
    ## 229                  NA
    ## 230                  NA
    ## 231                  NA
    ## 232                  NA
    ## 233                  NA
    ## 234                  NA
    ## 235                  NA
    ## 236                  NA
    ## 237                  NA
    ## 238                  NA
    ## 239                  NA
    ## 240                  NA
    ## 241                  NA
    ## 242                  NA
    ## 243                  NA
    ## 244                  NA
    ## 245                  NA
    ## 246                  NA
    ## 247                  NA
    ## 248                  NA
    ## 249                  NA
    ## 250                  NA
    ## 251                  NA
    ## 252                  NA
    ## 253                  NA
    ## 254                  NA
    ## 255                  NA
    ## 256                  NA
    ## 257                  NA
    ## 258                  NA
    ## 259                  NA
    ## 260                  NA
    ## 261                  NA
    ## 262                  NA
    ## 263                  NA
    ## 264                  NA
    ## 265                  NA
    ## 266                  NA
    ## 267                  NA
    ## 268                  NA
    ## 269                  NA
    ## 270                  NA
    ## 271                  NA
    ## 272                  NA
    ## 273                  NA
    ## 274                  NA
    ## 275                  NA
    ## 276                  NA
    ## 277                  NA
    ## 278                  NA
    ## 279                  NA
    ## 280                  NA
    ## 281                  NA
    ## 282                  NA
    ## 283                  NA
    ## 284                  NA
    ## 285                  NA
    ## 286                  NA
    ## 287                  NA
    ## 288                  NA
    ## 289                  NA
    ## 290                  NA
    ## 291                  NA
    ## 292                  NA
    ## 293                  NA
    ## 294                  NA
    ## 295                  NA
    ## 296                  NA
    ## 297                  NA
    ## 298                  NA
    ## 299                  NA
    ## 300                  NA
    ## 301                  NA
    ## 302                  NA
    ## 303                  NA
    ## 304                  NA
    ## 305                  NA
    ## 306                  NA
    ## 307                  NA
    ## 308                  NA
    ## 309                  NA
    ## 310                  NA
    ## 311                  NA
    ## 312                  NA
    ## 313                  NA
    ## 314                  NA
    ## 315                  NA
    ## 316                  NA
    ## 317                  NA
    ## 318                  NA
    ## 319                  NA
    ## 320           0.8606557
    ## 321           7.7500000
    ## 322           2.1290323
    ## 323           7.7777778
    ## 324           2.7500000
    ## 325           9.2857143
    ## 326           5.5714286
    ## 327           4.1818182
    ## 328           5.1250000
    ## 329          25.0000000
    ## 330           5.2500000
    ## 331          10.0000000
    ## 332           5.0000000
    ## 333          16.2500000
    ## 334           6.1538462
    ## 335          14.0000000
    ## 336          10.0000000
    ## 337           5.1250000
    ## 338           2.5217391
    ## 339           8.0000000
    ## 340          10.6666667
    ## 341           5.2857143
    ## 342           4.8888889
    ## 343          10.3333333
    ## 344           2.8500000
    ## 345           8.5714286
    ## 346          12.0000000
    ## 347           8.3333333
    ## 348           8.0000000
    ## 349          13.0000000
    ## 350          13.3333333
    ## 351           5.0000000
    ## 352           5.6250000
    ## 353           6.6666667
    ## 354           6.9230769
    ## 355           6.2500000
    ## 356           7.5000000
    ## 357           8.3333333
    ## 358          15.0000000
    ## 359           6.6666667
    ## 360          11.6666667
    ## 361          15.0000000
    ## 362           7.5000000
    ## 363           2.7500000
    ## 364                  NA
    ## 365                  NA
    ## 366                  NA
    ## 367           8.7500000
    ## 368           5.0000000
    ## 369           7.5000000
    ## 370           6.6666667
    ## 371         100.0000000
    ## 372          10.0000000
    ## 373          15.0000000
    ## 374           6.4285714
    ## 375           7.7777778
    ## 376           6.6666667
    ## 377           7.0000000
    ## 378          14.0000000
    ## 379           7.0000000
    ## 380           8.3333333
    ## 381          13.3333333
    ## 382           7.5000000
    ## 383           2.6190476
    ## 384           2.6000000
    ## 385           2.4137931
    ## 386           4.0909091
    ## 387           1.1046512
    ## 388           1.7045455
    ## 389           3.3333333
    ## 390           0.9417808
    ## 391           2.6086957
    ## 392           2.4074074
    ## 393           0.6092437
    ## 394           1.6666667
    ## 395           1.5306122
    ## 396           1.0989011
    ## 397          12.0000000
    ## 398           0.7971014
    ## 399           3.6666667
    ## 400           0.6741573
    ## 401           2.6363636
    ## 402          15.0000000
    ## 403           2.3214286
    ## 404           2.1428571
    ## 405           1.0769231
    ## 406           2.4137931
    ## 407           2.4489796
    ## 408           6.4285714
    ## 409           2.7500000
    ## 410           5.0000000
    ## 411           3.5714286
    ## 412           1.7500000
    ## 413           2.9444444
    ## 414           0.3474747
    ## 415           1.1585366
    ## 416           3.0555556
    ## 417           4.8888889
    ## 418           2.4000000
    ## 419           3.5384615
    ## 420                  NA
    ## 421           5.7142857
    ## 422           2.8260870
    ## 423           3.2352941
    ## 424           3.0555556
    ## 425           1.1842105
    ## 426           8.8888889
    ## 427           2.1428571
    ## 428          10.0000000
    ## 429           1.0795455
    ## 430          10.0000000
    ## 431          15.0000000
    ## 432           5.2631579
    ## 433                  NA
    ## 434                  NA
    ## 435          20.0000000
    ## 436           8.0000000
    ## 437           5.7142857
    ## 438           1.9117647
    ## 439           1.2857143
    ## 440          12.0000000
    ## 441           4.7619048
    ## 442           5.2631579
    ## 443                  NA
    ## 444           3.2352941
    ## 445           1.9444444
    ## 446           1.7021277
    ## 447           5.7142857
    ## 448          12.5000000
    ## 449           1.2328767
    ## 450           1.5000000
    ## 451           2.7777778
    ## 452           2.6086957
    ## 453           2.0312500
    ## 454           3.2352941
    ## 455          10.0000000
    ## 456           5.0000000
    ## 457           4.3478261
    ## 458           4.2857143
    ## 459           8.8888889
    ## 460           1.8965517
    ## 461          10.0000000
    ## 462          15.0000000
    ## 463           2.6315789
    ## 464           4.5000000
    ## 465           5.0000000
    ## 466           8.0000000
    ## 467          15.0000000
    ## 468           5.2380952
    ## 469           3.4482759
    ## 470                  NA
    ## 471                  NA
    ## 472                  NA
    ## 473           2.9411765
    ## 474           6.4285714
    ## 475          15.0000000
    ## 476           9.2857143
    ## 477           2.6086957
    ## 478           2.7272727
    ## 479          12.5000000
    ## 480           8.7500000
    ## 481          12.5000000
    ## 482          13.0000000
    ## 483          10.8333333
    ## 484           4.5454545
    ## 485           8.3333333
    ## 486           7.6923077
    ## 487          10.8333333
    ## 488           4.1666667
    ## 489           4.4444444
    ## 490           7.5000000
    ## 491           3.9285714
    ## 492           2.5000000
    ## 493          10.0000000
    ## 494           8.5714286
    ## 495          11.6666667
    ## 496           6.0000000
    ## 497           9.1666667
    ## 498           7.2727273
    ## 499           7.5000000
    ## 500           1.1428571
    ## 501           1.8181818
    ## 502           0.7708049
    ## 503           0.9929078
    ## 504           1.6228070
    ## 505           1.9938650
    ## 506           1.9142857
    ## 507           4.9462366
    ## 508           2.3208191
    ## 509           6.5161290
    ## 510           4.6511628
    ## 511           0.5070318
    ## 512           2.7669903
    ## 513           3.2911392
    ## 514           4.3157895
    ## 515           7.9545455
    ## 516           6.7441860
    ## 517           9.2592593
    ## 518           2.6540284
    ## 519           3.2500000
    ## 520           3.5329341
    ## 521           4.5918367
    ## 522           4.8387097
    ## 523           3.3333333
    ## 524           5.6338028
    ## 525           5.1063830
    ## 526           6.7241379
    ## 527           5.5405405
    ## 528           2.6609442
    ## 529           3.1976744
    ## 530           2.0241692
    ## 531           8.2222222
    ## 532           3.0285714
    ## 533           3.2947977
    ## 534          32.7272727
    ## 535           5.2777778
    ## 536           5.5248619
    ## 537          10.8108108
    ## 538           6.6666667
    ## 539           0.7836991
    ## 540           3.9426523
    ## 541           4.4000000
    ## 542           6.6115702
    ## 543           6.5359477
    ## 544           7.3913043
    ## 545           2.1069182
    ## 546           3.2666667
    ## 547           4.2857143
    ## 548           0.2533156
    ## 549           5.7142857
    ## 550           3.3125000
    ## 551           6.1428571
    ## 552           2.1333333
    ## 553           4.4000000
    ## 554           6.3492063
    ## 555           5.7823129
    ## 556           5.2023121
    ## 557           8.7500000
    ## 558           4.6315789
    ## 559                  NA
    ## 560           7.0909091
    ## 561           1.2801205
    ## 562           0.9465792
    ## 563           0.7548430
    ## 564           0.9378085
    ## 565           1.3352273
    ## 566           2.1538462
    ## 567           5.4929577
    ## 568           3.6986301
    ## 569           2.2382671
    ## 570           2.4390244
    ## 571           1.8300654
    ## 572           1.7994859
    ## 573           1.7195767
    ## 574           1.6504854
    ## 575           1.6747573
    ## 576           2.1232877
    ## 577           2.2857143
    ## 578           3.6486486
    ## 579                  NA
    ## 580           3.4375000
    ## 581           3.0030030
    ## 582           3.8235294
    ## 583           3.1914894
    ## 584           4.5000000
    ## 585           5.3571429
    ## 586           7.4468085
    ## 587          13.8297872
    ## 588          17.1428571
    ## 589           7.4074074
    ## 590           9.7402597
    ## 591          33.3333333
    ## 592           1.1042945
    ## 593           2.6244344
    ## 594           3.5483871
    ## 595           1.5804598
    ## 596           3.1496063
    ## 597           2.6368159
    ## 598           2.7184466
    ## 599           2.8042328
    ## 600           3.3939394
    ## 601           2.6923077
    ## 602           4.1052632
    ## 603           3.1791908
    ## 604           4.5744681
    ## 605           3.1205674
    ## 606           2.5116279
    ## 607           5.1724138
    ## 608           6.8702290
    ## 609           5.8333333
    ## 610           9.0909091
    ## 611           1.0208333
    ## 612           5.5555556
    ## 613           3.9597315
    ## 614           3.4838710
    ## 615           1.3455657
    ## 616           6.1764706
    ## 617           1.4146341
    ## 618           4.1044776
    ## 619           3.8636364
    ## 620           2.9166667
    ## 621           5.7142857
    ## 622                  NA
    ## 623           1.8965517
    ## 624           4.1111111
    ## 625                  NA
    ## 626           2.2222222
    ## 627           1.8421053
    ## 628           3.0769231
    ## 629           1.6304348
    ## 630           1.9696970
    ## 631           1.9117647
    ## 632           1.5306122
    ## 633           1.8055556
    ## 634           1.8125000
    ## 635           2.5000000
    ## 636           1.9444444
    ## 637           2.0967742
    ## 638           2.2580645
    ## 639           1.4150943
    ## 640           2.1666667
    ## 641           2.5000000
    ## 642           1.7500000
    ## 643           1.9117647
    ## 644           1.5909091
    ## 645           1.6111111
    ## 646           2.5000000
    ## 647           1.6666667
    ## 648           1.8292683
    ## 649           2.1428571
    ## 650           3.5714286
    ## 651           1.4150943
    ## 652           2.0689655
    ## 653           2.3076923
    ## 654           1.6250000
    ## 655           2.2916667
    ## 656           1.8918919
    ## 657           4.0000000
    ## 658           2.5000000
    ## 659           3.4375000
    ## 660           3.8888889
    ## 661                  NA
    ## 662                  NA
    ## 663           1.4583333
    ## 664           2.2222222
    ## 665           2.0588235
    ## 666           1.8421053
    ## 667           2.4193548
    ## 668           1.9230769
    ## 669           2.7500000
    ## 670           2.0312500
    ## 671           1.8571429
    ## 672           2.0000000
    ## 673           1.8918919
    ## 674           1.8750000
    ## 675           2.6190476
    ## 676           2.1875000
    ## 677           2.4000000
    ## 678           2.0588235
    ## 679                  NA
    ## 680           1.9714286
    ## 681           2.1212121
    ## 682           1.7857143
    ## 683           1.8461538
    ## 684           2.5600000
    ## 685           1.9705882
    ## 686           1.7105263
    ## 687           1.3725490
    ## 688           1.2545455
    ## 689                  NA
    ## 690           3.8135593
    ## 691           4.5652174
    ## 692           5.0537634
    ## 693           2.1562500
    ## 694           2.6000000
    ## 695           2.5652174
    ## 696           4.6236559
    ## 697           3.4394904
    ## 698           5.3658537
    ## 699           5.2500000
    ## 700           2.1220930
    ## 701           2.0161290
    ## 702           1.7701149
    ## 703           2.7232143
    ## 704           1.7675545
    ## 705           3.0303030
    ## 706           2.6991150
    ## 707           3.5294118
    ## 708           3.8842975
    ## 709           2.3344948
    ## 710           2.1812081
    ## 711           9.7435897
    ## 712           3.6363636
    ## 713           7.2222222
    ## 714           2.7149321
    ## 715           5.4545455
    ## 716           3.3526012
    ## 717                  NA
    ## 718                  NA
    ## 719                  NA
    ## 720           1.9875776
    ## 721           1.7512690
    ## 722           2.6363636
    ## 723           2.0000000
    ## 724           3.5714286
    ## 725           6.0000000
    ## 726           6.0000000
    ## 727           6.0000000
    ## 728           2.8775510
    ## 729                  NA
    ## 730           2.2121212
    ## 731           3.0500000
    ## 732           3.5000000
    ## 733           5.5000000
    ## 734           1.5084746
    ## 735           1.8500000
    ## 736           2.7500000
    ## 737           0.5652174
    ## 738           3.4615385
    ## 739           2.2666667
    ## 740           2.5000000
    ## 741           5.7142857
    ## 742           5.4285714
    ## 743           3.3333333
    ## 744           1.6666667
    ## 745           2.1212121
    ## 746           4.5000000
    ## 747           5.0000000
    ## 748           2.0967742
    ## 749           1.4150943
    ## 750           1.5677966
    ## 751           1.4252874
    ## 752           1.8666667
    ## 753           2.5714286
    ## 754           1.7500000
    ## 755           1.8571429
    ## 756           1.9574468
    ## 757           1.9696970
    ## 758           4.4444444
    ## 759           1.5957447
    ## 760           4.0000000
    ## 761           2.6086957
    ## 762           4.3750000
    ## 763           2.2336770
    ## 764           2.0604396
    ## 765           1.5841584
    ## 766           2.4774775
    ## 767           1.4285714
    ## 768           5.4794521
    ## 769           1.3605442
    ## 770           2.1246459
    ## 771           1.8617021
    ## 772           2.3673469
    ## 773           2.3465704
    ## 774           1.6293279
    ## 775           4.5945946
    ## 776           2.4336283
    ## 777           1.8867925
    ## 778           1.9210526
    ## 779           1.0714286
    ## 780           1.7108168
    ## 781           1.8203883
    ## 782           2.6976744
    ## 783           1.2365591
    ## 784           3.3653846
    ## 785           3.6363636
    ## 786           1.7429194
    ## 787           2.9040404
    ## 788           2.4436090
    ## 789           2.5000000
    ## 790           3.1250000
    ## 791           6.0344828
    ## 792                  NA
    ## 793           2.1428571
    ## 794           1.8309859
    ## 795           1.0467980
    ## 796                  NA
    ## 797           1.6452442
    ## 798           3.5632184
    ## 799           6.4406780
    ## 800           2.9147982
    ## 801           3.1690141
    ## 802           2.9605263
    ## 803           3.0821918
    ## 804           3.1666667
    ## 805           4.4000000
    ## 806           3.3582090
    ## 807           3.2178218
    ## 808           2.0700637
    ## 809           3.3834586
    ## 810           2.6388889
    ## 811           2.0942408
    ## 812           2.9850746
    ## 813           1.9753086
    ## 814           1.8518519
    ## 815           1.6083916
    ## 816           2.7624309
    ## 817           2.4390244
    ## 818           4.1414141
    ## 819           3.0952381
    ## 820           3.3834586
    ## 821           2.6633166
    ## 822           4.0740741
    ## 823           1.7045455
    ## 824                  NA
    ## 825                  NA
    ## 826                  NA
    ## 827           2.3121387
    ## 828           2.4390244
    ## 829           2.1296296
    ## 830           2.2900763
    ## 831           3.1250000
    ## 832           2.5560538
    ## 833           2.0578778
    ## 834           3.7037037
    ## 835           3.8461538
    ## 836           2.2357724
    ## 837           1.8539326
    ## 838           3.6800000
    ## 839           3.8059701
    ## 840           2.1886792
    ## 841           2.1912351
    ## 842           3.4188034
    ## 843           3.9130435
    ## 844           1.8678161
    ## 845           1.5217391
    ## 846           1.7349398
    ## 847           2.8985507
    ## 848           1.9178082
    ## 849           3.5483871
    ## 850           3.7241379

``` r
head(fish_sampling)
```

    ##   system riffle pass species weight_g length_cm       Date shocking_effort_s
    ## 1  Pepin     14    1    LAMP     6.80        15 08/19/2021                 -
    ## 2  Pepin     14    1    LAMP     4.80        15 08/19/2021                 -
    ## 3  Pepin     14    1    LAMP     1.05        10 08/19/2021                 -
    ## 4  Pepin     14    1    LAMP     0.90         7 08/19/2021                 -
    ## 5  Pepin     14    1    LAMP     0.90         6 08/19/2021                 -
    ## 6  Pepin     14    1    LAMP     2.25        10 08/19/2021                 -
    ##   sampled_riffle_length_m                Comments ...11 length_weight_ratio
    ## 1                     6.4      Water temp = 14.6C  <NA>            2.205882
    ## 2                       - Pass 1: 200V, 30Hz, 12%  <NA>            3.125000
    ## 3                       -                    <NA>  <NA>            9.523810
    ## 4                       -                    <NA>  <NA>            7.777778
    ## 5                       -                    <NA>  <NA>            6.666667
    ## 6                       -                    <NA>  <NA>            4.444444

By modifying the date column of my data, I will be able to quickly
assure that there is no date bias in my data. With several more field
seasons left of my project, as I begin to amalgamate data, I need a date
reference in which to compare fish abundance to. Part of my research
involves changes in fish activity and the timing of activities such as
spawning from year to year as a result of environmental changes. By
organizing my date as year-week, I will be able to organize my data over
the following field seasons according to “week” so as to compare fish
activity accordingly.

<!----------------------------------------------------------------------------->

# Exercise 2: Modelling

## 2.0 (no points)

Pick a research question, and pick a variable of interest (we’ll call it
“Y”) that’s relevant to the research question. Indicate these.

<!-------------------------- Start your work below ---------------------------->

**Research Question**: How does the length to weight ratio differ
between species in Pepin Creek and Bertrand Creek?

**Variable of interest**: length to weight ratio of different species

<!----------------------------------------------------------------------------->

## 2.1 (5 points)

Fit a model or run a hypothesis test that provides insight on this
variable with respect to the research question. Store the model object
as a variable, and print its output to screen. We’ll omit having to
justify your choice, because we don’t expect you to know about model
specifics in STAT 545.

-   **Note**: It’s OK if you don’t know how these models/tests work.
    Here are some examples of things you can do here, but the sky’s the
    limit.
    -   You could fit a model that makes predictions on Y using another
        variable, by using the `lm()` function.
    -   You could test whether the mean of Y equals 0 using `t.test()`,
        or maybe the mean across two groups are different using
        `t.test()`, or maybe the mean across multiple groups are
        different using `anova()` (you may have to pivot your data for
        the latter two).
    -   You could use `lm()` to test for significance of regression.

<!-------------------------- Start your work below ---------------------------->

In this exercise I used a one-way ANOVA to assess whether the length to
weight ratio differed significatnly among the fish species sampled. I
stored the output of the ANOVA as a variable

``` r
one.way<-aov(length_weight_ratio ~ species,data = fish_sampling)
summary(one.way)
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## species       6   4286   714.3   22.34 <2e-16 ***
    ## Residuals   652  20843    32.0                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 191 observations deleted due to missingness

<!----------------------------------------------------------------------------->

## 2.2 (5 points)

Produce something relevant from your fitted model: either predictions on
Y, or a single value like a regression coefficient or a p-value.

-   Be sure to indicate in writing what you chose to produce.
-   Your code should either output a tibble (in which case you should
    indicate the column that contains the thing you’re looking for), or
    the thing you’re looking for itself.
-   Obtain your results using the `broom` package if possible. If your
    model is not compatible with the broom function you’re needing, then
    you can obtain your results by some other means, but first indicate
    which broom function is not compatible.

<!-------------------------- Start your work below ---------------------------->

Using the ‘broom’ package, I analyzed the output of my ANOVA from
exercise 2.1. As can be seen below, it appears as though the the length
to weight ratio is not uniform among the species sampled in Bertrand
Creek and Pepin Creek. The null hypothesis states that all species
sampled have a uniform length to weight ratio. However, the p-value is
less than the alpha value of 0.05 therefore, I can reject my null
hypothesis.

``` r
tidy_one.way<-tidy(one.way)
tidy_one.way
```

    ## # A tibble: 2 × 6
    ##   term         df  sumsq meansq statistic   p.value
    ##   <chr>     <dbl>  <dbl>  <dbl>     <dbl>     <dbl>
    ## 1 species       6  4286.  714.       22.3  5.40e-24
    ## 2 Residuals   652 20843.   32.0      NA   NA

<!----------------------------------------------------------------------------->

# Exercise 3: Reading and writing data

Get set up for this exercise by making a folder called `output` in the
top level of your project folder / repository. You’ll be saving things
there.

## 3.1 (5 points)

Take a summary table that you made from Milestone 2 (Exercise 1.2), and
write it as a csv file in your `output` folder. Use the `here::here()`
function.

-   **Robustness criteria**: You should be able to move your Mini
    Project repository / project folder to some other location on your
    computer, or move this very Rmd file to another location within your
    project repository / folder, and your code should still work.
-   **Reproducibility criteria**: You should be able to delete the csv
    file, and remake it simply by knitting this Rmd file.

<!-------------------------- Start your work below ---------------------------->

``` r
species_abundance<-fish_sampling%>%
  group_by(system,species)%>%
  tally()
species_abundance
```

    ## # A tibble: 15 × 3
    ## # Groups:   system [2]
    ##    system   species      n
    ##    <chr>    <chr>    <int>
    ##  1 Bertrand CO           2
    ##  2 Bertrand CRAYFISH    14
    ##  3 Bertrand CUT         30
    ##  4 Bertrand DAC        126
    ##  5 Bertrand LAMP         2
    ##  6 Bertrand RBT         17
    ##  7 Bertrand STK         41
    ##  8 Pepin    CO         102
    ##  9 Pepin    CRAYFISH    40
    ## 10 Pepin    CUT         79
    ## 11 Pepin    DAC         15
    ## 12 Pepin    LAMP       199
    ## 13 Pepin    RBT         58
    ## 14 Pepin    STK         94
    ## 15 Pepin    TROUT       31

``` r
write.csv(species_abundance,here::here("ouput","Species_Abundance.csv"))
```

<!----------------------------------------------------------------------------->

## 3.2 (5 points)

Write your model object from Exercise 2 to an R binary file (an RDS),
and load it again. Be sure to save the binary file in your `output`
folder. Use the functions `saveRDS()` and `readRDS()`.

-   The same robustness and reproducibility criteria as in 3.1 apply
    here.

<!-------------------------- Start your work below ---------------------------->

``` r
saveRDS(tidy_one.way,here::here("ouput","One_way_ANOVA.RDS"))
readRDS(here::here("ouput","One_way_ANOVA.RDS"))
```

    ## # A tibble: 2 × 6
    ##   term         df  sumsq meansq statistic   p.value
    ##   <chr>     <dbl>  <dbl>  <dbl>     <dbl>     <dbl>
    ## 1 species       6  4286.  714.       22.3  5.40e-24
    ## 2 Residuals   652 20843.   32.0      NA   NA

<!----------------------------------------------------------------------------->

# Tidy Repository

Now that this is your last milestone, your entire project repository
should be organized. Here are the criteria we’re looking for.

## Main README (3 points)

There should be a file named `README.md` at the top level of your
repository. Its contents should automatically appear when you visit the
repository on GitHub.

Minimum contents of the README file:

-   In a sentence or two, explains what this repository is, so that
    future-you or someone else stumbling on your repository can be
    oriented to the repository.
-   In a sentence or two (or more??), briefly explains how to engage
    with the repository. You can assume the person reading knows the
    material from STAT 545A. Basically, if a visitor to your repository
    wants to explore your project, what should they know?

Once you get in the habit of making README files, and seeing more README
files in other projects, you’ll wonder how you ever got by without them!
They are tremendously helpful.

## File and Folder structure (3 points)

You should have at least four folders in the top level of your
repository: one for each milestone, and one output folder. If there are
any other folders, these are explained in the main README.

Each milestone document is contained in its respective folder, and
nowhere else.

Every level-1 folder (that is, the ones stored in the top level, like
“Milestone1” and “output”) has a `README` file, explaining in a sentence
or two what is in the folder, in plain language (it’s enough to say
something like “This folder contains the source for Milestone 1”).

## Output (2 points)

All output is recent and relevant:

-   All Rmd files have been `knit`ted to their output, and all data
    files saved from Exercise 3 above appear in the `output` folder.
-   All of these output files are up-to-date – that is, they haven’t
    fallen behind after the source (Rmd) files have been updated.
-   There should be no relic output files. For example, if you were
    knitting an Rmd to html, but then changed the output to be only a
    markdown file, then the html file is a relic and should be deleted.

Our recommendation: delete all output files, and re-knit each
milestone’s Rmd file, so that everything is up to date and relevant.

PS: there’s a way where you can run all project code using a single
command, instead of clicking “knit” three times. More on this in STAT
545B!

## Error-free code (1 point)

This Milestone 3 document knits error-free. (We’ve already graded this
aspect for Milestone 1 and 2)

## Tagged release (1 point)

You’ve tagged a release for Milestone 3. (We’ve already graded this
aspect for Milestone 1 and 2)
