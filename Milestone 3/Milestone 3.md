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
  select(system:Date)%>%
  mutate(Date=mdy(Date))%>%
  mutate(Date=yearweek(Date))
```

    ##       system riffle pass  species weight_g length_cm     Date
    ## 1      Pepin  14.00 1.00     LAMP     6.80     15.00 2021 W33
    ## 2      Pepin  14.00 1.00     LAMP     4.80     15.00 2021 W33
    ## 3      Pepin  14.00 1.00     LAMP     1.05     10.00 2021 W33
    ## 4      Pepin  14.00 1.00     LAMP     0.90      7.00 2021 W33
    ## 5      Pepin  14.00 1.00     LAMP     0.90      6.00 2021 W33
    ## 6      Pepin  14.00 1.00     LAMP     2.25     10.00 2021 W33
    ## 7      Pepin  14.00 1.00      CUT     3.10      6.60 2021 W33
    ## 8      Pepin  14.00 1.00       CO     2.70      5.70 2021 W33
    ## 9      Pepin  14.00 1.00      CUT     6.40      7.20 2021 W33
    ## 10     Pepin  14.00 1.00      RBT     2.50      5.80 2021 W33
    ## 11     Pepin  14.00 1.00      CUT     3.40      6.60 2021 W33
    ## 12     Pepin  14.00 1.00      CUT     3.40      6.70 2021 W33
    ## 13     Pepin  14.00 1.00      STK     2.00      5.60 2021 W33
    ## 14     Pepin  14.00 1.00      CUT     2.20      5.90 2021 W33
    ## 15     Pepin  14.00 1.00     LAMP     3.60     15.00 2021 W33
    ## 16     Pepin  14.00 1.00       CO     1.10      4.60 2021 W33
    ## 17     Pepin  14.00 1.00      STK     0.60      3.80 2021 W33
    ## 18     Pepin  14.00 1.00      RBT     0.04      3.60 2021 W33
    ## 19     Pepin  14.00 1.00      CUT     1.60      4.80 2021 W33
    ## 20     Pepin  14.00 1.00     LAMP     1.20      8.00 2021 W33
    ## 21     Pepin  14.00 1.00     LAMP     1.50      8.00 2021 W33
    ## 22     Pepin  14.00 1.00     LAMP     3.60     12.00 2021 W33
    ## 23     Pepin  14.00 1.00     LAMP     0.50      5.00 2021 W33
    ## 24     Pepin  14.00 1.00     LAMP     1.50      7.00 2021 W33
    ## 25     Pepin  14.00 1.00     LAMP     1.80      8.00 2021 W33
    ## 26     Pepin  14.00 2.00     LAMP     1.10      8.00 2021 W33
    ## 27     Pepin  14.00 2.00     LAMP     0.90      8.00 2021 W33
    ## 28     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33
    ## 29     Pepin  14.00 2.00     LAMP     1.00      9.00 2021 W33
    ## 30     Pepin  14.00 2.00     LAMP     1.00      7.00 2021 W33
    ## 31     Pepin  14.00 2.00     LAMP     1.40      9.00 2021 W33
    ## 32     Pepin  14.00 2.00     LAMP     2.60     14.00 2021 W33
    ## 33     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33
    ## 34     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33
    ## 35     Pepin  14.00 2.00     LAMP     3.20     11.00 2021 W33
    ## 36     Pepin  14.00 2.00     LAMP     1.30      9.00 2021 W33
    ## 37     Pepin  14.00 2.00     LAMP     1.20      9.00 2021 W33
    ## 38     Pepin  14.00 2.00     LAMP     1.00      7.00 2021 W33
    ## 39     Pepin  14.00 2.00     LAMP     1.30      7.00 2021 W33
    ## 40     Pepin  14.00 2.00     LAMP     2.30     11.00 2021 W33
    ## 41     Pepin  14.00 2.00     LAMP     1.60      8.00 2021 W33
    ## 42     Pepin  14.00 2.00     LAMP     1.00      8.00 2021 W33
    ## 43     Pepin  14.00 2.00     LAMP     1.90     10.00 2021 W33
    ## 44     Pepin  14.00 2.00     LAMP     0.70      7.00 2021 W33
    ## 45     Pepin  14.00 2.00     LAMP     0.50      6.00 2021 W33
    ## 46     Pepin  14.00 2.00      RBT     3.30      7.00 2021 W33
    ## 47     Pepin  14.00 2.00       CO     4.80      8.00 2021 W33
    ## 48     Pepin  14.00 2.00       CO     2.90      6.50 2021 W33
    ## 49     Pepin  14.00 2.00      STK     2.10      5.50 2021 W33
    ## 50     Pepin  14.00 2.00     LAMP     0.70      5.00 2021 W33
    ## 51     Pepin  14.00 3.00     LAMP     0.60      7.00 2021 W33
    ## 52     Pepin  14.00 3.00     LAMP     1.50      9.00 2021 W33
    ## 53     Pepin  14.00 3.00     LAMP     2.90     11.00 2021 W33
    ## 54     Pepin  14.00 3.00     LAMP     3.30     11.00 2021 W33
    ## 55     Pepin  14.00 3.00     LAMP     3.10     12.00 2021 W33
    ## 56     Pepin  14.00 3.00     LAMP     1.60      9.00 2021 W33
    ## 57     Pepin  14.00 3.00     LAMP     0.80      6.00 2021 W33
    ## 58     Pepin  14.00 3.00     LAMP     4.40     11.00 2021 W33
    ## 59     Pepin  14.00 3.00     LAMP     3.50     10.00 2021 W33
    ## 60     Pepin  14.00 3.00     LAMP     4.20     12.00 2021 W33
    ## 61     Pepin  14.00 3.00       CO     1.90      6.00 2021 W33
    ## 62     Pepin  14.00 3.00      RBT     3.10      6.50 2021 W33
    ## 63     Pepin  14.00 3.00      DAC     8.40      9.50 2021 W33
    ## 64     Pepin  14.00 3.00      RBT     2.20      6.00 2021 W33
    ## 65     Pepin  14.00 3.00      RBT     2.20      6.00 2021 W33
    ## 66     Pepin  12.00 1.00      CUT     3.40      6.50 2021 W33
    ## 67     Pepin  12.00 1.00      CUT     7.70      9.50 2021 W33
    ## 68     Pepin  12.00 1.00      CUT     1.50      6.50 2021 W33
    ## 69     Pepin  12.00 1.00      STK     1.30      4.50 2021 W33
    ## 70     Pepin  12.00 1.00     LAMP     1.90     10.00 2021 W33
    ## 71     Pepin  12.00 1.00     LAMP     3.60     15.00 2021 W33
    ## 72     Pepin  12.00 1.00     LAMP     3.60     14.00 2021 W33
    ## 73     Pepin  12.00 1.00     LAMP     0.70      7.00 2021 W33
    ## 74     Pepin  12.00 1.00     LAMP     1.10      7.00 2021 W33
    ## 75     Pepin  12.00 1.00     LAMP     0.40      6.00 2021 W33
    ## 76     Pepin  12.00 2.00      RBT     1.20      4.50 2021 W33
    ## 77     Pepin  12.00 2.00     LAMP     1.30     10.00 2021 W33
    ## 78     Pepin  12.00 2.00     LAMP     3.50     11.00 2021 W33
    ## 79     Pepin  12.00 2.00     LAMP     0.60      8.00 2021 W33
    ## 80     Pepin  12.00 2.00     LAMP     1.20     10.00 2021 W33
    ## 81     Pepin  12.00 2.00     LAMP     0.70      8.00 2021 W33
    ## 82     Pepin  12.00 3.00 CRAYFISH       NA        NA 2021 W33
    ## 83     Pepin   6.00 1.00      STK     0.40      3.40 2021 W33
    ## 84     Pepin   6.00 1.00      CUT    33.00     15.40 2021 W33
    ## 85     Pepin   6.00 1.00       CO     4.10      7.10 2021 W33
    ## 86     Pepin   6.00 1.00      RBT     0.80      4.30 2021 W33
    ## 87     Pepin   6.00 1.00      STK     0.70      4.10 2021 W33
    ## 88     Pepin   6.00 1.00       CO     3.50      6.70 2021 W33
    ## 89     Pepin   6.00 1.00      STK     0.60      3.80 2021 W33
    ## 90     Pepin   6.00 1.00       CO     3.70      6.80 2021 W33
    ## 91     Pepin   6.00 1.00      RBT     1.60      5.30 2021 W33
    ## 92     Pepin   6.00 1.00       CO     1.50      5.20 2021 W33
    ## 93     Pepin   6.00 1.00      CUT     5.70      8.30 2021 W33
    ## 94     Pepin   6.00 1.00      CUT     1.80      5.40 2021 W33
    ## 95     Pepin   6.00 1.00      CUT     2.20      6.10 2021 W33
    ## 96     Pepin   6.00 1.00      CUT    14.30     11.20 2021 W33
    ## 97     Pepin   6.00 1.00      CUT     1.70      5.30 2021 W33
    ## 98     Pepin   6.00 1.00      CUT    18.40     12.50 2021 W33
    ## 99     Pepin   6.00 1.00      CUT     2.60      6.20 2021 W33
    ## 100    Pepin   6.00 1.00       CO     3.70      6.80 2021 W33
    ## 101    Pepin   6.00 1.00       CO     2.00      5.20 2021 W33
    ## 102    Pepin   6.00 1.00      RBT     0.60      3.30 2021 W33
    ## 103    Pepin   6.00 1.00       CO     2.70      6.20 2021 W33
    ## 104    Pepin   6.00 1.00      CUT     1.50      5.20 2021 W33
    ## 105    Pepin   6.00 1.00      CUT    15.20     11.90 2021 W33
    ## 106    Pepin   6.00 1.00      RBT     0.90      4.20 2021 W33
    ## 107    Pepin   6.00 1.00      DAC     3.90      6.90 2021 W33
    ## 108    Pepin   6.00 1.00      DAC     7.90      8.90 2021 W33
    ## 109    Pepin   6.00 1.00       CO     3.30      6.60 2021 W33
    ## 110    Pepin   6.00 1.00     LAMP     0.60      6.70 2021 W33
    ## 111    Pepin   6.00 1.00       CO     2.50      6.00 2021 W33
    ## 112    Pepin   6.00 1.00      CUT     4.80      7.60 2021 W33
    ## 113    Pepin   6.00 1.00      RBT     1.80      4.20 2021 W33
    ## 114    Pepin   6.00 1.00      RBT     3.00      6.70 2021 W33
    ## 115    Pepin   6.00 1.00      DAC     9.30      9.50 2021 W33
    ## 116    Pepin   6.00 1.00       CO     2.90      5.40 2021 W33
    ## 117    Pepin   6.00 1.00      STK     1.00      3.90 2021 W33
    ## 118    Pepin   6.00 1.00      CUT     7.80      9.80 2021 W33
    ## 119    Pepin   6.00 1.00       CO     1.40      4.60 2021 W33
    ## 120    Pepin   6.00 1.00      STK     1.20      4.60 2021 W33
    ## 121    Pepin   6.00 1.00      STK     0.80      3.70 2021 W33
    ## 122    Pepin   6.00 1.00      STK     3.00      6.00 2021 W33
    ## 123    Pepin   6.00 1.00      DAC     4.20      7.20 2021 W33
    ## 124    Pepin   6.00 1.00     LAMP     1.10      9.00 2021 W33
    ## 125    Pepin   6.00 1.00      DAC     7.30      8.50 2021 W33
    ## 126    Pepin   6.00 2.00       CO     1.20      4.70 2021 W33
    ## 127    Pepin   6.00 2.00      CUT     1.80      5.70 2021 W33
    ## 128    Pepin   6.00 2.00       CO     4.10      7.10 2021 W33
    ## 129    Pepin   6.00 2.00      RBT     3.00      6.50 2021 W33
    ## 130    Pepin   6.00 2.00      STK     4.30      6.80 2021 W33
    ## 131    Pepin   6.00 2.00      CUT     1.30      4.90 2021 W33
    ## 132    Pepin   6.00 2.00       CO     2.40      6.20 2021 W33
    ## 133    Pepin   6.00 2.00      CUT     1.10      4.10 2021 W33
    ## 134    Pepin   6.00 2.00      CUT     2.50      5.50 2021 W33
    ## 135    Pepin   6.00 2.00       CO     3.00      6.30 2021 W33
    ## 136    Pepin   6.00 2.00      STK     1.50      5.50 2021 W33
    ## 137    Pepin   6.00 2.00      CUT     1.50      5.40 2021 W33
    ## 138    Pepin   6.00 2.00      STK     0.80      4.00 2021 W33
    ## 139    Pepin   6.00 2.00       CO     1.30      4.90 2021 W33
    ## 140    Pepin   6.00 2.00       CO     2.00      5.50 2021 W33
    ## 141    Pepin   6.00 2.00     LAMP     0.90      6.20 2021 W33
    ## 142    Pepin   6.00 2.00     LAMP     1.60      8.50 2021 W33
    ## 143    Pepin   6.00 3.00      RBT     0.90      4.40 2021 W33
    ## 144    Pepin   6.00 3.00      RBT     0.60      3.30 2021 W33
    ## 145    Pepin   6.00 3.00     LAMP     0.70      8.00 2021 W33
    ## 146    Pepin  11.00 1.00      STK     0.70      4.50 2021 W33
    ## 147    Pepin  11.00 1.00       CO     1.60      5.50 2021 W33
    ## 148    Pepin  11.00 1.00       CO     1.90      5.40 2021 W33
    ## 149    Pepin  11.00 1.00      STK     1.60      5.20 2021 W33
    ## 150    Pepin  11.00 1.00      CUT     1.10      4.60 2021 W33
    ## 151    Pepin  11.00 1.00      STK     0.50      3.40 2021 W33
    ## 152    Pepin  11.00 1.00      STK     1.20      5.40 2021 W33
    ## 153    Pepin  11.00 1.00     LAMP     3.40      9.80 2021 W33
    ## 154    Pepin  11.00 2.00 CRAYFISH       NA        NA 2021 W33
    ## 155    Pepin  11.00 3.00      CUT     2.20      6.10 2021 W33
    ## 156    Pepin  11.00 3.00      CUT     2.80      6.70 2021 W33
    ## 157    Pepin  11.00 3.00       CO     0.90      4.60 2021 W33
    ## 158    Pepin   9.50 1.00      CUT       NA      6.10 2021 W34
    ## 159    Pepin   9.50 1.00      STK       NA      5.00 2021 W34
    ## 160    Pepin   9.50 1.00      STK       NA      2.70 2021 W34
    ## 161    Pepin   9.50 1.00      RBT       NA      4.30 2021 W34
    ## 162    Pepin   9.50 1.00      CUT       NA      8.20 2021 W34
    ## 163    Pepin   9.50 1.00      RBT       NA      4.80 2021 W34
    ## 164    Pepin   9.50 1.00      CUT       NA      7.70 2021 W34
    ## 165    Pepin   9.50 1.00       CO       NA      6.60 2021 W34
    ## 166    Pepin   9.50 1.00       CO       NA      5.50 2021 W34
    ## 167    Pepin   9.50 1.00      RBT       NA      5.00 2021 W34
    ## 168    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34
    ## 169    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34
    ## 170    Pepin   9.50 1.00 CRAYFISH       NA        NA 2021 W34
    ## 171    Pepin   9.50 2.00      CUT       NA      8.30 2021 W34
    ## 172    Pepin   9.50 2.00      STK       NA      5.70 2021 W34
    ## 173    Pepin   9.50 2.00      STK       NA      3.70 2021 W34
    ## 174    Pepin   9.50 3.00      CUT       NA      6.80 2021 W34
    ## 175    Pepin   9.50 3.00       CO       NA      4.90 2021 W34
    ## 176    Pepin   9.50 3.00      RBT       NA      5.40 2021 W34
    ## 177    Pepin   9.50 4.00       CO       NA      5.90 2021 W34
    ## 178    Pepin   9.00 1.00      CUT       NA      9.10 2021 W34
    ## 179    Pepin   9.00 1.00      CUT       NA      8.30 2021 W34
    ## 180    Pepin   9.00 1.00       CO       NA      5.80 2021 W34
    ## 181    Pepin   9.00 1.00     LAMP       NA      7.00 2021 W34
    ## 182    Pepin   9.00 1.00       CO       NA      4.90 2021 W34
    ## 183    Pepin   9.00 1.00      CUT       NA     10.90 2021 W34
    ## 184    Pepin   9.00 1.00      STK       NA      5.30 2021 W34
    ## 185    Pepin   9.00 1.00     LAMP       NA      7.00 2021 W34
    ## 186    Pepin   9.00 1.00       CO       NA      5.20 2021 W34
    ## 187    Pepin   9.00 1.00     LAMP       NA      6.00 2021 W34
    ## 188    Pepin   9.00 1.00       CO       NA      5.20 2021 W34
    ## 189    Pepin   9.00 1.00       CO       NA      5.90 2021 W34
    ## 190    Pepin   9.00 1.00      DAC       NA      8.50 2021 W34
    ## 191    Pepin   9.00 1.00      STK       NA      5.40 2021 W34
    ## 192    Pepin   9.00 1.00     LAMP       NA      5.00 2021 W34
    ## 193    Pepin   9.00 1.00      STK       NA      5.40 2021 W34
    ## 194    Pepin   9.00 1.00      RBT       NA      5.10 2021 W34
    ## 195    Pepin   9.00 1.00      STK       NA      5.10 2021 W34
    ## 196    Pepin   9.00 1.00      RBT       NA      3.90 2021 W34
    ## 197    Pepin   9.00 1.00      STK       NA      5.40 2021 W34
    ## 198    Pepin   9.00 1.00     LAMP       NA      8.00 2021 W34
    ## 199    Pepin   9.00 1.00      CUT       NA      8.20 2021 W34
    ## 200    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 201    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 202    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 203    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 204    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 205    Pepin   9.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 206    Pepin   9.00 2.00       CO       NA      5.40 2021 W34
    ## 207    Pepin   9.00 2.00      CUT       NA     10.20 2021 W34
    ## 208    Pepin   9.00 2.00       CO       NA      7.30 2021 W34
    ## 209    Pepin   9.00 2.00      CUT       NA     12.10 2021 W34
    ## 210    Pepin   9.00 2.00       CO       NA      7.00 2021 W34
    ## 211    Pepin   9.00 2.00      STK       NA      5.60 2021 W34
    ## 212    Pepin   9.00 2.00      STK       NA      2.60 2021 W34
    ## 213    Pepin   9.00 2.00       CO       NA      5.40 2021 W34
    ## 214    Pepin   9.00 2.00       CO       NA      6.40 2021 W34
    ## 215    Pepin   9.00 2.00      STK       NA      4.60 2021 W34
    ## 216    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 217    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 218    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 219    Pepin   9.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 220    Pepin   9.00 3.00      STK       NA      5.10 2021 W34
    ## 221    Pepin   9.00 3.00      STK       NA      2.80 2021 W34
    ## 222    Pepin   9.00 3.00      STK       NA      3.30 2021 W34
    ## 223    Pepin   9.00 3.00       CO       NA      5.40 2021 W34
    ## 224    Pepin   9.00 3.00      STK       NA      5.10 2021 W34
    ## 225    Pepin   9.00 3.00      RBT       NA      3.30 2021 W34
    ## 226    Pepin   9.00 3.00       CO       NA      5.20 2021 W34
    ## 227    Pepin   9.00 3.00      STK       NA      3.40 2021 W34
    ## 228    Pepin   9.00 3.00       CO       NA      6.30 2021 W34
    ## 229    Pepin   9.00 3.00      DAC       NA      9.70 2021 W34
    ## 230    Pepin   9.00 3.00       CO       NA      7.20 2021 W34
    ## 231    Pepin   9.00 3.00     LAMP       NA      9.00 2021 W34
    ## 232    Pepin   9.00 3.00       CO       NA      6.50 2021 W34
    ## 233    Pepin   9.00 3.00      DAC       NA      8.80 2021 W34
    ## 234    Pepin   9.00 3.00      STK       NA      3.60 2021 W34
    ## 235    Pepin   9.00 3.00 CRAYFISH       NA        NA 2021 W34
    ## 236    Pepin   9.00 3.00 CRAYFISH       NA        NA 2021 W34
    ## 237    Pepin   9.00 4.00      CUT       NA     11.80 2021 W34
    ## 238    Pepin   9.00 4.00      RBT       NA      5.10 2021 W34
    ## 239    Pepin   9.00 4.00      STK       NA      5.30 2021 W34
    ## 240    Pepin   9.00 4.00      STK       NA      5.40 2021 W34
    ## 241    Pepin   9.00 4.00      CUT       NA      6.10 2021 W34
    ## 242    Pepin   9.00 4.00     LAMP       NA     12.00 2021 W34
    ## 243    Pepin   9.00 4.00       CO       NA      6.00 2021 W34
    ## 244    Pepin   9.00 4.00       CO       NA      6.70 2021 W34
    ## 245    Pepin   9.00 4.00      STK       NA      4.80 2021 W34
    ## 246    Pepin   9.00 4.00       CO       NA      5.00 2021 W34
    ## 247    Pepin   9.00 4.00      STK       NA      3.10 2021 W34
    ## 248    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 249    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 250    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 251    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 252    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 253    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 254    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 255    Pepin   9.00 4.00 CRAYFISH       NA        NA 2021 W34
    ## 256    Pepin  10.00 1.00      RBT       NA      4.40 2021 W34
    ## 257    Pepin  10.00 1.00       CO       NA      4.40 2021 W34
    ## 258    Pepin  10.00 1.00      RBT       NA      3.30 2021 W34
    ## 259    Pepin  10.00 1.00       CO       NA      5.10 2021 W34
    ## 260    Pepin  10.00 1.00       CO       NA      6.80 2021 W34
    ## 261    Pepin  10.00 1.00       CO       NA      4.50 2021 W34
    ## 262    Pepin  10.00 1.00       CO       NA      4.70 2021 W34
    ## 263    Pepin  10.00 1.00      STK       NA      3.10 2021 W34
    ## 264    Pepin  10.00 1.00      RBT       NA      5.70 2021 W34
    ## 265    Pepin  10.00 1.00     LAMP       NA      8.00 2021 W34
    ## 266    Pepin  10.00 1.00     LAMP       NA      9.00 2021 W34
    ## 267    Pepin  10.00 1.00     LAMP       NA      9.00 2021 W34
    ## 268    Pepin  10.00 1.00      CUT       NA      5.30 2021 W34
    ## 269    Pepin  10.00 1.00       CO       NA      5.70 2021 W34
    ## 270    Pepin  10.00 1.00       CO       NA      6.00 2021 W34
    ## 271    Pepin  10.00 1.00     LAMP       NA      7.00 2021 W34
    ## 272    Pepin  10.00 1.00     LAMP       NA      6.00 2021 W34
    ## 273    Pepin  10.00 1.00       CO       NA      5.40 2021 W34
    ## 274    Pepin  10.00 1.00     LAMP       NA      5.00 2021 W34
    ## 275    Pepin  10.00 1.00       CO       NA      5.30 2021 W34
    ## 276    Pepin  10.00 1.00     LAMP       NA      3.00 2021 W34
    ## 277    Pepin  10.00 1.00     LAMP       NA      6.00 2021 W34
    ## 278    Pepin  10.00 1.00      RBT       NA      5.90 2021 W34
    ## 279    Pepin  10.00 1.00      CUT       NA      7.80 2021 W34
    ## 280    Pepin  10.00 1.00      RBT       NA      4.20 2021 W34
    ## 281    Pepin  10.00 1.00      STK       NA      5.50 2021 W34
    ## 282    Pepin  10.00 1.00       CO       NA      6.10 2021 W34
    ## 283    Pepin  10.00 1.00      DAC       NA     10.20 2021 W34
    ## 284    Pepin  10.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 285    Pepin  10.00 2.00       CO       NA      4.30 2021 W34
    ## 286    Pepin  10.00 2.00     LAMP       NA      9.00 2021 W34
    ## 287    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34
    ## 288    Pepin  10.00 2.00     LAMP       NA      7.00 2021 W34
    ## 289    Pepin  10.00 2.00      STK       NA      5.60 2021 W34
    ## 290    Pepin  10.00 2.00      STK       NA      5.50 2021 W34
    ## 291    Pepin  10.00 2.00     LAMP       NA      4.00 2021 W34
    ## 292    Pepin  10.00 2.00      RBT       NA      5.10 2021 W34
    ## 293    Pepin  10.00 2.00     LAMP       NA      7.00 2021 W34
    ## 294    Pepin  10.00 2.00       CO       NA      6.10 2021 W34
    ## 295    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34
    ## 296    Pepin  10.00 2.00      RBT       NA      3.40 2021 W34
    ## 297    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34
    ## 298    Pepin  10.00 2.00      STK       NA      3.00 2021 W34
    ## 299    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34
    ## 300    Pepin  10.00 2.00       CO       NA      5.20 2021 W34
    ## 301    Pepin  10.00 2.00     LAMP       NA      8.00 2021 W34
    ## 302    Pepin  10.00 2.00     LAMP       NA      6.00 2021 W34
    ## 303    Pepin  10.00 2.00      STK       NA      4.90 2021 W34
    ## 304    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 305    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 306    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 307    Pepin  10.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 308    Pepin  10.00 3.00       CO       NA      6.50 2021 W34
    ## 309    Pepin  10.00 3.00      RBT       NA      3.60 2021 W34
    ## 310    Pepin  10.00 3.00     LAMP       NA      5.00 2021 W34
    ## 311    Pepin  10.00 3.00       CO       NA      5.80 2021 W34
    ## 312    Pepin  10.00 3.00      STK       NA      5.40 2021 W34
    ## 313    Pepin  10.00 3.00     LAMP       NA      7.00 2021 W34
    ## 314    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34
    ## 315    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34
    ## 316    Pepin  10.00 3.00      STK       NA      5.50 2021 W34
    ## 317    Pepin  10.00 3.00     LAMP       NA     10.00 2021 W34
    ## 318    Pepin  10.00 3.00     LAMP       NA      9.00 2021 W34
    ## 319    Pepin  10.00 3.00     LAMP       NA     11.00 2021 W34
    ## 320    Pepin   3.00 1.00      CUT    12.20     10.50 2021 W34
    ## 321    Pepin   3.00 1.00      STK     0.40      3.10 2021 W34
    ## 322    Pepin   3.00 1.00       CO     3.10      6.60 2021 W34
    ## 323    Pepin   3.00 1.00     LAMP     0.90      7.00 2021 W34
    ## 324    Pepin   3.00 1.00      RBT     2.00      5.50 2021 W34
    ## 325    Pepin   3.00 1.00     LAMP     0.70      6.50 2021 W34
    ## 326    Pepin   3.00 1.00      STK     0.70      3.90 2021 W34
    ## 327    Pepin   3.00 1.00      RBT     1.10      4.60 2021 W34
    ## 328    Pepin   3.00 1.00      STK     0.80      4.10 2021 W34
    ## 329    Pepin   3.00 1.00      STK     0.10      2.50 2021 W34
    ## 330    Pepin   3.00 1.00      STK     0.80      4.20 2021 W34
    ## 331    Pepin   3.00 1.00     LAMP     0.70      7.00 2021 W34
    ## 332    Pepin   3.00 1.00      STK     0.80      4.00 2021 W34
    ## 333    Pepin   3.00 1.00     LAMP     0.40      6.50 2021 W34
    ## 334    Pepin   3.00 1.00     LAMP     1.30      8.00 2021 W34
    ## 335    Pepin   3.00 1.00     LAMP     0.50      7.00 2021 W34
    ## 336    Pepin   3.00 1.00     LAMP     0.70      7.00 2021 W34
    ## 337    Pepin   3.00 1.00      STK     0.80      4.10 2021 W34
    ## 338    Pepin   3.00 1.00      STK     2.30      5.80 2021 W34
    ## 339    Pepin   3.00 1.00     LAMP     1.00      8.00 2021 W34
    ## 340    Pepin   3.00 1.00      STK     0.30      3.20 2021 W34
    ## 341    Pepin   3.00 1.00      STK     0.70      3.70 2021 W34
    ## 342    Pepin   3.00 1.00      RBT     0.90      4.40 2021 W34
    ## 343    Pepin   3.00 1.00      STK     0.30      3.10 2021 W34
    ## 344    Pepin   3.00 1.00      CUT     2.00      5.70 2021 W34
    ## 345    Pepin   3.00 1.00     LAMP     0.70      6.00 2021 W34
    ## 346    Pepin   3.00 1.00     LAMP     0.50      6.00 2021 W34
    ## 347    Pepin   3.00 1.00     LAMP     0.90      7.50 2021 W34
    ## 348    Pepin   3.00 2.00     LAMP     1.00      8.00 2021 W34
    ## 349    Pepin   3.00 2.00     LAMP     0.50      6.50 2021 W34
    ## 350    Pepin   3.00 2.00     LAMP     0.60      8.00 2021 W34
    ## 351    Pepin   3.00 2.00      RBT     0.90      4.50 2021 W34
    ## 352    Pepin   3.00 2.00     LAMP     1.60      9.00 2021 W34
    ## 353    Pepin   3.00 2.00     LAMP     1.20      8.00 2021 W34
    ## 354    Pepin   3.00 2.00     LAMP     1.30      9.00 2021 W34
    ## 355    Pepin   3.00 2.00     LAMP     1.20      7.50 2021 W34
    ## 356    Pepin   3.00 2.00     LAMP     1.00      7.50 2021 W34
    ## 357    Pepin   3.00 2.00     LAMP     1.20     10.00 2021 W34
    ## 358    Pepin   3.00 2.00     LAMP     0.40      6.00 2021 W34
    ## 359    Pepin   3.00 2.00     LAMP     1.20      8.00 2021 W34
    ## 360    Pepin   3.00 2.00     LAMP     0.60      7.00 2021 W34
    ## 361    Pepin   3.00 2.00     LAMP     0.30      4.50 2021 W34
    ## 362    Pepin   3.00 2.00     LAMP     1.20      9.00 2021 W34
    ## 363    Pepin   3.00 2.00      RBT     2.00      5.50 2021 W34
    ## 364    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 365    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 366    Pepin   3.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 367    Pepin   3.00 3.00      STK     0.40      3.50 2021 W34
    ## 368    Pepin   3.00 3.00      STK     0.90      4.50 2021 W34
    ## 369    Pepin   3.00 3.00      STK     0.40      3.00 2021 W34
    ## 370    Pepin   3.00 3.00      STK     0.60      4.00 2021 W34
    ## 371    Pepin   3.00 3.00     LAMP     0.03      3.00 2021 W34
    ## 372    Pepin   3.00 3.00     LAMP     0.70      7.00 2021 W34
    ## 373    Pepin   3.00 3.00     LAMP     0.40      6.00 2021 W34
    ## 374    Pepin   3.00 3.00      RBT     0.70      4.50 2021 W34
    ## 375    Pepin   3.00 3.00     LAMP     0.90      7.00 2021 W34
    ## 376    Pepin   3.00 3.00     LAMP     1.20      8.00 2021 W34
    ## 377    Pepin   3.00 3.00      STK     0.50      3.50 2021 W34
    ## 378    Pepin   3.00 3.00     LAMP     0.50      7.00 2021 W34
    ## 379    Pepin   3.00 3.00      STK     0.50      3.50 2021 W34
    ## 380    Pepin   3.00 3.00     LAMP     0.90      7.50 2021 W34
    ## 381    Pepin   3.00 3.00     LAMP     0.30      4.00 2021 W34
    ## 382    Pepin   3.00 3.00     LAMP     1.20      9.00 2021 W34
    ## 383    Pepin   7.00 1.00      STK     2.10      5.50 2021 W34
    ## 384    Pepin   7.00 1.00       CO     2.50      6.50 2021 W34
    ## 385    Pepin   7.00 1.00      RBT     2.90      7.00 2021 W34
    ## 386    Pepin   7.00 1.00      CUT     1.10      4.50 2021 W34
    ## 387    Pepin   7.00 1.00      DAC     8.60      9.50 2021 W34
    ## 388    Pepin   7.00 1.00       CO     4.40      7.50 2021 W34
    ## 389    Pepin   7.00 1.00      STK     1.20      4.00 2021 W34
    ## 390    Pepin   7.00 1.00      CUT    11.68     11.00 2021 W34
    ## 391    Pepin   7.00 1.00      RBT     2.30      6.00 2021 W34
    ## 392    Pepin   7.00 1.00       CO     2.70      6.50 2021 W34
    ## 393    Pepin   7.00 1.00      CUT    23.80     14.50 2021 W34
    ## 394    Pepin   7.00 1.00       CO     4.50      7.50 2021 W34
    ## 395    Pepin   7.00 1.00       CO     4.90      7.50 2021 W34
    ## 396    Pepin   7.00 1.00      CUT     9.10     10.00 2021 W34
    ## 397    Pepin   7.00 1.00     LAMP     0.50      6.00 2021 W34
    ## 398    Pepin   7.00 1.00      CUT    13.80     11.00 2021 W34
    ## 399    Pepin   7.00 1.00      CUT     1.50      5.50 2021 W34
    ## 400    Pepin   7.00 1.00      CUT    17.80     12.00 2021 W34
    ## 401    Pepin   7.00 1.00       CO     2.20      5.80 2021 W34
    ## 402    Pepin   7.00 1.00      STK     0.20      3.00 2021 W34
    ## 403    Pepin   7.00 1.00      RBT     2.80      6.50 2021 W34
    ## 404    Pepin   7.00 1.00       CO     2.80      6.00 2021 W34
    ## 405    Pepin   7.00 1.00      CUT     9.10      9.80 2021 W34
    ## 406    Pepin   7.00 1.00       CO     2.90      7.00 2021 W34
    ## 407    Pepin   7.00 1.00     LAMP     4.90     12.00 2021 W34
    ## 408    Pepin   7.00 1.00       CO     0.70      4.50 2021 W34
    ## 409    Pepin   7.00 1.00       CO     2.00      5.50 2021 W34
    ## 410    Pepin   7.00 1.00      RBT     0.90      4.50 2021 W34
    ## 411    Pepin   7.00 1.00      STK     1.40      5.00 2021 W34
    ## 412    Pepin   7.00 1.00       CO     4.00      7.00 2021 W34
    ## 413    Pepin   7.00 1.00       CO     1.80      5.30 2021 W34
    ## 414    Pepin   7.00 1.00      CUT    49.50     17.20 2021 W34
    ## 415    Pepin   7.00 1.00      CUT     8.20      9.50 2021 W34
    ## 416    Pepin   7.00 1.00       CO     1.80      5.50 2021 W34
    ## 417    Pepin   7.00 1.00      RBT     0.90      4.40 2021 W34
    ## 418    Pepin   7.00 1.00       CO     2.50      6.00 2021 W34
    ## 419    Pepin   7.00 1.00      RBT     1.30      4.60 2021 W34
    ## 420    Pepin   7.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 421    Pepin   7.00 2.00      RBT     0.70      4.00 2021 W34
    ## 422    Pepin   7.00 2.00      RBT     2.30      6.50 2021 W34
    ## 423    Pepin   7.00 2.00      CUT     1.70      5.50 2021 W34
    ## 424    Pepin   7.00 2.00       CO     1.80      5.50 2021 W34
    ## 425    Pepin   7.00 2.00      CUT     7.60      9.00 2021 W34
    ## 426    Pepin   7.00 2.00     LAMP     0.90      8.00 2021 W34
    ## 427    Pepin   7.00 2.00      CUT     2.80      6.00 2021 W34
    ## 428    Pepin   7.00 2.00     LAMP     0.70      7.00 2021 W34
    ## 429    Pepin   7.00 2.00      DAC     8.80      9.50 2021 W34
    ## 430    Pepin   7.00 2.00     LAMP     0.70      7.00 2021 W34
    ## 431    Pepin   7.00 2.00     LAMP     0.40      6.00 2021 W34
    ## 432    Pepin   7.00 2.00     LAMP     1.90     10.00 2021 W34
    ## 433    Pepin   7.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 434    Pepin   7.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 435    Pepin   7.00 3.00     LAMP     0.20      4.00 2021 W34
    ## 436    Pepin   7.00 3.00     LAMP     1.00      8.00 2021 W34
    ## 437    Pepin   7.00 3.00      STK     0.70      4.00 2021 W34
    ## 438    Pepin   7.00 3.00       CO     3.40      6.50 2021 W34
    ## 439    Pepin   7.00 3.00      CUT     7.00      9.00 2021 W34
    ## 440    Pepin   7.00 3.00     LAMP     0.50      6.00 2021 W34
    ## 441    Pepin   7.00 3.00     LAMP     2.10     10.00 2021 W34
    ## 442    Pepin   7.00 3.00     LAMP     1.90     10.00 2021 W34
    ## 443    Pepin   7.00 3.00 CRAYFISH       NA        NA 2021 W34
    ## 444    Pepin   5.00 1.00      STK     1.70      5.50 2021 W34
    ## 445    Pepin   5.00 1.00       CO     3.60      7.00 2021 W34
    ## 446    Pepin   5.00 1.00      CUT     4.70      8.00 2021 W34
    ## 447    Pepin   5.00 1.00      STK     0.70      4.00 2021 W34
    ## 448    Pepin   5.00 1.00      STK     0.20      2.50 2021 W34
    ## 449    Pepin   5.00 1.00      DAC     7.30      9.00 2021 W34
    ## 450    Pepin   5.00 1.00      CUT     5.00      7.50 2021 W34
    ## 451    Pepin   5.00 1.00      STK     1.80      5.00 2021 W34
    ## 452    Pepin   5.00 1.00       CO     2.30      6.00 2021 W34
    ## 453    Pepin   5.00 1.00       CO     3.20      6.50 2021 W34
    ## 454    Pepin   5.00 1.00      CUT     1.70      5.50 2021 W34
    ## 455    Pepin   5.00 1.00     LAMP     0.60      6.00 2021 W34
    ## 456    Pepin   5.00 1.00      RBT     0.90      4.50 2021 W34
    ## 457    Pepin   5.00 1.00     LAMP     2.30     10.00 2021 W34
    ## 458    Pepin   5.00 1.00     LAMP     2.80     12.00 2021 W34
    ## 459    Pepin   5.00 1.00     LAMP     0.90      8.00 2021 W34
    ## 460    Pepin   5.00 1.00      STK     2.90      5.50 2021 W34
    ## 461    Pepin   5.00 1.00     LAMP     0.60      6.00 2021 W34
    ## 462    Pepin   5.00 1.00     LAMP     0.40      6.00 2021 W34
    ## 463    Pepin   5.00 1.00      STK     1.90      5.00 2021 W34
    ## 464    Pepin   5.00 1.00      RBT     1.00      4.50 2021 W34
    ## 465    Pepin   5.00 1.00      STK     0.80      4.00 2021 W34
    ## 466    Pepin   5.00 1.00     LAMP     1.00      8.00 2021 W34
    ## 467    Pepin   5.00 1.00     LAMP     0.40      6.00 2021 W34
    ## 468    Pepin   5.00 1.00     LAMP     2.10     11.00 2021 W34
    ## 469    Pepin   5.00 1.00     LAMP     2.90     10.00 2021 W34
    ## 470    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 471    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 472    Pepin   5.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 473    Pepin   5.00 2.00      RBT     1.70      5.00 2021 W34
    ## 474    Pepin   5.00 2.00     LAMP     1.40      9.00 2021 W34
    ## 475    Pepin   5.00 2.00     LAMP     0.40      6.00 2021 W34
    ## 476    Pepin   5.00 2.00     LAMP     0.70      6.50 2021 W34
    ## 477    Pepin   5.00 2.00      RBT     2.30      6.00 2021 W34
    ## 478    Pepin   5.00 2.00       CO     2.20      6.00 2021 W34
    ## 479    Pepin   5.00 2.00     LAMP     0.40      5.00 2021 W34
    ## 480    Pepin   5.00 2.00     LAMP     0.80      7.00 2021 W34
    ## 481    Pepin   5.00 2.00     LAMP     0.40      5.00 2021 W34
    ## 482    Pepin   5.00 2.00     LAMP     0.50      6.50 2021 W34
    ## 483    Pepin   5.00 2.00     LAMP     0.60      6.50 2021 W34
    ## 484    Pepin   5.00 2.00     LAMP     2.20     10.00 2021 W34
    ## 485    Pepin   5.00 2.00      STK     0.30      2.50 2021 W34
    ## 486    Pepin   5.00 3.00     LAMP     1.30     10.00 2021 W34
    ## 487    Pepin   5.00 3.00     LAMP     0.60      6.50 2021 W34
    ## 488    Pepin   5.00 3.00     LAMP     2.40     10.00 2021 W34
    ## 489    Pepin   5.00 3.00     LAMP     1.80      8.00 2021 W34
    ## 490    Pepin   5.00 3.00     LAMP     0.80      6.00 2021 W34
    ## 491    Pepin   5.00 3.00      RBT     1.40      5.50 2021 W34
    ## 492    Pepin   5.00 3.00      CUT     2.40      6.00 2021 W34
    ## 493    Pepin   5.00 3.00     LAMP     0.60      6.00 2021 W34
    ## 494    Pepin   5.00 3.00     LAMP     0.70      6.00 2021 W34
    ## 495    Pepin   5.00 3.00     LAMP     0.60      7.00 2021 W34
    ## 496    Pepin   5.00 0.03     LAMP     1.50      9.00 2021 W34
    ## 497    Pepin   5.00 3.00     LAMP     0.60      5.50 2021 W34
    ## 498    Pepin   5.00 3.00     LAMP     1.10      8.00 2021 W34
    ## 499    Pepin   5.00 3.00     LAMP     1.20      9.00 2021 W34
    ## 500    Pepin   5.00 3.00      CUT    10.50     12.00 2021 W34
    ## 501    Pepin   5.75 1.00      CUT     3.63      6.60 2021 W34
    ## 502    Pepin   5.75 1.00      CUT    14.66     11.30 2021 W34
    ## 503    Pepin   5.75 1.00      CUT     9.87      9.80 2021 W34
    ## 504    Pepin   5.75 1.00       CO     4.56      7.40 2021 W34
    ## 505    Pepin   5.75 1.00       CO     3.26      6.50 2021 W34
    ## 506    Pepin   5.75 1.00       CO     3.50      6.70 2021 W34
    ## 507    Pepin   5.75 1.00       CO     0.93      4.60 2021 W34
    ## 508    Pepin   5.75 1.00       CO     2.93      6.80 2021 W34
    ## 509    Pepin   5.75 1.00     LAMP     1.55     10.10 2021 W34
    ## 510    Pepin   5.75 1.00     LAMP     2.15     10.00 2021 W34
    ## 511    Pepin   5.75 1.00      RBT    27.02     13.70 2021 W34
    ## 512    Pepin   5.75 1.00      STK     2.06      5.70 2021 W34
    ## 513    Pepin   5.75 1.00      STK     1.58      5.20 2021 W34
    ## 514    Pepin   5.75 1.00      STK     0.95      4.10 2021 W34
    ## 515    Pepin   5.75 1.00      STK     0.44      3.50 2021 W34
    ## 516    Pepin   5.75 1.00      STK     0.43      2.90 2021 W34
    ## 517    Pepin   5.75 1.00      STK     0.27      2.50 2021 W34
    ## 518    Pepin   5.75 1.00    TROUT     2.11      5.60 2021 W34
    ## 519    Pepin   5.75 1.00    TROUT     1.60      5.20 2021 W34
    ## 520    Pepin   5.75 1.00    TROUT     1.67      5.90 2021 W34
    ## 521    Pepin   5.75 1.00    TROUT     0.98      4.50 2021 W34
    ## 522    Pepin   5.75 1.00    TROUT     0.93      4.50 2021 W34
    ## 523    Pepin   5.75 1.00    TROUT     1.80      6.00 2021 W34
    ## 524    Pepin   5.75 1.00    TROUT     0.71      4.00 2021 W34
    ## 525    Pepin   5.75 1.00    TROUT     0.94      4.80 2021 W34
    ## 526    Pepin   5.75 1.00    TROUT     0.58      3.90 2021 W34
    ## 527    Pepin   5.75 1.00    TROUT     0.74      4.10 2021 W34
    ## 528    Pepin   5.75 2.00      RBT     2.33      6.20 2021 W34
    ## 529    Pepin   5.75 2.00       CO     1.72      5.50 2021 W34
    ## 530    Pepin   5.75 2.00       CO     3.31      6.70 2021 W34
    ## 531    Pepin   5.75 2.00      STK     0.45      3.70 2021 W34
    ## 532    Pepin   5.75 2.00       CO     1.75      5.30 2021 W34
    ## 533    Pepin   5.75 2.00      RBT     1.73      5.70 2021 W34
    ## 534    Pepin   5.75 2.00      STK     0.11      3.60 2021 W34
    ## 535    Pepin   5.75 2.00     LAMP     1.80      9.50 2021 W34
    ## 536    Pepin   5.75 2.00     LAMP     1.81     10.00 2021 W34
    ## 537    Pepin   5.75 2.00     LAMP     0.74      8.00 2021 W34
    ## 538    Pepin   5.75 2.00     LAMP     1.35      9.00 2021 W34
    ## 539    Pepin   5.75 3.00      STK     6.38      5.00 2021 W34
    ## 540    Pepin   5.75 3.00     LAMP     2.79     11.00 2021 W34
    ## 541    Pepin   5.75 3.00     LAMP     2.50     11.00 2021 W34
    ## 542    Pepin   5.75 3.00     LAMP     1.21      8.00 2021 W34
    ## 543    Pepin   5.75 3.00     LAMP     1.53     10.00 2021 W34
    ## 544    Pepin   5.75 3.00     LAMP     1.15      8.50 2021 W34
    ## 545    Pepin   5.75 3.00       CO     3.18      6.70 2021 W34
    ## 546    Pepin   5.75 3.00      STK     1.50      4.90 2021 W34
    ## 547    Pepin   5.75 3.00      RBT     1.19      5.10 2021 W34
    ## 548    Pepin   5.90 1.00      RBT    75.40     19.10 2021 W34
    ## 549    Pepin   5.90 1.00    TROUT     0.70      4.00 2021 W34
    ## 550    Pepin   5.90 1.00    TROUT     1.60      5.30 2021 W34
    ## 551    Pepin   5.90 1.00    TROUT     0.70      4.30 2021 W34
    ## 552    Pepin   5.90 1.00    TROUT     3.00      6.40 2021 W34
    ## 553    Pepin   5.90 1.00    TROUT     1.00      4.40 2021 W34
    ## 554    Pepin   5.90 2.00     LAMP     1.26      8.00 2021 W34
    ## 555    Pepin   5.90 2.00     LAMP     1.47      8.50 2021 W34
    ## 556    Pepin   5.90 2.00     LAMP     1.73      9.00 2021 W34
    ## 557    Pepin   5.90 2.00    TROUT     0.40      3.50 2021 W34
    ## 558    Pepin   5.90 2.00    TROUT     0.95      4.40 2021 W34
    ## 559    Pepin   5.90 2.00    TROUT       NA      4.50 2021 W34
    ## 560    Pepin   5.90 2.00    TROUT     0.55      3.90 2021 W34
    ## 561    Pepin   8.00 1.00      CUT     6.64      8.50 2021 W34
    ## 562    Pepin   8.00 1.00      CUT    10.67     10.10 2021 W34
    ## 563    Pepin   8.00 1.00      CUT    14.97     11.30 2021 W34
    ## 564    Pepin   8.00 1.00      CUT    10.13      9.50 2021 W34
    ## 565    Pepin   8.00 1.00      CUT     7.04      9.40 2021 W34
    ## 566    Pepin   8.00 1.00      CUT     2.60      5.60 2021 W34
    ## 567    Pepin   8.00 1.00      CUT     0.71      3.90 2021 W34
    ## 568    Pepin   8.00 1.00      CUT     1.46      5.40 2021 W34
    ## 569    Pepin   8.00 1.00      CUT     2.77      6.20 2021 W34
    ## 570    Pepin   8.00 1.00      CUT     2.46      6.00 2021 W34
    ## 571    Pepin   8.00 1.00       CO     4.59      8.40 2021 W34
    ## 572    Pepin   8.00 1.00       CO     3.89      7.00 2021 W34
    ## 573    Pepin   8.00 1.00       CO     3.78      6.50 2021 W34
    ## 574    Pepin   8.00 1.00       CO     4.12      6.80 2021 W34
    ## 575    Pepin   8.00 1.00       CO     4.12      6.90 2021 W34
    ## 576    Pepin   8.00 1.00       CO     2.92      6.20 2021 W34
    ## 577    Pepin   8.00 1.00       CO     2.45      5.60 2021 W34
    ## 578    Pepin   8.00 1.00       CO     1.48      5.40 2021 W34
    ## 579    Pepin   8.00 1.00       CO       NA      5.20 2021 W34
    ## 580    Pepin   8.00 1.00       CO     1.28      4.40 2021 W34
    ## 581    Pepin   8.00 1.00     LAMP     3.33     10.00 2021 W34
    ## 582    Pepin   8.00 1.00     LAMP     3.40     13.00 2021 W34
    ## 583    Pepin   8.00 1.00     LAMP     3.76     12.00 2021 W34
    ## 584    Pepin   8.00 1.00     LAMP     2.00      9.00 2021 W34
    ## 585    Pepin   8.00 1.00     LAMP     1.12      6.00 2021 W34
    ## 586    Pepin   8.00 1.00     LAMP     0.94      7.00 2021 W34
    ## 587    Pepin   8.00 1.00     LAMP     0.47      6.50 2021 W34
    ## 588    Pepin   8.00 1.00     LAMP     0.35      6.00 2021 W34
    ## 589    Pepin   8.00 1.00     LAMP     0.81      6.00 2021 W34
    ## 590    Pepin   8.00 1.00     LAMP     0.77      7.50 2021 W34
    ## 591    Pepin   8.00 1.00     LAMP     0.15      5.00 2021 W34
    ## 592    Pepin   8.00 1.00      DAC     8.15      9.00 2021 W34
    ## 593    Pepin   8.00 1.00      STK     2.21      5.80 2021 W34
    ## 594    Pepin   8.00 1.00      STK     1.55      5.50 2021 W34
    ## 595    Pepin   8.00 1.00      STK     3.48      5.50 2021 W34
    ## 596    Pepin   8.00 1.00      STK     1.27      4.00 2021 W34
    ## 597    Pepin   8.00 1.00    TROUT     2.01      5.30 2021 W34
    ## 598    Pepin   8.00 1.00    TROUT     2.06      5.60 2021 W34
    ## 599    Pepin   8.00 1.00    TROUT     1.89      5.30 2021 W34
    ## 600    Pepin   8.00 1.00    TROUT     1.65      5.60 2021 W34
    ## 601    Pepin   8.00 1.00    TROUT     1.82      4.90 2021 W34
    ## 602    Pepin   8.00 1.00    TROUT     0.95      3.90 2021 W34
    ## 603    Pepin   8.00 1.00    TROUT     1.73      5.50 2021 W34
    ## 604    Pepin   8.00 1.00    TROUT     0.94      4.30 2021 W34
    ## 605    Pepin   8.00 1.00    TROUT     1.41      4.40 2021 W34
    ## 606    Pepin   8.00 1.00    TROUT     2.15      5.40 2021 W34
    ## 607    Pepin   8.00 1.00    TROUT     0.87      4.50 2021 W34
    ## 608    Pepin   8.00 2.00     LAMP     1.31      9.00 2021 W34
    ## 609    Pepin   8.00 2.00     LAMP     1.20      7.00 2021 W34
    ## 610    Pepin   8.00 2.00     LAMP     0.66      6.00 2021 W34
    ## 611    Pepin   8.00 2.00      RBT     9.60      9.80 2021 W34
    ## 612    Pepin   8.00 2.00      STK     0.63      3.50 2021 W34
    ## 613    Pepin   8.00 2.00      STK     1.49      5.90 2021 W34
    ## 614    Pepin   8.00 2.00    TROUT     1.55      5.40 2021 W34
    ## 615    Pepin   8.00 3.00      CUT     6.54      8.80 2021 W34
    ## 616    Pepin   8.00 3.00      RBT     0.68      4.20 2021 W34
    ## 617    Pepin   8.00 3.00      DAC     6.15      8.70 2021 W34
    ## 618    Pepin   8.00 3.00     LAMP     2.68     11.00 2021 W34
    ## 619 Bertrand   7.00 1.00      DAC     1.10      4.25 2021 W34
    ## 620 Bertrand   7.00 1.00      CUT     2.40      7.00 2021 W34
    ## 621 Bertrand   7.00 1.00      DAC     0.70      4.00 2021 W34
    ## 622 Bertrand   7.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 623 Bertrand   7.00 2.00      DAC     2.90      5.50 2021 W34
    ## 624 Bertrand   7.00 3.00      DAC     0.90      3.70 2021 W34
    ## 625 Bertrand   6.00 1.00      DAC       NA        NA 2021 W34
    ## 626 Bertrand   6.00 1.00      DAC     2.70      6.00 2021 W34
    ## 627 Bertrand   6.00 1.00      DAC     3.80      7.00 2021 W34
    ## 628 Bertrand   6.00 1.00     LAMP     3.90     12.00 2021 W34
    ## 629 Bertrand   6.00 1.00      DAC     4.60      7.50 2021 W34
    ## 630 Bertrand   6.00 1.00      CUT     3.30      6.50 2021 W34
    ## 631 Bertrand   6.00 1.00      CUT     3.40      6.50 2021 W34
    ## 632 Bertrand   6.00 1.00      CUT     4.90      7.50 2021 W34
    ## 633 Bertrand   6.00 1.00      RBT     3.60      6.50 2021 W34
    ## 634 Bertrand   6.00 1.00      DAC     4.00      7.25 2021 W34
    ## 635 Bertrand   6.00 1.00      DAC     2.40      6.00 2021 W34
    ## 636 Bertrand   6.00 1.00      DAC     3.60      7.00 2021 W34
    ## 637 Bertrand   6.00 1.00      DAC     3.10      6.50 2021 W34
    ## 638 Bertrand   6.00 1.00      DAC     3.10      7.00 2021 W34
    ## 639 Bertrand   6.00 1.00      DAC     5.30      7.50 2021 W34
    ## 640 Bertrand   6.00 1.00      DAC     3.00      6.50 2021 W34
    ## 641 Bertrand   6.00 1.00      DAC     2.40      6.00 2021 W34
    ## 642 Bertrand   6.00 1.00      DAC     4.00      7.00 2021 W34
    ## 643 Bertrand   6.00 1.00      CUT     3.40      6.50 2021 W34
    ## 644 Bertrand   6.00 1.00      RBT     4.40      7.00 2021 W34
    ## 645 Bertrand   6.00 1.00      CUT     4.50      7.25 2021 W34
    ## 646 Bertrand   6.00 1.00      DAC     2.60      6.50 2021 W34
    ## 647 Bertrand   6.00 1.00      DAC     4.20      7.00 2021 W34
    ## 648 Bertrand   6.00 1.00      DAC     4.10      7.50 2021 W34
    ## 649 Bertrand   6.00 1.00      CUT     2.80      6.00 2021 W34
    ## 650 Bertrand   6.00 1.00      RBT     1.40      5.00 2021 W34
    ## 651 Bertrand   6.00 1.00      CUT     5.30      7.50 2021 W34
    ## 652 Bertrand   6.00 1.00      DAC     2.90      6.00 2021 W34
    ## 653 Bertrand   6.00 1.00      DAC     2.60      6.00 2021 W34
    ## 654 Bertrand   6.00 1.00      DAC     4.00      6.50 2021 W34
    ## 655 Bertrand   6.00 1.00      DAC     2.40      5.50 2021 W34
    ## 656 Bertrand   6.00 1.00      DAC     3.70      7.00 2021 W34
    ## 657 Bertrand   6.00 1.00      DAC     1.00      4.00 2021 W34
    ## 658 Bertrand   6.00 1.00      DAC     2.60      6.50 2021 W34
    ## 659 Bertrand   6.00 1.00      DAC     1.60      5.50 2021 W34
    ## 660 Bertrand   6.00 1.00      DAC     0.90      3.50 2021 W34
    ## 661 Bertrand   6.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 662 Bertrand   6.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 663 Bertrand   6.00 2.00      CUT     4.80      7.00 2021 W34
    ## 664 Bertrand   6.00 2.00      DAC     2.70      6.00 2021 W34
    ## 665 Bertrand   6.00 2.00      DAC     3.40      7.00 2021 W34
    ## 666 Bertrand   6.00 2.00      CUT     3.80      7.00 2021 W34
    ## 667 Bertrand   6.00 2.00      DAC     3.10      7.50 2021 W34
    ## 668 Bertrand   6.00 2.00      DAC     3.90      7.50 2021 W34
    ## 669 Bertrand   6.00 2.00      RBT     2.00      5.50 2021 W34
    ## 670 Bertrand   6.00 2.00      CUT     3.20      6.50 2021 W34
    ## 671 Bertrand   6.00 2.00      CUT     3.50      6.50 2021 W34
    ## 672 Bertrand   6.00 2.00      DAC     3.50      7.00 2021 W34
    ## 673 Bertrand   6.00 2.00      DAC     3.70      7.00 2021 W34
    ## 674 Bertrand   6.00 2.00      CUT     3.20      6.00 2021 W34
    ## 675 Bertrand   6.00 2.00      RBT     2.10      5.50 2021 W34
    ## 676 Bertrand   6.00 2.00      DAC     3.20      7.00 2021 W34
    ## 677 Bertrand   6.00 2.00      DAC     2.50      6.00 2021 W34
    ## 678 Bertrand   6.00 2.00      DAC     3.40      7.00 2021 W34
    ## 679 Bertrand   6.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 680 Bertrand   6.00 3.00      DAC     3.50      6.90 2021 W34
    ## 681 Bertrand   6.00 3.00      DAC     3.30      7.00 2021 W34
    ## 682 Bertrand   6.00 3.00      DAC     4.20      7.50 2021 W34
    ## 683 Bertrand   6.00 3.00      DAC     3.90      7.20 2021 W34
    ## 684 Bertrand   6.00 3.00      DAC     2.50      6.40 2021 W34
    ## 685 Bertrand   6.00 3.00      DAC     3.40      6.70 2021 W34
    ## 686 Bertrand   6.00 3.00      CUT     3.80      6.50 2021 W34
    ## 687 Bertrand   6.00 3.00      DAC     5.10      7.00 2021 W34
    ## 688 Bertrand   6.00 3.00      DAC     5.50      6.90 2021 W34
    ## 689 Bertrand   6.00 3.00 CRAYFISH       NA        NA 2021 W34
    ## 690 Bertrand   4.00 1.00      STK     1.18      4.50 2021 W34
    ## 691 Bertrand   4.00 1.00      DAC     0.92      4.20 2021 W34
    ## 692 Bertrand   4.00 1.00      STK     0.93      4.70 2021 W34
    ## 693 Bertrand   4.00 1.00      CUT     3.20      6.90 2021 W34
    ## 694 Bertrand   4.00 1.00      DAC     2.50      6.50 2021 W34
    ## 695 Bertrand   4.00 1.00      CUT     2.30      5.90 2021 W34
    ## 696 Bertrand   4.00 1.00      DAC     0.93      4.30 2021 W34
    ## 697 Bertrand   4.00 1.00      DAC     1.57      5.40 2021 W34
    ## 698 Bertrand   4.00 1.00      DAC     0.82      4.40 2021 W34
    ## 699 Bertrand   4.00 1.00      DAC     0.80      4.20 2021 W34
    ## 700 Bertrand   4.00 1.00      DAC     3.44      7.30 2021 W34
    ## 701 Bertrand   4.00 1.00      DAC     3.72      7.50 2021 W34
    ## 702 Bertrand   4.00 1.00      DAC     4.35      7.70 2021 W34
    ## 703 Bertrand   4.00 1.00      DAC     2.24      6.10 2021 W34
    ## 704 Bertrand   4.00 1.00      RBT     4.13      7.30 2021 W34
    ## 705 Bertrand   4.00 1.00      STK     1.65      5.00 2021 W34
    ## 706 Bertrand   4.00 1.00      DAC     2.26      6.10 2021 W34
    ## 707 Bertrand   4.00 1.00      STK     1.36      4.80 2021 W34
    ## 708 Bertrand   4.00 1.00      STK     1.21      4.70 2021 W34
    ## 709 Bertrand   4.00 1.00      DAC     2.87      6.70 2021 W34
    ## 710 Bertrand   4.00 1.00      DAC     2.98      6.50 2021 W34
    ## 711 Bertrand   4.00 1.00      STK     0.39      3.80 2021 W34
    ## 712 Bertrand   4.00 1.00      STK     1.32      4.80 2021 W34
    ## 713 Bertrand   4.00 1.00      STK     0.54      3.90 2021 W34
    ## 714 Bertrand   4.00 1.00      DAC     2.21      6.00 2021 W34
    ## 715 Bertrand   4.00 1.00      STK     0.77      4.20 2021 W34
    ## 716 Bertrand   4.00 1.00      DAC     1.73      5.80 2021 W34
    ## 717 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 718 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 719 Bertrand   4.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 720 Bertrand   4.00 2.00      RBT     3.22      6.40 2021 W34
    ## 721 Bertrand   4.00 2.00      RBT     3.94      6.90 2021 W34
    ## 722 Bertrand   4.00 2.00      DAC     2.20      5.80 2021 W34
    ## 723 Bertrand   4.00 2.00      RBT     3.30      6.60 2021 W34
    ## 724 Bertrand   4.00 2.00      STK     1.40      5.00 2021 W34
    ## 725 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34
    ## 726 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34
    ## 727 Bertrand   4.00 2.00      STK     0.60      3.60 2021 W34
    ## 728 Bertrand   4.00 2.00     LAMP     4.90     14.10 2021 W34
    ## 729 Bertrand   4.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 730 Bertrand   4.00 3.00      DAC     3.30      7.30 2021 W34
    ## 731 Bertrand   4.00 3.00      DAC     2.00      6.10 2021 W34
    ## 732 Bertrand   4.00 3.00      STK     1.40      4.90 2021 W34
    ## 733 Bertrand   4.00 3.00      STK     1.00      5.50 2021 W34
    ## 734 Bertrand   4.00 3.00      DAC     5.90      8.90 2021 W34
    ## 735 Bertrand   4.00 3.00      RBT     4.00      7.40 2021 W34
    ## 736 Bertrand   4.00 3.00      DAC     2.00      5.50 2021 W34
    ## 737 Bertrand   3.50 1.00      RBT    23.00     13.00 2021 W34
    ## 738 Bertrand   3.50 1.00      STK     1.30      4.50 2021 W34
    ## 739 Bertrand   3.50 1.00      DAC     1.50      3.40 2021 W34
    ## 740 Bertrand   3.50 1.00      DAC     2.20      5.50 2021 W34
    ## 741 Bertrand   3.50 2.00      DAC     0.70      4.00 2021 W34
    ## 742 Bertrand   3.50 3.00      STK     0.70      3.80 2021 W34
    ## 743 Bertrand   3.00 1.00      STK     1.20      4.00 2021 W34
    ## 744 Bertrand   3.00 1.00      DAC     4.20      7.00 2021 W34
    ## 745 Bertrand   3.00 1.00      DAC     3.30      7.00 2021 W34
    ## 746 Bertrand   3.00 1.00      DAC     1.00      4.50 2021 W34
    ## 747 Bertrand   3.00 1.00      DAC     0.90      4.50 2021 W34
    ## 748 Bertrand   3.00 1.00      RBT     3.10      6.50 2021 W34
    ## 749 Bertrand   3.00 1.00      CUT     5.30      7.50 2021 W34
    ## 750 Bertrand   3.00 1.00      DAC     4.72      7.40 2021 W34
    ## 751 Bertrand   3.00 1.00      DAC     4.35      6.20 2021 W34
    ## 752 Bertrand   3.00 1.00      STK     2.25      4.20 2021 W34
    ## 753 Bertrand   3.00 1.00      STK     1.75      4.50 2021 W34
    ## 754 Bertrand   3.00 1.00      DAC     3.60      6.30 2021 W34
    ## 755 Bertrand   3.00 1.00      DAC     3.50      6.50 2021 W34
    ## 756 Bertrand   3.00 1.00      DAC     2.35      4.60 2021 W34
    ## 757 Bertrand   3.00 2.00      CUT     3.30      6.50 2021 W34
    ## 758 Bertrand   3.00 2.00      DAC     0.90      4.00 2021 W34
    ## 759 Bertrand   3.00 2.00      CUT     4.70      7.50 2021 W34
    ## 760 Bertrand   3.00 2.00      DAC     1.00      4.00 2021 W34
    ## 761 Bertrand   3.00 2.00      DAC     2.30      6.00 2021 W34
    ## 762 Bertrand   3.00 2.00      DAC     0.80      3.50 2021 W34
    ## 763 Bertrand   3.00 2.00      DAC     2.91      6.50 2021 W34
    ## 764 Bertrand   3.00 2.00      CUT     3.64      7.50 2021 W34
    ## 765 Bertrand   3.00 2.00      DAC     5.05      8.00 2021 W34
    ## 766 Bertrand   3.00 2.00      DAC     2.22      5.50 2021 W34
    ## 767 Bertrand   3.00 3.00      DAC     5.60      8.00 2021 W34
    ## 768 Bertrand   3.00 3.00      DAC     0.73      4.00 2021 W34
    ## 769 Bertrand   2.00 1.00      DAC     5.88      8.00 2021 W34
    ## 770 Bertrand   2.00 1.00      DAC     3.53      7.50 2021 W34
    ## 771 Bertrand   2.00 1.00      DAC     3.76      7.00 2021 W34
    ## 772 Bertrand   2.00 1.00      DAC     2.45      5.80 2021 W34
    ## 773 Bertrand   2.00 1.00      DAC     2.77      6.50 2021 W34
    ## 774 Bertrand   2.00 1.00      DAC     4.91      8.00 2021 W34
    ## 775 Bertrand   2.00 1.00      DAC     0.74      3.40 2021 W34
    ## 776 Bertrand   2.00 1.00      DAC     2.26      5.50 2021 W34
    ## 777 Bertrand   2.00 1.00      DAC     3.71      7.00 2021 W34
    ## 778 Bertrand   2.00 1.00      DAC     3.80      7.30 2021 W34
    ## 779 Bertrand   2.00 1.00      DAC     8.40      9.00 2021 W34
    ## 780 Bertrand   2.00 1.00      DAC     4.53      7.75 2021 W34
    ## 781 Bertrand   2.00 1.00      DAC     4.12      7.50 2021 W34
    ## 782 Bertrand   2.00 1.00      DAC     2.15      5.80 2021 W34
    ## 783 Bertrand   2.00 2.00      DAC     4.65      5.75 2021 W34
    ## 784 Bertrand   2.00 2.00      DAC     1.56      5.25 2021 W34
    ## 785 Bertrand   2.00 2.00      DAC     1.10      4.00 2021 W34
    ## 786 Bertrand   2.00 2.00      DAC     4.59      8.00 2021 W34
    ## 787 Bertrand   2.00 2.00      DAC     1.98      5.75 2021 W34
    ## 788 Bertrand   2.00 2.00      DAC     2.66      6.50 2021 W34
    ## 789 Bertrand   2.00 2.00      DAC     2.40      6.00 2021 W34
    ## 790 Bertrand   2.00 2.00      DAC     1.92      6.00 2021 W34
    ## 791 Bertrand   2.00 2.00      DAC     0.58      3.50 2021 W34
    ## 792 Bertrand   2.00 2.00 CRAYFISH       NA        NA 2021 W34
    ## 793 Bertrand   2.00 3.00      DAC     2.80      6.00 2021 W34
    ## 794 Bertrand   2.00 3.00      CUT     3.55      6.50 2021 W34
    ## 795 Bertrand   2.00 3.00      DAC     8.12      8.50 2021 W34
    ## 796 Bertrand   2.00 3.00 CRAYFISH       NA        NA 2021 W34
    ## 797 Bertrand   2.00 4.00      DAC     3.89      6.40 2021 W34
    ## 798 Bertrand   2.00 4.00      DAC     1.74      6.20 2021 W34
    ## 799 Bertrand   1.00 1.00      STK     0.59      3.80 2021 W34
    ## 800 Bertrand   1.00 1.00      CUT     2.23      6.50 2021 W34
    ## 801 Bertrand   1.00 1.00      STK     1.42      4.50 2021 W34
    ## 802 Bertrand   1.00 1.00      STK     1.52      4.50 2021 W34
    ## 803 Bertrand   1.00 1.00      STK     1.46      4.50 2021 W34
    ## 804 Bertrand   1.00 1.00      STK     1.50      4.75 2021 W34
    ## 805 Bertrand   1.00 1.00      STK     1.00      4.40 2021 W34
    ## 806 Bertrand   1.00 1.00      STK     1.34      4.50 2021 W34
    ## 807 Bertrand   1.00 1.00      DAC     2.02      6.50 2021 W34
    ## 808 Bertrand   1.00 1.00      DAC     3.14      6.50 2021 W34
    ## 809 Bertrand   1.00 1.00      STK     1.33      4.50 2021 W34
    ## 810 Bertrand   1.00 1.00      DAC     2.16      5.70 2021 W34
    ## 811 Bertrand   1.00 1.00      STK     1.91      4.00 2021 W34
    ## 812 Bertrand   1.00 1.00      STK     1.34      4.00 2021 W34
    ## 813 Bertrand   1.00 1.00      CUT     3.24      6.40 2021 W34
    ## 814 Bertrand   1.00 1.00       CO     3.51      6.50 2021 W34
    ## 815 Bertrand   1.00 1.00      CUT     4.29      6.90 2021 W34
    ## 816 Bertrand   1.00 1.00      RBT     1.81      5.00 2021 W34
    ## 817 Bertrand   1.00 1.00      STK     1.64      4.00 2021 W34
    ## 818 Bertrand   1.00 1.00      STK     0.99      4.10 2021 W34
    ## 819 Bertrand   1.00 1.00      STK     1.26      3.90 2021 W34
    ## 820 Bertrand   1.00 1.00      STK     1.33      4.50 2021 W34
    ## 821 Bertrand   1.00 1.00      RBT     1.99      5.30 2021 W34
    ## 822 Bertrand   1.00 1.00      DAC     1.08      4.40 2021 W34
    ## 823 Bertrand   1.00 1.00      DAC     4.40      7.50 2021 W34
    ## 824 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 825 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 826 Bertrand   1.00 1.00 CRAYFISH       NA        NA 2021 W34
    ## 827 Bertrand   1.00 2.00      STK     1.73      4.00 2021 W34
    ## 828 Bertrand   1.00 2.00      STK     1.64      4.00 2021 W34
    ## 829 Bertrand   1.00 2.00      DAC     3.24      6.90 2021 W34
    ## 830 Bertrand   1.00 2.00      CUT     2.62      6.00 2021 W34
    ## 831 Bertrand   1.00 2.00      STK     1.44      4.50 2021 W34
    ## 832 Bertrand   1.00 2.00      RBT     2.23      5.70 2021 W34
    ## 833 Bertrand   1.00 2.00      CUT     3.11      6.40 2021 W34
    ## 834 Bertrand   1.00 2.00      STK     1.08      4.00 2021 W34
    ## 835 Bertrand   1.00 2.00      STK     1.17      4.50 2021 W34
    ## 836 Bertrand   1.00 2.00      RBT     2.46      5.50 2021 W34
    ## 837 Bertrand   1.00 2.00      CUT     3.56      6.60 2021 W34
    ## 838 Bertrand   1.00 3.00      STK     1.25      4.60 2021 W34
    ## 839 Bertrand   1.00 3.00      STK     1.34      5.10 2021 W34
    ## 840 Bertrand   0.50 1.00      DAC     2.65      5.80 2021 W34
    ## 841 Bertrand   0.50 1.00      DAC     2.51      5.50 2021 W34
    ## 842 Bertrand   0.50 1.00      DAC     1.17      4.00 2021 W34
    ## 843 Bertrand   0.50 1.00      DAC     1.15      4.50 2021 W34
    ## 844 Bertrand   0.50 1.00      CUT     3.48      6.50 2021 W34
    ## 845 Bertrand   0.50 1.00      CUT     5.06      7.70 2021 W34
    ## 846 Bertrand   0.50 1.00      CUT     4.15      7.20 2021 W34
    ## 847 Bertrand   0.50 2.00      DAC     2.07      6.00 2021 W34
    ## 848 Bertrand   0.50 2.00       CO     3.65      7.00 2021 W34
    ## 849 Bertrand   0.50 2.00      RBT     1.24      4.40 2021 W34
    ## 850 Bertrand   0.50 3.00      DAC     1.45      5.40 2021 W34

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
