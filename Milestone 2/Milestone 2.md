Milestone 2
================
John Gray

# Task 1: Process and summarize your data

## Task 1.1:

1.  How do populations of fish differ between Pepin Creek and Bertrand
    Creek?
2.  How does the weight of the fish differ between Pepin Creek and
    Bertrand Creek?
3.  How does the length to weight ratio differ between species?
4.  Is there a difference in fish abundance between riffles
    (i.e. sampling sites)?

## Task 1.2:

*1. How do populations of fish differ between Pepin Creek and Bertrand
Creek?* \*First I computed the number of sampled fish of each species
within each creek and then I represented this data using a plot with two
“geom\_‘x’” functions. (Exercises 2 and 5)

``` r
fish_sampling%>%
  group_by(system,species)%>%
  tally()%>%
  ggplot(aes(system,n))+
  geom_boxplot()+
  geom_dotplot(binaxis = 'y',dotsize=1,stackdir='center',aes(fill=species))+
  labs(y="Species Abundance",x="System", title = "Fish species abundnance within Bertrand and Pepin Creek")
```

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with `binwidth`.

![](Milestone-2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- --> *2. How
does the weight of the fish differ between Pepin Creek and Bertrand
Creek?* \*Compute the mean, standard deviation, median, variance, and
range of fish species weights collected in Pepin Creek and Bertrand
Creek I then plotted the weight the different species in Pepin Creek and
Bertrand Creek, where I had to edit the alpha transparency. (Exercises 1
and 7)

``` r
fish_sampling%>%
  transform(weight_g=as.numeric(weight_g))%>%
  group_by(system, species)%>%
  summarize(species_mean_weight=mean(weight_g,na.rm=TRUE),
            species_weight_SD=sd(weight_g,na.rm=TRUE),
            species_median_weight=median(weight_g,na.rm=TRUE),
            species_weight_variance=var(weight_g,na.rm=TRUE),
            species_weight_range=range(weight_g,na.rm=TRUE))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in min(x): no non-missing arguments to min; returning Inf

    ## Warning in max(x): no non-missing arguments to max; returning -Inf

    ## Warning in min(x): no non-missing arguments to min; returning Inf

    ## Warning in max(x): no non-missing arguments to max; returning -Inf

    ## `summarise()` has grouped output by 'system', 'species'. You can override using the `.groups` argument.

    ## # A tibble: 30 × 7
    ## # Groups:   system, species [15]
    ##    system   species  species_mean_weight species_weight_SD species_median_weight
    ##    <chr>    <chr>                  <dbl>             <dbl>                 <dbl>
    ##  1 Bertrand CO                      3.58            0.0990                  3.58
    ##  2 Bertrand CO                      3.58            0.0990                  3.58
    ##  3 Bertrand CRAYFISH              NaN              NA                      NA   
    ##  4 Bertrand CRAYFISH              NaN              NA                      NA   
    ##  5 Bertrand CUT                     3.67            0.862                   3.49
    ##  6 Bertrand CUT                     3.67            0.862                   3.49
    ##  7 Bertrand DAC                     2.89            1.47                    2.77
    ##  8 Bertrand DAC                     2.89            1.47                    2.77
    ##  9 Bertrand LAMP                    4.4             0.707                   4.4 
    ## 10 Bertrand LAMP                    4.4             0.707                   4.4 
    ## # … with 20 more rows, and 2 more variables: species_weight_variance <dbl>,
    ## #   species_weight_range <dbl>

``` r
fish_sampling%>%
  arrange(species)%>%
  ggplot(aes(species,weight_g))+
  geom_jitter(alpha=0.2,aes(color=system))+
  scale_x_discrete(labels=c("CO"="Coho", "CRAYFISH"="Crayfish","CUT"="Cutthroat","RBT"="Steelhead", "LAMP"= "Lamprey","STK"="Stickleback","DAC"="Dace"))+
  scale_y_discrete(breaks = seq(from =0,to = max(fish_sampling$weight_g),by=0.5),expand=c(0,0))+
  coord_cartesian(clip = "off")+
  ylab("Weight (g)")+
  xlab("Species")
```

![](Milestone-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

*3. How does the length to weight ratio differ between species?*
\*Compute the mean, standard deviation, median, variance, and range of
fish species length to weight ratio collected in Pepin Creek and
Bertrand Creek I then plotted the length to weight ratio of the
different species in Pepin Creek and Bertrand Creek, where I had to edit
the alpha transparency. (Exercises 1 and 7)

``` r
fish_sampling%>%
  transform(weight_g=as.numeric(weight_g),length_cm=as.numeric(length_cm))%>%
  mutate(length_weight_ratio=length_cm/weight_g)%>%
  group_by(species)%>%
  summarize(species_mean_length_weight_ratio=mean(length_weight_ratio,na.rm=TRUE),
            species_length_weight_ratio_SD=sd(weight_g,na.rm=TRUE),
            species_median_length_weight_ratio=median(length_weight_ratio,na.rm=TRUE),
            species_length_weight_ratio_variance=var(length_weight_ratio,na.rm=TRUE),
            species_length_weight_ratio_range=range(length_weight_ratio,na.rm=TRUE))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in min(x): no non-missing arguments to min; returning Inf

    ## Warning in max(x): no non-missing arguments to max; returning -Inf

    ## `summarise()` has grouped output by 'species'. You can override using the `.groups` argument.

    ## # A tibble: 16 × 6
    ## # Groups:   species [8]
    ##    species  species_mean_len… species_length_… species_median_… species_length_…
    ##    <chr>                <dbl>            <dbl>            <dbl>            <dbl>
    ##  1 CO                    2.53            1.08              2.26            0.854
    ##  2 CO                    2.53            1.08              2.26            0.854
    ##  3 CRAYFISH            NaN              NA                NA              NA    
    ##  4 CRAYFISH            NaN              NA                NA              NA    
    ##  5 CUT                   2.00            6.95              1.87            0.974
    ##  6 CUT                   2.00            6.95              1.87            0.974
    ##  7 DAC                   2.48            1.91              2.18            1.23 
    ##  8 DAC                   2.48            1.91              2.18            1.23 
    ##  9 LAMP                  8.61            1.13              7.5            66.4  
    ## 10 LAMP                  8.61            1.13              7.5            66.4  
    ## 11 RBT                   4.62           10.4               2.75          130.   
    ## 12 RBT                   4.62           10.4               2.75          130.   
    ## 13 STK                   5.21            0.872             3.88           17.0  
    ## 14 STK                   5.21            0.872             3.88           17.0  
    ## 15 TROUT                 4.26            0.621             3.82            2.50 
    ## 16 TROUT                 4.26            0.621             3.82            2.50 
    ## # … with 1 more variable: species_length_weight_ratio_range <dbl>

``` r
fish_sampling%>%
  transform(weight_g=as.numeric(weight_g),length_cm=as.numeric(length_cm))%>%
  mutate(length_weight_ratio=length_cm/weight_g)%>%
  ggplot(aes(species,length_weight_ratio))+
  geom_jitter(alpha=0.4,width=0.3,aes(color=system))+
  scale_y_log10()+
  labs(y="Fish Length to Weight Ratio",x="Species", title = "The length to weight ratio of fish species within Bertrand and Pepin Creek")+
  scale_x_discrete(labels=c("CO"="Coho", "CRAYFISH"="Crayfish","CUT"="Cutthroat","RBT"="Steelhead", "LAMP"= "Lamprey","STK"="Stickleback","DAC"="Dace","TROUT"="Trout sp."))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning: Removed 191 rows containing missing values (geom_point).

![](Milestone-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

*4. Is there a difference in fish abundance between riffles
(i.e. sampling sites)?* \* First I computed the number of sampled fish
of each species, within each riffle, in each creek and then I
represented this data using a plot with two “geom\_‘x’”
functions.(Exercises 2 and 5)

``` r
fish_sampling%>%
  group_by(system,riffle,species)%>%
  tally()%>%
  ggplot(aes(riffle,species))+
  ggridges::geom_density_ridges()+
  geom_point()
```

    ## Picking joint bandwidth of 1.65

![](Milestone-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
fish_sampling
```

    ## # A tibble: 850 × 11
    ##    system riffle  pass species weight_g length_cm Date       shocking_effort_s
    ##    <chr>   <dbl> <dbl> <chr>   <chr>    <chr>     <chr>      <chr>            
    ##  1 Pepin      14     1 LAMP    6.8      15        08/19/2021 -                
    ##  2 Pepin      14     1 LAMP    4.8      15        08/19/2021 -                
    ##  3 Pepin      14     1 LAMP    1.05     10        08/19/2021 -                
    ##  4 Pepin      14     1 LAMP    0.9      7         08/19/2021 -                
    ##  5 Pepin      14     1 LAMP    0.9      6         08/19/2021 -                
    ##  6 Pepin      14     1 LAMP    2.25     10        08/19/2021 -                
    ##  7 Pepin      14     1 CUT     3.1      6.6       08/19/2021 -                
    ##  8 Pepin      14     1 CO      2.7      5.7       08/19/2021 -                
    ##  9 Pepin      14     1 CUT     6.4      7.2       08/19/2021 -                
    ## 10 Pepin      14     1 RBT     2.5      5.8       08/19/2021 -                
    ## # … with 840 more rows, and 3 more variables: sampled_riffle_length_m <chr>,
    ## #   Comments <chr>, ...11 <chr>

## Task 1.3:

As can be seen in the displayed data summaries and plots, I now have a
better numerical and visual observation of the collected data. Visually,
I believe I am able to see some trends in the data however, I now need
to approach the data statistically to assess whether these trends I
notice are statistically significant. However, I am content with my
research questions as they are. Though they are generic, I feel that
they incorporate some aspects of the streams fish ecology that needs to
be assessed so they can be related to different abiotic factors of the
streams.

# Task 2: Tidy the data

## Task 2.1:

Based on the definition of “Tidy Data”, I am under the impression that
my data is “tidy”. Each column, except for the “comments” column, is a
variable, each row is one collected observation and each of the cells
represent a value derived from the observation of a particular variable.
Therefore, the organization of my columns and rows, and the values
contained within my cells aid in answering my research questions.

**System** - is tidy. It represents a variable which describes system
(i.e. Pepin Creek or Bertrand Creek) the sample was taken in. Therefore,
each row is an observation, as Pepin or Bertrand, within the column
describes where the data was observed; collected. Each entry in this
column is a value.

**Riffle** - is tidy. It represents a variable which describes at which
sampling site (i.e. riffle) the data was collected. Therefore, each row
is an observation describing at which riffle the data was collected and
each value is a value represents at which riffle in which system the
data was collected in.

**Pass** - is tidy. It represents a variable which describes how the
data was collected in. Each observation describes at which
electroshocking pass the data was collected, at which row and in which
system. Therefore, each value describes how the data was collected and
in what pass.

**Species** - is tidy. It represents a variable which describes the fish
that were collected. Each observation indicates which species were
caught in which pass, at which riffle, in which system. Therefore, the
value indicates the species.

**Weight\_g** - is tidy. It represents a variable that describes the
weight of a collected fish. Each observation describes the size of a
sampled fish, the species, which pass it was collected on, at which
riffle and in which system. Therefore, the value represents the size of
a specimen, where it was caught and how it was caught.

**Length\_cm** - is tidy. It represents a variable that describes the
length of a collected fish. Each observation describes the size of a
sampled fish, the species, which pass it was collected on, at which
riffle and in which system. Therefore, the value represents the length
of a specimen, where it was caught and how it was caught.

**Date** - is tidy. It represents a variable that describes when a fish
was collected. Each observation describes when a fish was collected, its
species, which pass it was collected on, at which riffle and in which
system. Therefore, the value indicates when a specimen was collected,
where it was caught and how it was caught.

**Sampled Riffle Length** - is tidy. It represents a variable that
describes how long the riffle is that was sampled to provide an
indicator of fish per unit area. Each observation describes how long the
was, when a fish was collected, its species, which pass it was collected
on, at which riffle and in which system. Therefore, the value indicates
the length of the sampling site when a specimen was collected, where it
was caught and how it was caught

## Task 2.2:

**Un-tidying my data:**

``` r
fish_sampling2_2A<-more_fish_sampling%>%
  select(system,riffle,species)%>%
  group_by(riffle)%>%
  mutate(row=row_number())%>%
  tidyr::pivot_wider(names_from = riffle,
                     values_from = species)%>%
  select(-row)
print(fish_sampling2_2A)
```

    ## # A tibble: 192 × 19
    ##    system `14`  `12`  `6`   `11`     `9.5` `9`   `10`  `3`   `7`   `5`   `5.75`
    ##    <chr>  <chr> <chr> <chr> <chr>    <chr> <chr> <chr> <chr> <chr> <chr> <chr> 
    ##  1 Pepin  LAMP  CUT   STK   STK      CUT   CUT   RBT   CUT   STK   STK   CUT   
    ##  2 Pepin  LAMP  CUT   CUT   CO       STK   CUT   CO    STK   CO    CO    CUT   
    ##  3 Pepin  LAMP  CUT   CO    CO       STK   CO    RBT   CO    RBT   CUT   CUT   
    ##  4 Pepin  LAMP  STK   RBT   STK      RBT   LAMP  CO    LAMP  CUT   STK   CO    
    ##  5 Pepin  LAMP  LAMP  STK   CUT      CUT   CO    CO    RBT   DAC   STK   CO    
    ##  6 Pepin  LAMP  LAMP  CO    STK      RBT   CUT   CO    LAMP  CO    DAC   CO    
    ##  7 Pepin  CUT   LAMP  STK   STK      CUT   STK   CO    STK   STK   CUT   CO    
    ##  8 Pepin  CO    LAMP  CO    LAMP     CO    LAMP  STK   RBT   CUT   STK   CO    
    ##  9 Pepin  CUT   LAMP  RBT   CRAYFISH CO    CO    RBT   STK   RBT   CO    LAMP  
    ## 10 Pepin  RBT   LAMP  CO    CUT      RBT   LAMP  LAMP  STK   CO    CO    LAMP  
    ## # … with 182 more rows, and 7 more variables: 5.9 <chr>, 8 <chr>, 4 <chr>,
    ## #   3.5 <chr>, 2 <chr>, 1 <chr>, 0.5 <chr>

**Tidying my data:**

``` r
fish_sampling2_2B<-fish_sampling2_2A%>%
  pivot_longer(cols = c(-system),
               names_to = "riffle",
               values_to = "species")
print(fish_sampling2_2B)
```

    ## # A tibble: 3,456 × 3
    ##    system riffle species
    ##    <chr>  <chr>  <chr>  
    ##  1 Pepin  14     LAMP   
    ##  2 Pepin  12     CUT    
    ##  3 Pepin  6      STK    
    ##  4 Pepin  11     STK    
    ##  5 Pepin  9.5    CUT    
    ##  6 Pepin  9      CUT    
    ##  7 Pepin  10     RBT    
    ##  8 Pepin  3      CUT    
    ##  9 Pepin  7      STK    
    ## 10 Pepin  5      STK    
    ## # … with 3,446 more rows

## Task 2.3:

I will be keeping the questions: how do populations of fish differ
between Pepin Creek and Bertrand Creek and how does the length to weight
ratio differ between species? Both questions incorporate important data
that needs to be analyzed however, both questions approach the data
differently which will make it more interesting to work with.
Additionally, they incorporate different aspects of my own research
which will be necessary to understand when I am conducting my own
analysis.

*The version of the data set I want to use:*

``` r
more_fish_sampling<-fish_sampling%>%
  transform(weight_g=as.numeric(weight_g),length_cm=as.numeric(length_cm))%>%
  mutate(length_weight_ratio=length_cm/weight_g)%>%
  subset(select=-c(shocking_effort_s,sampled_riffle_length_m,Comments,...11))%>%
  group_by(system)
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
head(more_fish_sampling)
```

    ## # A tibble: 6 × 8
    ## # Groups:   system [1]
    ##   system riffle  pass species weight_g length_cm Date       length_weight_ratio
    ##   <chr>   <dbl> <dbl> <chr>      <dbl>     <dbl> <chr>                    <dbl>
    ## 1 Pepin      14     1 LAMP        6.8         15 08/19/2021                2.21
    ## 2 Pepin      14     1 LAMP        4.8         15 08/19/2021                3.12
    ## 3 Pepin      14     1 LAMP        1.05        10 08/19/2021                9.52
    ## 4 Pepin      14     1 LAMP        0.9          7 08/19/2021                7.78
    ## 5 Pepin      14     1 LAMP        0.9          6 08/19/2021                6.67
    ## 6 Pepin      14     1 LAMP        2.25        10 08/19/2021                4.44
