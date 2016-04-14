Volta Preliminary Analysis
================
Juan Carlos Rocha

TAI-Volta project
-----------------

The TAI-Volta project goal is **targeting agricultural innovations** in the Volta river basin, between Ghana and Burkina Faso. This document present a preliminary analysis of the data we've got so far as a showcase of we can do once more data comes into the pipeline. Katja Malmborg has been collecting data from project collaborators and the statistics beaureau from both countries. Raw data are mainly excel files with several tables per sheet indicating area or crops production over time per district or province. She has organized the tables on suitable files (one table per sheet at least) and has also collected socio-economic information from national census reports. I use the script `ExtractDataTAI.R` to mine the different data files and compile a unique data object.

### Data so far..

You can include R code in the document as follows:

``` r
load(file='160414_Volta.RData')
ls()
```

    ## [1] "biophys"     "dat"         "datKeyCrops" "interact"    "resource"   
    ## [6] "users"       "volta.shp"

``` r
summary(dat)
```

    ##     TAI_ID1          Year           crop            area         
    ##  Min.   :1113   Min.   :1993   Cowpea : 1200   Min.   :     0.0  
    ##  1st Qu.:1625   1st Qu.:2001   Maize  : 1200   1st Qu.:     0.0  
    ##  Median :2137   Median :2004   Millet : 1200   Median :   177.2  
    ##  Mean   :2461   Mean   :2004   Rice   : 1200   Mean   :  5752.7  
    ##  3rd Qu.:3412   3rd Qu.:2008   Sorghum: 1200   3rd Qu.:  4100.0  
    ##  Max.   :3912   Max.   :2012   Soy    : 1200   Max.   :201397.4  
    ##                                (Other):15300   NA's   :4189      
    ##       prod          country             TAI_ID2     
    ##  Min.   :     0   Length:22500       1113   :  340  
    ##  1st Qu.:     0   Class :character   1115   :  340  
    ##  Median :   403   Mode  :character   1127   :  340  
    ##  Mean   : 10302                      1131   :  340  
    ##  3rd Qu.:  7475                      1132   :  340  
    ##  Max.   :598350                      1140   :  340  
    ##  NA's   :4494                        (Other):20460

``` r
str(dat)
```

    ## 'data.frame':    22500 obs. of  7 variables:
    ##  $ TAI_ID1: int  3103 3108 3113 3116 3117 3118 3203 3204 3206 3207 ...
    ##  $ Year   : num  2002 2002 2002 2002 2002 ...
    ##  $ crop   : Factor w/ 32 levels "Cabbage","Cassava",..: 12 12 12 12 12 12 12 12 12 12 ...
    ##  $ area   : num  3594 4134 32978 6286 9537 ...
    ##  $ prod   : num  7790 9954 41499 11337 34157 ...
    ##  $ country: chr  "GH" "GH" "GH" "GH" ...
    ##  $ TAI_ID2: Factor w/ 99 levels "1113","1115",..: 40 41 42 43 44 45 46 47 48 49 ...

Including Plots
---------------

You can also embed plots, for example:

![](01_VoltaPrelimAnalysis_files/figure-markdown_github/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
