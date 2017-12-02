Tidy Data Tutorial
================
Sonya Hua
December 2, 2017

source: <https://www.jstatsoft.org/article/view/v059i10>

#### In tidy data:

1.  Each variable forms a column.

2.  Each observation forms a row.

3.  Each type of observational unit forms a table.

Real datasets can, and often do, violate the three precepts of tidy data in almost every way imaginable. While occasionally you do get a dataset that you can start analyzing immediately, this is the exception, not the rule. This section describes the ???ve most common problems with messy datasets, along with their remedies: - Column headers are values, not variable names.

-   Multiple variables are stored in one column.

-   Variables are stored in both rows and columns.

-   Multiple types of observational units are stored in the same table.

-   A single observational unit is stored in multiple tables

### Preparation for Tutorial

First, let's install Ludvig's split chunk [add-in](http://ludvigolsen.dk/splitchunk-rstudio-addin/?lang=en) so we can easily split chunks and add markdown in between.

``` r
# R add-in for splitting chunks

#install.packages("devtools")
library(devtools)
```

    ## Warning: package 'devtools' was built under R version 3.4.2

``` r
devtools::install_github("LudvigOlsen/splitChunk")
```

    ## Skipping install of 'splitChunk' from a github remote, the SHA1 (d176ea2b) has not changed since last install.
    ##   Use `force = TRUE` to force installation

**xtable:** Converts an R object to an xtable object, which can then be printed as a LaTeX or HTML table.

``` r
options(stringsAsFactors = FALSE)

library("stringr")
library("reshape2")
```

    ## Warning: package 'reshape2' was built under R version 3.4.2

``` r
library("plyr")

read.fwf2 <- function(path, cols) {
  raw_stations <- readLines(path)
  stations <- data.frame(matrix(ncol = 0, nrow = length(raw_stations)))

  for(i in 1:nrow(cols)) {
    field <- cols[i, ]
    stations[[field$name]] <- str_trim(str_sub(raw_stations, field$start, field$end))
  }
  stations[stations == ""] <- NA
  stations[] <- lapply(stations, type.convert, as.is = TRUE)
  
  stations
}

# Change defaults for xtable to be more attractive
# Inspired by: http://cameron.bracken.bz/sweave-xtable-booktabs
library("xtable")
xtable <- function(x, file = "", ..., rownames = FALSE) {
  table <- xtable::xtable(x, ...)
  print(table, floating = FALSE, hline.after = NULL, 
    add.to.row = list(pos = list(-1,0, nrow(x)), 
    command = c('\\toprule\n ','\\midrule\n  ','\\bottomrule\n')),
    include.rownames = rownames, NA.string = "---",
    file = file, 
    comment = FALSE, timestamp = FALSE
  )
}
```

### Simulate Data

``` r
####################################################################
## 2. Defining tidy data
####################################################################

set.seed(1014)

preg <- matrix(c(NA, sample(20, 5)), ncol = 2, byrow = TRUE)
colnames(preg) <- paste0("treatment", c("a", "b"))
rownames(preg) <- c("John Smith", "Jane Doe", "Mary Johnson")

xtable(preg, "preg-raw-1.tex", rownames = TRUE, align = "lrr")
xtable(t(preg), "preg-raw-2.tex", rownames = TRUE, align = "lrrr")
preg
```

    ##              treatmenta treatmentb
    ## John Smith           NA          2
    ## Jane Doe             16         11
    ## Mary Johnson          3          1

``` r
# Make tidy version

(pregm <- melt(preg, id = "name"))
```

    ##           Var1       Var2 value
    ## 1   John Smith treatmenta    NA
    ## 2     Jane Doe treatmenta    16
    ## 3 Mary Johnson treatmenta     3
    ## 4   John Smith treatmentb     2
    ## 5     Jane Doe treatmentb    11
    ## 6 Mary Johnson treatmentb     1

``` r
# rename the columns
names(pregm) <- c("name", "trt", "result")
pregm
```

    ##           name        trt result
    ## 1   John Smith treatmenta     NA
    ## 2     Jane Doe treatmenta     16
    ## 3 Mary Johnson treatmenta      3
    ## 4   John Smith treatmentb      2
    ## 5     Jane Doe treatmentb     11
    ## 6 Mary Johnson treatmentb      1

``` r
# Sub "treatment" with ""
pregm$trt <- gsub("treatment", "", pregm$trt)
pregm
```

    ##           name trt result
    ## 1   John Smith   a     NA
    ## 2     Jane Doe   a     16
    ## 3 Mary Johnson   a      3
    ## 4   John Smith   b      2
    ## 5     Jane Doe   b     11
    ## 6 Mary Johnson   b      1

``` r
#install.packages("lubridate")
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.4.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
xtable(pregm, "preg-tidy.tex")
pregm
```

    ##           name trt result
    ## 1   John Smith   a     NA
    ## 2     Jane Doe   a     16
    ## 3 Mary Johnson   a      3
    ## 4   John Smith   b      2
    ## 5     Jane Doe   b     11
    ## 6 Mary Johnson   b      1

``` r
####################################################################
## 3. Tidying messy datasets
####################################################################
## 3.1 Column headers are values, not variable names: toy example
####################################################################

df <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
xtable(df, "melt-raw.tex")
df
```

    ##   row a b c
    ## 1   A 1 4 7
    ## 2   B 2 5 8
    ## 3   C 3 6 9

Melting a data frame by an id column puts all the column variables into 1 column and values into another

``` r
dfm <- melt(df, id = "row")
names(dfm)[2] <- "column"
xtable(dfm, "melt-output.tex")
dfm
```

    ##   row column value
    ## 1   A      a     1
    ## 2   B      a     2
    ## 3   C      a     3
    ## 4   A      b     4
    ## 5   B      b     5
    ## 6   C      b     6
    ## 7   A      c     7
    ## 8   B      c     8
    ## 9   C      c     9

``` r
####################################################################
## 3.1 Column headers are values, not variable names: Pew dataset
####################################################################

library("foreign")

# Data from http://pewforum.org/Datasets/Dataset-Download.aspx

# Load data -----------------------------------------------------------------

pew <- read.spss("pew.sav")
```

    ## re-encoding from CP1252

    ## Warning in read.spss("pew.sav"): Undeclared level(s) 2, 3, 4, 9 added in
    ## variable: density3

    ## Warning in read.spss("pew.sav"): Duplicated levels in factor denom:
    ## Electronic ministries

    ## Warning in read.spss("pew.sav"): Undeclared level(s) 1, 2, 3, 4, 5, 6, 7,
    ## 8, 9, 10, 11, 12, 14, 16, 23, 33 added in variable: children

    ## Warning in read.spss("pew.sav"): Undeclared level(s) 18, 19, 20, 21, 22,
    ## 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
    ## 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    ## 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    ## 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96 added in
    ## variable: age

``` r
pew <- as.data.frame(pew)
head(pew)
```

    ##     weight   psraid int_date    lang type   cregion       state      usr
    ## 1 4.512821 10000001    50807 English  RDD Northeast Connecticut Suburban
    ## 2 2.102564 10000002    50807 English  RDD Northeast       Maine    Rural
    ## 3 1.282051 10000003    50807 English  RDD Northeast       Maine    Rural
    ## 4 1.355323 10000004    50807 English  RDD Northeast       Maine    Rural
    ## 5 1.589744 10000005    50807 English  RDD Northeast    New York    Urban
    ## 6 1.410256 10000007    61507 English  RDD Northeast    New York    Urban
    ##       usr1   form density3           q1      q1a           q2      q2a
    ## 1 Suburban Form A        4    Satisfied     Very    Satisfied     Very
    ## 2    Rural Form B   Lowest    Satisfied     Very    Satisfied     Very
    ## 3    Rural Form A        2    Satisfied Somewhat    Satisfied     Very
    ## 4    Rural Form B        2 Dissatisfied     Very    Satisfied     Very
    ## 5    Urban Form A  Highest Dissatisfied     Very    Satisfied Somewhat
    ## 6    Urban Form A  Highest Dissatisfied     Very Dissatisfied     Very
    ##                  q3a                       q3b                   q3c
    ## 1     Very satisfied            Very satisfied        Very satisfied
    ## 2     Very satisfied            Very satisfied Somewhat dissatisfied
    ## 3     Very satisfied            Very satisfied        Very satisfied
    ## 4 Somewhat satisfied            Very satisfied     Very dissatisfied
    ## 5 Somewhat satisfied        Somewhat satisfied Somewhat dissatisfied
    ## 6 Somewhat satisfied Don<U+0092>t know/Refused (VOL.)     Very dissatisfied
    ##                 q3d
    ## 1    Very satisfied
    ## 2    Very satisfied
    ## 3    Very satisfied
    ## 4 Very dissatisfied
    ## 5    Very satisfied
    ## 6    Very satisfied
    ##                                                            q5a
    ## 1 I worry the government is getting too involved in the issue 
    ## 2 The government should do more to protect morality in society
    ## 3 I worry the government is getting too involved in the issue 
    ## 4 I worry the government is getting too involved in the issue 
    ## 5 I worry the government is getting too involved in the issue 
    ## 6 I worry the government is getting too involved in the issue 
    ##                                                            q5b
    ## 1 Homosexuality is a way of life that should be accepted by so
    ## 2 Homosexuality is a way of life that should be discouraged by
    ## 3 Homosexuality is a way of life that should be accepted by so
    ## 4 Homosexuality is a way of life that should be accepted by so
    ## 5 Homosexuality is a way of life that should be discouraged by
    ## 6 Homosexuality is a way of life that should be accepted by so
    ##                                                            q5c
    ## 1 Most people who want to get ahead can make it if they're wil
    ## 2 Most people who want to get ahead can make it if they're wil
    ## 3 Most people who want to get ahead can make it if they're wil
    ## 4 Hard work and determination are no guarantee of success for 
    ## 5 Most people who want to get ahead can make it if they're wil
    ## 6 Hard work and determination are no guarantee of success for 
    ##                                                            q5d
    ## 1 The government today can't afford to do much more to help th
    ## 2 The government today can't afford to do much more to help th
    ## 3 The government should do more to help needy Americans, even 
    ## 4 The government should do more to help needy Americans, even 
    ## 5 The government today can't afford to do much more to help th
    ## 6 The government should do more to help needy Americans, even 
    ##                                                            q5e
    ## 1 Stricter environmental laws and regulations cost too many jo
    ## 2 Stricter environmental laws and regulations are worth the co
    ## 3 Stricter environmental laws and regulations cost too many jo
    ## 4 Stricter environmental laws and regulations are worth the co
    ## 5 Stricter environmental laws and regulations are worth the co
    ## 6 Stricter environmental laws and regulations are worth the co
    ##                                                            q5f
    ## 1 The best way to ensure peace is through military strength - 
    ## 2 The best way to ensure peace is through military strength - 
    ## 3 The best way to ensure peace is through military strength - 
    ## 4 Good diplomacy is the best way to ensure peace - Second stat
    ## 5 Good diplomacy is the best way to ensure peace - Second stat
    ## 6 Good diplomacy is the best way to ensure peace - Second stat
    ##                                                            q5g
    ## 1 It's best for the future of our country to be active in worl
    ## 2 It's best for the future of our country to be active in worl
    ## 3 It's best for the future of our country to be active in worl
    ## 4 We should pay less attention to problems overseas and concen
    ## 5 It's best for the future of our country to be active in worl
    ## 6 It's best for the future of our country to be active in worl
    ##                                   q6                    q7
    ## 1 Smaller government, fewer services Illegal in most cases
    ## 2 Smaller government, fewer services Illegal in most cases
    ## 3   Bigger government, more services   Legal in most cases
    ## 4   Bigger government, more services   Legal in most cases
    ## 5 Smaller government, fewer services   Legal in most cases
    ## 6   Bigger government, more services    Legal in all cases
    ##                  q8                                     q8a
    ## 1  Most of the time                your personal experience
    ## 2  Most of the time                your personal experience
    ## 3 Only now and then                          your education
    ## 4  Most of the time                      or something else?
    ## 5  Some of the time what you have seen or read in the media
    ## 6  Most of the time                your personal experience
    ##                     q9                q10a             q10b
    ## 1 Should express views Completely disagree     Mostly agree
    ## 2      Should keep out    Completely agree Completely agree
    ## 3      Should keep out     Mostly disagree     Mostly agree
    ## 4      Should keep out     Mostly disagree  Mostly disagree
    ## 5      Should keep out     Mostly disagree     Mostly agree
    ## 6      Should keep out        Mostly agree  Mostly disagree
    ##                  q10c                                  q10d
    ## 1 Completely disagree Practical experience and common sense
    ## 2        Mostly agree Practical experience and common sense
    ## 3        Mostly agree Practical experience and common sense
    ## 4     Mostly disagree Practical experience and common sense
    ## 5    Completely agree                Scientific information
    ## 6    Completely agree       Religious teachings and beliefs
    ##              marital hisp  race                     q16  chr
    ## 1            Married   No White             Protestant  <NA>
    ## 2           Divorced   No White             Protestant  <NA>
    ## 3           Divorced   No White             Protestant  <NA>
    ## 4 Never been married  Yes White  Nothing in particular  <NA>
    ## 5 Never been married   No White       Jewish (Judaism)  <NA>
    ## 6 Never been married   No White       Jewish (Judaism)  <NA>
    ##                                                             q17
    ## 1  Church of Christ, or Disciples of Christ (Christian Church) 
    ## 2                                                      Baptist 
    ## 3                    Congregational or United Church of Christ 
    ## 4                                                          <NA>
    ## 5                                                          <NA>
    ## 6                                                          <NA>
    ##                                 q17a q17b q17c q17d q17e q17f q17g q17h
    ## 1                               <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 2  American Baptist Churches in USA  <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 3                               <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 4                               <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 5                               <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ## 6                               <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>
    ##   q17i q17j               q17k                      q17l q17m q17n q17o
    ## 1 <NA> <NA>  Church of Christ                       <NA> <NA> <NA> <NA>
    ## 2 <NA> <NA>               <NA>                      <NA> <NA> <NA> <NA>
    ## 3 <NA> <NA>               <NA>  United Church of Christ  <NA> <NA> <NA>
    ## 4 <NA> <NA>               <NA>                      <NA> <NA> <NA> <NA>
    ## 5 <NA> <NA>               <NA>                      <NA> <NA> <NA> <NA>
    ## 6 <NA> <NA>               <NA>                      <NA> <NA> <NA> <NA>
    ##   q17p q17q q17r                                               q17s q17t
    ## 1 <NA> <NA> <NA>                                               <NA> <NA>
    ## 2 <NA> <NA> <NA>                                               <NA> <NA>
    ## 3 <NA> <NA> <NA>                                               <NA> <NA>
    ## 4 <NA> <NA> <NA>                                               <NA> <NA>
    ## 5 <NA> <NA> <NA>                                            Reform  <NA>
    ## 6 <NA> <NA> <NA>  Jewish not further specified (just Jewish) (VOL)  <NA>
    ##   q17u q17v  q18                                        denom
    ## 1 <NA> <NA>  Yes                            Church of Christ 
    ## 2 <NA> <NA>  Yes        American Baptist Churches in the USA 
    ## 3 <NA> <NA>   No                     United Church of Christ 
    ## 4 <NA> <NA> <NA>                       Nothing in particular 
    ## 5 <NA> <NA> <NA>                               Reform Jewish 
    ## 6 <NA> <NA> <NA>  Jewish not further specified (just jewish) 
    ##                                          family
    ## 1  Restorationist in the Evangelical Tradition 
    ## 2            Baptist in the Mainline Tradition 
    ## 3  Congregationalist in the Mainline Tradition 
    ## 4                        Nothing in particular 
    ## 5                                      Jewish  
    ## 6                                      Jewish  
    ##                             reltrad             protfam q19a q19b
    ## 1  Evangelical Protestant Churches      Restorationist    No   No
    ## 2     Mainline Protestant Churches             Baptist    No   No
    ## 3     Mainline Protestant Churches   Congregationalist    No   No
    ## 4                     Unaffiliated      Not Protestant  <NA> <NA>
    ## 5                           Jewish      Not Protestant  <NA> <NA>
    ## 6                           Jewish      Not Protestant  <NA> <NA>
    ##                  q20                  q21                 q27  q28a
    ## 1        Once a week       Very important Between 100 and 500 Never
    ## 2             Seldom       Very important                <NA>  <NA>
    ## 3             Seldom    Not too important                <NA>  <NA>
    ## 4 A few times a year       Very important       Less than 100 Never
    ## 5             Seldom    Not too important                <NA>  <NA>
    ## 6              Never Not at all important                <NA>  <NA>
    ##                   q28b   q28c                  q28d q29 q30
    ## 1 Several times a year Seldom Once or twice a month Yes Yes
    ## 2                 <NA>   <NA>                  <NA>  No Yes
    ## 3                 <NA>   <NA>                  <NA>  No Yes
    ## 4                Never  Never                Seldom  No Yes
    ## 5                 <NA>   <NA>                  <NA>  No Yes
    ## 6                 <NA>   <NA>                  <NA>  No Yes
    ##                  q31                        q32                       q33
    ## 1 Absolutely certain            God is a person                       Yes
    ## 2     Fairly certain            God is a person                       Yes
    ## 3 Absolutely certain God is an impersonal force                       Yes
    ## 4 Absolutely certain God is an impersonal force                       Yes
    ## 5    Not too certain God is an impersonal force                        No
    ## 6    Not too certain  Don<U+0092>t know/refused (VOL.) Don<U+0092>t know/refused (VOL.)
    ##                  q34 q34a q34b q34c q34d q35 q36
    ## 1 Absolutely certain <NA> <NA> <NA> <NA> Yes Yes
    ## 2     Fairly certain <NA> <NA> <NA> <NA> Yes Yes
    ## 3     Fairly certain <NA> <NA> <NA> <NA> Yes Yes
    ## 4 Absolutely certain <NA> <NA> <NA> <NA> Yes  No
    ## 5               <NA> <NA> <NA> <NA> <NA>  No  No
    ## 6               <NA> <NA> <NA> <NA> <NA>  No  No
    ##                                                            q37
    ## 1                               [Holy book] is the word of God
    ## 2                               [Holy book] is the word of God
    ## 3                               [Holy book] is the word of God
    ## 4 [Holy book] is a book written by men and is not the word of 
    ## 5 [Holy book] is a book written by men and is not the word of 
    ## 6 [Holy book] is a book written by men and is not the word of 
    ##                                                            q38
    ## 1          [Holy book] is to be taken literally, word for word
    ## 2 Not everything in [Holy book] should be taken literally, wor
    ## 3 Not everything in [Holy book] should be taken literally, wor
    ## 4                                                         <NA>
    ## 5                                                         <NA>
    ## 6                                                         <NA>
    ##                  q39a                q39b                q39c
    ## 1        Mostly agree        Mostly agree Completely disagree
    ## 2    Completely agree        Mostly agree     Mostly disagree
    ## 3        Mostly agree        Mostly agree        Mostly agree
    ## 4    Completely agree     Mostly disagree        Mostly agree
    ## 5     Mostly disagree Completely disagree        Mostly agree
    ## 6 Completely disagree Completely disagree    Completely agree
    ##                                                         q40a
    ## 1 Many religions can lead to eternal life - Second statement
    ## 2 Many religions can lead to eternal life - Second statement
    ## 3 Many religions can lead to eternal life - Second statement
    ## 4                                                       <NA>
    ## 5 Many religions can lead to eternal life - Second statement
    ## 6                                   Don<U+0092>t know/refused (VOL)
    ##                                                           q40b
    ## 1 There is MORE than one true way to interpret the teachings o
    ## 2 There is only ONE true way to interpret the teachings of my 
    ## 3 There is MORE than one true way to interpret the teachings o
    ## 4                                                         <NA>
    ## 5 There is MORE than one true way to interpret the teachings o
    ## 6                                   Neither/Both equally (VOL)
    ##                                                           q40c
    ## 1               preserve its traditional beliefs and practices
    ## 2               preserve its traditional beliefs and practices
    ## 3 adjust traditional beliefs and practices in light of new cir
    ## 4                                                         <NA>
    ## 5 adjust traditional beliefs and practices in light of new cir
    ## 6                                    Don<U+0092>t know/refused (VOL.)
    ##                  q41                  q42a                 q42b
    ## 1 A few times a week Once or twice a month               Seldom
    ## 2             Seldom                Seldom               Seldom
    ## 3             Seldom                 Never                Never
    ## 4 A few times a week                Seldom At least once a week
    ## 5             Seldom                 Never                Never
    ## 6              Never                 Never                Never
    ##                   q42c                 q42d  q42e q42f   q42g
    ## 1                Never Several times a year Never <NA>   <NA>
    ## 2               Seldom               Seldom Never <NA>   <NA>
    ## 3               Seldom                Never Never <NA>   <NA>
    ## 4 At least once a week                 <NA>  <NA> <NA> Seldom
    ## 5               Seldom                Never  <NA> <NA>   <NA>
    ## 6                Never                Never  <NA> <NA>   <NA>
    ##                    q43a                 q43b                 q43c q43d
    ## 1  At least once a week               Seldom Several times a year   No
    ## 2  At least once a week At least once a week                 <NA>   No
    ## 3                 Never               Seldom                 <NA>   No
    ## 4  At least once a week At least once a week At least once a week  Yes
    ## 5                 Never                Never                 <NA>   No
    ## 6 Once or twice a month Several times a year                 <NA>   No
    ##                  q46                    q47                        q50
    ## 1 No, don<U+0092>t think so                   <NA>            Roman Catholic 
    ## 2 No, don<U+0092>t think so                   <NA>                Protestant 
    ## 3 No, don<U+0092>t think so                   <NA>     Nothing in particular 
    ## 4               <NA> Yes, there is conflict  Don<U+0092>t know/Refused (VOL) 
    ## 5 No, don<U+0092>t think so                   <NA>          Jewish (Judaism) 
    ## 6 No, don<U+0092>t think so                   <NA>     Nothing in particular 
    ##                        q50a      q50b          q51  q52
    ## 1                      <NA>      <NA>  Protestant  <NA>
    ## 2                      <NA>  Baptist          <NA> <NA>
    ## 3                      <NA>      <NA>         <NA> <NA>
    ## 4 Don<U+0092>t know/Refused (VOL.)      <NA>         <NA> <NA>
    ## 5                      <NA>      <NA>         <NA> <NA>
    ## 6                      <NA>      <NA>         <NA> <NA>
    ##                                         q53
    ## 1  Nondenominational or Independent Church 
    ## 2                                      <NA>
    ## 3                                      <NA>
    ## 4                                      <NA>
    ## 5                                      <NA>
    ## 6                                      <NA>
    ##                                                       children q55a q55b
    ## 1 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ## 2 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ## 3 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ## 4 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ## 5 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ## 6 No, not the parent or guardian of any children under 18 livi <NA> <NA>
    ##   q55c    sex age                     q60  q61
    ## 1 <NA> Female  43            Born in U.S. <NA>
    ## 2 <NA>   Male  61            Born in U.S. <NA>
    ## 3 <NA>   Male  58            Born in U.S. <NA>
    ## 4 <NA> Female  47            Born in U.S. <NA>
    ## 5 <NA>   Male  79            Born in U.S. <NA>
    ## 6 <NA> Female  74 Born in another country  Yes
    ##                             q63
    ## 1 Yes, father born outside U.S.
    ## 2 No, both parents born in U.S.
    ## 3 No, both parents born in U.S.
    ## 4 No, both parents born in U.S.
    ## 5 Yes, father born outside U.S.
    ## 6                          <NA>
    ##                                                           educ
    ## 1     Technical, trade, or vocational school AFTER high school
    ## 2           High school graduate (Grade 12 or GED certificate)
    ## 3        College graduate (B.S., B.A., or other 4-year degree)
    ## 4  Some college, no 4-year degree (including associate degree)
    ## 5 Post-graduate training or professional schooling after colle
    ## 6 Post-graduate training or professional schooling after colle
    ##                 income             regist           regicert       party
    ## 1 75 to under $100,000    Yes, registered Absolutely certain  Republican
    ## 2  20 to under $30,000 No, not registered               <NA>  Republican
    ## 3  30 to under $40,000 No, not registered               <NA> Independent
    ## 4    Less than $10,000 No, not registered               <NA> Independent
    ## 5  50 to under $75,000    Yes, registered Absolutely certain Independent
    ## 6  20 to under $30,000    Yes, registered Absolutely certain    Democrat
    ##    partyln         ideo                                  pvote04a
    ## 1     <NA>     Moderate                                     Voted
    ## 2     <NA> Conservative Did not vote (includes too young to vote)
    ## 3 Democrat Conservative Did not vote (includes too young to vote)
    ## 4 Democrat     Moderate Did not vote (includes too young to vote)
    ## 5 Democrat     Moderate                                     Voted
    ## 6     <NA> Very liberal                                     Voted
    ##          pvote04b
    ## 1            Bush
    ## 2            <NA>
    ## 3            <NA>
    ## 4            <NA>
    ## 5 Other candidate
    ## 6           Kerry

``` r
# Select 3 columns as a df
religion <- pew[c("q16", "reltrad", "income")]
head(religion)
```

    ##                       q16                           reltrad
    ## 1             Protestant   Evangelical Protestant Churches 
    ## 2             Protestant      Mainline Protestant Churches 
    ## 3             Protestant      Mainline Protestant Churches 
    ## 4  Nothing in particular                      Unaffiliated 
    ## 5       Jewish (Judaism)                            Jewish 
    ## 6       Jewish (Judaism)                            Jewish 
    ##                 income
    ## 1 75 to under $100,000
    ## 2  20 to under $30,000
    ## 3  30 to under $40,000
    ## 4    Less than $10,000
    ## 5  50 to under $75,000
    ## 6  20 to under $30,000

``` r
# Correct to character type
religion$reltrad <- as.character(religion$reltrad)

# Replace some of the values
head(religion$reltrad, 25)
```

    ##  [1] " Evangelical Protestant Churches "                             
    ##  [2] " Mainline Protestant Churches "                                
    ##  [3] " Mainline Protestant Churches "                                
    ##  [4] " Unaffiliated "                                                
    ##  [5] " Jewish "                                                      
    ##  [6] " Jewish "                                                      
    ##  [7] " Jewish "                                                      
    ##  [8] " Mainline Protestant Churches "                                
    ##  [9] " Mainline Protestant Churches "                                
    ## [10] " Don<U+0092>t know/refused (no information on religious affiliation) "
    ## [11] " Mainline Protestant Churches "                                
    ## [12] " Mainline Protestant Churches "                                
    ## [13] " Mainline Protestant Churches "                                
    ## [14] " Evangelical Protestant Churches "                             
    ## [15] " Evangelical Protestant Churches "                             
    ## [16] " Evangelical Protestant Churches "                             
    ## [17] " Evangelical Protestant Churches "                             
    ## [18] " Other Faiths "                                                
    ## [19] " Mainline Protestant Churches "                                
    ## [20] " Historically Black Protestant Churches "                      
    ## [21] " Mainline Protestant Churches "                                
    ## [22] " Evangelical Protestant Churches "                             
    ## [23] " Evangelical Protestant Churches "                             
    ## [24] " Mainline Protestant Churches "                                
    ## [25] " Unaffiliated "

``` r
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
head(religion$reltrad, 25)
```

    ##  [1] " Evangelical Protestant "                                           
    ##  [2] " Mainline Protestant "                                              
    ##  [3] " Mainline Protestant "                                              
    ##  [4] " Unaffiliated "                                                     
    ##  [5] " Jewish "                                                           
    ##  [6] " Jewish "                                                           
    ##  [7] " Jewish "                                                           
    ##  [8] " Mainline Protestant "                                              
    ##  [9] " Mainline Protestant "                                              
    ## [10] " Don<U+0092>t know/refused (no information on religious affiliation) "
    ## [11] " Mainline Protestant "                                              
    ## [12] " Mainline Protestant "                                              
    ## [13] " Mainline Protestant "                                              
    ## [14] " Evangelical Protestant "                                           
    ## [15] " Evangelical Protestant "                                           
    ## [16] " Evangelical Protestant "                                           
    ## [17] " Evangelical Protestant "                                           
    ## [18] " Other Faiths "                                                     
    ## [19] " Mainline Protestant "                                              
    ## [20] " Historically Black Protestant "                                    
    ## [21] " Mainline Protestant "                                              
    ## [22] " Evangelical Protestant "                                           
    ## [23] " Evangelical Protestant "                                           
    ## [24] " Mainline Protestant "                                              
    ## [25] " Unaffiliated "

``` r
# Clean up some of the labels
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

head(religion$reltrad, 25)
```

    ##  [1] "Evangelical Prot"        "Mainline Prot"          
    ##  [3] "Mainline Prot"           "Unaffiliated"           
    ##  [5] "Jewish"                  "Jewish"                 
    ##  [7] "Jewish"                  "Mainline Prot"          
    ##  [9] "Mainline Prot"           "Don<U+0092>t know/refused"
    ## [11] "Mainline Prot"           "Mainline Prot"          
    ## [13] "Mainline Prot"           "Evangelical Prot"       
    ## [15] "Evangelical Prot"        "Evangelical Prot"       
    ## [17] "Evangelical Prot"        "Other Faiths"           
    ## [19] "Mainline Prot"           "Historically Black Prot"
    ## [21] "Mainline Prot"           "Evangelical Prot"       
    ## [23] "Evangelical Prot"        "Mainline Prot"          
    ## [25] "Unaffiliated"

``` r
str(religion)
```

    ## 'data.frame':    35556 obs. of  3 variables:
    ##  $ q16    : Factor w/ 65 levels " Protestant ",..: 1 1 1 11 5 5 5 1 1 54 ...
    ##  $ reltrad: chr  "Evangelical Prot" "Mainline Prot" "Mainline Prot" "Unaffiliated" ...
    ##  $ income : Factor w/ 10 levels "Less than $10,000",..: 7 3 4 1 6 3 9 5 4 4 ...

``` r
religion$income <- c(`Less than $10,000` = "<$10k", `10 to under $20,000` = "$10--20k", 
  `20 to under $30,000` = "$20--30k", `30 to under $40,000` = "$30--40k", `40 to under $50,000` = "$40--50k", 
  `50 to under $75,000` = "$50--75k", `75 to under $100,000` = "$75--100k", `100 to under $150,000` = "$100--150k", 
  `$150,000 or more` = ">150k", `Don't know/Refused (VOL)` = "Don't know/refused")[religion$income]
head(religion$income)
```

    ## [1] "$75--100k" "$20--30k"  "$30--40k"  "<$10k"     "$50--75k"  "$20--30k"

``` r
# convert income values into factors
religion$income <- factor(religion$income, levels = c("<$10k", "$10--20k", "$20--30k", 
  "$30--40k", "$40--50k", "$50--75k", "$75--100k", "$100--150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

xtable(counts[1:10, ], file = "pew-clean.tex")
head(counts)
```

    ##   religion   income freq
    ## 1 Agnostic    <$10k   27
    ## 2 Agnostic $10--20k   34
    ## 3 Agnostic $20--30k   60
    ## 4 Agnostic $30--40k   81
    ## 5 Agnostic $40--50k   76
    ## 6 Agnostic $50--75k  137

``` r
# Convert into the form in which I originally saw it -------------------------

raw <- dcast(counts, religion ~ income)
```

    ## Using freq as value column: use value.var to override.

``` r
xtable(raw[1:10, 1:7], file = "pew-raw.tex")
head(raw)
```

    ##                    religion <$10k $10--20k $20--30k $30--40k $40--50k
    ## 1                  Agnostic    27       34       60       81       76
    ## 2                   Atheist    12       27       37       52       35
    ## 3                  Buddhist    27       21       30       34       33
    ## 4                  Catholic   418      617      732      670      638
    ## 5 Don<U+0092>t know/refused    15       14       15       11       10
    ## 6          Evangelical Prot   575      869     1064      982      881
    ##   $50--75k $75--100k $100--150k >150k Don't know/refused
    ## 1      137       122        109    84                 96
    ## 2       70        73         59    74                 76
    ## 3       58        62         39    53                 54
    ## 4     1116       949        792   633               1489
    ## 5       35        21         17    18                116
    ## 6     1486       949        723   414               1529

### 3.2. Multiple variables stored in one column

After melting, the column variable names often becomes a combination of multiple underlying variable names. This dataset comes from the World Health Organization, and records the counts of con???rmed tuberculosis cases by country, year, and demographic group. The demographic groups are broken down by sex (m, f) and age (0-14, 15-25, 25-34, 35-44, 45-54, 55-64, unknown).

``` r
####################################################################
## 3.2 Multiple variables stored in one column: TB dataset
####################################################################

# Load -----------------------------------------------------------------------
raw <- read.csv("tb.csv", na.strings = "")
head(raw)
```

    ##   iso2 year new_sp new_sp_m04 new_sp_m514 new_sp_m014 new_sp_m1524
    ## 1   AD 1989     NA         NA          NA          NA           NA
    ## 2   AD 1990     NA         NA          NA          NA           NA
    ## 3   AD 1991     NA         NA          NA          NA           NA
    ## 4   AD 1992     NA         NA          NA          NA           NA
    ## 5   AD 1993     15         NA          NA          NA           NA
    ## 6   AD 1994     24         NA          NA          NA           NA
    ##   new_sp_m2534 new_sp_m3544 new_sp_m4554 new_sp_m5564 new_sp_m65 new_sp_mu
    ## 1           NA           NA           NA           NA         NA        NA
    ## 2           NA           NA           NA           NA         NA        NA
    ## 3           NA           NA           NA           NA         NA        NA
    ## 4           NA           NA           NA           NA         NA        NA
    ## 5           NA           NA           NA           NA         NA        NA
    ## 6           NA           NA           NA           NA         NA        NA
    ##   new_sp_f04 new_sp_f514 new_sp_f014 new_sp_f1524 new_sp_f2534
    ## 1         NA          NA          NA           NA           NA
    ## 2         NA          NA          NA           NA           NA
    ## 3         NA          NA          NA           NA           NA
    ## 4         NA          NA          NA           NA           NA
    ## 5         NA          NA          NA           NA           NA
    ## 6         NA          NA          NA           NA           NA
    ##   new_sp_f3544 new_sp_f4554 new_sp_f5564 new_sp_f65 new_sp_fu
    ## 1           NA           NA           NA         NA        NA
    ## 2           NA           NA           NA         NA        NA
    ## 3           NA           NA           NA         NA        NA
    ## 4           NA           NA           NA         NA        NA
    ## 5           NA           NA           NA         NA        NA
    ## 6           NA           NA           NA         NA        NA

``` r
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)
names(raw)[1] <- "country"

names(raw) <- str_replace(names(raw), "new_sp_", "")
raw$m04 <- NULL
raw$m514 <- NULL
raw$f04 <- NULL
raw$f514 <- NULL

xtable(raw[1:10, 1:11], file = "tb-raw.tex")
head(raw)
```

    ##     country year m014 m1524 m2534 m3544 m4554 m5564 m65 mu f014 f1524
    ## 11       AD 2000    0     0     1     0     0     0   0 NA   NA    NA
    ## 37       AE 2000    2     4     4     6     5    12  10 NA    3    16
    ## 61       AF 2000   52   228   183   149   129    94  80 NA   93   414
    ## 88       AG 2000    0     0     0     0     0     0   1 NA    1     1
    ## 137      AL 2000    2    19    21    14    24    19  16 NA    3    11
    ## 166      AM 2000    2   152   130   131    63    26  21 NA    1    24
    ##     f2534 f3544 f4554 f5564 f65 fu
    ## 11     NA    NA    NA    NA  NA NA
    ## 37      1     3     0     0   4 NA
    ## 61    565   339   205    99  36 NA
    ## 88      1     0     0     0   0 NA
    ## 137    10     8     8     5  11 NA
    ## 166    27    24     8     8   4 NA

``` r
# Melt -----------------------------------------------------------------------

clean <- melt(raw, id = c("country", "year"), na.rm = TRUE)
names(clean)[3] <- "column"
names(clean)[4] <- "cases"
clean <- arrange(clean, country, column, year)
head(clean)
```

    ##   country year column cases
    ## 1      AD 2000   m014     0
    ## 2      AD 2000  m1524     0
    ## 3      AD 2000  m2534     1
    ## 4      AD 2000  m3544     0
    ## 5      AD 2000  m4554     0
    ## 6      AD 2000  m5564     0

str\_sub will recycle all arguments to be the same length as the longest argument. If any arguments are of length 0, the output will be a zero length character vector.

``` r
xtable(clean[1:15, ], file = "tb-clean-1.tex")

# Break up variable in to sex and age ----------------------------------------

clean$sex <- str_sub(clean$column, 1, 1)

ages <- c(`04` = "0--4", `514` = "5--14", `014` = "0--14", `1524` = "15--24", `2534` = "25--34", 
  `3544` = "35--44", `4554` = "45--54", `5564` = "55--64", `65` = "65+", u = NA)

clean$age <- factor(ages[str_sub(clean$column, 2)], levels = ages)

clean <- clean[c("country", "year", "sex", "age", "cases")]

xtable(clean[1:15, ], file = "tb-clean-2.tex")
head(clean)
```

    ##   country year sex    age cases
    ## 1      AD 2000   m  0--14     0
    ## 2      AD 2000   m 15--24     0
    ## 3      AD 2000   m 25--34     1
    ## 4      AD 2000   m 35--44     0
    ## 5      AD 2000   m 45--54     0
    ## 6      AD 2000   m 55--64     0

### 3.3 Variables are stored in both rows and columns

The most complicated form of messy data occurs when variables are stored in both rows and columns. Table 11 shows daily weather data from the Global Historical Climatology Network for one weather station (MX17004) in Mexico for ???ve months in 2010. It has variables in individual columns (id, year, month), spread across columns (day, d1-d31) and across rows (tmin, tmax) (minimum and maximum temperature). Months with less than 31 days have structural missing values for the last day(s) of the month. The element column is not a variable; it stores the names of variables. To tidy this dataset we ???rst melt it with colvars id, year, month and the column that contains variable names, element.

``` r
####################################################################
## 3.3 Variables are stored in both rows and columns: weather dataset
####################################################################

# Define format for fixed width file
cols <- data.frame(name = c("id", "year", "month", "element"), start = c(1, 12, 16, 
  18), end = c(11, 15, 17, 21))

head(cols)
```

    ##      name start end
    ## 1      id     1  11
    ## 2    year    12  15
    ## 3   month    16  17
    ## 4 element    18  21

``` r
names <- str_c(c("value", "mflag", "qflag", "sflag"), rep(1:31, each = 4), sep = "_")
starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
starts <- starts[-length(starts)]
ends <- c(starts[-1], starts[length(starts)] + 1) - 1

values <- data.frame(name = names, start = starts, end = ends)
cols <- rbind(cols, values)

# Load data and subset to small example
raw <- read.fwf2("weather.txt", cols)
raw <- subset(raw, year == 2010 & element %in% c("TMIN", "TMAX"))
raw <- raw[, c(1:4, which(str_detect(names(raw), "value")))]
raw$id <- str_c(str_sub(raw$id, 1, 2), str_sub(raw$id, -5, -1))

names(raw)[-(1:4)] <- str_c("d", 1:31)
raw[raw == -9999] <- NA
raw[-(1:4)] <- raw[-(1:4)]/10
rownames(raw) <- NULL
raw$element <- tolower(raw$element)

xtable(raw[1:10, 1:12], file = "weather-raw.tex", digits = 1)
head(raw)
```

    ##        id year month element d1   d2   d3 d4   d5 d6 d7 d8 d9  d10  d11
    ## 1 MX17004 2010     1    tmax NA   NA   NA NA   NA NA NA NA NA   NA   NA
    ## 2 MX17004 2010     1    tmin NA   NA   NA NA   NA NA NA NA NA   NA   NA
    ## 3 MX17004 2010     2    tmax NA 27.3 24.1 NA   NA NA NA NA NA   NA 29.7
    ## 4 MX17004 2010     2    tmin NA 14.4 14.4 NA   NA NA NA NA NA   NA 13.4
    ## 5 MX17004 2010     3    tmax NA   NA   NA NA 32.1 NA NA NA NA 34.5   NA
    ## 6 MX17004 2010     3    tmin NA   NA   NA NA 14.2 NA NA NA NA 16.8   NA
    ##   d12 d13 d14 d15  d16 d17 d18 d19 d20 d21 d22  d23 d24 d25 d26 d27 d28
    ## 1  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA
    ## 2  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA
    ## 3  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA 29.9  NA  NA  NA  NA  NA
    ## 4  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA  NA 10.7  NA  NA  NA  NA  NA
    ## 5  NA  NA  NA  NA 31.1  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA
    ## 6  NA  NA  NA  NA 17.6  NA  NA  NA  NA  NA  NA   NA  NA  NA  NA  NA  NA
    ##   d29  d30 d31
    ## 1  NA 27.8  NA
    ## 2  NA 14.5  NA
    ## 3  NA   NA  NA
    ## 4  NA   NA  NA
    ## 5  NA   NA  NA
    ## 6  NA   NA  NA

``` r
# Melt and tidy

clean1 <- melt(raw, id = 1:4, na.rm = TRUE)
clean1$day <- as.integer(str_replace(clean1$variable, "d", ""))
clean1$date <- as.Date(ISOdate(clean1$year, clean1$month, clean1$day))

clean1 <- clean1[c("id", "date", "element", "value")]
clean1 <- arrange(clean1, date, element)
clean1$date <- as.character(clean1$date)  # work around xtable bug
xtable(clean1[1:10, ], file = "weather-clean-1.tex", digits = 1)
head(clean1)
```

    ##        id       date element value
    ## 1 MX17004 2010-01-30    tmax  27.8
    ## 2 MX17004 2010-01-30    tmin  14.5
    ## 3 MX17004 2010-02-02    tmax  27.3
    ## 4 MX17004 2010-02-02    tmin  14.4
    ## 5 MX17004 2010-02-03    tmax  24.1
    ## 6 MX17004 2010-02-03    tmin  14.4

To arrange by element, cast a molten df into an array or df. Fixing the issue with the type of observation requires the cast, or unstack, operation. This performs the inverse of melting by rotating the element variable back out into the columns

Below is a tidy weather dataset. Each row represents the meteorological measurements for a single day. There are two measured variables, minimum (tmin) and maximum (tmax) temperature; all other variables are ???xed.

``` r
# Cast

clean2 <- dcast(clean1, ... ~ element)
xtable(clean2[1:10, ], file = "weather-clean-2.tex", digits = 1)

head(clean2)
```

    ##        id       date tmax tmin
    ## 1 MX17004 2010-01-30 27.8 14.5
    ## 2 MX17004 2010-02-02 27.3 14.4
    ## 3 MX17004 2010-02-03 24.1 14.4
    ## 4 MX17004 2010-02-11 29.7 13.4
    ## 5 MX17004 2010-02-23 29.9 10.7
    ## 6 MX17004 2010-03-05 32.1 14.2

### 3.4 Multiple Types in one table

Datasets often involve values collected at multiple levels, on di???erent types of observational units. During tidying, each type of observational unit should be stored in its own table. This is closely related to the idea of database normalization, where each fact is expressed in only one place. If this is not done, it is possible for inconsistencies to occur.

The Billboard dataset described below actually contains observations on two types of observational units: the song and its rank in each week. This manifests itself through the duplication of facts about the song: artist and time are repeated for every song in each week.

``` r
####################################################################
## 3.4 Multiple types in one table: Billboard dataset
####################################################################

library("lubridate")

raw <- read.csv("billboard.csv")

raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", 
  "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", 
  "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", 
  "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", 
  "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", 
  "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", 
  "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", 
  "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", 
  "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", 
  "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", 
  "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", 
  "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", 
  "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", 
  "x74th.week", "x75th.week", "x76th.week")]

names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

long_name <- nchar(raw$track) > 20
raw$track[long_name] <- paste0(substr(raw$track[long_name], 0, 20), "...")


xtable(raw[c(1:3, 6:10), 1:8], "billboard-raw.tex")

head(raw,25)
```

    ##    year               artist                   track time date.entered wk1
    ## 1  2000                2 Pac          Baby Don't Cry 4:22   2000-02-26  87
    ## 2  2000              2Ge+her The Hardest Part Of ... 3:15   2000-09-02  91
    ## 3  2000         3 Doors Down              Kryptonite 3:53   2000-04-08  81
    ## 4  2000         3 Doors Down                   Loser 4:24   2000-10-21  76
    ## 5  2000             504 Boyz           Wobble Wobble 3:35   2000-04-15  57
    ## 6  2000                  98? Give Me Just One Nig... 3:24   2000-08-19  51
    ## 7  2000              A*Teens           Dancing Queen 3:44   2000-07-08  97
    ## 8  2000              Aaliyah           I Don't Wanna 4:15   2000-01-29  84
    ## 9  2000              Aaliyah               Try Again 4:03   2000-03-18  59
    ## 10 2000       Adams, Yolanda           Open My Heart 5:30   2000-08-26  76
    ## 11 2000        Adkins, Trace                    More 3:05   2000-04-29  84
    ## 12 2000  Aguilera, Christina       Come On Over Baby 3:38   2000-08-05  57
    ## 13 2000  Aguilera, Christina           I Turn To You 4:00   2000-04-15  50
    ## 14 2000  Aguilera, Christina       What A Girl Wants 3:18   1999-11-27  71
    ## 15 2000         Alice Deejay        Better Off Alone 6:50   2000-04-08  79
    ## 16 2000          Allan, Gary Smoke Rings In The D... 4:18   2000-01-22  80
    ## 17 2000                Amber                  Sexual 4:38   1999-07-17  99
    ## 18 2000            Anastacia          I'm Outta Love 4:01   2000-04-01  92
    ## 19 2000        Anthony, Marc             My Baby You 3:59   2000-09-16  82
    ## 20 2000        Anthony, Marc          You Sang To Me 3:50   2000-02-26  77
    ## 21 2000                Avant           My First Love 4:28   2000-11-04  70
    ## 22 2000                Avant               Separated 4:13   2000-04-29  62
    ## 23 2000 Backstreet Boys, The       Shape Of My Heart 3:49   2000-10-14  39
    ## 24 2000 Backstreet Boys, The Show Me The Meaning ... 3:54   2000-01-01  74
    ## 25 2000 Backstreet Boys, The                 The One 3:46   2000-05-27  58
    ##    wk2 wk3 wk4 wk5 wk6 wk7 wk8 wk9 wk10 wk11 wk12 wk13 wk14 wk15 wk16 wk17
    ## 1   82  72  77  87  94  99  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 2   87  92  NA  NA  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 3   70  68  67  66  57  54  53  51   51   51   51   47   44   38   28   22
    ## 4   76  72  69  67  65  55  59  62   61   61   59   61   66   72   76   75
    ## 5   34  25  17  17  31  36  49  53   57   64   70   75   76   78   85   92
    ## 6   39  34  26  26  19   2   2   3    6    7   22   29   36   47   67   66
    ## 7   97  96  95 100  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 8   62  51  41  38  35  35  38  38   36   37   37   38   49   61   63   62
    ## 9   53  38  28  21  18  16  14  12   10    9    8    6    1    2    2    2
    ## 10  76  74  69  68  67  61  58  57   59   66   68   61   67   59   63   67
    ## 11  84  75  73  73  69  68  65  73   83   92   NA   NA   NA   NA   NA   NA
    ## 12  47  45  29  23  18  11   9   9   11    1    1    1    1    4    8   12
    ## 13  39  30  28  21  19  20  17  17   17   17    3    3    7   10   17   25
    ## 14  51  28  18  13  13  11   1   1    2    2    3    3    4   12   11   13
    ## 15  65  53  48  45  36  34  29  27   30   36   37   39   49   57   63   65
    ## 16  78  76  77  92  NA  NA  NA  NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 17  99  96  96 100  93  93  96  NA   NA   99   NA   96   96   99   98   98
    ## 18  NA  NA  95  NA  NA  NA  NA  NA   NA   NA   97   NA   NA   NA   NA   NA
    ## 19  76  76  70  82  81  74  80  76   76   73   74   87   83   89   93   94
    ## 20  54  50  43  30  27  21  18  15   13   13   13   13    5    2    2    5
    ## 21  62  56  43  39  33  26  26  26   31   32   31   38   38   52   57   64
    ## 22  32  30  23  26  30  35  32  32   25   23   28   27   29   27   31   31
    ## 23  25  24  15  12  12  10   9  10   12   17   19   29   26   24   31   52
    ## 24  62  55  25  16  14  12  10  12    9    7    6    6    6    8    9   13
    ## 25  50  43  37  31  30  39  47  55   61   76   90   93   93  100   NA   NA
    ##    wk18 wk19 wk20 wk21 wk22 wk23 wk24 wk25 wk26 wk27 wk28 wk29 wk30 wk31
    ## 1    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 2    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 3    18   18   14   12    7    6    6    6    5    5    4    4    4    4
    ## 4    67   73   70   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 5    96   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 6    84   93   94   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 7    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 8    67   83   86   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 9     2    3    4    5    5    6    9   13   14   16   23   22   33   36
    ## 10   71   79   89   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 11   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 12   22   23   43   44   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 13   29   29   40   43   50   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 14   15   18   20   30   40   39   44   NA   NA   NA   NA   NA   NA   NA
    ## 15   68   79   86   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 16   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 17   NA   95   88   88   79   76   69   69   59   58   58   49   44   42
    ## 18   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 19   94   91   90   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 20    7    9   12   12   16   20   20   22   25   26   29   35   33   40
    ## 21   73   81   79   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 22   33   40   51   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 23   63   62   70   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 24   19   23   26   32   34   45   47   NA   NA   NA   NA   NA   NA   NA
    ## 25   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ##    wk32 wk33 wk34 wk35 wk36 wk37 wk38 wk39 wk40 wk41 wk42 wk43 wk44 wk45
    ## 1    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 2    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 3     3    3    3    4    5    5    9    9   15   14   13   14   16   17
    ## 4    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 5    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 6    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 7    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 8    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 9    43   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 10   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 11   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 12   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 13   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 14   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 15   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 16   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 17   46   50   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 18   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 19   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 20   44   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 21   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 22   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 23   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 24   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 25   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ##    wk46 wk47 wk48 wk49 wk50 wk51 wk52 wk53 wk54 wk55 wk56 wk57 wk58 wk59
    ## 1    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 2    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 3    21   22   24   28   33   42   42   49   NA   NA   NA   NA   NA   NA
    ## 4    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 5    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 6    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 7    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 8    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 9    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 10   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 11   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 12   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 13   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 14   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 15   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 16   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 17   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 18   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 19   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 20   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 21   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 22   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 23   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 24   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 25   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ##    wk60 wk61 wk62 wk63 wk64 wk65 wk66 wk67 wk68 wk69 wk70 wk71 wk72 wk73
    ## 1    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 2    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 3    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 4    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 5    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 6    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 7    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 8    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 9    NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 10   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 11   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 12   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 13   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 14   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 15   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 16   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 17   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 18   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 19   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 20   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 21   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 22   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 23   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 24   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ## 25   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA
    ##    wk74 wk75 wk76
    ## 1    NA   NA   NA
    ## 2    NA   NA   NA
    ## 3    NA   NA   NA
    ## 4    NA   NA   NA
    ## 5    NA   NA   NA
    ## 6    NA   NA   NA
    ## 7    NA   NA   NA
    ## 8    NA   NA   NA
    ## 9    NA   NA   NA
    ## 10   NA   NA   NA
    ## 11   NA   NA   NA
    ## 12   NA   NA   NA
    ## 13   NA   NA   NA
    ## 14   NA   NA   NA
    ## 15   NA   NA   NA
    ## 16   NA   NA   NA
    ## 17   NA   NA   NA
    ## 18   NA   NA   NA
    ## 19   NA   NA   NA
    ## 20   NA   NA   NA
    ## 21   NA   NA   NA
    ## 22   NA   NA   NA
    ## 23   NA   NA   NA
    ## 24   NA   NA   NA
    ## 25   NA   NA   NA

The Billboard dataset needs to be broken down into two datasets: a song dataset which stores artist, song name and time, and a ranking dataset which gives the rank of the song in each week.

``` r
clean <- melt(raw, id = 1:5, na.rm = TRUE)
clean$week <- as.integer(str_replace_all(clean$variable, "[^0-9]+", ""))
clean$variable <- NULL

clean$date.entered <- ymd(clean$date.entered)
clean$date <- clean$date.entered + weeks(clean$week - 1)
clean$date.entered <- NULL
clean <- rename(clean, c(value = "rank"))
clean <- arrange(clean, year, artist, track, time, week)
clean <- clean[c("year", "artist", "time", "track", "date", "week", "rank")]

clean_out <- mutate(clean, date = as.character(date))
xtable(clean_out[1:15, ], "billboard-clean.tex")
head(clean, 25)
```

    ##    year       artist time                   track       date week rank
    ## 1  2000        2 Pac 4:22          Baby Don't Cry 2000-02-26    1   87
    ## 2  2000        2 Pac 4:22          Baby Don't Cry 2000-03-04    2   82
    ## 3  2000        2 Pac 4:22          Baby Don't Cry 2000-03-11    3   72
    ## 4  2000        2 Pac 4:22          Baby Don't Cry 2000-03-18    4   77
    ## 5  2000        2 Pac 4:22          Baby Don't Cry 2000-03-25    5   87
    ## 6  2000        2 Pac 4:22          Baby Don't Cry 2000-04-01    6   94
    ## 7  2000        2 Pac 4:22          Baby Don't Cry 2000-04-08    7   99
    ## 8  2000      2Ge+her 3:15 The Hardest Part Of ... 2000-09-02    1   91
    ## 9  2000      2Ge+her 3:15 The Hardest Part Of ... 2000-09-09    2   87
    ## 10 2000      2Ge+her 3:15 The Hardest Part Of ... 2000-09-16    3   92
    ## 11 2000 3 Doors Down 3:53              Kryptonite 2000-04-08    1   81
    ## 12 2000 3 Doors Down 3:53              Kryptonite 2000-04-15    2   70
    ## 13 2000 3 Doors Down 3:53              Kryptonite 2000-04-22    3   68
    ## 14 2000 3 Doors Down 3:53              Kryptonite 2000-04-29    4   67
    ## 15 2000 3 Doors Down 3:53              Kryptonite 2000-05-06    5   66
    ## 16 2000 3 Doors Down 3:53              Kryptonite 2000-05-13    6   57
    ## 17 2000 3 Doors Down 3:53              Kryptonite 2000-05-20    7   54
    ## 18 2000 3 Doors Down 3:53              Kryptonite 2000-05-27    8   53
    ## 19 2000 3 Doors Down 3:53              Kryptonite 2000-06-03    9   51
    ## 20 2000 3 Doors Down 3:53              Kryptonite 2000-06-10   10   51
    ## 21 2000 3 Doors Down 3:53              Kryptonite 2000-06-17   11   51
    ## 22 2000 3 Doors Down 3:53              Kryptonite 2000-06-24   12   51
    ## 23 2000 3 Doors Down 3:53              Kryptonite 2000-07-01   13   47
    ## 24 2000 3 Doors Down 3:53              Kryptonite 2000-07-08   14   44
    ## 25 2000 3 Doors Down 3:53              Kryptonite 2000-07-15   15   38

Normalization is useful for tidying and eliminating inconsistencies. However, there are few data analysis tools that work directly with relational data, so analysis usually also requires denormalization or merging the datasets back into one table.

``` r
# Normalization --------------------------------------------------------------

song <- unrowname(unique(clean[c("artist", "track", "time")]))
song$id <- 1:nrow(song)

narrow <- song[1:15, c("id", "artist", "track", "time")]
xtable(narrow, "billboard-song.tex")

rank <- join(clean, song, match = "first")
```

    ## Joining by: artist, time, track

``` r
rank <- rank[c("id", "date", "rank")]
rank$date <- as.character(rank$date)
xtable(rank[1:15, ], "billboard-rank.tex")
head(rank,25)
```

    ##    id       date rank
    ## 1   1 2000-02-26   87
    ## 2   1 2000-03-04   82
    ## 3   1 2000-03-11   72
    ## 4   1 2000-03-18   77
    ## 5   1 2000-03-25   87
    ## 6   1 2000-04-01   94
    ## 7   1 2000-04-08   99
    ## 8   2 2000-09-02   91
    ## 9   2 2000-09-09   87
    ## 10  2 2000-09-16   92
    ## 11  3 2000-04-08   81
    ## 12  3 2000-04-15   70
    ## 13  3 2000-04-22   68
    ## 14  3 2000-04-29   67
    ## 15  3 2000-05-06   66
    ## 16  3 2000-05-13   57
    ## 17  3 2000-05-20   54
    ## 18  3 2000-05-27   53
    ## 19  3 2000-06-03   51
    ## 20  3 2000-06-10   51
    ## 21  3 2000-06-17   51
    ## 22  3 2000-06-24   51
    ## 23  3 2000-07-01   47
    ## 24  3 2000-07-08   44
    ## 25  3 2000-07-15   38

### Case Study: Morality Data from Mexico

The following case study illustrates how tidy data and tidy tools make data analysis easier by easing the transitions between manipulation, visualization and modeling. You will not see any code that exists solely to get the output of one function into the right format to input to another. I will show the R code that performs the analysis, but even if you are not familiar with R or the exact idioms I use, I have tried to make it easy to understand by tightly interleaving code, results and explanation. The case study uses individual-level mortality data from Mexico. The goal is to ???nd causes of death with unusual temporal patterns within a day. Figure 1 shows the temporal pattern, the number of deaths per hour, for all causes of death. My goal is to ???nd the diseases that di???er most from this pattern. The full dataset has information on 539,530 deaths in Mexico in 2008 and 55 variables, including the location and time of death, the cause of death, and demographics of the deceased. Table 15 shows a small sample of the dataset, focusing on variables related to time of death (year, month, day and hour), and cause of death (cod).

``` r
####################################################################
## 5. Case study
####################################################################

library("ggplot2")
```

    ## Warning: package 'ggplot2' was built under R version 3.4.2

``` r
library("MASS")

if (!file.exists("deaths.rds")) {
  ## from https://github.com/hadley/mexico-mortality/raw/master/deaths/deaths08.csv.bz2
  deaths <- read.csv("deaths08.csv.bz2")
  unlink("deaths08.csv.bz2")
  deaths$hod[deaths$hod == 99] <- NA
  deaths$hod[deaths$hod == 24] <- 0
  deaths$hod[deaths$hod == 0] <- NA
  deaths$hod <- as.integer(deaths$hod)
  deaths <- arrange(deaths, yod, mod, dod, hod, cod)
  deaths <- deaths[c("yod", "mod", "dod", "hod", "cod")]  
  saveRDS(deaths, "deaths.rds")
}

deaths <- readRDS("deaths.rds")
head(deaths, 25)
```

    ##    yod mod dod hod cod
    ## 1    0   0   0  NA E14
    ## 2    0   0   0  NA E46
    ## 3    0   0   0  NA I21
    ## 4    0   0   0  NA K70
    ## 5    0   0   0  NA P21
    ## 6    0   0   0  NA Q89
    ## 7    0   0   0  NA R99
    ## 8    0   0   0  NA R99
    ## 9    0   0   0  NA R99
    ## 10   0   0   0  NA R99
    ## 11   0   0   0  NA R99
    ## 12   0   0   0  NA R99
    ## 13   0   0   0  NA R99
    ## 14   0   0   0  NA R99
    ## 15   0   0   0  NA R99
    ## 16   0   0   0  NA R99
    ## 17   0   0   0  NA R99
    ## 18   0   0   0  NA R99
    ## 19   0   0   0  NA V09
    ## 20   0   0   0  NA W34
    ## 21   0   0   0  NA W34
    ## 22   0   0   0  NA X59
    ## 23   0   0   0  NA X59
    ## 24   0   0   0  NA X59
    ## 25   0   0   0  NA X59

``` r
ok <- subset(deaths, yod == 2008 & mod != 0 & dod != 0)
xtable(ok[c(1, 1:14 * 2000), c("yod", "mod", "dod", "hod", "cod")], "raw.tex")
head(ok)
```

    ##        yod mod dod hod cod
    ## 11938 2008   1   1   1 B20
    ## 11939 2008   1   1   1 B22
    ## 11940 2008   1   1   1 C18
    ## 11941 2008   1   1   1 C34
    ## 11942 2008   1   1   1 C50
    ## 11943 2008   1   1   1 C50

``` r
codes <- read.csv("icd-main.csv")
codes$disease <- sapply(codes$disease, function(x) str_c(strwrap(x, width = 30), 
  collapse = "\n"))
names(codes)[1] <- "cod"
codes <- codes[!duplicated(codes$cod), ]

# Display overall hourly deaths
hod_all <- subset(count(deaths, "hod"), !is.na(hod))
qplot(hod, freq, data = hod_all, geom = "line") + scale_y_continuous("Number of deaths", 
  labels = function(x) format(x, big.mark = ",")) + xlab("Hour of day")
```

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-28-1.png" style="display: block; margin: auto;" />

``` r
ggsave("overall.pdf", width = 10, height = 6)
```

To achieve our goal of ???nding unusual temporal patterns, we do the following. First, we count the number of deaths in each hour (hod) for each cause (cod) with the tidy count function.

Then we remove missing (and hence uninformative for our purpose) values with subset.

To provide informative labels for disease, we next join the dataset to the codes dataset, connected by the cod variable. This adds a new variable, disease.

``` r
# Count deaths per hour, per disease
hod2 <- count(deaths, c("cod", "hod"))
hod2 <- subset(hod2, !is.na(hod))
hod2 <- join(hod2, codes)
```

    ## Joining by: cod

``` r
hod2 <- ddply(hod2, "cod", transform, prop = freq/sum(freq))

# Compare to overall abundance
overall <- ddply(hod2, "hod", summarise, freq_all = sum(freq))
overall <- mutate(overall, prop_all = freq_all/sum(freq_all))

hod2 <- join(overall, hod2, by = "hod")

# Pick better subset of rows to show
cods <- join(arrange(count(deaths, "cod"), desc(freq)), codes)
```

    ## Joining by: cod

``` r
mutate(tail(subset(cods, freq > 100), 30), disease = str_sub(disease, 1, 30))
```

    ##     cod freq                         disease
    ## 321 V04  119 Pedestrian injured in\ncollisio
    ## 322 Q23  118 Congenital malformations of\nao
    ## 323 C51  117     Malignant neoplasm of vulva
    ## 324 P91  116 Other disturbances of\ncerebral
    ## 325 X44  116 Accidental poisoning by and\nex
    ## 326 J68  115 Respiratory conditions due to\n
    ## 327 D35  114 Benign neoplasm of other and\nu
    ## 328 Y57  114 Adverse effects in\ntherapeutic
    ## 329 I83  113 Varicose veins of lower\nextrem
    ## 330 J06  113 Acute upper respiratory\ninfect
    ## 331 E84  112                 Cystic fibrosis
    ## 332 Q22  112 Congenital malformations of\npu
    ## 333 I72  111                  Other aneurysm
    ## 334 V05  108 Pedestrian injured in\ncollisio
    ## 335 X94  108 Assault (homicide) by rifle,\ns
    ## 336 O14  107 Gestational\n[pregnancy-induced
    ## 337 R64  107                        Cachexia
    ## 338 B16  106               Acute hepatitis B
    ## 339 J85  106 Abscess of lung and\nmediastinu
    ## 340 K86  106      Other diseases of pancreas
    ## 341 V19  105 Pedal cyclist injured in\nother
    ## 342 W26  105 Contact with knife, sword, or\n
    ## 343 P25  104 Interstitial emphysema and\nrel
    ## 344 C40  103 Malignant neoplasm of bone\nand
    ## 345 P00  103 Newborn affected by maternal\nc
    ## 346 X33  103             Victim of lightning
    ## 347 J04  102 Acute laryngitis and\ntracheiti
    ## 348 C07  101 Malignant neoplasm of parotid\n
    ## 349 M13  101                 Other arthritis
    ## 350 N04  101              Nephrotic syndrome

``` r
hod3 <- subset(hod2, cod %in% c("I21", "N18", "E84", "B16") & hod >= 8 & hod <= 12)[1:15, 
  c("hod", "cod", "disease", "freq", "prop", "freq_all", "prop_all")]

xtable(hod3[c("hod", "cod", "freq")], "counts.tex")
xtable(hod3[c("disease")], "counts-disease.tex")
xtable(hod3[5], "counts-prop.tex")
xtable(hod3[6:7], "counts-all.tex")

devi <- ddply(hod2, "cod", summarise, n = sum(freq), dist = mean((prop - prop_all)^2))
devi <- subset(devi, n > 50)

# Find outliers
xlog10 <- scale_x_log10(breaks = c(100, 1000, 10000), labels = c(100, 1000, 10000), 
  minor_breaks = log10(outer(1:9, 10^(1:5), "*")))
ylog10 <- scale_y_log10(breaks = 10^-c(3, 4, 5), labels = c("0.001", "0.0001", "0.00001"), 
  minor_breaks = log10(outer(1:9, 10^-(3:6), "*")))

qplot(n, dist, data = devi)
```

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-1.png" style="display: block; margin: auto;" />

``` r
ggsave("n-dist-raw.pdf", width = 6, height = 6)
qplot(n, dist, data = devi) + geom_smooth(method = "rlm", se = FALSE) + xlog10 + ylog10
```

    ## Warning in self$trans$transform(self$minor_breaks): NaNs produced

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-2.png" style="display: block; margin: auto;" />

``` r
ggsave("n-dist-log.pdf", width = 6, height = 6)
```

    ## Warning in self$trans$transform(self$minor_breaks): NaNs produced

``` r
devi$resid <- resid(rlm(log(dist) ~ log(n), data = devi))
coef(rlm(log(dist) ~ log(n), data = devi))
```

    ## (Intercept)      log(n) 
    ##  -3.6471353  -0.8970054

``` r
ggplot(devi, aes(n, resid)) + geom_hline(yintercept = 1.5, colour = "grey50") + geom_point() + 
  xlog10
```

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-3.png" style="display: block; margin: auto;" />

``` r
ggsave("n-dist-resid.pdf", width = 6, height = 6)

unusual <- subset(devi, resid > 1.5)
hod_unusual_big <- match_df(hod2, subset(unusual, n > 350))
```

    ## Matching on: cod

``` r
hod_unusual_sml <- match_df(hod2, subset(unusual, n <= 350))
```

    ## Matching on: cod

``` r
# Visualize unusual causes of death
ggplot(hod_unusual_big, aes(hod, prop)) + geom_line(aes(y = prop_all), data = overall, 
  colour = "grey50") + geom_line() + facet_wrap(~disease, ncol = 3)
```

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-4.png" style="display: block; margin: auto;" />

``` r
ggsave("unusual-big.pdf", width = 8, height = 6)
last_plot() %+% hod_unusual_sml
```

<img src="tidy-data-tutorial_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-29-5.png" style="display: block; margin: auto;" />

``` r
ggsave("unusual-sml.pdf", width = 8, height = 4) 
```
