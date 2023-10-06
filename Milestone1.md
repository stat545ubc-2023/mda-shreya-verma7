---
Title: "Data Analysis Milestone 1" 
Author: "Shreya Verma"
output: 
  html_document:
    keep_md: true
---

## Overview

The report included the Exploratory Data Analysis, that was performed on a data set chosen from the `datateacher` package. Over the course of the report, you will be familiarized with different types of data set. The final data set will be used to formulate four research questions

## Setting up

First we need to load the following two packages:

``` r
# Installing packages if missing, required only once
# install.packages("devtools")
# devtools::install_github("UBC-MDS/datateachr")

library(datateachr)
library(tidyverse)
```

```         
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.3     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

## Data set

The `datateachr` is comprised of the 7 semi-tidy data sets:

**Tip** : help(package = "datateachr") helps us understand the package and it's function!

> 1.  apt_buildings: Acquired courtesy of The City of Toronto's Open Data Portal. It currently has 3455 rows and 37 columns.

> 2.  building_permits: Acquired courtesy of The City of Vancouver's Open Data Portal. It currently has 20680 rows and 14 columns.

> 3.  cancer_sample: Acquired courtesy of UCI Machine Learning Repository. It currently has 569 rows and 32 columns.

> 4.  flow_sample: Acquired courtesy of The Government of Canada's Historical Hydrometric Database. It currently has 218 rows and 7 columns.

> 5.  parking_meters: Acquired courtesy of The City of Vancouver's Open Data Portal. It currently has 10032 rows and 22 columns.

> 6.  steam_games: Acquire d courtesy of Kaggle. It currently has 40833 rows and 21 columns.

> 7.  vancouver_trees: Acquired courtesy of The City of Vancouver's Open Data Portal. It currently has 146611 rows and 20 columns.

## Task 1.1: Preliminary Data set Selection

All the 7 data sets look very interesting, but the ones that I am specifically intrigued in look are:

1: *building_permits:* Vancouver rent is expensive! My building manager always gives me a reply that "it is hard to get building permits", so I want to understand what is the whole deal about it!

2: *cancer_sample:* I am a cancer survivor and this research topic is very close to my heart. I have previously worked on early detection of skin care.

3:*parking_meters:* Just learning how to drive! Parking meters sure do pay a very important part in Vancouver. I want to find parking spots that are mostly free.

4: *vancouver_trees:* Fall is here! It will be really cool if I can see the species of trees on different streets. Also, curious to see how city plans planting the trees.

## Task 1.2: Introductory Data set Exploration

In this section, we will individually perform introductory exploration into the four chosen data sets using dyplr to find the associated data attributes. The goal is to understand the information contained within each data set and enable a more informed decision.

#### Building Permits

``` r
# Checking the object type of data frame 
class(building_permits)
```

```         
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

``` r
# Let us now see the dimensions, columns and data types
glimpse(building_permits)
```

```         
## Rows: 20,680
## Columns: 14
## $ permit_number               <chr> "BP-2016-02248", "BU468090", "DB-2016-0445…
## $ issue_date                  <date> 2017-02-01, 2017-02-01, 2017-02-01, 2017-…
## $ project_value               <dbl> 0, 0, 35000, 15000, 181178, 0, 15000, 0, 6…
## $ type_of_work                <chr> "Salvage and Abatement", "New Building", "…
## $ address                     <chr> "4378 W 9TH AVENUE, Vancouver, BC V6R 2C7"…
## $ project_description         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ building_contractor         <chr> NA, NA, NA, "Mercury Contracting Ltd", "08…
## $ building_contractor_address <chr> NA, NA, NA, "88 W PENDER ST  \r\nUnit 2069…
## $ applicant                   <chr> "Raffaele & Associates DBA: Raffaele and A…
## $ applicant_address           <chr> "2642 East Hastings\r\nVancouver, BC  V5K …
## $ property_use                <chr> "Dwelling Uses", "Dwelling Uses", "Dwellin…
## $ specific_use_category       <chr> "One-Family Dwelling", "Multiple Dwelling"…
## $ year                        <dbl> 2017, 2017, 2017, 2017, 2017, 2017, 2017, …
## $ bi_id                       <dbl> 524, 535, 539, 541, 543, 546, 547, 548, 54…
```

``` r
#The newly learnt summary function to help "summarize" the data
summary(building_permits)
```

```         
##  permit_number        issue_date         project_value       type_of_work      
##  Length:20680       Min.   :2017-01-03   Min.   :        0   Length:20680      
##  Class :character   1st Qu.:2017-09-25   1st Qu.:    10739   Class :character  
##  Mode  :character   Median :2018-07-18   Median :    48000   Mode  :character  
##                     Mean   :2018-07-24   Mean   :   609166                     
##                     3rd Qu.:2019-05-13   3rd Qu.:   217791                     
##                     Max.   :2020-04-29   Max.   :807185500                     
##                                          NA's   :52                            
##    address          project_description building_contractor
##  Length:20680       Length:20680        Length:20680       
##  Class :character   Class :character    Class :character   
##  Mode  :character   Mode  :character    Mode  :character   
##                                                            
##                                                            
##                                                            
##                                                            
##  building_contractor_address  applicant         applicant_address 
##  Length:20680                Length:20680       Length:20680      
##  Class :character            Class :character   Class :character  
##  Mode  :character            Mode  :character   Mode  :character  
##                                                                   
##                                                                   
##                                                                   
##                                                                   
##  property_use       specific_use_category      year          bi_id      
##  Length:20680       Length:20680          Min.   :2017   Min.   :    1  
##  Class :character   Class :character      1st Qu.:2017   1st Qu.: 5171  
##  Mode  :character   Mode  :character      Median :2018   Median :10340  
##                                           Mean   :2018   Mean   :10340  
##                                           3rd Qu.:2019   3rd Qu.:15510  
##                                           Max.   :2020   Max.   :20680  
## 
```

``` r
# To get a better sense of data, I am more interested in knowing the count per year. Is it really hard to get builing permits now(like my building manager says) or has it always been an issue?
building_permits %>% 
  group_by(year) %>%
  summarise(n=n())
```

```         
## # A tibble: 4 × 2
##    year     n
##   <dbl> <int>
## 1  2017  6734
## 2  2018  6758
## 3  2019  5572
## 4  2020  1616
```

``` r
# Let us see different permit types
distinct(building_permits, type_of_work)
```

```         
## # A tibble: 6 × 1
##   type_of_work                        
##   <chr>                               
## 1 Salvage and Abatement               
## 2 New Building                        
## 3 Addition / Alteration               
## 4 Demolition / Deconstruction         
## 5 Temporary Building / Structure      
## 6 Outdoor Uses (No Buildings Proposed)
```

**Notable Observation:** Referring back to summary, we can see there are 52 missing numerical values.

#### Cancer Sample

``` r
# Making a practice to check the object type of data frame 
class(cancer_sample)
```

```         
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

``` r
# Let us now see the dimensions, columns and data types
glimpse(cancer_sample)
```

```         
## Rows: 569
## Columns: 32
## $ ID                      <dbl> 842302, 842517, 84300903, 84348301, 84358402, …
## $ diagnosis               <chr> "M", "M", "M", "M", "M", "M", "M", "M", "M", "…
## $ radius_mean             <dbl> 17.990, 20.570, 19.690, 11.420, 20.290, 12.450…
## $ texture_mean            <dbl> 10.38, 17.77, 21.25, 20.38, 14.34, 15.70, 19.9…
## $ perimeter_mean          <dbl> 122.80, 132.90, 130.00, 77.58, 135.10, 82.57, …
## $ area_mean               <dbl> 1001.0, 1326.0, 1203.0, 386.1, 1297.0, 477.1, …
## $ smoothness_mean         <dbl> 0.11840, 0.08474, 0.10960, 0.14250, 0.10030, 0…
## $ compactness_mean        <dbl> 0.27760, 0.07864, 0.15990, 0.28390, 0.13280, 0…
## $ concavity_mean          <dbl> 0.30010, 0.08690, 0.19740, 0.24140, 0.19800, 0…
## $ concave_points_mean     <dbl> 0.14710, 0.07017, 0.12790, 0.10520, 0.10430, 0…
## $ symmetry_mean           <dbl> 0.2419, 0.1812, 0.2069, 0.2597, 0.1809, 0.2087…
## $ fractal_dimension_mean  <dbl> 0.07871, 0.05667, 0.05999, 0.09744, 0.05883, 0…
## $ radius_se               <dbl> 1.0950, 0.5435, 0.7456, 0.4956, 0.7572, 0.3345…
## $ texture_se              <dbl> 0.9053, 0.7339, 0.7869, 1.1560, 0.7813, 0.8902…
## $ perimeter_se            <dbl> 8.589, 3.398, 4.585, 3.445, 5.438, 2.217, 3.18…
## $ area_se                 <dbl> 153.40, 74.08, 94.03, 27.23, 94.44, 27.19, 53.…
## $ smoothness_se           <dbl> 0.006399, 0.005225, 0.006150, 0.009110, 0.0114…
## $ compactness_se          <dbl> 0.049040, 0.013080, 0.040060, 0.074580, 0.0246…
## $ concavity_se            <dbl> 0.05373, 0.01860, 0.03832, 0.05661, 0.05688, 0…
## $ concave_points_se       <dbl> 0.015870, 0.013400, 0.020580, 0.018670, 0.0188…
## $ symmetry_se             <dbl> 0.03003, 0.01389, 0.02250, 0.05963, 0.01756, 0…
## $ fractal_dimension_se    <dbl> 0.006193, 0.003532, 0.004571, 0.009208, 0.0051…
## $ radius_worst            <dbl> 25.38, 24.99, 23.57, 14.91, 22.54, 15.47, 22.8…
## $ texture_worst           <dbl> 17.33, 23.41, 25.53, 26.50, 16.67, 23.75, 27.6…
## $ perimeter_worst         <dbl> 184.60, 158.80, 152.50, 98.87, 152.20, 103.40,…
## $ area_worst              <dbl> 2019.0, 1956.0, 1709.0, 567.7, 1575.0, 741.6, …
## $ smoothness_worst        <dbl> 0.1622, 0.1238, 0.1444, 0.2098, 0.1374, 0.1791…
## $ compactness_worst       <dbl> 0.6656, 0.1866, 0.4245, 0.8663, 0.2050, 0.5249…
## $ concavity_worst         <dbl> 0.71190, 0.24160, 0.45040, 0.68690, 0.40000, 0…
## $ concave_points_worst    <dbl> 0.26540, 0.18600, 0.24300, 0.25750, 0.16250, 0…
## $ symmetry_worst          <dbl> 0.4601, 0.2750, 0.3613, 0.6638, 0.2364, 0.3985…
## $ fractal_dimension_worst <dbl> 0.11890, 0.08902, 0.08758, 0.17300, 0.07678, 0…
```

``` r
# One of the most important aspect of Biomedical research is  nominal and ordinal data, therefore let us summarise the data
summary(cancer_sample)
```

```         
##        ID             diagnosis          radius_mean      texture_mean  
##  Min.   :     8670   Length:569         Min.   : 6.981   Min.   : 9.71  
##  1st Qu.:   869218   Class :character   1st Qu.:11.700   1st Qu.:16.17  
##  Median :   906024   Mode  :character   Median :13.370   Median :18.84  
##  Mean   : 30371831                      Mean   :14.127   Mean   :19.29  
##  3rd Qu.:  8813129                      3rd Qu.:15.780   3rd Qu.:21.80  
##  Max.   :911320502                      Max.   :28.110   Max.   :39.28  
##  perimeter_mean     area_mean      smoothness_mean   compactness_mean 
##  Min.   : 43.79   Min.   : 143.5   Min.   :0.05263   Min.   :0.01938  
##  1st Qu.: 75.17   1st Qu.: 420.3   1st Qu.:0.08637   1st Qu.:0.06492  
##  Median : 86.24   Median : 551.1   Median :0.09587   Median :0.09263  
##  Mean   : 91.97   Mean   : 654.9   Mean   :0.09636   Mean   :0.10434  
##  3rd Qu.:104.10   3rd Qu.: 782.7   3rd Qu.:0.10530   3rd Qu.:0.13040  
##  Max.   :188.50   Max.   :2501.0   Max.   :0.16340   Max.   :0.34540  
##  concavity_mean    concave_points_mean symmetry_mean    fractal_dimension_mean
##  Min.   :0.00000   Min.   :0.00000     Min.   :0.1060   Min.   :0.04996       
##  1st Qu.:0.02956   1st Qu.:0.02031     1st Qu.:0.1619   1st Qu.:0.05770       
##  Median :0.06154   Median :0.03350     Median :0.1792   Median :0.06154       
##  Mean   :0.08880   Mean   :0.04892     Mean   :0.1812   Mean   :0.06280       
##  3rd Qu.:0.13070   3rd Qu.:0.07400     3rd Qu.:0.1957   3rd Qu.:0.06612       
##  Max.   :0.42680   Max.   :0.20120     Max.   :0.3040   Max.   :0.09744       
##    radius_se        texture_se      perimeter_se       area_se       
##  Min.   :0.1115   Min.   :0.3602   Min.   : 0.757   Min.   :  6.802  
##  1st Qu.:0.2324   1st Qu.:0.8339   1st Qu.: 1.606   1st Qu.: 17.850  
##  Median :0.3242   Median :1.1080   Median : 2.287   Median : 24.530  
##  Mean   :0.4052   Mean   :1.2169   Mean   : 2.866   Mean   : 40.337  
##  3rd Qu.:0.4789   3rd Qu.:1.4740   3rd Qu.: 3.357   3rd Qu.: 45.190  
##  Max.   :2.8730   Max.   :4.8850   Max.   :21.980   Max.   :542.200  
##  smoothness_se      compactness_se      concavity_se     concave_points_se 
##  Min.   :0.001713   Min.   :0.002252   Min.   :0.00000   Min.   :0.000000  
##  1st Qu.:0.005169   1st Qu.:0.013080   1st Qu.:0.01509   1st Qu.:0.007638  
##  Median :0.006380   Median :0.020450   Median :0.02589   Median :0.010930  
##  Mean   :0.007041   Mean   :0.025478   Mean   :0.03189   Mean   :0.011796  
##  3rd Qu.:0.008146   3rd Qu.:0.032450   3rd Qu.:0.04205   3rd Qu.:0.014710  
##  Max.   :0.031130   Max.   :0.135400   Max.   :0.39600   Max.   :0.052790  
##   symmetry_se       fractal_dimension_se  radius_worst   texture_worst  
##  Min.   :0.007882   Min.   :0.0008948    Min.   : 7.93   Min.   :12.02  
##  1st Qu.:0.015160   1st Qu.:0.0022480    1st Qu.:13.01   1st Qu.:21.08  
##  Median :0.018730   Median :0.0031870    Median :14.97   Median :25.41  
##  Mean   :0.020542   Mean   :0.0037949    Mean   :16.27   Mean   :25.68  
##  3rd Qu.:0.023480   3rd Qu.:0.0045580    3rd Qu.:18.79   3rd Qu.:29.72  
##  Max.   :0.078950   Max.   :0.0298400    Max.   :36.04   Max.   :49.54  
##  perimeter_worst    area_worst     smoothness_worst  compactness_worst
##  Min.   : 50.41   Min.   : 185.2   Min.   :0.07117   Min.   :0.02729  
##  1st Qu.: 84.11   1st Qu.: 515.3   1st Qu.:0.11660   1st Qu.:0.14720  
##  Median : 97.66   Median : 686.5   Median :0.13130   Median :0.21190  
##  Mean   :107.26   Mean   : 880.6   Mean   :0.13237   Mean   :0.25427  
##  3rd Qu.:125.40   3rd Qu.:1084.0   3rd Qu.:0.14600   3rd Qu.:0.33910  
##  Max.   :251.20   Max.   :4254.0   Max.   :0.22260   Max.   :1.05800  
##  concavity_worst  concave_points_worst symmetry_worst   fractal_dimension_worst
##  Min.   :0.0000   Min.   :0.00000      Min.   :0.1565   Min.   :0.05504        
##  1st Qu.:0.1145   1st Qu.:0.06493      1st Qu.:0.2504   1st Qu.:0.07146        
##  Median :0.2267   Median :0.09993      Median :0.2822   Median :0.08004        
##  Mean   :0.2722   Mean   :0.11461      Mean   :0.2901   Mean   :0.08395        
##  3rd Qu.:0.3829   3rd Qu.:0.16140      3rd Qu.:0.3179   3rd Qu.:0.09208        
##  Max.   :1.2520   Max.   :0.29100      Max.   :0.6638   Max.   :0.20750
```

``` r
# Curious to see the type of diagnosis
distinct(cancer_sample, diagnosis)
```

```         
## # A tibble: 2 × 1
##   diagnosis
##   <chr>    
## 1 M        
## 2 B
```

``` r
#It is clear the data has both Malignat and Begnin tumors. But, now I want to see how big can a tumor get?  What is the mean radius between the diagnosis?
cancer_sample %>% 
  group_by(diagnosis) %>%
  summarize(meanRadius=mean(radius_mean))
```

```         
## # A tibble: 2 × 2
##   diagnosis meanRadius
##   <chr>          <dbl>
## 1 B               12.1
## 2 M               17.5
```

**Notable Observation:** Here is the catch! There is difference between mean radius of benign and malignant tumor. May be this can help me classify them?

#### Parking Meters

``` r
# Always checking the object type of data frame 
class(parking_meters)
```

```         
## [1] "tbl_df"     "tbl"        "data.frame"
```

``` r
# Analysing dataframe dimensions, columns, and data types 
glimpse(parking_meters)
```

```         
## Rows: 10,032
## Columns: 22
## $ meter_head     <chr> "Twin", "Pay Station", "Twin", "Single", "Twin", "Twin"…
## $ r_mf_9a_6p     <chr> "$2.00", "$1.00", "$1.00", "$1.00", "$2.00", "$2.00", "…
## $ r_mf_6p_10     <chr> "$4.00", "$1.00", "$1.00", "$1.00", "$1.00", "$1.00", "…
## $ r_sa_9a_6p     <chr> "$2.00", "$1.00", "$1.00", "$1.00", "$2.00", "$2.00", "…
## $ r_sa_6p_10     <chr> "$4.00", "$1.00", "$1.00", "$1.00", "$1.00", "$1.00", "…
## $ r_su_9a_6p     <chr> "$2.00", "$1.00", "$1.00", "$1.00", "$2.00", "$2.00", "…
## $ r_su_6p_10     <chr> "$4.00", "$1.00", "$1.00", "$1.00", "$1.00", "$1.00", "…
## $ rate_misc      <chr> NA, "$ .50", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ time_in_effect <chr> "METER IN EFFECT: 9:00 AM TO 10:00 PM", "METER IN EFFEC…
## $ t_mf_9a_6p     <chr> "2 Hr", "10 Hrs", "2 Hr", "2 Hr", "2 Hr", "3 Hr", "2 Hr…
## $ t_mf_6p_10     <chr> "4 Hr", "10 Hrs", "4 Hr", "4 Hr", "4 Hr", "4 Hr", "4 Hr…
## $ t_sa_9a_6p     <chr> "2 Hr", "10 Hrs", "2 Hr", "2 Hr", "2 Hr", "3 Hr", "2 Hr…
## $ t_sa_6p_10     <chr> "4 Hr", "10 Hrs", "4 Hr", "4 Hr", "4 Hr", "4 Hr", "4 Hr…
## $ t_su_9a_6p     <chr> "2 Hr", "10 Hrs", "2 Hr", "2 Hr", "2 Hr", "3 Hr", "2 Hr…
## $ t_su_6p_10     <chr> "4 Hr", "10 Hrs", "4 Hr", "4 Hr", "4 Hr", "4 Hr", "4 Hr…
## $ time_misc      <chr> NA, "No Time Limit", NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ credit_card    <chr> "No", "Yes", "No", "No", "No", "No", "No", "No", "No", …
## $ pay_phone      <chr> "66890", "59916", "57042", "57159", "51104", "60868", "…
## $ longitude      <dbl> -123.1289, -123.0982, -123.1013, -123.1862, -123.1278, …
## $ latitude       <dbl> 49.28690, 49.27215, 49.25468, 49.26341, 49.26354, 49.27…
## $ geo_local_area <chr> "West End", "Strathcona", "Riley Park", "West Point Gre…
## $ meter_id       <chr> "670805", "471405", "C80145", "D03704", "301023", "5913…
```

``` r
# Function to provides result summaries
summary(parking_meters)
```

```         
##   meter_head         r_mf_9a_6p         r_mf_6p_10         r_sa_9a_6p       
##  Length:10032       Length:10032       Length:10032       Length:10032      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   r_sa_6p_10         r_su_9a_6p         r_su_6p_10         rate_misc        
##  Length:10032       Length:10032       Length:10032       Length:10032      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##  time_in_effect      t_mf_9a_6p         t_mf_6p_10         t_sa_9a_6p       
##  Length:10032       Length:10032       Length:10032       Length:10032      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##   t_sa_6p_10         t_su_9a_6p         t_su_6p_10         time_misc        
##  Length:10032       Length:10032       Length:10032       Length:10032      
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##  credit_card         pay_phone           longitude         latitude    
##  Length:10032       Length:10032       Min.   :-123.2   Min.   :49.21  
##  Class :character   Class :character   1st Qu.:-123.1   1st Qu.:49.26  
##  Mode  :character   Mode  :character   Median :-123.1   Median :49.27  
##                                        Mean   :-123.1   Mean   :49.27  
##                                        3rd Qu.:-123.1   3rd Qu.:49.28  
##                                        Max.   :-123.0   Max.   :49.29  
##  geo_local_area       meter_id        
##  Length:10032       Length:10032      
##  Class :character   Class :character  
##  Mode  :character   Mode  :character  
##                                       
##                                       
## 
```

``` r
# Let's find parking spots! 
parking_meters %>% 
  group_by(geo_local_area) %>%
  arrange(geo_local_area) %>%
  summarize(n=n())
```

```         
## # A tibble: 18 × 2
##    geo_local_area               n
##    <chr>                    <int>
##  1 Arbutus-Ridge              148
##  2 Downtown                  3771
##  3 Fairview                  1624
##  4 Grandview-Woodland         312
##  5 Hastings-Sunrise             7
##  6 Kensington-Cedar Cottage    50
##  7 Kerrisdale                 139
##  8 Killarney                   32
##  9 Kitsilano                  920
## 10 Mount Pleasant             898
## 11 Renfrew-Collingwood         43
## 12 Riley Park                 280
## 13 Shaughnessy                 15
## 14 South Cambie                91
## 15 Strathcona                 508
## 16 Sunset                      77
## 17 West End                   940
## 18 West Point Grey            177
```

**Notable Observation** **:** Only a Vancouverite can understand the pain of having most parking meters in Downtown and still not able to find parking!

#### Vancouver Trees

``` r
# Checking the object type of data frame 
class(vancouver_trees)
```

```         
## [1] "tbl_df"     "tbl"        "data.frame"
```

``` r
# Checking Dataframe dimensions, columns, and data types 
glimpse(vancouver_trees)
```

```         
## Rows: 146,611
## Columns: 20
## $ tree_id            <dbl> 149556, 149563, 149579, 149590, 149604, 149616, 149…
## $ civic_number       <dbl> 494, 450, 4994, 858, 5032, 585, 4909, 4925, 4969, 7…
## $ std_street         <chr> "W 58TH AV", "W 58TH AV", "WINDSOR ST", "E 39TH AV"…
## $ genus_name         <chr> "ULMUS", "ZELKOVA", "STYRAX", "FRAXINUS", "ACER", "…
## $ species_name       <chr> "AMERICANA", "SERRATA", "JAPONICA", "AMERICANA", "C…
## $ cultivar_name      <chr> "BRANDON", NA, NA, "AUTUMN APPLAUSE", NA, "CHANTICL…
## $ common_name        <chr> "BRANDON ELM", "JAPANESE ZELKOVA", "JAPANESE SNOWBE…
## $ assigned           <chr> "N", "N", "N", "Y", "N", "N", "N", "N", "N", "N", "…
## $ root_barrier       <chr> "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "…
## $ plant_area         <chr> "N", "N", "4", "4", "4", "B", "6", "6", "3", "3", "…
## $ on_street_block    <dbl> 400, 400, 4900, 800, 5000, 500, 4900, 4900, 4900, 7…
## $ on_street          <chr> "W 58TH AV", "W 58TH AV", "WINDSOR ST", "E 39TH AV"…
## $ neighbourhood_name <chr> "MARPOLE", "MARPOLE", "KENSINGTON-CEDAR COTTAGE", "…
## $ street_side_name   <chr> "EVEN", "EVEN", "EVEN", "EVEN", "EVEN", "ODD", "ODD…
## $ height_range_id    <dbl> 2, 4, 3, 4, 2, 2, 3, 3, 2, 2, 2, 5, 3, 2, 2, 2, 2, …
## $ diameter           <dbl> 10.00, 10.00, 4.00, 18.00, 9.00, 5.00, 15.00, 14.00…
## $ curb               <chr> "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "…
## $ date_planted       <date> 1999-01-13, 1996-05-31, 1993-11-22, 1996-04-29, 19…
## $ longitude          <dbl> -123.1161, -123.1147, -123.0846, -123.0870, -123.08…
## $ latitude           <dbl> 49.21776, 49.21776, 49.23938, 49.23469, 49.23894, 4…
```

``` r
# Function to provides result summaries
summary(vancouver_trees)
```

```         
##     tree_id        civic_number    std_street         genus_name       
##  Min.   :    12   Min.   :    0   Length:146611      Length:146611     
##  1st Qu.: 65464   1st Qu.: 1306   Class :character   Class :character  
##  Median :134903   Median : 2604   Mode  :character   Mode  :character  
##  Mean   :131892   Mean   : 2937                                        
##  3rd Qu.:194450   3rd Qu.: 4005                                        
##  Max.   :266203   Max.   :17888                                        
##                                                                        
##  species_name       cultivar_name      common_name          assigned        
##  Length:146611      Length:146611      Length:146611      Length:146611     
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##                                                                             
##                                                                             
##                                                                             
##                                                                             
##  root_barrier        plant_area        on_street_block  on_street        
##  Length:146611      Length:146611      Min.   :   0    Length:146611     
##  Class :character   Class :character   1st Qu.:1300    Class :character  
##  Mode  :character   Mode  :character   Median :2600    Mode  :character  
##                                        Mean   :2909                      
##                                        3rd Qu.:4000                      
##                                        Max.   :9900                      
##                                                                          
##  neighbourhood_name street_side_name   height_range_id     diameter     
##  Length:146611      Length:146611      Min.   : 0.000   Min.   :  0.00  
##  Class :character   Class :character   1st Qu.: 1.000   1st Qu.:  3.50  
##  Mode  :character   Mode  :character   Median : 2.000   Median :  9.00  
##                                        Mean   : 2.627   Mean   : 11.49  
##                                        3rd Qu.: 4.000   3rd Qu.: 16.50  
##                                        Max.   :10.000   Max.   :435.00  
##                                                                         
##      curb            date_planted          longitude         latitude    
##  Length:146611      Min.   :1989-10-27   Min.   :-123.2   Min.   :49.20  
##  Class :character   1st Qu.:1998-02-23   1st Qu.:-123.1   1st Qu.:49.23  
##  Mode  :character   Median :2004-01-28   Median :-123.1   Median :49.25  
##                     Mean   :2004-04-07   Mean   :-123.1   Mean   :49.25  
##                     3rd Qu.:2010-03-02   3rd Qu.:-123.1   3rd Qu.:49.26  
##                     Max.   :2019-07-03   Max.   :-123.0   Max.   :49.29  
##                     NA's   :76548        NA's   :22771    NA's   :22771
```

``` r
# Determining distinct genus-specie pairs! Let's find beautiful fall trees
distinct(vancouver_trees, genus_name, species_name)
```

```         
## # A tibble: 361 × 2
##    genus_name species_name
##    <chr>      <chr>       
##  1 ULMUS      AMERICANA   
##  2 ZELKOVA    SERRATA     
##  3 STYRAX     JAPONICA    
##  4 FRAXINUS   AMERICANA   
##  5 ACER       CAMPESTRE   
##  6 PYRUS      CALLERYANA  
##  7 ACER       PLATANOIDES 
##  8 TILIA      EUCHLORA   X
##  9 HIBISCUS   SYRIACA     
## 10 FRAXINUS   OXYCARPA    
## # ℹ 351 more rows
```

**Notable Observation:** There are 361 different pairs of genus-species combinations.

## Task 1.3: Intermediate Data set Selection

The four data sets were very interesting, but two problems that stood out to be are:

1.  *cancer_sample*
2.  *building_permits*

As much as I am interested in exploring the **building permits** and resolving the "why is my rent so expensive" issue, I think I am more inclined towards **cancer sample**. Like I mentioned, I am a survivor myself and already during EDA I can see there might be a scope to classify the tumors! I see a ray of hope.

## Task 1.4: Final Data set Selection

Final decision did come easy to me, thanks to EDA! I was able to (preliminary) identify attribute, mean radius, that can help me classify the tumor to Malignant or Benign. I want to further explore and see if I can find more attributes. It is a very personal research problem and area I have been very passionately working towards. I am very curious about bias in data set too.

## Task 2: Detailed Exploration and Reasoning for plot choices.

Please find a detailed investigation of the data set **cancer sample** below. The goal is to understand about data characteristics, trends and insights, which will then eventually help in formulating research questions.

#### 2.1.1: Plotting distribution of numeric variable 'radius_mean' between the two (response) classes

-   EDA helped us understand that the data set comprises mostly of numerical values.
-   'radius_mean' was chosen, since cancer cells are larger in size.
-   Finally, a density plot is the correct choice since as our response variable is binary (classification) and not numerical (regression). Therefore, a density plot much better reveals the distribution of radius_mean across the output categories.

``` r
cancer_sample %>%
  ggplot(aes(radius_mean))+
  geom_density(aes(fill=diagnosis, alpha=0.3))
```

![](Milestone1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

**Note** There is a correlation between radius_mean and diagnosis classification.

**Challenge** There is also a significant overlap in values across the classifications it, which means we need to add other appropriate variables to our model.

#### 2.1.4.a: Exploring the relationship between two variables 'radius_mean' and 'symmetry_mean'

-   My curiosity peaked, I wonder if larger (malignant) cells are very different from smaller (benign) cells in regards to symmetry. This also based on a research paper I read!
-   Exploring relationship between radius and area, using scatter plots, will highlight the degree or correlation.

``` r
cancer_sample %>%
  ggplot(aes(radius_mean, symmetry_mean))+
  geom_point(aes(colour=diagnosis, size=0.3, alpha=0.3))
```

![](Milestone1_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Unfortunately, It can be seen that there is not a correlation between 'radius_mean' and 'symmetry_mean'. Let us move on and below we explore another variable with respect to 'radius_mean'.

#### 2.1.4.b: Exploring the relationship between two variables 'radius_mean' and 'concave_points_mean'

``` r
cancer_sample %>%
  ggplot(aes(radius_mean, concave_points_mean))+
  geom_point(aes(colour=diagnosis, size=0.3, alpha=0.3))
```

![](Milestone1_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

Voila! It can be seen that there is a strong relationship (i.e. correlation) between 'radius_mean' and 'concave_points_mean'.

#### 2.1.6: Using box plot to examine frequency of variable 'area_mean'.

-   Box plots provide a lot of detail on numerical attributes and are thus selected to contrast the 'radius_mean' which is already shown above to be a strong dependent variable.

``` r
cancer_sample %>%
  ggplot(aes(diagnosis, area_mean))+
  geom_boxplot(width=0.3)
```

![](Milestone1_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

#### 2.1.8: Using density plot to explore related variables 'radius_mean', 'radius_se', and 'radius_worst'

-   Density plots are very applicable for my data set as they compare the dependent variables nicely across the two classification outputs. Thus using another density plot to investigate suitability of variable 'fractal_dimension-mean'

``` r
cancer_sample %>%
  ggplot(aes(fractal_dimension_mean))+
  geom_density(aes(fill=diagnosis, alpha=0.3))
```

![](Milestone1_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

Having an almost identical distribution overlapping on each other, shows us that 'fractal_dimension_mean' is not a strong dependent variable.

One of the key indicators of Breast Cancer is texture. I want to explore the relationship between radius_mean and texture_mean.

``` r
cancer_sample %>%
  ggplot(aes(radius_mean, texture_mean))+
  geom_point(aes(colour=diagnosis, size=0.3, alpha=0.3))
```

![](Milestone1_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

I was expecting to see a correlation and that would have been very interesting to further explore. Unfortunately, we don't see a strong relationship.

## Task 3: Research Questions

1.  **Can a model classify the binary response of benign or malignant given the set of variables?**

2.  **Is (variable) data distribution and characteristics comparable between the benign and malignant classes?**

3.  **How can we find the optimized set of variables so that we have the highest performance? Which variable has the strongest correlation to the response variable**

4.  Model sensitivity is an important factor. Therefore, we need **identify what variables lead to highest model sensitivity?**
