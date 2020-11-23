Generate File of Most Recent Biomonitoring Results
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
11/19/2020

  - [Introduction](#introduction)
  - [Load Libraries](#load-libraries)
  - [Load Data](#load-data)
      - [Establish Folder References](#establish-folder-references)
          - [Station Data](#station-data)
  - [Create Recent Data](#create-recent-data)
      - [Relate to Local
        Imperviousness](#relate-to-local-imperviousness)
      - [Save As CSV](#save-as-csv)

<img
  src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
  style="position:absolute;top:10px;right:50px;" />

# Introduction

Maine DEP has developed a robust stream classification model that is
based on collecting stream invertebrates from “rock baskets” left in
streams for about a month. They have also developed methods to assess
water quality based on comments of the algae community.

For GIS presentation in State of Casco Bay, it is convenient to have a
simple file to import into GIS. Here we generate that file. This
partially duplicates a file generated in Python.

# Load Libraries

``` r
#library(readr)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

# Load Data

## Establish Folder References

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
fn <- "Biomonitoring_Samples_CB.csv"

# dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

DEP uses “NA” to indicate “Non Attainment” in the data. By default, R
interprets that as `NA`, meaning missing data. We need to address that
in our code for reading the data. WE also need to be careful in later
analyses that R does not drop that value. We considered changing the
data label, but DEP uses “NA” consistently, and for simplicity of
communications it is easiest if we retain the original designation.

**Note that this data contains invertebrate, wetland, and algae sample
data, including sometimes sharing the same site designations.** \#\#
Read the Data \#\#\# Primary Sample Data

``` r
the_data <- read_csv(file.path(sibling, fn), na = '') %>%
  rename_with( ~ sub(' Class| Determination| ID', '', .x)) %>%
  rename(Station = `Station Number`,
         Date = `Sample Date`,
         Type = `Sample Type`) %>%
  mutate(Date = as.Date(Date, format = '%m/%d/%Y'),
         Year = as.integer(format(Date, format = '%Y'))) %>%
  relocate(Attained, .after = Final)
```

    ## Parsed with column specification:
    ## cols(
    ##   `Station Number` = col_character(),
    ##   `Sample Type` = col_character(),
    ##   `Sample ID` = col_character(),
    ##   `Sample Date` = col_character(),
    ##   `Statutory Class` = col_character(),
    ##   `Attained Class` = col_character(),
    ##   Report = col_character(),
    ##   `Final Determination` = col_character()
    ## )

### Station Data

``` r
fn <- "Biomonitoring_Stations_CB.csv"
station_data <- read_csv(file.path(sibling, fn), na = '') %>%
select(-contains('FID')) %>%
  select(-Sample_Typ) %>%
  rename(Station_Name = Station,
         Station = Station_Nu,
         Drainage = Major_Drai,
         Imperv = PctImperv)
```

    ## Parsed with column specification:
    ## cols(
    ##   FID = col_double(),
    ##   FID_1 = col_double(),
    ##   Station_Nu = col_character(),
    ##   Station = col_character(),
    ##   Town = col_character(),
    ##   County = col_character(),
    ##   Major_Drai = col_character(),
    ##   Site_Type = col_character(),
    ##   Sample_Typ = col_character(),
    ##   Latitude = col_double(),
    ##   Longitude = col_double(),
    ##   PctImperv = col_double()
    ## )

# Create Recent Data

Note that this data contains invertebrate, wetland, and algae sample
data, including sometimes sharing the same site designations.

We are only interested in “most recent” invertebrate biomonitoting data.
Further, we are interested only in data that relates to the recent past.
Here we restrict ourselves to sites from the most recent ten years of
available data. Currently, that corresponds to 2009 through 2018.

``` r
most_recent_year <- max(the_data$Year)
earliest_recent =  most_recent_year - 10

recent_data <- the_data %>%
  filter(Type == 'MACROINVERTEBRATE') %>%
  mutate(Final_f = ordered(Final, levels = c('A', 'B', 'C', 'NA'))) %>%
  group_by(Station) %>%
  arrange(Date) %>%
  mutate(keep = Sample == last(Sample)) %>%
  filter(keep) %>%
  filter(Year > earliest_recent) %>%
  select(-keep, -Report) 
```

## Relate to Local Imperviousness

``` r
recent_data <- recent_data %>%
  mutate(local_imperv = station_data$Imperv[match(Station,
                                                  station_data$Station)]) %>%
  select(-Sample)
```

## Save As CSV

This “Most Recent” data is somewhat more convenient to work with that
the one produced in Python, in the ‘access\_biomonitoring’ repository.

``` r
fn = 'Recent_Stream_Biomonitoring.csv'
write_csv(recent_data, file.path(sibling, fn))
```
