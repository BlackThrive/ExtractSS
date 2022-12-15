# extractSS

A package for facilitating research on stop and search in the UK by making up-to-date stop and search data more accessible and usable.

# Introduction

The UK Police make stop and search data openly available via https://data.police.uk. Currently, the two main ways of accessing these data is:
- By downloading data files by Force and time period, producing a separate data file for each Force-time period combination, or
- By making http GET and POST requests via the site's applciation programming interface (API). Using this method, users can request searches occuring within a timeframe at locations/within areas, or again by Force.

For those interested in using these data for research on stop and search, there are limitations to these methods of data retrieval. The first method is inefficient because it requires that users manually combine datafiles across Forces and time points. In addition, though the data will include longitude/latitude variables, there is no geographic specification beyond this. The second method for retrieval requires users to know (or learn) how to use the API to extract data and, in addition, to be able to take the results of these queries and collate them into a datafile. Again, this process is time-consuming and inefficient. 

The purpose of extractss is to address these limitations in the accessibility and usability of stop and search data by extracting data from the Police API through iterative http POST requests and organising them geographically by Local Authority District (LAD). Using the main function, extract_ss_data, users can specify time periods and LADs for which to extract data, up to 36 months backwards. The output of this process is a comprehensive dataset of all stop and search records meeting the user's specification which can be readily used to explore research questions pertaining to the use of stop and search. 

Also included in this package are functions that we have used for our analyses exploring racial disparities in the use of stop and search, and which may be of use to other researchers. The principal function is analyse_records, which creates contingency tables from the extracted data to calculate ratios between the rates at which people of different ethnicities are stopped and searched.

# Packages
This package was built in R version 4.2.2 and has the following dependencies:
-	magrittr version 2.0.3
-	dplyr version 1.0.10
-	tidyr version 1.2.1
-	purr version 0.3.5
-	httr version 1.4.4
-	forcats version 0.5.2
-	epitools version 0.5-10.1
-	gmodels version 2.18.1.1
-	chron version 2.3-58

## Installation
To install this package, use 
```R
devtools::install_github(“BlackThrive/extractss”)
```
## How it works
The extract_ss_data function works by making iterative http POST requests to the Police API (https://data.police.uk/api/stops-street?). There are two components within each POST request: the date (month and year) and character string of longitudes and latitudes that describe a polygon (i.e., a Local Authority District). The user can specify which LAD(s) and time periods(s) to search. 

The function draws on a list of coordinates defining the boundaries of each LAD in the UK (contained within this package, called 'coords'). This list is based on 2021 Local Authority District boundary data acquired from the Office for National Statistics Open Geography Portal (https://geoportal.statistics.gov.uk/), which were combined with two lookups from the same source: (1) a lookup to match LADs to Counties, Regions, and Countries and (2) a lookup to match LADs to Police Force Areas. The resultant list thus contains an element for each LAD in the UK, within which are sub-elements correspodning to the LAD's coordinates, county, region, country, and Police Force. The script used to create the list is available in the R folder and is called 'create_coords_list.R'.

## Examples
The function extract_ss_data is used to extract stop and search records from the Police API. The user must assign the output of this function to an R object. The simplest usage requires only the argument ‘coord_list’, which is the list of areas for which to acquire data. By default, the function will extract for the most recent 12 months. An example call could be:
```R
extraction <- extract_ss_data(coord_list)
```

 The output of this function is a list containing three elements: 
-	result: This is the data frame containing the acquired stop records. Can be assigned as a named data frame using, e.g., if function output has been called data, data frame can be assigned as `df <- data[[1]]`.
-	missing_entries: Data frame which records any local authorities  for which no data was acquired.
-	server_errors: Data frame which records any unresolved server errors (i.e., cases where the function stopped trying the POST request).
In most cases, missing_entries and server_errors will be empty. The user can coerce the ‘result’ list element to a data frame using: 

```R
extraction_df <- extraction[[“result”]]
```
This dataframe can then be saved to CSV or RDS: 
```
write.csv(extraction_df, file = “inpath/filename.csv”)
saveRDS(extraction_df, file = “inpath/filename.Rds”) 
```

### Other options

#### Area specification 

Extraction can take a long time depending on the user's needs. In many cases it may be preferred to only extract data from specified areas. For example, since Policing in the UK is devolved, many users may be interested in stop records for England and Wales. The built-in ‘coords’ list covers the entirity of the UK, but can be subset to specific areas using the 'subset_coords' function. To subset to England and Wales, for example, the user could use:

```R
england_wales <- subset(coords(coords, country = c("England", "Wales"))
```

Using 'subset_coords', it is possible to subset based on the following groupings:
 - Local Authority
 - County
 - Region
 - Country 
 - Force
 
So, users interested in just the LADs Lambeth, Haringey, and Birmingham could use:

```R
LADs_of_interest <- subset_coords(coords, la = c(“Lambeth”, “Haringey”, “Birmingham”))
```

Likewise, if the user is only interested in London, they can specify:

```R
london_LADs <- subset_coord(coords, region = “London”)
```

It is also possible to subset to specific Police Force, e.g.

```R
met_police <- subset_coord(coords, force = “Metropolitan Police”)
```

To find out what areas are available for subsetting, use the function 'show_area_values' . Providing no argument will produce a data frame listing all LADs and their corresponding area details (i.e., county, region, country, force), e.g.:

```R
area_values <- show_area_values()
```

The user can also specify one of "la", "county", "region",  "country", or "force" to produce a character vector listing all the names corresponding to the area type. For example:

```R
# just see the values
show_area_values(type = "region")
# or assign to an object
regions <- show_area_values(type = "region")
```

#### Time specification

As mentioned previously, by default extract_ss_data extracts data for the most recent 12 months that are available in the API. This default behaviour can be changed using additional arguments. 
-	num_months_backwards: The user can change how many months backwards (from the most recently available data) to extract, up to 36 months (this is the limit imposed by the API itself). 
-	back_to_month/back_to_year: Instead of specifying a number of months backwards, the user can specify a specific month-year combination from which data will be acquired to the most recent data available. See below for how to find out what the last available date is.
-	most_recent_month/most_recent_year: The user can specify what the most recent month-year should be if they do not want to use the most recently available. In combination with num_months_backwards, this will acquire data from the specified month-year backwards the number of months specified . In combination with back_to_month/back_to_year, the function will acquire data between the month_year specification of the two groups of arguments. See below for how to find out what the most recent available date is.

To find out what the most recent/oldest dates that are available in the API, the user can use the functions 'newest_data' and 'oldest_data', respectively.
