A package for facilitating research on stop and search in the UK by making up-to-date stop and search data more accessible and usable.
## Introduction

The UK Police make stop and search data openly available via https://data.police.uk. Currently, the two main ways of accessing these data is:
- By downloading data files by Force and time period, producing a separate data file for each Force-time period combination, or
- By making http GET and POST requests via the site's applciation programming interface (API). Using this method, users can request searches occuring within a timeframe at locations/within areas, or again by Force.

For those interested in using these data for research on stop and search, there are limitations to these methods of data retrieval. The first method is inefficient because it requires that users manually combine datafiles across Forces and time points. In addition, though the data will include longitude/latitude variables, there is no geographic specification beyond this. The second method for retrieval requires users to know (or learn) how to use the API to extract data and, in addition, to be able to take the results of these queries and collate them into a datafile. Again, this process is time-consuming and inefficient. 

The purpose of extractss is to address these limitations in the accessibility and usability of stop and search data by extracting data from the Police API through iterative http POST requests and organising them geographically by Local Authority District (LAD). Using the main function, extract_ss_data, users can specify time periods and LADs for which to extract data, up to 36 months backwards. The output of this process is a comprehensive dataset of all stop and search records meeting the user's specification which can be readily used to explore research questions pertaining to the use of stop and search. 

Also included in this package are functions that we have used for our analyses exploring racial disparities in the use of stop and search, and which may be of use to other researchers. The principal function is analyse_records, which creates contingency tables from the extracted data to calculate ratios between the rates at which people of different ethnicities are stopped and searched.

## Packages
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
To install this package, use devtools::install_github(“BlackThrive/extractss”)
## How it works
The extract_ss_data function works by making iterative http POST requests to the Police API (https://data.police.uk/api/stops-street?). There are two components within each POST request: the date (month and year) and character string of longitudes and latitudes that describe a polygon (i.e., a Local Authority District). The user can specify which LAD(s) and time periods(s) to search. 
The function draws on a list of coordinates defining the boundaries of each LAD in the UK. The coordinate list is based on 2021 Local Authority District boundary data acquired from the Office for National Statistics Open Geography Portal (https://geoportal.statistics.gov.uk/), which were combined with lookups from the same source (1) to match LADs to Counties, Regions, and Countries and (2) to match LADs to Police Force Areas. The resultant list thus contained an element for each LAD in the UK, within which were contained the LAD’s coordinates and associated county, region, country, and Police Force. This coordinate list is built into the package and is called ‘coords’. The script used to create the list is available in the R folder.
