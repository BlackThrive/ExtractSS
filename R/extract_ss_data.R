#' Extract stop and search records
#'
#' Extracts stop and search records from Police API and organises them by local
#' authority.
#'
#' @param coord_list List of coordinates for which to acquire records
#' @param num_months_backwards The number of months backwards from the start
#' point for which to acquire data. Numeric value. Default is 12. Maximum is 36
#' (limit imposed by API).
#' @param oldest_month Instead of specifying `num_months_backwards`, the user
#' can specify the oldest month of interest (in combination with the oldest year
#' of interest). Numeric value (e.g., 8 for August).
#' @param oldest_year Instead of specifying `num_months_backwards`, the user
#' can specify the oldest year of interest (in combination with the oldest
#' month of interest). Numeric value (e.g., 2019).
#' @param most_recent_month The most recent month of interest. Numeric value
#' (e.g., 8 for August). By default the function will determine this based on
#' the most recent API update.
#' @param most_recent_year The most recent month of interest. Numeric value
#' (e.g., 2021). By default the function will determine this based on the most
#' recent API update.
#' @param wait_time If there is a server error when submitting POST request,
#' the function will wait `wait_time` seconds before retrying. Numeric value.
#' Default is 5.
#' @param max_tries Specify the maximum number of times to retry a failed
#' POST request. Numeric value. Default is 10. Failures are usually due to
#' timeouts, which are often resolved by retrying, but can also be because there
#' is no data available for the area/date combination.
#'
#' @return A list containing three elements.
#' * result: This is the data frame containing the acquired stop records.
#' Can be assigned as a named data frame using, e.g., if function output has
#' been called data, data frame can be assigned as `df <- data[[1]]`.
#' * missing_entries: Data frame which records any local authorities  for which
#' no data was acquired.
#' * server_errors: Data frame which records any unresolved server errors
#' (i.e., cases where the function stopped trying the POST request).
#'
#' @export
#'
#' @examples
#'
#' # get data from the most recently available 12 months (default)
#' data <- extract_ss_data(coords)
#'
#' # get data from january 2019 to the most recent date
#' data <- extract_ss_data(coords, oldest_month = 1, oldest_year = 2019)
#'
#' # get data between january 2019 and october 2020
#' data <- extract_ss_data(coords, oldest_month = 1,
#'                                 oldest_year = 2019,
#'                                 most_recent_month = 10,
#'                                 most_recent_year = 2020)
#'
#' # get data 24 months backwards from august 2021
#' data <- extract_ss_data(coords, most_recent_month = 8,
#'                                 most_recent_year = 2021,
#'                                 num_months_backwards = 24)
#'
extract_ss_data <- function(coord_list,
                            num_months_backwards = 12,
                            oldest_month = NULL,
                            oldest_year = NULL,
                            most_recent_month = NULL,
                            most_recent_year = NULL,
                            wait_time = 5,
                            max_tries = 5){

  ### Error handling ###

  # Make sure that user can't specify an incorrect time period
  # 1. Too many months backwards
  if(!is.null(num_months_backwards)){
    if(num_months_backwards > 36){
      stop("Error in num_months_backwards. The API only holds data for the previous 36 months. Respecify num_months_backwards")
    }
  }

  # 2. Back to year/month specification exceeds 36 months backwards
  oldest_date <- as.Date(paste0(oldest_data(),"-01"))
  if(!is.null(oldest_month) && !is.null(oldest_year)){
    if(as.Date(paste0(oldest_year,"-",oldest_month,"-01")) < oldest_date){
      stop("Error in oldest_month/oldest_year. The API only holds data for the previous 36 months. The oldest date is ", substr(oldest_date, 1, 7))
    }
  }

  # 3. Most recent specification is too recent
  newest_date <- as.Date(paste0(newest_data(),"-01"))
  if(!is.null(most_recent_month) && !is.null(most_recent_year)){
    if(as.Date(paste0(most_recent_year,"-", most_recent_month,"-01")) > newest_date){
      stop("Error in most_recent_month/most_recent_year. The most recent data available is ", substr(newest_date, 1, 7))
    }
  }

  # 4. Date not fully specified
  ## one of the 'oldest' arguments missing
  if((!is.null(oldest_month) && is.null(oldest_year)) ||
      (is.null(oldest_month) && !is.null(oldest_year))){
    stop("Error: Both oldest_month and oldest_year must be specified.")
  }
  ## one of the 'most_recent' arguments missing
  if((!is.null(most_recent_month) && is.null(most_recent_year)) ||
     (is.null(most_recent_month) && !is.null(most_recent_year))){
    stop("Error: Both most_recent_month and most_recent_year must be specified.")
  }
  ## most recent specified with no target in the past (with default setting for num_months_backwards this won't happen)
  if(!is.null(most_recent_month) && !is.null(most_recent_year) &&
      is.null(num_months_backwards) &&
     (is.null(oldest_month) || is.null(oldest_year))){
    stop("Error: Time period not specified or incomplete. Either num_months_backwards or both oldest_month and oldest_year must be specified")
  }

  ### End: Error handling ###

  # initialise dataframes
  overall_output <- data.frame()
  no_entries_df <- data.frame(setNames(rep(list(NA), 6), c("Index", "Name","County","Region","Country","Force")))
  server_error_df <- data.frame(setNames(rep(list(NA), 10), c("Index","Name","County","Region","Country","Force","Date", "Date_Index", "Coordinate_Set_Index", "Status_Code")))

  ### h loop: iterates over LAs ###
  for(h in 1:length(coord_list)){
    print(paste0("Started area ", h)) # report start (useful for debugging)

    la_name <- coord_list[[h]][["la_name"]] # LA name
    la_code <- coord_list[[h]][["la_code"]] # la census code

    county <- coord_list[[h]][["county"]] # county name
    if(purrr::is_empty(county)){
      county <- NA # set NA if missing
    }

    region <- coord_list[[h]][["region"]] # region name
    if(purrr::is_empty(region)){
      region <- NA # set NA if missing
    }

    country <- coord_list[[h]][["country"]] # country name

    force <- coord_list[[h]][["force"]]

    # get most recent update if to data not specified
    if(is.null(most_recent_month) || is.null(most_recent_year)){
      # get most recent update from API:
      date <- httr::content(
        httr::GET("https://data.police.uk/api/crimes-street-dates"))[[1]][["date"]]
      most_recent_month <- as.numeric(substr(date,6,7))
      most_recent_year <- as.numeric(substr(date,1,4))
    }
    else{
      most_recent_month <- most_recent_month
      most_recent_year <- most_recent_year
    }

    # if oldest_month/year has been specified, redefine num_months_backwards
    if(!is.null(oldest_month) && !is.null(oldest_year)){
        num_months_backwards <- 1 + (12 * (most_recent_year - oldest_year)) + (most_recent_month - oldest_month)
    }
    else{
      num_months_backwards <- num_months_backwards
    }

    area_output <- data.frame() # initialise area output df
    number_months_acquired <- 0 # initialise number of months acquired counter

    ### i loop: iterates over the months required ###
    for(i in 1:num_months_backwards){
      month_output <- data.frame()

      # format date to what is needed for API query ("yyyy-mm")
      if(i == 1){ # set values for first iteration
        month_num <- most_recent_month
        year <- most_recent_year
      }
      else{ # subsequent iterations
        month_num <- month_num - 1 # backwards a month each iteration
        if(month_num %% 12 == 0){ # if reach a new year, start months from 12 again
          month_num <- 12
          year <- year - 1 # backwards a year
        }
      }
      if(month_num < 10){ # paste 0 for months lower than 10
        month <- paste("0", month_num, sep = "")
      }
      else{
        month <- month_num
      }

      date <- paste(year, "-", month, sep = "") # combine dates into one string

      ### j loop ###
      # Iterates over the coordinate sets within each LA and creates a
      # polygon string to be searched, and then submits the query.
      # Most LAs have only one coordinate set, but some have multiple (e.g.,
      # those that include islands). The function therefore needs to search
      # each coordinate set within a LA separately.
      coord_string <- c() # initialise vector for coord string
      for(j in 1:length(coord_list[[h]][["coords"]])){

        # set this iteration's coordinate set
        area_coords <- coord_list[[h]][["coords"]][[j]]
        # combine coord strings into format required by API (much quicker than looping)
        coord_string <- paste0(area_coords$lat,",",area_coords$long, collapse = ":")

        # create body for post request
        body <- list("poly" = coord_string,
                     "date" = date)

        # search API for this coordinate set and date:
        post_request <- httr::POST("https://data.police.uk/api/stops-street?", body = body)

        # if search quota reached, break (shouldn't be an issue but just in case)
        if(post_request[["status_code"]] == 429){
          print("Quota reached. Abandoning request.")
          break
        }
        else{
          # if the request didn't succeed, wait some time ('wait_time') and
          # keep trying up until 'max_tries' attempts.
          attempt <- 1
          while(post_request[["status_code"]] != 200 && attempt <= max_tries){
            print(paste0("Server error: ", post_request[["status_code"]], ". Trying again (", attempt,")"))
            Sys.sleep(wait_time) # wait some time before trying again
            try(
              post_request <- httr::POST("https://data.police.uk/api/stops-street?", body = body)
            )
            # if search quota reached, break (shouldn't be an issue but just in case)
            if(post_request[["status_code"]] == 429){
              print("Quota reached. Abandoning request.")
              break
            }
            attempt <- attempt + 1
          }

          # once max_tries is met, give up retry, save info including status code
          if(post_request[["status_code"]] != 200 && attempt > max_tries){
            print(paste0("Max tries reached (", max_tries,"). Continuing."))
            if(is.na(server_error_df[1,1])){ # if first occurrence, replaces NAs
              server_error_df[1,] <- c(h, la_name, county, region, country, force, date, i, j, post_request[["status_code"]])
            }
            else{ # rbind subsequent occurrences
              server_error_df <- rbind(server_error_df, c(h, la_name, county, region, country, force, date, i, j, post_request[["status_code"]]))
            }
          }
        }

        # get data from results of query
        df <- httr::content(post_request)

        # unlist data and convert to dataframe
        df_2 <- lapply(df, unlist)
        df_3 <- do.call(dplyr::bind_rows, df_2)
        df_3$coord_set <- j # record which coordinate set data is from

        # add results of this coordinate set iteration (j) to the output for
        # this month iteration (i). Use bindrows for (high) possibility that columns
        # from different iterations will be in different order/missing
        month_output <- dplyr::bind_rows(month_output, df_3)

        cat("\014") # clear console
        # report overall (i.e., LA) progress
        print(paste0("Working... LA ", h, " of ", length(coord_list),
                     " (",
                     round(100 * (h / length(coord_list)), 2), "%)"))
        # report month progress
        print(paste0("Working... Month ", i, " of ", num_months_backwards,
                     " (", date, ")"))
        # report coordinate set progress
        print(paste0("Working... ", j, " of ",
                     length(coord_list[[h]][["coords"]]),
                     " coordinate sets retrieved"))


      } # coordinate set loop (j) ends

      ### End: j loop ###

      # if records have been acquired, increase months count
      if(nrow(month_output) > 0){
        number_months_acquired <- number_months_acquired + 1
      }

      # add data from this month (i) to overall LA output (h)
      area_output <- dplyr::bind_rows(area_output, month_output)

    } # month loop (i) ends

    ### End: Month loop ###

    # If there were no records for this LA, record the LA iteration number
    # and name, then proceed to the next LA
    if(nrow(area_output) == 0){
      if(is.na(no_entries_df[1,1])){ # if first occurrence, replaces NAs
        no_entries_df[1,] <- c(h, la_name, county, region, country, force)
      }
      else{ # rbind subsequent occurrences
        no_entries_df <- rbind(no_entries_df, c(h, la_name, county, region, country, force))
      }
      print(paste0("No records for ", la_name))
      next # proceed to next LA
    }

    # add columns for LA name, county, region, country, and the iteration index
    # for the LA (useful for quickly identifying which LA the function reached
    # if it breaks unexpectedly)
    area_output$la_name <- la_name
    area_output$la_code <- la_code
    area_output$county <- county
    area_output$region <- region
    area_output$country <- country
    area_output$index <- h
    area_output$number_months_acquired <- number_months_acquired
    area_output$proportion_months_acquired <- number_months_acquired / num_months_backwards
    area_output$force <- force

    # separate datetime into 2 columns
    area_output$time <- substr(area_output$datetime, 12, 19)
    area_output$date <- as.Date(substr(area_output$datetime, 1, 10))
    # set as time
    #area_output$time <- chron(times. =  df$time, format = c(times = "hh:mm:ss"))
    save(area_output, file = "test.Rdata")


    # add records from this area (h) to overall output
    overall_output <- dplyr::bind_rows(overall_output, area_output)

    # create a temporary output list and save it every time a LA completes, so
    # that there is a backup in case function breaks. Saves to same folder as
    # script
    save_progress <- list(result = overall_output,
                          missing_entries = no_entries_df,
                          server_errors = server_error_df,
                          last_area_acquired = h)
    save(save_progress, file = "./save_progress.Rdata")
  } # LA loop (h) ends

  ### End: h loop ###

  ### Data tidying and completion ###

  # move index and location data to front of df
  overall_output <- overall_output %>%
  dplyr::select(index,
         la_name,
         la_code,
         county,
         region,
         country,
         force,
         date,
         time,
         age_range,
         gender,
         self_defined_ethnicity,
         officer_defined_ethnicity,
         object_of_search,
         legislation,
         outcome,
         location.longitude,
         location.latitude,
         location.street.id,
         location.street.name,
         #! the below variables are not consistently included across areas !#
         # outcome_linked_to_object_of_search,
         # type,
         # involved_person,
         # removal_of_more_than_outer_clothing,
         # operation,
         everything()) %>%
  subset(., select = -c(datetime, outcome_object.id, outcome_object.name)) # get rid of redundant columns

  final_output <- list(result = overall_output,
                       missing_entries = no_entries_df,
                       server_errors = server_error_df)

  save(final_output, file = "./full_extraction.Rdata")

  ### End: Data tidying and completion ###

  # return output. 'result' is the data. 'missing_entries' provides a list of
  # LAs that are missing from the data because there were no records for the
  # LA in the specified time period.
  return(final_output)

}
