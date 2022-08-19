# get oldest date live
oldest_data <- function(){
  oldest <- httr::GET("https://data.police.uk/api/crimes-street-dates")
  k <- httr::content(oldest)
  l <- lapply(k, unlist)
  m <- do.call(dplyr::bind_rows, l)

  # format as date to be able to use min/max functions
  for(i in 1:nrow(m)){
    m$date[i] <- paste0(m$date[i],"-01")
  }
  m$date <- as.Date(m$date)

  substr(min(m$date), 1, 7)

}
