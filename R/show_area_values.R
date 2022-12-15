#' Show the names of area-based variables
#'
#' Show all the names of the areas in the built-in coordinate data ('coords').
#' User can request names for all area types or unique values of a particular
#' area type. Useful for reference when looking to subset the in-built 'coords'
#' object.
#'
#' @param type The area type for which to specify names. The options are:
#' - la (Local Authority District)
#' - county
#' - region
#' - country
#' - force
#'
#' Can also be omitted, in which case all of the above area types will be
#' returned
#'
#' @return If type is not specified, a data frame containing names for all Local
#' Authority Districts and their corresponding super-ordinate area data (i.e.,
#' county, region, country, force).
#'
#' If type is specified, acharacter vector listing all the names contained
#' within the area type of interest.
#'
#' @export
#'
#' @examples
#'
#' # Show me all Local Authority names in 'coords'
#' show_area_values(type = "la")
#'
#' # Show me all Forces in 'coords'
#' show_area_values(type = "force")
#'
#' # Get all the area types and assign to an object
#' all_areas <- show_area_values()
#'
show_area_values <- function(type = NULL){

  # initialise df
  area_values <- data.frame(matrix(ncol=5,nrow=length(coords)))
  # set colnames
  colnames(area_values) <- c("la","county","region","country","force")
  # iterate through 'coords' and get the names of all area types
  for(i in 1:length(coords)){
    area_values$la[i] <- coords[[i]][["la_name"]]
    area_values$county[i] <- coords[[i]][["county"]]
    area_values$region[i] <- coords[[i]][["region"]]
    area_values$country[i] <- coords[[i]][["country"]]
    area_values$force[i] <- coords[[i]][["force"]]
  }

  # get unique values for requested area type
  if(is.null(type)){
    values <- area_values
  }
  else if(type == "la"){
    values <- unique(area_values$la)
  }
  else if(type == "county"){
    values <- unique(area_values$county)
  }
  else if(type == "region"){
    values <- unique(area_values$region)
  }
  else if(type == "country"){
    values <- unique(area_values$country)
  }
  else if(type == "force"){
    values <- unique(area_values$force)
  }
  # else if(is.null(type)){
  #   values <- area_values
  # }

  return(values)
}


