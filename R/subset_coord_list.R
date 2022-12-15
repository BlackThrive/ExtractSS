#' Subset coordinate list
#'
#' Subset the built-in coordinates list to desired areas. User can specify one
#' or multiple Local Authority, County, Region, Country, or Police Force names.
#' Note subsetting can only be done by one geography type at a time. Specifying
#' more than one geography parameter (e.g., la and region) will trigger an error.
#'
#' @param coord_list The list of coordinates to subset from. In most cases this
#' will be 'coords', which is bundled with the package.
#' @param la Character variable or vector of names of Local Authorities to which
#' to subset.
#' @param county Character variable or vector of names of counties to which
#' to subset.
#' @param region Character variable or vector of names of regions to which
#' to subset.
#' @param country Character variable or vector of names of countries to which
#' to subset.
#' @param force Character variable or vector of names of Police Forces to which
#' to subset.
#'
#' @return A list, which is a subset of 'coord_list', containing the coordinates
#' and other geographic data for the specified geographies.
#'
#' @export
#'
#' @examples
#'
#' # Subset to just the Metropolitan Police
#' subset_coord_list <- subset_coords(coords, force = "Metropolitan Police")
#'
#' # Subset to specific Local Authorities
#' subset_coord_list <- subset_coords(coords, la = c("Lambeth", "Haringey", "Birmingham"))

#' # Subset to specific regions
#' subset_coord_list <- subset_coords(coords, region = c("South East", "South West"))

subset_coords <- function(coords,
                         la = NULL,
                         county = NULL,
                         region = NULL,
                         country = NULL,
                         force = NULL){

  # error handling: If user specifies multiple geographies, trigger an error
  arg_names <- c("la","county","region","country","force")
  # get user's input arguments (minus the first arg which is function name)
  this_call_args <- names(match.call())[-c(1)]
  # if there are more than 1 area arguments defined by user trigger error
  if(sum(arg_names %in% this_call_args) > 1){
    stop("Only one area parameter can be specified")
  }

  # LA specified
  if(!is.null(la)){
    subset <- coords[la] # use this naming method as it is fastest
  }

  # County specified
  else if(!is.null(county)){
    # iterate through specified areas to find indexes of specified areas
    # then add hits to index list
    indexes <- c() # initialise index vector
    for(i in 1:length(county)){
      indexes <- as.numeric(
        append(indexes, which(
          sapply(coords, function(x) county[i] %in% x[1:length(x)][["county"]]))
        )
      )
    }
    # use index list to subset coords to specified areas
    subset <- coords[indexes]
  }

  # Region specified
  else if(!is.null(region)){
    # iterate through specified areas to find indexes of specified areas
    # then add hits to index list
    indexes <- c() # initialise index vector
    for(i in 1:length(region)){
      indexes <- as.numeric(
        append(indexes, which(
          sapply(coords, function(x) region[i] %in% x[1:length(x)][["region"]]))
        )
      )
    }
    # use index list to subset coords to specified areas
    subset <- coords[indexes]
  }

  # Country specified
  else if(!is.null(country)){
    # iterate through specified areas to find indexes of specified areas
    # then add hits to index list
    indexes <- c() # initialise index vector
    for(i in 1:length(country)){
      indexes <- as.numeric(
        append(indexes, which(
          sapply(coords, function(x) country[i] %in% x[1:length(x)][["country"]]))
        )
      )
    }
    # use index list to subset coords to specified areas
    subset <- coords[indexes]
  }

  # Force specified
  else if(!is.null(force)){
    # iterate through specified areas to find indexes of specified areas
    # then add hits to index list
    indexes <- c() # initialise index vector
    for(i in 1:length(force)){
      indexes <- as.numeric(
        append(indexes, which(
          sapply(coords, function(x) force[i] %in% x[1:length(x)][["force"]]))
        )
      )
    }
    # use index list to subset coords to specified areas
    subset <- coords[indexes]
  }
}
