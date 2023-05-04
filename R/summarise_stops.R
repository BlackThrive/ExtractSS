#' Summarise stops by object of search, legislation, or outcome
#'
#' Takes a raw stop and search data frame and summarises it by object of search,
#' legislation, or outcome. By default if will summarise by area. The summary
#' can also be broken down by ethnicity.
#'
#' @param data The data to analyse. Must be an R object in the environment
#' @param metric Specify what summary statistics are desired. The options are
#' 'object of search' (i.e., what is being searched for, e.g,. Controlled Drugs),
#' 'legislation' (e.g., "Misuse of Drugs Act), or 'outcome' (e.g., arrest).
#' @param by_area Boolean indicating whether to analyse by local authority or not.
#' If TRUE, will group data by local authority before summarising. Default is TRUE.
#' @param by_ethnicity Boolean indicating whether to analyse by ethnicity or not.
#' If true, will group data by ethnicity before summarising. Default is FALSE.
#' @param ethnicity_definition If by_ethnicity is TRUE, ethnicity_definition must be
#' specified. "self" = self-defined; "officer" = officer-defined. Default is
#' officer-defined.
#' @param plot If by_area is FALSE, by default the function will produce summary
#' plots. This can be changed by setting plot = FALSE.
#' @param save_plot If plot is TRUE, the plot can be saved to png with save_plot.
#' Default is FALSE.
#' @param metric_by_ethnicity Specify whether the summarisation should nest ethnicity
#' within the metric (TRUE), or the metric within ethnicity (FALSE). This will
#' change what percentages represent.  If FALSE, calculated percentages will
#' represent the number of stops for each level of the metric as a proportion of
#' the total number of stops for the ethnicity; e.g., for object_of_search, percentages
#' will represent the number of stops of each object of search as a proportion of
#' the total number of stops for the ethnicity. If TRUE, percentages will
#' represent the number of stops for each ethnicity as a proportion of the total
#' number of stops for the level of the metric; e.g., for object_of_search,
#' percentages will represent the number of stops of each ethnicity as  proportion
#' of the total number of stops for each object. By default FALSE.
#'
#' @return A grouped data frame containing the frequency and percentage of stops
#' for each grouping factor. If plot = TRUE, a summary plot will be presented. If
#' save_plot = TRUE, a png image will be saved in the folder "./outputs/".
#'
#' @export
#'
#' @examples summary_tab <- summarise_stops(data, metric = "outcome", by_ethnicity = TRUE)
#'
summarise_stops <- function(data,
                              metric = c("object_of_search", "legislation","outcome"),
                              by_area = TRUE,
                              by_ethnicity = FALSE,
                              ethnicity_definition = c("officer", "self"),
                              plot = TRUE,
                              save_plot = FALSE,
                              metric_by_ethnicity = FALSE){

  ### 0. Initial processing ###

  # Take metric string for plots
  metric_string <- metric
  # Take metric name for insertion {{}}
  metric <- as.name(metric) # make argument a name so that {{}} understands it

  data <- data

  ### END 0 ###

  ### 1. Error handling ###

  if(!is.object(data)){
    stop("Data must be a data frame object")
  }
  # if(by_ethnicity == TRUE & length(ethnicity_definition) != 1){
  #   stop("Please specify which ethnicity definition to use - 'self' or 'officer'")
  # }
  if(metric_by_ethnicity == T & by_ethnicity == F){
    stop("by_ethnicity must be TRUE for metric_by_ethnicity to apply")
  }
  if(save_plot == T & plot == F){
    stop("plot must be TRUE for save_plot to apply")
  }

  # set default ethnicity definition
  if(by_ethnicity == TRUE){
    ethnicity_definition <- "officer"
  }

  ### END 1 ###


  ### 2 Summarisation ###

  ### 2.1 Overall (i.e., not by area ###
  if(by_area == FALSE){

    ### Overall ###

    if(by_ethnicity == FALSE){ # Aggregated
      summary_tab <- data %>%
        group_by({{ metric }}) %>%
        summarise(
          frequency = n()
        ) %>%
        mutate(
          percentage = 100 * (frequency / sum(frequency))
        )
    }else if(by_ethnicity == TRUE){ # by ethnicity
      # Assign the desired ethnicity type
      if(ethnicity_definition == "self"){
        data$ethnicity <- data$self_defined_ethnicity
      }else if(ethnicity_definition == "officer"){
        data$ethnicity <- data$officer_defined_ethnicity
      }

      # express percentages as ethnicity within reason
      # i.e., "18% of stops of for controlled drugs are stops of Asians"
      if(metric_by_ethnicity == T){
        summary_tab <- data %>%
          group_by({{ metric }}, ethnicity) %>% # ethnicity within reason
          summarise(
            frequency = n()
          ) %>%
          mutate(
            percentage = 100 * (frequency / sum(frequency))
          )

      # express percentages as reason within ethnicity
      # i.e., "74% of stops of Asians are for controlled drugs"
      }else{
        summary_tab <- data %>%
          group_by(ethnicity, {{ metric }}) %>% # reason within ethnicity
          summarise(
            frequency = n()
          ) %>%
          mutate(
            percentage = 100 * (frequency / sum(frequency))
          )
      }
    }

    ### 2.1.1 Plots ###

    if(plot == TRUE){

      if(by_ethnicity == TRUE){
        summary_plot <- ggplot(summary_tab,
                               aes(x = if(metric_by_ethnicity == T) {{ metric }} else ethnicity,
                                   y = percentage,
                                   fill = if(metric_by_ethnicity == T) ethnicity else {{ metric }})) + # if by_ethnicity is T add fill
          geom_col(colour = "black", position = "dodge2") +
          xlab(if(metric_by_ethnicity == T) metric_string else "Ethnicity") + ylab("Percentage of stops (within group)") +
          ggtitle(if(metric_by_ethnicity == T)
            paste0("Percentage of stops by ", metric_string, " and ethnicity (ethnicity within ",metric_string,")")
                  else paste0("Percentage of stops by ethnicity and ", metric_string," (", metric_string, " within ethnicity)")) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_fill_discrete(name = if(metric_by_ethnicity == T) paste0("Ethnicity (",ethnicity_definition,"-defined)") else metric_string) +
          if(nrow(summary_tab) <= length(unique(data[,metric_string]))){
            geom_label(aes(label = frequency))
          }
      }else{
        summary_plot <- ggplot(summary_tab, aes(x = {{ metric }}, y = percentage)) +
          geom_col(colour = "black", position = "dodge2") +
          xlab("Object of search") + ylab("Percentage of stops (within group)") +
          ggtitle(paste0("Percentage of stops by ",metric_string)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_fill_discrete(name = paste0("Ethnicity (",ethnicity_definition,"-defined)")) +
          if(nrow(summary_tab) <= length(unique(data[,metric_string]))){
            geom_label(aes(label = frequency))
          }
      }

      # save plot if desired
      if(save_plot == TRUE){
        ggsave(filename = "./outputs/reasons_plot.png", device = "png")
      }

      print(summary_plot) # this must come after save

    }

    ### END 2.1.1 ###

    ### END 2.1 ###

  ### 2.2 By area ###

  }else{
    if(by_ethnicity == FALSE){ # Aggregated
      summary_tab <- data %>%
        group_by(la_name, {{ metric }}) %>%
        summarise(
          frequency = n()
        ) %>%
        mutate(
          percentage = 100 * (frequency / sum(frequency))
        )
    }else if(by_ethnicity == TRUE){ # By ethnicity
      # Assign the desired ethnicity type
      if(ethnicity_definition == "self"){
        data$ethnicity <- data$self_defined_ethnicity
      }else if(ethnicity_definition == "officer"){
        data$ethnicity <- data$officer_defined_ethnicity
      }

      # express percentages as ethnicity within reason
      # i.e., "18% of stops of for controlled drugs are stops of Asians"
      if(metric_by_ethnicity == TRUE){
        summary_tab <- data %>%
          group_by(la_name, {{ metric }}, ethnicity) %>% # ethnicity within reason
          summarise(
            frequency = n()
          ) %>%
          mutate(
            percentage = 100 * (frequency / sum(frequency))
          )

        # express percentages as reason within ethnicity
        # i.e., "74% of stops of Asians are for controlled drugs"
      }else{
        summary_tab <- data %>%
          group_by(la_name, ethnicity, {{ metric }}) %>% # reason within ethnicity
          summarise(
            frequency = n()
          ) %>%
          mutate(
            percentage = 100 * (frequency / sum(frequency))
          )
      }
    }

  }

  ### END 2.2 ###

  ### END 2 ###

  # Print some summary data
  print(paste0("Total stops: ", sum(summary_tab$frequency)))
  print(paste0("Time period: ", min(data$date), " to ", max(data$date)))
  print(paste0("Number of areas: ", length(unique(data$la_name))))

  # return the summarised tibble
  return(summary_tab)

  ### END ALL ###
}
