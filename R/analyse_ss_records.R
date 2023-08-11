# Calculates stop rates and disproportionality for each borough for each year 2019-2021

rm(list = ls())
library(tidyverse)

data <- read_csv("./data/data_36_months_to_dec_21.csv")

data <- data %>%
  subset(., country != "Northern Ireland" & country != "Scotland")

# add region for Wales
data$region[data$country == "Wales"] <- "Wales"

# load population estimates
# new pop ests from census 2021: https://www.ons.gov.uk/datasets/TS021/editions/2021/versions/1
population_ests <- read.csv("./data/census_2021_pop_ests_collapsed.csv")
population_ests <- population_ests #%>%
  # # translate ONS missing estimate characters to NAs
  # dplyr::mutate(across(ncol(population_ests)), dplyr::na_if(., "!")) %>%
  # dplyr::mutate(across(ncol(population_ests)), dplyr::na_if(., "-")) %>%
  # dplyr::mutate(across(ncol(population_ests)), dplyr::na_if(., "~"))


# set region of all Welsh LAs to 'Wales' and of Scottish LAs to 'Scotland'
data$region[which(data$country == "Wales")] <- "Wales"
#data$region[which(data$country == "Scotland")] <- "Scotland"

# collapse self-defined ethnicity (not necessary for officer-defined)
data$self_defined_ethnicity <- as.factor(data$self_defined_ethnicity)
data$self_defined_ethnicity <-
  forcats::fct_collapse(data$self_defined_ethnicity,
                        Asian = c("Asian/Asian British - Any other Asian background",
                                  "Asian/Asian British - Bangladeshi",
                                  "Asian/Asian British - Chinese",
                                  "Asian/Asian British - Indian",
                                  "Asian/Asian British - Pakistani"),
                        Black = c("Black/African/Caribbean/Black British - African",
                                  "Black/African/Caribbean/Black British - Any other Black/African/Caribbean background",
                                  "Black/African/Caribbean/Black British - Caribbean",
                                  "Mixed/Multiple ethnic groups - White and Black African", # have included mixed in Black category
                                  "Mixed/Multiple ethnic groups - White and Black Caribbean"),
                        Mixed = c("Mixed/Multiple ethnic groups - Any other Mixed/Multiple ethnic background",
                                  "Mixed/Multiple ethnic groups - White and Asian"),
                        Other = c("Other ethnic group - Any other ethnic group",
                                  "Other ethnic group - Not stated"),
                        White = c("White - Any other White background",
                                  "White - English/Welsh/Scottish/Northern Irish/British",
                                  "White - Irish")
  )

# define ethnicity as either self-defined or officer-defined
data <- data %>%
  dplyr::mutate(
    ethnicity = self_defined_ethnicity
  )

ethnicity_1 <- "White"
ethnicity_2 <- "Black"

# subset to ethnicities to compare
data_subset <- subset(data, ethnicity == ethnicity_1 | ethnicity == ethnicity_2)
# refactor. First in comparison is reference, second is treatment
data_subset$ethnicity <- factor(data_subset$ethnicity, levels = c("White","Black"))

# separate date into separate columns
data_subset$year <- as.numeric(substr(data_subset$date, 1, 4))
data_subset$month <- as.numeric(substr(data_subset$date, 6, 7))
data_subset$day <- as.numeric(substr(data_subset$date, 9, 10))

# make year_month variable for indexing
data_subset$year_month <-
  as.Date(paste(
    as.character(data_subset$year),
    as.character(data_subset$month),"01", sep= "-"))

las <- unique(data_subset$la_name) # get la names from the dataset
# initialise
all_results <- data.frame()
all_mats <- matrix(data = c(0,0,0,0), ncol = 2, nrow = 2)
all_dfs <- data.frame()
count <- 0

dates <- min(data_subset$year):max(data_subset$year)

# regional fork would be here

for(i in 1:length(las)){
  results_df <- data.frame() # initialise

  # get data for la
  la <- as.character(unique(data_subset[which(data_subset$la_name == las[i]), "la_name"]))
  this_la_code <- as.character(unique(data_subset[which(data_subset$la_name == las[i]), "la_code"]))
  county <- unique(data_subset[which(data_subset$la_name == las[i]), "county"])
  region <- unique(data_subset[which(data_subset$la_name == las[i]), "region"])
  country <- unique(data_subset[which(data_subset$la_name == las[i]), "country"])
  force <- unique(data_subset[which(data_subset$la_name == las[i]), "force"])

  # add date spec
  for(j in 1:length(dates)){
      this_date <- dates[j]
      temp_df <- data_subset %>% # subset to la
        subset(., la_code == this_la_code & year == this_date)

  # when not specifying date
  # temp_df <- data_subset %>%
  #   subset(., la_name == la)

  # flag for indicating whether a 0 in not_stopped has been coerced to make
  # stats run. default is 0 and changes if not_stopped less than 1 (below)
  stats_altered_flag <- 0


    if(length(unique(temp_df$ethnicity)) > 0){
      temp_df <- temp_df %>%
        dplyr::group_by(ethnicity) %>%
        dplyr::summarise(
          stopped = dplyr::n()
        )

      if(nrow(temp_df) < 2){
        temp_df <- temp_df %>%
          add_row(ethnicity = ifelse(ethnicity_1 %in% temp_df$ethnicity, ethnicity_2, ethnicity_1),
                stopped = 0)

      }

      temp_df <- temp_df %>%
        dplyr::mutate(
          pop = c(as.numeric(population_ests[which(population_ests$LAD_code == this_la_code), ethnicity_1]),
                  as.numeric(population_ests[which(population_ests$LAD_code == this_la_code), ethnicity_2])),
          percentage = 100 * (stopped/pop),
          not_stopped = pop - stopped
        ) %>%
        as.data.frame()

      row.names(temp_df) <- c(ethnicity_1, ethnicity_2)

      ethn_1 <- data.frame("ethnicity_1" = c("stopped" = temp_df[ethnicity_1, "stopped"],
                                             "not_stopped" = temp_df[ethnicity_1, "not_stopped"]))

      ethn_2 <- data.frame("ethnicity_2" = c("stopped" = temp_df[ethnicity_2, "stopped"],
                                             "not_stopped" = temp_df[ethnicity_2, "not_stopped"]))

      comp_mat <- as.matrix(cbind(ethn_2, ethn_1)) # matrix for crosstable



      if(sum(is.na(comp_mat)) == 0){ # if there are figures for all cells, run stats

        # if there are more stops than people (!!) set not_stopped to 0 so that
        # chi square tests can run
        for(k in 1:ncol(comp_mat)){
          if(comp_mat["not_stopped", k] < 0){
            comp_mat["not_stopped", k] <-  0
            stats_altered_flag <- 1
          }
          else{
            comp_mat["not_stopped", k] <- comp_mat["not_stopped", k]

          }
        }
        comp_df <- as.data.frame(comp_mat) # df for custom rr function
        # use tryCatch to collect warning messages
        xtab <- tryCatch(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T),
                         warning = function(w) return(list(gmodels::CrossTable(comp_mat, chisq = T, fisher = T, expected = T), w)))
        rr <- riskratio_from_df(comp_df, "Stop & Search")
        results_df <- data.frame(
          "date" = this_date,
          "la" = la,
          "la_code" = this_la_code,
          "county" = county,
          "region" = region,
          "country" = country,
          "force" = force,
          "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
          "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
          "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
          "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
          "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
          "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
          "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
          "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
          "or" = ifelse(is.list(xtab[[1]]),
                        xtab[[1]][["fisher.ts"]][["estimate"]][["odds ratio"]],
                        xtab[["fisher.ts"]][["estimate"]][["odds ratio"]]),
          "or_ci_low" = ifelse(is.list(xtab[[1]]),
                               xtab[[1]][["fisher.ts"]][["conf.int"]][1],
                               xtab[["fisher.ts"]][["conf.int"]][1]),
          "or_ci_upp" = ifelse(is.list(xtab[[1]]),
                               xtab[[1]][["fisher.ts"]][["conf.int"]][2],
                               xtab[["fisher.ts"]][["conf.int"]][2]),
          "rr" = rr$rr,
          "rr_ci_low" = rr$ci_low,
          "rr_ci_upp" = rr$ci_upp,
          "warning" = ifelse(is.list(xtab[[1]]), xtab[[2]][["message"]], NA),
          "stats_altered_flag" = stats_altered_flag)

        all_mats <- all_mats + comp_mat
        count <- count + 1 # increase count of areas for which stats have been acquired

      }
      else{ # if there are missing values, don't run stats but still add frequency data
        results_df <- data.frame(
          "date" = this_date,
          "la" = la,
          "la_code" = this_la_code,
          "county" = county,
          "region" = region,
          "country" = country,
          "force" = force,
          "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
          "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
          "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
          "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
          "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
          "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
          "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
          "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
          "or" = NA,
          "or_ci_low" = NA,
          "or_ci_upp" = NA,
          "rr" = NA,
          "rr_ci_low" = NA,
          "rr_ci_upp" = NA,
          "warning" = NA,
          "stats_altered_flag" = stats_altered_flag)
      }

  }
  # if there is not data for both black and white, and/or there are missing values
  else{

    # if no cases for either ethnicity for this date, need to record as 0 and calculate rates
    temp_df <- data.frame(ethnicity = c(ethnicity_1, ethnicity_2),
                          stopped = c(0,0),
                          pop = c(as.numeric(population_ests[which(population_ests$LAD_code == this_la_code), ethnicity_1]),
                                  as.numeric(population_ests[which(population_ests$LAD_code == this_la_code), ethnicity_2]))
                          ) %>%
      dplyr::mutate(
        percentage = 100 * (stopped/pop),
        not_stopped = pop - stopped
      )

    row.names(temp_df) <- c(ethnicity_1, ethnicity_2)

    results_df <- data.frame(
      "date" = this_date,
      "la" = la,
      "la_code" = this_la_code,
      "county" = county,
      "region" = region,
      "country" = country,
      "force" = force,
      "ethnicity_2_stopped" = temp_df[ethnicity_2, "stopped"],
      "ethnicity_2_not_stopped" = temp_df[ethnicity_2, "not_stopped"],
      "ethnicity_2_population" = temp_df[ethnicity_2, "pop"],
      "ethnicity_2_stop_rate" = temp_df[ethnicity_2, "percentage"],
      "ethnicity_1_stopped" = temp_df[ethnicity_1, "stopped"],
      "ethnicity_1_not_stopped" = temp_df[ethnicity_1, "not_stopped"],
      "ethnicity_1_population" = temp_df[ethnicity_1, "pop"],
      "ethnicity_1_stop_rate" = temp_df[ethnicity_1, "percentage"],
      "or" = NA,
      "or_ci_low" = NA,
      "or_ci_upp" = NA,
      "rr" = NA,
      "rr_ci_low" = NA,
      "rr_ci_upp" = NA,
      "warning" = NA,
      "stats_altered_flag" = stats_altered_flag)
  }


  all_results <- rbind(all_results, results_df) # add results to all results
  all_dfs <- as.data.frame(all_mats)
  cat("\014")
  print(paste0(i, " of ", length(las), " complete (", round(100 * (i / length(las)),2 ), "%)"))

} # date loop ends - remove if not specifying date

} # la loop ends

# add names to ethnicity categories
names(all_results) <- sub("^ethnicity_1", ethnicity_1, names(all_results))
names(all_results) <- sub("^ethnicity_2", ethnicity_2, names(all_results))


# la <- "Kingston upon Thames"
# temp_df <- data_subset %>%
#   subset(., la_name == la)
#
# temp_df <- temp_df %>%
#   dplyr::group_by(ethnicity) %>%
#   dplyr::summarise(
#     stopped = dplyr::n()
#   )%>%
#   dplyr::mutate(
#     pop = c(as.numeric(population_ests[which(population_ests$LAD == la), ethnicity_1]),
#             as.numeric(population_ests[which(population_ests$LAD == la), ethnicity_2])),
#     percentage = 100 * (stopped/pop),
#     not_stopped = pop - stopped
#   ) %>%
#   as.data.frame()
#
lookup <- read_csv("./data/lad_csp_pfa_lookup2022.csv") %>%
  distinct(.,LAD22NM, .keep_all = T)

combined <- left_join(all_results, lookup[,c("LAD22CD","PFA22CD")],
                      by = c("la_code" = "LAD22CD")) %>%
  relocate(
    PFA22CD, .after = force
  ) %>%
  rename(
    force_code = PFA22CD
  )

write_csv(combined, file = paste0("./data/",Sys.Date(), " - summarised_stops_2019-2021.csv"))
# saveRDS(all_results, file = "./data/london_summarised_stats_la_by_year.rds")
