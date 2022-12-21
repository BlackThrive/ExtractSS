#' Risk ratio from data frame
#'
#' Calculates relative risk ratio and confidence intervals from a
#' contingency table constructed for analysis of stop and search disparities
#'
#' @param df Contingency table as data frame on which to calculate risk ratio
#' @param name Desired name of resultant data frame containing statistics
#'
#' @return A data frame containing risk ratio and confidence intervals
#'
#' @export
#'
#' @examples
#'
#' rr <- riskratio_from_df(df, "Stop and Search disparity")
#'
riskratio_from_df <- function(df, name){

  # organise matrix in format required by riskratio
  mat <- matrix(c(df[2,2],
                  df[2,1],
                  df[1,2],
                  df[1,1]), 2, 2)

  rr <- epitools::riskratio(mat) # run

  # output into formatted data frame
  df_out <- data.frame("indicator" = name,
                       "rr" = rr[["measure"]][2,1],
                       "ci_low" = rr[["measure"]][2,2],
                       "ci_upp" = rr[["measure"]][2,3])
  return(df_out)
}
