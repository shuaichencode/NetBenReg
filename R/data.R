#' Possibly censored cost-effectiveness data with covariates
#'
#' Simulated patient-level data for possibly censored yearly costs and quality-adjusted life years (QALY) with treatments and covariates
#' Report ...
#'
#' @format ## `CEdata`
#' A data frame with 2000 rows and 30 columns:
#' \describe{
#'   \item{id}{patient ID}
#'   \item{survival}{follow-up time (in years)}
#'   \item{dead}{death indicator, 1 for the occurrence of death (i.e., complete data) and 0 for censoring (i.e., known to be alive at the time indicated by 'survival'). }
#'   \item{Trt}{treatment indicator, 1 for new treatment group and 0 for usual care comparison group}
#'   \item{Age65}{baseline covariate, 1 for age>=65 and 0 for age<65}
#'   \item{LBBB}{baseline covariate, 1 for LBBB group and 0 for non-LBBB group}
#'   \item{Female}{baseline covariate, 1 for female and 0 for male}
#'   \item{cost.1 ... cost.15}{yearly grouped observed costs during each of Year 1 to 15}
#'   \item{QALY.1 ... QALY.15}{yearly grouped observed QALY during each of Year 1 to 15}
#'   \item{tot.cost}{total observed costs, by summing variables cost.1 to cost.15}
#'   \item{tot.QALY}{total observed QALY, by summing variables QALY.1 to QALY.15}
#' }