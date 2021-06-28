#' @title Pregnancy Termination Calendar
#' @author Matt Gunther
#' @description Create all variables related to the Pregnancy Termination
#' calendar
#' (see details). If this calendar was not included in the sample (or if some
#' of the required information is not available), all variables will still be
#'  created, but all values will be NA.
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#' \itemize{
#'   \item{
#'     \strong{abort} Logical: TRUE if \code{vcal_term} is "A". FALSE if
#'     \code{vcal_term} is any other non-missing character. Not available if
#'     \code{vcal_term} is blank (e.g. " ") or not available (NA).
#'   }
#'   \item{
#'     \strong{count_abort} Integer: total number of months per person where
#'     \code{abort} is TRUE. Not available if \code{vcal_term} is not available
#'     (NA).
#'   }
#'   \item{
#'     \strong{miscar} Logical: TRUE if \code{vcal_term} is "M" or "C". FALSE
#'     if \code{vcal_term} is any other non-missing character. Not available if
#'     \code{vcal_term} is blank (e.g. " ") or not available (NA).
#'   }
#'   \item{
#'     \strong{count_miscar} Integer: total number of months per person where
#'     \code{miscar} is TRUE. Not available if \code{vcal_term} is not
#'     available (NA).
#'   }
#'   \item{
#'     \strong{sbirth} Logical: TRUE if \code{vcal_term} is "S". FALSE if
#'     \code{vcal_term} is any other non-missing character. Not available if
#'     \code{vcal_term} is blank (e.g. " ") or not available (NA).
#'   }
#'   \item{
#'     \strong{count_sbirth} Integer: total number of months per person where
#'     \code{sbirth} is TRUE. Not available if \code{vcal_term} is not
#'     available (NA).
#'   }
#' }
#' @param dat A data file created by \code{vcal_reprod()} (may be passed to
#' any other function starting with "vcal" first).
#' @export vcal_term
vcal_term <- function(dat){
  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # make new variable(s)
  is_avail <- !all(is.na(dat$vcal_term))

  dat <- dat %>%
    mutate(
      abort = case_when(
        vcal_term == "A" ~ T,
        vcal_term %in% c("M", "C", "S") ~ F
      ),
      miscar = case_when(
        vcal_term %in% c("M", "C") ~ T,
        vcal_term %in% c("A", "S") ~ F
      ),
      sbirth = case_when(
        vcal_term == "S" ~ T,
        vcal_term %in% c("M", "C", "A") ~ F
      )
    ) %>%
    group_by(caseid) %>%
    mutate(
      count_abort = case_when(is_avail ~ sum(abort, na.rm = T)),
      count_miscar = case_when(is_avail ~ sum(miscar, na.rm = T)),
      count_sbirth = case_when(is_avail ~ sum(sbirth, na.rm = T))
    ) %>%
    ungroup()


  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp

  return(dat)
}
