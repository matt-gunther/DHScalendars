#' @title vcal_bfeed
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_bfeed
vcal_bfeed <- function(dat){
  dat %>%
    mutate(breastf = case_when(
      grepl("x", vcal_bfeed, ign = T) ~ 1,
      grepl("N", vcal_bfeed, ign = T) ~ 2,
      vcal_bfeed == "0" ~ 0
    ))
}
