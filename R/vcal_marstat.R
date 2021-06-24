#' @title vcal_marstat
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_marstat
vcal_marstat <- function(dat){
  dat %>%
    mutate(married = case_when(
      grepl("x", vcal_marstat, ign = T) ~ T,
      vcal_marstat == 0 ~ F
    ))
}
