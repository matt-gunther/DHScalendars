#' @title vcal_marstat
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_marstat
vcal_marstat <- function(dat){
  dat %>%
    mutate(married = case_when(
      grepl("x", vcal_marstat, ign = T) ~ T,
      vcal_marstat == 0 ~ F
    ))
}
