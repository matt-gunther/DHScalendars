#' @title vcal_ultra
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_ultra
vcal_ultra <- function(dat){
  dat %>%
    mutate(
      ultrasound = case_when(
        vcal_ultra == "Y" ~ T,
        vcal_ultra == "N" ~ F
      )
    )
}
