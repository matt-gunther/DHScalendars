#' @title vcal_ppabstain
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_ppabstain
vcal_ppabstain <- function(dat){
  dat %>%
    mutate(ppabs = case_when(
      grepl("x", vcal_ppabstain, ign = T) ~ T,
      vcal_ppabstain == "0" ~ F
    ))
}
