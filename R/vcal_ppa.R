#' @title vcal_ppa
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_ppa
vcal_ppa <- function(dat){
  dat %>%
    mutate(ppam = case_when(
      grepl("x", vcal_ppa, ign = T) ~ T,
      vcal_ppa == "0" ~ F
    ))
}
