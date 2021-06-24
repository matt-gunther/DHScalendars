#' @title vcal_ppa
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_ppa
vcal_ppa <- function(dat){
  dat %>%
    mutate(ppam = case_when(
      grepl("x", vcal_ppa, ign = T) ~ T,
      vcal_ppa == "0" ~ F
    ))
}
