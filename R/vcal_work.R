#' @title vcal_work
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_work
vcal_work <- function(dat){
  dat %>% mutate(empl = as.numeric(vcal_work))
}
