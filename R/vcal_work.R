#' @title vcal_work
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_work
vcal_work <- function(dat){
  dat %>% mutate(empl = as.numeric(vcal_work))
}
