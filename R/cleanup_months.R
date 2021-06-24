#' @title cleanup_months
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export cleanup_months
cleanup_months <- function(dat){
  dat %>% filter(cmc_month <= v008)  # no more future months
}
