#' @title cleanup_months
#' @description description
#' @param dat dat
#' @export cleanup_months
cleanup_months <- function(dat){
  dat %>% filter(cmc_month <= v008)  # no more future months
}
