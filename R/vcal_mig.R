#' @title vcal_mig
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_mig
vcal_mig <- function(dat){
  dat %>%
    mutate(
      move = case_when(
        grepl("x", vcal_mig, ign = T) ~ T,
        vcal_mig != " " ~ F
      ),
      urban = case_when(
        vcal_mig == "1" ~ 1,
        vcal_mig == "2" ~ 2,
        vcal_mig == "3" ~ 3,
        vcal_mig == "4" ~ 4,
      )
    )
}
