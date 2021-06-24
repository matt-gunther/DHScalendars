#' @title vcal_term
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_term
vcal_term <- function(dat){
  is_avail <- !all(is.na(dat$vcal_term))

  dat %>%
    mutate(
      abort = case_when(
        vcal_term == "A" ~ T,
        vcal_term %in% c("M", "C", "S") ~ F
      ),
      miscar = case_when(
        vcal_term %in% c("M", "C") ~ T,
        vcal_term %in% c("A", "S") ~ F
      ),
      sbirth = case_when(
        vcal_term == "S" ~ T,
        vcal_term %in% c("M", "C", "A") ~ F
      )
    ) %>%
    group_by(caseid) %>%
    mutate(
      count_abort = case_when(is_avail ~ sum(abort, na.rm = T)),
      count_miscar = case_when(is_avail ~ sum(miscar, na.rm = T)),
      count_sbirth = case_when(is_avail ~ sum(sbirth, na.rm = T))
    ) %>%
    ungroup()
}
