#' @title vcal_discont
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export vcal_discont
vcal_discont <- function(dat){
  dat <- dat %>%
    mutate(disc_event = case_when(
      !all(is.na(vcal_discont)) & is.na(disc_event) ~ vcal_discont != " ",
      T ~ disc_event # vcal_discont not available | disc_event already defined
    )) %>%
    group_by(caseid) %>%
    mutate(disc_total = case_when(
      !all(is.na(vcal_discont)) ~ sum(disc_event, na.rm = T)
    )) %>%
    ungroup

  dat <- suppressMessages(read_csv("resources/vcal_discont_recodes.csv")) %>%
    filter(
      sample %in% c("all", dat$sample),
      year == "all" | year %in% dat$v007
    ) %>%
    filter(!duplicated(input, fromLast = T)) %>%
    select(input, output) %>%
    rename(vcal_discont = input) %>%
    right_join(dat, by = "vcal_discont") %>%
    mutate(vcal_discont = if_else(
      is.na(output),
      suppressWarnings(as.numeric(vcal_discont)),
      output
    )) %>%
    select(-output) %>%
    relocate(vcal_discont, .after = vcal_reprod) %>%
    arrange(id, desc(cmc_month)) %>%
    rename(reason = vcal_discont)

  return(dat)
}
