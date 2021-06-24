#' @title testing
#' @author Matt Gunther
#' @description description
#' @param samp description
#' @export testing
testing <- function(samp){

  maryia <- samp %>%
    gsub("ir", "_lr.csv.gz", x = .) %>%
    paste0("/pkg/ipums/dhs/general/calendar/programming/unlinked_lr_data/", .) %>%
    read.csv() %>%
    tibble() %>%
    mutate(
      switch = case_when(
        event == 0 ~ as.double(0),
        T ~ as.double(switch)
      ),
      switch_new = case_when(
        event == 0 ~ as.double(0),
        T ~ as.double(switch_new)
      )
    )

  # dat <- samp %>%
  #   read_dhs_dict_as_ddi() %>%
  #   get_vcal_input_data() %>%
  #   reshape_vcal_input_data(sample_tracking) %>%
  #   vcal_reprod() %>%
  #   vcal_discont() %>%
  #   vcal_ppa() %>%
  #   vcal_ppabstain() %>%
  #   vcal_bfeed() %>%
  #   vcal_marstat() %>%
  #   vcal_mig() %>%
  #   vcal_work() %>%
  #   vcal_term() %>%
  #   vcal_ultra() %>%
  #   cleanup_months()

  dat <- paste0(samp, "_long.rds") %>%
    file.path("output", .) %>%
    read_rds() %>%
    rename_with(~gsub("^cal", "", .x))


  names(dat)
  names(maryia)

  ## vcal_reprod (not preg length related)
  dat %>% count(eventwfp, eventpbt, birth, preg, term, contr)
  maryia %>% count(birth, preg, term, contr)

  dat %>% count(count_birth)
  maryia %>% count(count_birth)

  dat %>% count(count_term)
  maryia %>% count(count_term)

  dat %>% count(total_preg)
  maryia %>% count(total_preg)


  dat %>% count(contr_duration)
  maryia %>% count(contr_duration)

  dat %>% count(switch)
  maryia %>% count(switch)

  dat %>% count(switch_new)
  maryia %>% count(switch_new)

  ## preg length related
  dat %>% count(preg_length)
  dat %>% count(preg_flag)
  dat %>% count(trunc)
  dat %>% count(prfirst)

  ## disc
  dat %>% count(reason) %>% print(n= Inf)
  maryia %>% count(reason) %>% print(n= Inf)

  dat %>% count(disc_total)
  maryia %>% count(disc_total)

  dat %>% count(disc_event)
  maryia %>% count(disc_event)

  ## limited availability
  dat %>% count(ultrasound)
  maryia %>% count(ultrasound)

  dat %>% count(sbirth, miscar, abort)
  maryia %>% count(sbirth, misc, abort)
  dat %>% count(count_sbirth)
  maryia %>% count(count_sbirth)

  dat %>% count(empl)
  maryia %>% count(empl)

  dat %>% count(urban)
  maryia %>% count(urban)

  dat %>% count(move)

  dat %>% count(breastf)
  maryia %>% count(breastf)

  dat %>% count(ppabs)
  maryia %>% count(ppabs)

  dat %>% count(ppam)
  maryia %>% count(ppam)

  dat %>% count(married)
  maryia %>% count(married)
}

