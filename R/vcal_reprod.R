#' @title vcal_reprod
#' @author Matt Gunther
#' @description description
#' @param dat description
#' @export vcal_reprod
vcal_reprod <- function(dat){

  # recode vcal_reprod using the recode CSV file
  dat <- suppressMessages(read_csv("resources/vcal_reprod_recodes.csv")) %>%
    filter(
      sample %in% c("all", dat$sample),
      year == "all" | year %in% dat$v007
    ) %>%
    filter(!duplicated(input, fromLast = T)) %>%
    select(input, output) %>%
    rename(vcal_reprod = input) %>%
    right_join(dat, by = "vcal_reprod") %>%
    mutate(vcal_reprod = if_else(
      is.na(output),
      suppressWarnings(as.numeric(vcal_reprod)),
      output
    )) %>%
    select(-output) %>%
    relocate(vcal_reprod, .after = cmc_month) %>%
    arrange(id, desc(cmc_month))

  contr_avail <- any(dat$vcal_reprod < 90 & !is.na(dat$vcal_reprod))
  eventpbt_avail <- any(dat$vcal_reprod %in% 100:300 & !is.na(dat$vcal_reprod))

  dat <- dat %>%
    mutate(
      caseid_cmc = paste0(caseid, "_", cmc_month),
      seq = as.integer(cmc_month - v017 + 1),
      birth = vcal_reprod == 100,
      preg = vcal_reprod == 200,
      term = vcal_reprod == 300,
      contr = case_when(contr_avail ~ vcal_reprod %in% 1:90),
      eventpbt = case_when(eventpbt_avail ~ birth | preg | term),
      eventwfp = eventpbt | contr,
    )

  dat <- dat %>%
    group_by(caseid) %>%
    mutate(
      switch = case_when(
        vcal_reprod %in% 1:90 &
          lead(vcal_reprod) != vcal_reprod & lead(vcal_reprod) < 90 ~ T,
        vcal_reprod %in% 1:90 &
          lead(vcal_reprod) == vcal_reprod & lead(vcal_reprod) < 90 ~ F
      ),
      switch_new = case_when(
        switch == T & lead(vcal_reprod) > 0 ~ T,
        switch == T & lead(vcal_reprod) == 0 ~ F,
        T ~ switch
      ),
      disc_event = case_when(
        vcal_reprod %in% 1:90 & lag(vcal_reprod) != vcal_reprod ~ T,
        vcal_reprod %in% 1:90 & lag(vcal_reprod) == vcal_reprod ~ F
      ),
      total_preg = case_when(eventpbt_avail ~ sum(eventpbt, na.rm = T)),
      count_birth = case_when(eventpbt_avail ~ sum(birth, na.rm = T)),
      count_term = case_when(eventpbt_avail ~ sum(term, na.rm = T)),
      contr_duration = case_when(contr_avail ~ sum(contr, na.rm = T))
    ) %>%
    ungroup()

  dat <- dat %>%
    filter(eventpbt) %>%
    select(id, cmc_month, v008, v017, preg, birth, term) %>%
    group_by(id) %>%
    mutate(
      preg_end = birth | term | (preg & cmc_month == v008),
      preg_rc = case_when(birth | term ~ F, preg_end ~ T),
      preg_lc = case_when(
        cmc_month == v017 ~ T,
        (birth | term) & (lead(preg) == FALSE | is.na(lead(preg))) ~ T
      ),
      preg_count = preg_end & !preg_rc,
      preg_count = cumsum(preg_count)
    ) %>%
    group_by(id, preg_count) %>%
    mutate(
      trunc = preg_count == 0,
      preg_count = case_when(
        any(preg_lc) ~ 0L,
        T ~ preg_count
      ),
      prfirst = any(preg_lc, na.rm = T),
      preg_length = case_when(
        trunc == FALSE & prfirst == FALSE ~ sum(preg)
      ),
      preg_flag = preg_length > 9
    ) %>%
    ungroup() %>%
    select(id, cmc_month, trunc, prfirst, preg_length, preg_flag) %>%
    full_join(dat, ., by = c("id", "cmc_month"))

  dat <- dat %>%
    mutate(reprod_event = vcal_reprod)

  return(dat)
}
