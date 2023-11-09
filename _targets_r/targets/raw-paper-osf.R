list(
  # covid_ta2.csv
  tar_target(covid_ta2_osf,
             "yr3w8"),
  tar_target(covid_ta2_mod_date, 
             get_osf_mod_date(covid_ta2_osf),
             cue = tar_cue("always")), 
  tar_target(covid_ta2,
             load_osf_csv(covid_ta2_osf,
                          covid_ta2_mod_date))
)
