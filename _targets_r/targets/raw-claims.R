list(
  # finalized_claim4_table.tsv
  tar_target(finalized_claim4_file,
             "17hohLFjMb0moxYpcFRb9-26Ei1QLFUgq"),
  tar_target(finalized_claim4_moddate,
             get_google_mod_date(finalized_claim4_file),
             cue = tar_cue("always")),
  tar_target(finalized_claim4_table,
             read_google_tsv(finalized_claim4_file,
                             finalized_claim4_moddate)),
  
  # tagtable_covid_p1.tsv
  tar_target(tagtable_covid_p1_file,
             "1lc2P9N2RPXR2YZB6_Zh1E8eXNiRdm50t"),
  tar_target(tagtable_covid_p1_moddate,
             get_google_mod_date(tagtable_covid_p1_file),
             cue = tar_cue("always")),
  tar_target(tagtable_covid_p1,
             read_google_tsv(tagtable_covid_p1_file,
                             tagtable_covid_p1_moddate))
)
