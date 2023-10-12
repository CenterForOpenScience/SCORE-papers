list(
  # rr_statistics_output_p2.txt
  tar_target(rr_statistics_output_p2_osf,
             "6u7hc"),
  tar_target(rr_statistics_output_p2_mod_date, 
             get_osf_mod_date(rr_statistics_output_p2_osf),
             cue = tar_cue("always")), 
  tar_target(
    rr_statistics_output_p2,
    load_rr_statistics_output_p2(rr_statistics_output_p2_osf,
                                 rr_statistics_output_p2_mod_date)
  )
)
