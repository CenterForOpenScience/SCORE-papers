list(
  # orig_statistics_output_p2.txt
  tar_target(orig_statistics_output_p2_osf,
             "hk63r"),
  tar_target(orig_statistics_output_p2_mod_date, 
             get_osf_mod_date(orig_statistics_output_p2_osf),
             cue = tar_cue("always")), 
  tar_target(
    orig_statistics_output_p2,
    load_orig_statistics_output_p2(orig_statistics_output_p2_osf,
                                   orig_statistics_output_p2_mod_date)
  )
)
