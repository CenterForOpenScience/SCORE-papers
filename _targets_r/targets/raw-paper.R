list(
  # status.csv
  tar_target(status_file,
             "1lETiML-WFVGK1MS0ELCZejIaXnDDXYWh"),
  tar_target(status_moddate,
             get_google_mod_date(status_file),
             cue = tar_cue("always")),
  tar_target(status,
             read_google_csv(status_file,
                             status_moddate))
)
