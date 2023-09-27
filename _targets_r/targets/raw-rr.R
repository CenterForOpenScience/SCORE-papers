list(
  # changes/ ----
  # repli_input_changelog (Google Sheet)
  tar_target(repli_input_changelog_file,
             "1jbkVYHLinIN9zXBuPTok26bHjDt69-7D_Z7yVOin3mM"),
  tar_target(repli_input_changelog_moddate,
             get_google_mod_date(repli_input_changelog_file),
             cue = tar_cue("always")),
  tar_target(repli_input_changelog,
             load_changelog(repli_input_changelog_file,
                            repli_input_changelog_moddate)),
  
  # checkins/ ----
  # rr_reporting_checkin.tsv
  tar_target(rr_reporting_checkin_file,
             "1jB97FM-desZatqabwBVMPrSYk0qXWFjM"),
  tar_target(rr_reporting_checkin_moddate,
             get_google_mod_date(rr_reporting_checkin_file),
             cue = tar_cue("always")),
  tar_target(rr_reporting_checkin,
             read_google_tsv(rr_reporting_checkin_file,
                             rr_reporting_checkin_moddate)),
  
  # codebooks/ ----
  # p2_repli_vf.csv
  tar_target(p2_repli_vf_file,
             "1fwWX7ebwhBF9IYt2dQyhEByBnRNavOmj"),
  tar_target(p2_repli_vf_moddate,
             get_google_mod_date(p2_repli_vf_file),
             cue = tar_cue("always")),
  tar_target(p2_repli_vf,
             read_google_csv(p2_repli_vf_file,
                             p2_repli_vf_moddate)),
  
  # p2_repro_vf.csv
  tar_target(p2_repro_vf_file,
             "1_UJ_JbcoOHO-xRmDeTRJD2Apdzvqz2MX"),
  tar_target(p2_repro_vf_moddate,
             get_google_mod_date(p2_repro_vf_file),
             cue = tar_cue("always")),
  tar_target(p2_repro_vf,
             read_google_csv(p2_repro_vf_file,
                             p2_repro_vf_moddate)),
  
  # outcomes/ ----
  # rr_outcomes_dataset_p1.tsv
  tar_target(rr_outcomes_dataset_p1_file,
             "113CSsObKagvQToj-inv4bO3KaS5zif6D"),
  tar_target(rr_outcomes_dataset_p1_moddate,
             get_google_mod_date(rr_outcomes_dataset_p1_file),
             cue = tar_cue("always")),
  tar_target(rr_outcomes_dataset_p1,
             read_google_tsv(rr_outcomes_dataset_p1_file,
                             rr_outcomes_dataset_p1_moddate)),
  
  # project_minting/ ----
  # rr_attempts_minted.csv
  tar_target(rr_attempts_minted_file,
             "1aHuTwRVeqADWVC4Lxq5n3B4ebbSa9Mqc"),
  tar_target(rr_attempts_minted_moddate,
             get_google_mod_date(rr_attempts_minted_file),
             cue = tar_cue("always")),
  tar_target(rr_attempts_minted,
             load_rr_attempts_minted(rr_attempts_minted_file,
                                     rr_attempts_minted_moddate))
)

