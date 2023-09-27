list(
  # rr_projects.csv
  tar_target(rr_projects_file,
             "11paXdFzhCmz8ywFomUhZ6qrfp1wlKLIx"),
  tar_target(rr_projects_moddate,
             get_google_mod_date(rr_projects_file),
             cue = tar_cue("always")),
  tar_target(rr_projects,
             read_google_csv(rr_projects_file,
                             rr_projects_moddate)),

  tar_target(valid_ids,
             create_id_list(status,
                            tagtable_covid_p1,
                            finalized_claim4_table,
                            rr_projects))
)
