list(
  
  tar_target(pr_gsheet,
             "1E8LvpcuoUyVZV-EDqVPW_H1Cd-4OJCuR3Kl8HMJMVtI",
             cue = tar_cue("always")),
  tar_target(pr_data_raw,
             load_pr_data(pr_gsheet)),
  
  tar_target(pr_changelog_gsheet,
             "1zek9Lu-s303ZXs3B84h2vy-sJ1avOZSgpZEsna40pdA"),
  tar_target(pr_changelog_moddate,
             get_google_mod_date(pr_changelog_gsheet),
             cue = tar_cue("always")),
  tar_target(pr_input_changelog,
             load_changelog(pr_changelog_gsheet,
                            pr_changelog_moddate))
  
)
