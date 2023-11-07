list(
  
  tar_target(pr_gsheet,
             "1E8LvpcuoUyVZV-EDqVPW_H1Cd-4OJCuR3Kl8HMJMVtI",
             cue = tar_cue("always")),
  tar_target(pr_data_raw,
             load_pr_data(pr_gsheet))
  
)
