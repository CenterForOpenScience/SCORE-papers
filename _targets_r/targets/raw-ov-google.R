list(
  # SCORE-P2-103: Original variables coding form (Responses)
  tar_target(orig_vars_qa_gsheet,
             "1_rgG3r318tpsakc8wSA0dMAYJ--pkiybVq5s9dfek6M"),
  tar_target(orig_vars_qa_mod_date,
             get_google_mod_date(orig_vars_qa_gsheet),
             cue = tar_cue("always")), 
  tar_target(orig_vars_qa,
             load_orig_vars_qa(orig_vars_qa_gsheet,
                               orig_vars_qa_mod_date,
                               valid_ids),
             cue = tar_cue("always"))
)
