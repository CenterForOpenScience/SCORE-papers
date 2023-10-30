list(
  # File: SCORE-P2-081: Variable form - replications (Responses)
  tar_target(replication_qa_gsheet,
             "1Cs7iFLA29JwVDfp8DY0ZIaTlRTTnK0KsFSudLDHxjFQ"),
  tar_target(replication_qa,
             load_replication_qa(replication_qa_gsheet,
                                 p2_repli_vf,
                                 valid_ids,
                                 rr_attempts_minted),
             cue = tar_cue("always"))
  
  # # File: SCORE-P2-113: Variable form - reproductions (Responses)
  # tar_target(reproduction_qa_gsheet,
  #            "1vCVHLymiT-IcVGvWH_a3cJ4dfhjrf8lvmi4qe24LuZs"),
  # tar_target(reproduction_qa,
  #            load_reproduction_qa(reproduction_qa_gsheet,
  #                                 p2_repro_vf),
  #            cue = tar_cue("always"))
)
