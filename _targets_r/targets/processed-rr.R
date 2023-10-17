list(
  tar_target(repli_export,
             export_repli(rr_outcomes_dataset_p1,
                          replication_qa,
                          rr_reporting_checkin,
                          repli_input_changelog)),
  
  tar_target(repli_primary,
             split_repli_primary(repli_export)),
  
  tar_target(repli_secondary,
             split_repli_secondary(repli_export,
                                   repli_primary)),
  
  tar_target(hybrid,
             split_hybrid(repli_export)),
  
  # Create replication analytic dataset
  tar_target(repli_outcomes,
             create_repli_analytic(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2))
)
