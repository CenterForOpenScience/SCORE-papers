list(
  tar_target(repli_export,
             export_repli(rr_outcomes_dataset_p1,
                          replication_qa,
                          rr_reporting_checkin,
                          repli_input_changelog)),
  
  # Create replication analytic dataset
  tar_target(repli_outcomes,
             create_repli_analytic(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2))
)
