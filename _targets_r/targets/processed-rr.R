list(
  # Full replication dataset
  tar_target(repli_export,
             export_repli(rr_outcomes_dataset_p1,
                          replication_qa,
                          rr_reporting_checkin,
                          repli_input_changelog)),
  
  # Create replication analytic dataset
  tar_target(repli_outcomes,
             create_repli_analytic(repli_export,
                                  rr_attempts_minted,
                                  rr_statistics_output_p2)),
  
  # Full reproduction dataset
  tar_target(repro_export,
             export_repro(reproduction_qa,
                         p2_repro_input_changelog))
)
