list(
  # orig_statistics_dataset_p1.tsv
  tar_target(orig_statistics_p1_file,
             "1TLm9loSU8n3pSRoz0ugekJ6VfSiS792p"),
  tar_target(orig_statistics_p1_moddate,
             get_google_mod_date(orig_statistics_p1_file),
             cue = tar_cue("always")),
  tar_target(orig_statistics_dataset_p1,
             load_orig_statistics_p1(orig_statistics_p1_file,
                                     valid_ids,
                                     orig_statistics_p1_moddate)),
  
  # orig_statistics_manual_dataset.tsv
  tar_target(orig_statistics_manual_file,
             "1B8-fTy_wpCcwHJAGt8FZKIuIoeHeE0T6"),
  tar_target(orig_statistics_manual_moddate,
             get_google_mod_date(orig_statistics_manual_file),
             cue = tar_cue("always")),
  tar_target(orig_statistics_manual_data_entry,
             load_orig_statistics_manual(orig_statistics_manual_file,
                                         valid_ids,
                                         orig_statistics_manual_moddate)),
  
  # original_inftest_dataset.tsv
  tar_target(original_inftest_file,
             "1goGxpXYcpCWNxBa1HE1oEEifVFlUOix7"),
  tar_target(original_inftest_moddate,
             get_google_mod_date(original_inftest_file),
             cue = tar_cue("always")),
  tar_target(original_inftest_dataset,
             load_original_inftest_dataset(original_inftest_file,
                                           valid_ids,
                                           original_inftest_moddate)),
  
  # changes/ ----
  # orig_input_changelog (Google Sheet)
  tar_target(orig_input_changelog_file,
             "1M8H_76ajxwdhVuuSyIo18BxRrJbMF3hXpAkzYY39g2k"),
  tar_target(orig_input_changelog_moddate,
             get_google_mod_date(orig_input_changelog_file),
             cue = tar_cue("always")),
  tar_target(orig_input_changelog,
             load_changelog(orig_input_changelog_file,
                            orig_input_changelog_moddate))
)
