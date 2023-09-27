# README: `pipeline/` files

## data_processing

Contains scripts that load and transform data into forms that are ready to be used in analysis and figures.

- `helpers.R`: Subfunctions that are reused in multiple functions

### original_variables/

- `export_original_variables.R`: functions that merge and update raw original variables data.
- `load_ov_files.R`: functions to properly load in original variables data, check for obviously incorrect entries, and make any transformations before use in the data pipeline.

### rr/

- `export_replication.R`: functions that merge and update raw replication variables data
- `export_reproduction.R`: functions that merge and update raw reproduction variables data
- `load_rr_files.R`: functions to properly load in rr data, check for obviously incorrect entries, and make any transformations before use in the data pipeline.

### temp/

This folder holds temporary files generated when running the pipeline. In general, this folder should be empty.
