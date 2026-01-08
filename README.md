# SCORE Papers

The code in this repository can be used to reproduce the data and analysis used during the SCORE project. 

## Repository Structure

Project code is largely separated by type into `Analysis/`, `pipeline/`, and `upkeep/`. 

- `Analysis/`: contains code for data analysis
- `pipeline/`: contains code run by the targets-based data pipeline
- `upkeep/`: contains code that is mostly internal-process focused. These files are not used in any final data processing or analysis, but are included for transparency and general interest.

## Packages

This project uses the following R packages:

- BayesFactor
- BayesRep
- BFF (Version 4.2.1 REQUIRED)
- dplyr
- effectsize
- ggExtra
- glue
- googlesheets4
- googledrive
- here
- kableExtra
- lubridate
- metaBMA
- metafor
- osfr
- purrr
- pwr
- readr
- ReplicationSuccess
- rmarkdown
- sjPlot
- stringr
- targets
- tibble
- tidyr  
- usethis
- visNetwork
               
The following code can be used to install all of these packages:

```
install.packages(
  c(BayesFactor,
    BayesRep,
    dplyr,
    effectsize,
    ggExtra,
    glue,
    googlesheets4,
    googledrive,
    here,
    kableExtra,
    lubridate,
    metaBMA,
    metafor,
    osfr, 
    purrr, 
    pwr,
    readr, 
    ReplicationSuccess,
    rmarkdown,
    sjPlot,
    stringr,
    targets,
    tibble,
    tidyr,
    usethis,
    visNetwork)
)

# An older version of the BFF package is required
# Newer versions of this package will cause errors in the data pipeline
pkg <- "https://cran.r-project.org/src/contrib/Archive/BFF/BFF_4.2.1.tar.gz"

install.packages(pkg, repos=NULL, type="source")

```

The data pipeline uses the R package [osfr](https://cran.r-project.org/web/packages/osfr/index.html) to download raw data from OSF. If you have not used osfr before, you may also need to complete authentication using `osfr::osf_auth()` and a token from [OSF](https://osf.io/settings/tokens/). See the [osfr reference manual](https://cran.r-project.org/web/packages/osfr/osfr.pdf) for more information.

## The Data Pipeline

The code in the `pipeline/` folder can be used to reproduce the analytic datasets used in the SCORE project from the raw data found on [Project-Wide Data OSF Project](https://osf.io/x9fb6/overview). 

This project uses the R package `{targets}` to facilitate reproducibility. Targets is a [make](https://www.gnu.org/software/make/)-like tool for reproducibly running the code that generates project products, such as analytic datasets, models, and figures. 

The file `targets.Rmd` and the file it generates, `targets.html`, contains the human-readable documentation of all data products generated and the functions that are used to create them. Rendering this file using `rmarkdown::render("targets.Rmd")` executes a series of functions in the correct order to build target files (also known as "targets", e.g., intermediate data files, analytic datasets, figures) from source files (e.g., raw data). It also documents the dependencies of the target files, which includes all files (whether source files or other targets) which are used as inputs to the functions used to generate the target file. It also generates the read-only file `_targets.R`, which configures and defines the pipeline.

### Running the targets pipeline

Run `rmarkdown::render("targets.Rmd")` in the console to initiate reproducible targets pipeline on your computer. Please note that this process may run for several minutes and will download files from the OSF when it is first run. See `targets.Rmd` for more details.

After running `rmarkdown::render("targets.Rmd")`, during first time setup, you will have access to all SCORE data products. To load any particular dataset into the R environment, use `targets::tar_load()` with the name of the target you want as the parameter. Multiple datasets can be loaded in at one time by using a concatenated list of target names (e.g., `targets::tar_load(c(repli_outcomes, repro_outcomes))`). Target names are documented in `targets.Rmd`. 

### Useful targets functions:

- `targets::tar_make()` runs the pipeline of functions and stores data and their metadata in the folder `_targets/`. Rendering `targets.Rmd` is required for first-time set up, but `tar_make()` can be used subsequently to re-run the pipeline.
- `targets::tar_visnetwork()`: displays the dependency graph of of all project files.  
- `tar::tar_read()` and `tar::tar_load()`: read and load a target's return value, respectively.  

Knitting `targets.Rmd` also runs `targets::tar_make()` and `targets::tar_visnetwork()`. Rendering this Rmd file writes scripts to a special `_targets_r/` directory to define individual targets and global objects. The `targets.html` file rendered from the Rmd file can be used to view the documentation for all targets in the pipeline. 
