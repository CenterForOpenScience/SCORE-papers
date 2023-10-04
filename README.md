# SCORE Papers

This project uses the R packages `renv` and `{targets}` to facilitate reproducibility. 

## Quick Start Guide

For first time setup of this repository, open the project in R Studio and run `renv::restore()` in the console. This will install **all** packages used in the project in the versions that were used during development. If you are having issues with `renv::restore()`, make sure your installation of R and its [mandatory tools](https://mac.r-project.org/tools/) are up to date.

Next, run `usethis::edit_r_environ()` in the console to open your .Renviron file. Enter 'google_oauth_email="youremail\@cos.io"' as a new line in the file, making sure to replace "youremail\@cos.io" with your actual email within quotations. This will allow Google to silently authenticate without needing to change any code. If you have not used the packages `googledrive` or `googlesheets4` with the email denoted in the .Renviron file, `run options(gargle_oauth_email = Sys.getenv("google_oauth_email"))` in the console, then run `googledrive::drive_auth()` and `googlesheets4::gs4_auth()` in the console to complete a one-time authentication through your browser. If you have not used osfr before, you may also need to complete authentication using `osfr::osf_auth()` and a token from [OSF](https://osf.io/settings/tokens/). See the [osfr reference manual](https://cran.r-project.org/web/packages/osfr/osfr.pdf) for more information.

Finally, run `rmarkdown::render("targets.Rmd")` in the console. This will complete the first time set up of the reproducible targets pipeline on your computer. 

Please read through the rest of this document for more information on `renv`, `{targets}`, and best practices for working on project code. 

## renv

The R package [renv](https://rstudio.github.io/renv/articles/renv.html) is used to create a reproducible environment for the project by archiving a private library of packages and versions used. When you open this project for the first time, renv will automatically bootstrap itself, downloading and installing the appropriate version of renv. It will also ask if you want to download and install all the packages the project needs by running `renv::restore()`. The packages (and all the metadata needed to reinstall them) are recorded into a lockfile, `renv.lock`. `.Rprofile` runs `renv/activate.R` to ensure that the library is used every time you open the project. If you update a package or install another package, use `renv::snapshot()` to record the new packages/versions and their sources in the lockfile.

## {targets}

The R package [\{targets\}](https://books.ropensci.org/targets/) is a pipeline tool. It watches the dependency graph of the whole project workflow to update only steps, or "targets", whose code, data, and upstream dependencies have changed since the last run of the pipeline, and skip targets that have not changed since the last run. The read-only file `_targets.R` configures and defines the pipeline. The function `targets::tar_visnetwork()` displays the dependency graph of the pipeline. The function `targets::tar_make()` runs the pipeline and stores targets and their metadata in the folder `_targets/`. The functions `tar::tar_read()` and `tar::tar_load()` read and load a target's return value, respectively. The file `targets.Rmd` is used to generate `_targets.R`. Knitting `targets.Rmd` also runs `targets::tar_make()` and `targets::tar_visnetwork()`. Rendering this Rmd file writes scripts to a special `_targets_r/` directory to define individual targets and global objects. The `targets.html` file rendered from the Rmd file can be used to view the documentation for all targets in the pipeline. 

### Adding to the {targets} pipeline

The targets pipeline requires users to adopt a [function-oriented](https://books.ropensci.org/targets/functions.html) style of programming. All data transformations (including any model and figure generation) must be encapsulated within discrete functions that use any number of targets as inputs and create a single output. Once a function has been tested and is ready to add to the pipeline, you will need to add a new target or list of targets to `targets.Rmd`. See the [targets](https://books.ropensci.org/targets/targets.html) package documentation for more information on making a good target.

After identifying an appropriate section to place the new targets, insert a new code chunk into the Rmd with the type "targets" (e.g. ```````{targets}````). It is best practice to also name the code chunk so that it can be more easily found in the future (e.g. ````{targets chunk-name}`). 

A single target is defined using `tar_target(name, command)`, where "name" defines how the target will be referred to in the pipeline and "command" is the R code to run the target. "Command" is often either a string (in the case of when the target is a file identifier) or a function. To initialize multiple targets within a single code chunk, use a list (e.g. `list(tar_target(name, command), tar_target(name2, command2))`). Run the code chunk to add the targets to the `_targets_r/targets/` folder, then run `targets::tar_visnetwork()` to make sure the new targets are placed where they are expected in the pipeline dependency tree. 

Coordinate with the COS research team data manager if necessary to find a logical place within the targets Rmd file to place your new targets or if you have any questions about this process.

## Repository Structure

Files for the SCORE project are separated by type into `pipeline/`, and `upkeep/`. The folders `pipeline/` and `upkeep/` both contain scripts used to generate and update SCORE data. Functions in the `pipeline/` folder are used in the targets pipeline and must be a part of an acyclic workflow. Functions in the `upkeep/` folder are cyclic (they input and output the same file) or run on a more complicated schedule, so they must be kept separate. Each of these subfolders contains a README file which documents their contents.

## Rules of Engagement

Currently, all data generated as part of this initiative should be treated as sensitive. **Do not share with outside parties.** This repository is meant to be branched only. No forks.

All code within the pipeline/data_processing folder follows the [tidyverse style guide](https://style.tidyverse.org/). It is highly recommended to use this style guide when adding additional code to this repository.
