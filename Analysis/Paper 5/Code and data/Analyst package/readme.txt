This folder contains three files:
“tagged stats and figures.R” contains all the main analysis functions
“analyst data.RData” contains all datasets needed to generate the stats and figures
“common functions.R” contains helper functions for statistics and figures
“repli_binary.R” contains a function to generate the binary replication success measures

The submission contains a version of the manuscript with tagged statistics and text contained in {} brackets.

To reproduce those tagged statistics:

Preparation:
1. Extract all files noted above into the same folder
2. Open the “tagged stats and figures.R” file
3. Install any R packages needed (packages are current CRAN versions as of Friday Feb 28, 2025)
Important Note: ggridges uses the Github version. To install, run devtools::install_github("wilkelab/ggridges")

Generating tagged statistics and figures:
1) Run the "tagged stats and figures.R" file to prepare all functions needed
2) In the console, generate_all() function ]
3) This will load all data and functions needed and generate the full set of statistics and figures into the R environment as two objects: "results_tagged_stats" containing all the statistics contained within the manuscript, and "results_figures" containing all the figures as ggplot objects
4) To access those statistics and figures, simply locate them with $, as in results_tagged_stats$n_journals or results_figures$figure_2