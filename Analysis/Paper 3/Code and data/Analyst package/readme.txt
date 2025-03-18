This folder contains three files:
“tagged stats and figures.R” contains all the main analysis functions
“analyst data.RData” contains all datasets needed to generate the stats and figures
“common functions.R” contains helper functions for statistics and figures

The submission contains a version of the manuscript with tagged statistics and text contained in {} brackets.

To reproduce those tagged statistics:

Preparation:
1. Extract all files noted above into the same folder
2. Open the “tagged stats and figures.R” file
3. Install any R packages needed (packages are current CRAN versions as of Marcy 18, 2025)

Generating tagged statistics and figures:
1) Run the "tagged stats and figures.R" file to prepare all functions needed
2) In the console, generate_all() function ]
3) This will load all data and functions needed and generate the full set of statistics and figures into the R environment as two objects: "results_tagged_stats" containing all the statistics contained within the manuscript, and "results_figures" containing all the figures as ggplot objects
4) To access those statistics and figures, simply locate them with $, as in results_tagged_stats$n_claims or results_figures$figure_2

Tested with:
R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.3.2

No non-standard hardware required

Install time from scratch: 20 minutes
Typical run time: 5 minutes
Expected output: R objects containing all figures and statistics

Package versions as tested (March 18, 2025):
colorspace 2.1-1
corrplot 0.95
cowplot 1.1.3
DescTools 0.99.59
dplyr 1.1.4
DT 0.33
forcats 1.0.0
funkyheatmap 0.5.1
ggExtra 0.10.1
ggplot2 3.5.1
ggridges 0.5.6.9000
ggside 0.3.1
glue 1.8.0
googledrive 2.1.1
googlesheets4 1.1.1
Hmisc 5.2-2
lubridate 1.9.4
pbapply 1.7-2
purrr 1.0.4
readr 2.1.5
scales 1.3.0
stringr 1.5.1
targets 1.10.1
tibble 3.2.1
tidyr 1.3.1
tidyverse 2.0.0
wCorr 1.9.8
weights 1.0.4
zcurve 2.4.2

The MIT License (MIT)
Copyright (c) 2025 Center for Open Science

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.