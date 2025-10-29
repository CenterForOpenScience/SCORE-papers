This folder contains the following files:
“analysis and knit.R” contains all the code to generate all stats and figures and to knit it into the placeholder manuscript document
“analyst data.RData” contains all datasets needed to generate the stats and figures
“common functions.R” contains helper functions for statistics and figures
“repli_binary.R” contains optional code pertaining to the binary replication measures
“template manuscript.docx” is the full manuscript file, where all statistics and figures are placeholders to be calculated
"Analyst package.Rproj" is an R Project file for setting the root directory
 
The template manuscript.docx contains a version of the manuscript with tagged statistics and text contained in {} brackets.
 
To reproduce those tagged statistics:
 
Preparation:
1. Extract or download all files noted above into the same folder
2. (Optional) Open the "Analyst package.Rproj" to ensure all files run from the correct directory
2. Open the “analysis and knit.R” file
3. Install any R packages needed (packages are current CRAN versions (see package versions below)

Generating tagged statistics and figures:
1) Run the "analysis and knit.R" file to prepare all functions needed
2) In the console, knit_manuscript() function
3) This will do as follows:
  a) Load all data and functions needed
  b) Generate the full set of statistics and figures into the R environment as two objects:
     i) "results_placeholder_stats" containing all the statistics contained within the manuscript, and 
     iii) "results_figures" containing all the figures as ggplot objects
  c) Search the template manuscript document for placeholders contained in {} brackets
  d) Replace all placeholders with calculated text and figures (i.e. "knit" the manuscript document) and create/replace the "knitted manuscript.docx" file
4) To access those statistics and figures in the global environment, simply locate them with $, as in results_placeholder_stats$n_claims or results_figures$figure_2
 
Tested with:
R version: 4.5.0 (2025-04-11)
Platform: aarch64-apple-darwin20
Running under: macOS 26.0.1

Knit also requires MS Office installed
 
No non-standard hardware required

Install time from scratch: 20 minutes
Typical run time: 5 minutes
Expected output: R objects containing all figures and statistics
 
Package versions as tested:
corrplot 0.95
cowplot 1.2.0
dplyr 1.1.4
DT 0.34.0
forcats 1.0.1
ggExtra 0.11.0
ggplot2 4.0.0
ggridges 0.5.7
ggside 0.4.0
glue 1.8.0
Hmisc 5.2-4
officer 0.7.0
pandoc 0.2.0
scales 1.4.0
stringr 1.5.2
tidyr 1.3.1
wCorr 1.9.8
weights 1.1.2
 
The MIT License (MIT)
Copyright (c) 2025 Center for Open Science
 
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 
The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.