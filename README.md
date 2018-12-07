# Resolving the measurement uncertainty paradox in ecological management


[![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/boettiger-lab/pomdp-intro/master?urlpath=rstudio)
[![Build Status](https://travis-ci.com/boettiger-lab/pomdp-intro.svg?token=HrMbVv2Gfn8BzLNkrr1q&branch=master)](https://travis-ci.com/boettiger-lab/pomdp-intro)

- Authors: Milad Memarzadeh, Carl Boettiger

- :file_folder: [Manuscript](/manuscripts): R Markdown source document for manuscript. Includes code to reproduce for figures from tables generated by the analysis.
- :file_folder: [Appendix](/appendix): R Markdown source documents for both appendices, containing all necessary R code to generate all results presented in both the manuscript and appendices.  
- :file_folder: [data](/data): Data generated in the analysis.  Includes `.csv` tables shown in the figures, and the `.policyx` XML files generated by running the SARSOP algorithm. 
- :file_folder: [reviews](/reviews): Encrypted reviews (copywrite of the reviewers) 


## Reproducibility


This repository is organized as a reproducible research compendium. 
Click the ![Binder](http://mybinder.org/badge.svg) button above to explore in an interactive RStudio session.   Binder uses [rocker-project.org](https://rocker-project.org) Docker images to ensure a consistent and reproducible computational environment.  These Docker images can also be used locally.  

To explore the code locally, clone or download this repository into RStudio or your perferred environment and install the compendium by running `devtools::install()`.  To install additional dependencies used only in formatting the figures, use `devtools::install(dep=TRUE)`.  


This compendium is checked by Travis-CI continuous integration.  Click the ![Build Status](https://travis-ci.org/cboettig/noise-phenomena.svg?branch=master) button above for details. 

