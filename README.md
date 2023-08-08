# vaccination-india
Research projects assessing impacts of flooding on health in Bangladesh

## Description

This repository includes R code to run all of the analysis for the paper:

"Spatial targeting and integration across multiple outcomes to inform program delivery: A case study of vaccination, vitamin A and deworming programs throughout India."

Rerolle, F., Arnab, K. D., Benmarhnia, T., Arnold, B. F.

Should you have any questions about the files in this repository, please contact Francois Rerolle at UCSF (francois.rerolle@ucsf.edu).

## Additional Resources

### Open Science Framework 

All data used in the analysis is publicly available but access must be granted by the DHS program for the population surveys conducted in India (2019-2021). When requesting data, include GPS coordinates and spatial covariates data. The other data are deposited on OSF: https://osf.io/vrfmz/

### System Requirements

All analyses were run using R software version 4.1.1 on Mac OSX Big Sur using the RStudio IDE (https://www.rstudio.com).

> sessionInfo()

R version 4.3.0 (2023-04-21)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.4

### Installation Guide

You can download and install R from CRAN: https://cran.r-project.org

You can download and install RStudio from their website: https://www.rstudio.com

All R packages required to run the analyses are sourced in the file `0-config.R`.

The installation time should be < 10 minutes total on a typical desktop computer.

### Instructions for Use

To reproduce all analyses in the paper, we recommend that you: 

1. clone the GitHub repository

2. Create a `data` directory with 2 subdirectories: `untouched` and `final`.

3. In the `data/untouched` directory copy and paste repository from OSF: https://osf.io/vrfmz/files/osfstorage

4. In the `data/untouched/dhs` directory paste downloaded DHS data. You should have 2 subdirectories: `IAGE7AFL` and `IAKR7EDT`

5. Create `child-mortality-dhs\output` directory with a `figures` subdirectory

6. All of the analysis scripts should run smoothly. 

Running the all analyses on the above Mac laptop configuration required ~15min. 
