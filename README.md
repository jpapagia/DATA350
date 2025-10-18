
# Sleep Duration, Difficulty, and Health Correlates Among U.S. Adults (NHANES 2009–2011)

![[cover.png]]

This repository contains an exploratory data analysis (EDA) project examining the relationships between sleep duration, difficulty sleeping, and various health and lifestyle factors among U.S. adults.  

The analysis was performed using the **NHANESraw (2009–2011)** dataset collected by the Centers for Disease Control and Prevention (CDC).

## Overview

This project investigates how demographic characteristics, physical health, and lifestyle factors such as smoking and alcohol use are associated with sleep quality and duration.  

All figures, tables, and statistical summaries presented in the final report can be **fully reproduced** using the included dataset and R code.

## Requirements

This analysis was conducted in **R** using the following packages:

```r
tidyverse
gridExtra
scales
broom
````

Ensure these are installed before running the `.Rmd` file:

```r
install.packages(c("tidyverse", "gridExtra", "scales", "broom"))
```

## Repository Structure

```
├── DATA350_Project_1.Rmd      # Main R Markdown analysis report
├── NHANESraw.csv              # Required dataset (must be in working directory)
├── Figures/                   # Screenshots of figures, code snippets, and stats outputs
└── README.md                  # Project documentation (this file)
```

## Figures Directory

The `Figures/` directory contains:

- Individual screenshots of each figure
- The R code used to produce them
- Corresponding statistical test outputs (e.g., correlation tables, summary results)

## Refrences

Buysse, D. J. (2014). Sleep health: Can we define it? Does it matter? Sleep, 37(1), 9–17. [https://doi.org/10.5665/sleep.3298](https://doi.org/10.5665/sleep.3298)

Cappuccio, F. P., D’Elia, L., Strazzullo, P., & Miller, M. A. (2010). Sleep duration and all-cause mortality: A systematic review and meta-analysis of prospective studies. Sleep, 33(5), 585–592. [https://doi.org/10.1093/sleep/33.5.585  
  
](https://doi.org/10.1093/sleep/33.5.585)

Centers for Disease Control and Prevention (CDC). (2013). National Health and Nutrition Examination Survey: Analytic guidelines, 2011–2012. U.S. Department of Health and Human Services. [https://www.cdc.gov/nchs/nhanes](https://www.cdc.gov/nchs/nhanes)

Itani, O., Jike, M., Watanabe, N., & Kaneita, Y. (2017). Short sleep duration and health outcomes: A systematic review, meta-analysis, and meta-regression. Sleep Medicine, 32, 246–256. [https://doi.org/10.1016/j.sleep.2016.08.006](https://doi.org/10.1016/j.sleep.2016.08.006)
  
MedPsych Health. (n.d.). Woman sleeping peacefully in bed [Photograph]. MedPsych Health. [https://www.medpsychhealth.com/ wp-content/uploads/shutterstock_1427337869-1440x810.jpg](https://www.medpsychhealth.com/wp-content/uploads/shutterstock_1427337869-1440x810.jpg)

**Authors:** Yianni Papagiannopoulos, Madhavan Narkeeran, Alexandra Julka  
**Date:** October 15, 2025  
**Course:** DATA350 – Exploratory Data Analysis  
**Institution:** University of Maryland
