# Online supplement

This repository is an online supplement for the article entitled "**Gold in Them Tha-R Hills: A Review of R Packages for Exploratory Data Analysis**" in _Applied Economics Teaching Resources_ (forthcoming).

+ citation: TBA
+ Authors: Kota Minegishi and Taro Mieno

## RMarkdown file: 

+ **teaching_resource_1.5.Rmd** 

This is used to produce earlier drafts of the article. The body text slightly differs from the article itself due to subsequent editing in MS Word, but the R code chunks are exactly the same as those used in preparation for the final manuscript. 


## R files:
+ **ag_examples_A.R**

This is used to prepare data for section 4 "Data Exploration with dplyr", including US Agriculture census data (`us17`, `state17`, `county17`, `df_NAICS`, and `df_NAICS_simple`), and produce sankey flowchart diagrams. 

+ **ag_examples_B.R**

This is used to prepare data and analysis for section 5 "Analytical Demonstration". Historical population data and agricultural production data are merged and processed. Regression analysis is developed (which is subsequently copied to the RMarkdown file.) 

+ **helpers.R**

This contains functions to produce data maps of US counties. 

## Datasets:
The datasets contained in this folder allows one to replicate the results. Much of raw data are currently not included in the folder due to their large file sizes. Parts of the R code pertaining to raw data processing are commented out with "#" to avoid repeated data processing. Processed data are stored in the RData format, some of which are pseudo-raw data and others are data summaries.  
    
+ **Ag_census_2017.RData**
 
This is the main data extracted from 2017 US Agriculture Census, including datasets named `us17`, `state17`, and `county17`. 

+ `df_inventory_NAICS.RData`, `df_NAICS.RData`, `df_NAICS_simple.RData`

These are processed data summaries with respect to NAICS code.

+ `df_shape_counties.RData`, `geo_us_data.RData`

These are geography-based datasets for US county maps.

+ `df_county_pop1.Rdata`, `df_pop_grain.RData`

These are the datasets used in the Analytical Demonstration section. 





