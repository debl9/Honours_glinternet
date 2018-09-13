# Appendix for Honours Project: GLINTERNET

## Introduction
This page contains the supporting content in my Honours statistics thesis regarding hierarchical group-lasso 
regression and modelling pairwise interactions using the `GLINTERNET` package. The following includes the R
codes for each section 

## 1. Data Wrangling 
Codes for data cleaning and structuring it into continuous and categorical variables.

[Data wrangling codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code1_data_wrangling.R)

## 2. VIF 
Variance inflation factor (VIF) was utilised to detect multicollinearity in the dataset amongst the continuous
variable and reduce the dimensions of the dataset. 

## 3. GLMNET 
Applied generalised lasso regularisation to the the subset dataset, to select the significant main effect 
variables. 

## 4. GLINTERNET
We used this package to fit a logistic model with overlapping group lasso to uncover underlying interactions. 

## 5. Analysis 
