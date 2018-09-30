# Appendix for Honours Project: GLINTERNET.

## Introduction
This page contains the supporting content in my Honours statistics thesis regarding hierarchical group-lasso 
regression and modelling pairwise interactions within a credit loan dataset. Included are all of the R codes 
used in data cleaning, building models and analysis. Outputs can be found in the folder 
[Figures](https://github.com/debl9/Honours_glinternet/tree/master/Figures).

## 1. Data Wrangling 
Codes for data cleaning, imputation and structuring it into continuous and categorical variables. 

[Data wrangling codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code1_data_wrangling.R)

## 2. VIF 
Variance inflation factor (VIF) was utilised to detect multicollinearity in the dataset amongst the continuous
variable and reduce the dimensions of the dataset. 

[VIF codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code2_vif.R)

## 3. GLMNET 
Applied generalised lasso regularisation to the the subset dataset, to select the significant main effect 
variables.

[GLMNET codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code3_glmnet.R)

![GitHub Logo](/Figures/glmnet_lasso.png)

## 4. GLINTERNET
We used this package to fit a logistic model with overlapping group lasso to uncover underlying interactions.

[GLINTERNET variable selection/bootstrapping](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code4_glinternet.R)

## 5. Analysis 
### Mean AUC

For each value of lambda's, we observe the mean AUC (area under the ROC). We select the lambda within 1 standard error
of the maximum lambda. 

[Codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code4_glinternet.R)

![GitHub Logo](/Figures/mean_auc2.png)

### GLINTERNET ROC of 5 Bootstrap Samples

![GitHub Logo](/Figures/bs_roc2.png)

### Bootstrap Percentiles and Full Fit Coefficients

[Codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code6_extractcoefs_analysis.R)

![Percentiles](/Figures/bootstrap_percentiles.PNG)
