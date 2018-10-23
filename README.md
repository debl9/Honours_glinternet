# Honours Project Appendix: GLINTERNET - A regularised approach to identify significant variables and interactions in personal loan data.

## Introduction
This page contains the supporting content in my Honours statistics thesis regarding hierarchical group-lasso 
regression and modelling pairwise interactions within a credit loan dataset. Included are all of the [R codes](https://github.com/debl9/Honours_glinternet/tree/master/Rcodes) 
used in data cleaning, model building and analysis & the complete data dictionary [part 1](https://github.com/debl9/Honours_glinternet/blob/master/data_dictionary1.jpeg), 
[part 2]((https://github.com/debl9/Honours_glinternet/blob/master/data_dictionary2.jpeg). Outputs can be found in the folder 
[Figures](https://github.com/debl9/Honours_glinternet/tree/master/Figures). 

## 1. Data Wrangling 
Codes for data cleaning, imputation and structuring it into continuous and categorical variables. 

[Data wrangling codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code1_data_wrangling.R)

## 2. GLMNET 
Applied generalised lasso regularisation to the the subset dataset, to select the significant main effect 
variables.

[GLMNET codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code3_glmnet.R)

![GitHub Logo](/Figures/plot_glmnet_lasso.png)

## 3. GLINTERNET
We used this package to fit a logistic model with overlapping group lasso to uncover underlying interactions.

[GLINTERNET variable selection/bootstrapping](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code4_glinternet.R)

## 4. Analysis 
### Mean AUC

For each value of lambda's, we observe the mean AUC (area under the ROC). We select the lambda within 1 standard error
of the maximum AUC because of our preference to underfit rather than overfit the model. 

[Codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code4_glinternet.R)

![GitHub Logo](/Figures/glint_mean_auc.png)

### ROC Curve Comparisons for 3 Regularised Models

![GitHub Logo](/Figures/bootstrap_models_rocs.png)

### Bootstrap Percentiles and Full Fit Coefficients

[Codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code6_extractcoefs_analysis.R)

![Percentiles](/Figures/glint_bootstrap_percentiles.png)

### Significant Variables selected via GLINTERNET

For the full table of [coefficients](https://github.com/debl9/Honours_glinternet/blob/master/full_coefs.csv).
Below is a snippet of the large main effects and interactions that has coefficients exceeding 0.05 in magnitude.

- Main Effects

![main_effects](/Figures/glint_main_effects.png)

- Interaction Effects

![Interactions](/Figures/glint_interaction_effects.png)

### ROC Analysis for GLINTERNET and its Optimal Threshold Cutoff 

[ROC Analysis Table](https://github.com/debl9/Honours_glinternet/blob/master/roc_table.csv)

Based on our loss function C = (1-p)a(1-specificity) + pb(1-sensitivity), the optimal threshold is 0.09. 

p = proportion of defaults in the data, a = cost of a false positive, b = cost of a false negative. 

[Codes](https://github.com/debl9/Honours_glinternet/blob/master/Rcodes/code8_roc_analysis.R)

![min_loss](/Figures/expected_loss_function.jpeg)