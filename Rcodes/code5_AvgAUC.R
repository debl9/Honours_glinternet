
# Average AUC -------------------------------------------------------------

# This script contains the codes to calculate the AUC from the glinternet
# bootstrapped samples


# Load libraries and data -------------------------------------------------
library(pROC)
library(plyr)
library(foreach)
library(ggplot2)

load("bootstrapped_glint.rda") # variables selected via GLMNET with max 23 vars
load("glint_varselect.rda")

## Steps ## 
# Group the predictions by each nLambdas to calculate the mean AUC for each
# Observe which nLambda regularisation term produces the highest AUC and 
# corresponding model variables/coefficients

# Function to extract y predictions for each nLambda in each bootstrap
boot_slice <- function (o) {
  ypred <- o$ypred
  bootsamps <- o$bootsamps
  notbootsamps <- o$notbootsamps
  slice <- matrix(nrow=n, ncol=nLambda)
  slice[notbootsamps,] <- ypred
  return(slice)
}

n <- length(Y)
nLambda <- ncol(output[[1]]$ypred) # nLambda = 20
B <- length(output) # 50 bootstrap samples

# Extracting the predictions from each bootstrap sample (50)
OOBpred <- lapply(output, FUN = function (o) boot_slice(o))

# Function inputs for ONE bootstrap sample, output_pred, lambda value
auc_per_bs_per_lambda <- function (bs, opred, l) {
  notbootsamps <- opred[[bs]]$notbootsamps
  ypred <- (OOBpred[[bs]][,l])[!is.na(OOBpred[[bs]][,l])] # ignore NAs, ypred for each lambda value
  ypred <- exp(ypred)/(1+exp(ypred)) # Transform to obtain probability of default
  AUC_per_lambda <- pROC::auc(response = Y[notbootsamps], predictor = ypred)
  return(AUC_per_lambda) # AUC for each lambda
}

# Iterate auc_per_bs_per_lambda for all lambdas in each boostrap samples 
auc_per_bs <- function(bs) {
  sapply(1:nLambda, FUN = function(l) auc_per_bs_per_lambda(bs, output, l))
} 

# Generate AUC matrix
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
AUC_matrix <- foreach::foreach(b=1:B, .combine = "rbind") %dopar% {
  auc_per_bs(b)
}
parallel::stopCluster(cl)

# Generate the mean
meanauc_per_lambda <- colMeans(AUC_matrix)
sd_per_lambda <- apply(AUC_matrix, MARGIN = 2, sd)
lower_bound_lambda <- meanauc_per_lambda - sd_per_lambda
upper_bound_lambda <- meanauc_per_lambda + sd_per_lambda

# Bootstrap lambda results
auc_lambda_results <- data.frame(cbind(log_lambda = log(output[[1]]$model$lambda), 
                            mean_auc_per_lambda = meanauc_per_lambda, sd = sd_per_lambda, 
                            lower = lower_bound_lambda, upper = upper_bound_lambda))
max(auc_lambda_results[,2]) # 19th row gives maximum AUC from bootstrapped samples

# Selecting the lambda within 1 standard deviation
lmax <- which.max(auc_lambda_results$mean_auc_per_lambda)
target <- auc_lambda_results$mean_auc_per_lambda[lmax] - auc_lambda_results$sd[lmax]
l_1se <- min(which(auc_lambda_results$mean_auc_per_lambda > target)) 

auc_lambda_results[l_1se,]

# Plotting the AUC vs log(lambda)
ggplot(auc_lambda_results, aes(x = log_lambda, y = mean_auc_per_lambda)) +
  geom_errorbar(aes(ymin=mean_auc_per_lambda-sd, ymax=mean_auc_per_lambda+sd), 
                width=.1, col = "blue") +
  geom_point() +
  geom_vline(xintercept = c(-12.28277, -12.89199), linetype = "dotted") +
  ggtitle('Mean AUC')


# ROC of bootstraps -------------------------------------------------------
# 5 bootstrap samples examples
samps <- sample(1:50, 5, replace = F)
output_samps <- output[samps]
# Lambda within 1se of the max
l = 18

roc_samps <- function (bs, opred, l) {
  notbootsamps <- opred[[bs]]$notbootsamps
  ypred <- opred[[bs]]$ypred[,l] # ignore NAs, ypred for each lambda value
  ypred <- exp(ypred)/(1+exp(ypred)) # Transform to obtain probability of default
  ROC_out <- pROC::roc(response = as.factor(Y[notbootsamps]), predictor = ypred)
  return(ROC_out) # AUC for each lambda
}
roc_list <- list()
for (i in 1:length(samps)) {
  roc_list[[i]] = roc_samps(i, output_samps, l)
}

ggroc(list(roc_list[[1]], roc_list[[2]], roc_list[[3]], roc_list[[4]], roc_list[[5]]),
      legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  guides(color=guide_legend("Samples")) +
  ggtitle("Bootstrapped Samples ROC")


# SAVE --------------------------------------------------------------------

# save(auc_lambda_results, file = "auc_lambda_results.rda")
