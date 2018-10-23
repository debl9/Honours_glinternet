
# ROC Analysis Codes ------------------------------------------------------
# This script is used to generate the ROC summary statistics and cost function to 
# determine the optimal threshold cut-off.

# Loading necessary files and libraries

library(pROC)
load(file = "bootstrapped_glint.rda")   # glinternet bootstraps

boot_slice <- function (o) {
  ypred <- o$ypred
  bootsamps <- o$bootsamps
  notbootsamps <- o$notbootsamps
  slice <- matrix(nrow=n, ncol=nLambda)
  slice[notbootsamps,] <- ypred
  return(slice)
}
n <- length(Y)
nLambda <- ncol(output[[1]]$ypred)
B <- length(output) # 50 bootstrap samples

# Extracting the predictions from each bootstrap sample (50)
OOBpred <- lapply(output, FUN = function (o) boot_slice(o))

predvstrue <- function (bs, opred, l) {
  notbootsamps <- opred[[bs]]$notbootsamps
  ypred <- (OOBpred[[bs]][,l])[!is.na(OOBpred[[bs]][,l])] # ignore NAs, ypred for each lambda value
  ypred <- exp(ypred)/(1+exp(ypred)) # Transform to obtain probabilities of default
  best_df <- data.frame(Y[notbootsamps], ypred)
  return(best_df) 
}

# Choose the optimal lambda from our glinternet results to be 'l=19'
l=19
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
library(foreach)
predvstrue_df <- foreach::foreach(bs=1:B, .combine = "rbind") %dopar% {
  predvstrue(bs, opred = output, l)
}
parallel::stopCluster(cl)


# AUC Table ---------------------------------------------------------------
rocval <- pROC::roc(predvstrue_df$Y.notbootsamps., predvstrue_df$ypred)
roctable1 <- pROC::coords(rocval, x = seq(0.01,0.9,0.01), ret = c("accuracy", "ppv", "npv",
                                                                  "sensitivity", "specificity"))

pROC::coords(rocval, x = "best", ret = c("accuracy", "ppv", "npv",
                                                   "sensitivity", "specificity",
                                                   "threshold"))

# Calculate Optimal Cutoff Threshold --------------------------------------
# C = (1-p)*a*(1-specificity) + p*b*(1-sensitivity)
# a = cost of a false positive (i.e. non defaulter classified as defaulter)
# b = cost of a false negative (i.e. defaulter classified as non-defaulter)
a = 1
b = 10 # cost of missing a default
p = sum(predvstrue_df$Y.notbootsamps.)/(length(predvstrue_df$Y.notbootsamps.))

# Calculate the expected loss between false positive/false negative 

expected_loss <- (1-p)*a*(1-roctable1[5,]) + p*b*(1-roctable1[4,])
expected_loss_df <- data.frame(expected_loss, highlight = ifelse(expected_loss == min(expected_loss), T , F))

library(ggplot2)
qplot(seq(0.01, 0.9, 0.01), expected_loss) +
  xlab("Cutoff Thresholds") +
  ylab("Expected Loss Values") +
  geom_point(aes(colour = expected_loss_df$highlight)) + 
  scale_color_manual(values = c("black", "red")) + 
  theme(legend.position="none")
