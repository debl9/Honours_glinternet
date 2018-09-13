
# GLMNET ------------------------------------------------------------------
# load libraries

library(glmnet)

# FITTING GLMNET MODEL ----------------------------------------------------
X_glmnet <- data.matrix(designX3)

full.glmnet.fit <- glmnet::glmnet(X_glmnet, as.factor(Y), alpha = 1, family = "binomial",
                                  type.multinomial = "grouped")
# Lasso plot
plot(full.glmnet.fit, label = T)


# CROSS VALIDATION --------------------------------------------------------
# Cross validation to entire dataset
set.seed(101)
cvfit <- glmnet::cv.glmnet(X_glmnet, as.factor(Y), type.measure = "auc",
                           family = "binomial")
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
cvbeta <- coef(cvfit, s="lambda.1se") 

# extract the names of the main effects in glmnet model
main.effects <- dimnames(cvbeta)[[1]][which(cvbeta != 0)]


# MATRIX WITH ONLY MAIN EFFECTS (ME) --------------------------------------

vifglmnet.matrix <- dplyr::select(designX3, main.effects[-1])
