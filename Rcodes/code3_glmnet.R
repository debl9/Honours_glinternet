
# GLMNET ------------------------------------------------------------------
# load libraries

library(glmnet)
load("vif.rda")

# FITTING GLMNET MODEL ----------------------------------------------------
X_glmnet <- data.matrix(designX3)

full.glmnet.fit <- glmnet::glmnet(X_glmnet, as.factor(Y), alpha = 1, family = "binomial",
                                  type.multinomial = "grouped")
# Lasso plot
plot(full.glmnet.fit, label = T)


# CROSS VALIDATION --------------------------------------------------------
# Cross validation to entire dataset
set.seed(5015064)

cvfit <- function (X,Y, df){
  fit <- glmnet::cv.glmnet(X, Y, type.measure = "auc",
                           family = "binomial", dfmax = df)
  plot(fit)
  cvbeta <- coef(fit, s="lambda.1se") 
  main.effects <- dimnames(cvbeta)[[1]][which(cvbeta != 0)]
  return(list(fit = fit, lambda.min = fit$lambda.min, lambda.1se = fit$lambda.1se, 
              cvbeta = cvbeta, main.effects = main.effects))
}

# Evaluate effectiveness of VIF
fullcv <- cvfit(data.matrix(designX), Y, df = length(designX)) 
vifcv <- cvfit(X_glmnet, Y, df = length(X_glmnet)) 
cv23 <- cvfit(data.matrix(designX), Y, df = 23)


# MATRIX WITH ONLY MAIN EFFECTS (ME) --------------------------------------

vifglmnet.matrix <- dplyr::select(designX3, vifcv$main.effects[-1])
glmnet.matrix <- dplyr::select(designX, vif$main.effects[-1])

save(fullcv, vifcv, cv23, vifglmnet.matrix, glmnet.matrix, file = "vif_glmnet.rda")