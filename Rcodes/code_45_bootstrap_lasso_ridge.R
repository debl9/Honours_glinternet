
# Bootstrap Lasso ---------------------------------------------------------

bootlasso <- function (X, Y, numLevels, B=50, nLambdas=20, maxLambda=2.294837e-02, 
                            minLambda=6.830241e-05, n = nrow(glmnet.matrix2)) {
  
  lambdas <- seq(maxLambda, minLambda, length.out = nLambdas)
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  library(foreach)
  output <- foreach::foreach(b = 1:B, .packages = "glmnet") %dopar% {
    bootsamps <- sample(x = 1:n, size = n, replace = TRUE)
    fit <- glmnet::glmnet(x = X[bootsamps,], y = Y[bootsamps],
                                  lambda = lambdas, 
                                  family = "binomial", alpha = 1)
    notbootsamps <- (1:n)[!(1:n) %in% bootsamps]
    ypred <- predict(fit, newx = X[notbootsamps,], type = "link")
    return(list(model = fit, bootsamps = bootsamps, notbootsamps = notbootsamps,
                ypred = ypred))
  }
  save(output, file = "bootstrapped_lasso.rda")
  parallel::stopCluster(cl)
}

bootlasso(glmnet.matrix2, Y, nLambdas=20, maxLambda=2.294837e-02, 
          minLambda=6.830241e-05, n = nrow(glmnet.matrix2))

# Bootstrap Ridge ---------------------------------------------------------

bootridge <- function (X, Y, numLevels, B=50, nLambdas=20, maxLambda=22.948368741, 
                       minLambda=0.002294837, n = nrow(glmnet.matrix2)) {
  
  lambdas <- seq(maxLambda, minLambda, length.out = nLambdas)
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  library(foreach)
  output <- foreach::foreach(b = 1:B, .packages = "glmnet") %dopar% {
    bootsamps <- sample(x = 1:n, size = n, replace = TRUE)
    fit <- glmnet::glmnet(x = X[bootsamps,], y = Y[bootsamps],
                          lambda = lambdas, 
                          family = "binomial", alpha = 0)
    notbootsamps <- (1:n)[!(1:n) %in% bootsamps]
    ypred <- predict(fit, newx = X[notbootsamps,], type = "link")
    return(list(model = fit, bootsamps = bootsamps, notbootsamps = notbootsamps,
                ypred = ypred))
  }
  save(output, file = "bootstrapped_ridge.rda")
  parallel::stopCluster(cl)
}

bootridge(glmnet.matrix2, Y, nLambdas=20, maxLambda=22.948368741, 
          minLambda=0.002294837, n = nrow(glmnet.matrix2))

# Obtain the mean auc tables by running the R-script "code5_AvgAUC.R"
