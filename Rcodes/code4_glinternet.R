
# GLINTERNET Bootstrapping Codes ------------------------------------------

# Load necessary files and libraries
library(glinternet)
library(varhandle)
load("model_data2.rda")
load("vif_glmnet.rda")

# Convert categorical variables, dataframe to matrix required for GLINTERNET
glmnet.matrix2 <- data.matrix(varhandle::unfactor(glmnet.matrix))

# Selecting the numLevels argument in GLINTERNET
numLevels2 <- function (X) {
  keptlevels <- which(colnames(X_approved) %in% colnames(X))
  return(c(numLevels[keptlevels]))
}

glmnet.numlevels <- numLevels2(glmnet.matrix)
Y <- varhandle::unfactor(Y)

# GLINTERNET Full data fit to obtain lambda sequence
full_glint <- function (X, numLevels) {
  lambdas <- seq(4.054642e-05, 4.054642e-07, length.out = 20)
  fit <- glinternet::glinternet(X, Y, numLevels = numLevels, nLambda = 20, tol=1e-04,
                                lambda = lambdas, family = "binomial", verbose = T)
  return(fit)
}

glm_glint <- full_glint(data.matrix(glmnet.matrix2), glmnet.numlevels)

# Fits GLINTERNET to 50 bootstrapped samples

bootglinternet <- function (X, Y, numLevels, B=50, nLambdas=20, maxLambda=4.054642e-05, 
                            minLambda=4.054642e-07, n = nrow(glmnet.matrix2)) {
  
  lambdas <- seq(maxLambda, minLambda, length.out = nLambdas)
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  library(foreach)
  output <- foreach::foreach(b = 1:B, .packages = "glinternet") %dopar% {
    bootsamps <- sample(x = 1:n, size = n, replace = TRUE)
    fit <- glinternet::glinternet(X = X[bootsamps,], Y = Y[bootsamps],
                                  lambda = lambdas, numLevels = numLevels, 
                                  family = "binomial", tol=1e-04,
                                  nLambda = nLambdas, verbose = T)
    notbootsamps <- (1:n)[!(1:n) %in% bootsamps]
    ypred <- predict(fit, X = X[notbootsamps,], type = "link")
    return(list(model = fit, bootsamps = bootsamps, notbootsamps = notbootsamps,
                ypred = ypred))
  }
  save(output, file = "bootstrapped_glint.rda")
  parallel::stopCluster(cl)
}

bootglinternet(glmnet.matrix2, Y, glmnet.numlevels,B=50, nLambdas=20, maxLambda=4.054642e-05, 
                           minLambda=4.054642e-07, n = nrow(glmnet.matrix2))

# SAVE --------------------------------------------------------------------

save(glmnet.matrix2, glmnet.numlevels, Y, file = "glint_varselect.rda")
save(glm_glint, file = "full_glint_new.rda")
