
# Function that calculates the mean cross entropy for bootstrapped --------

notbootsamps_cross_entropy <- function (opred) {
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
  
  cross_entropy <- function (bs, opred, l) {
    notbootsamps <- opred[[bs]]$notbootsamps
    ypred <- (OOBpred[[bs]][,l])[!is.na(OOBpred[[bs]][,l])] # ignore NAs, ypred for each lambda value
    ypred <- exp(ypred)/(1+exp(ypred)) # Transform to obtain probability of default
    best_CEdf <- data.frame(Y[notbootsamps], ypred)
    return(best_CEdf) # AUC for each lambda
  }
  
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  all_CEdf <- foreach::foreach(bs=1:B, .combine = "rbind") %dopar% {
    cross_entropy(bs, opred = output, l)
  }
  parallel::stopCluster(cl)
  return(-sum(all_CEdf[,1] * log(all_CEdf[,2]))/nrow(all_CEdf))
}