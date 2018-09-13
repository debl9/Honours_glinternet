
# VARIANCE INFLATION FACTOR (VIF) -----------------------------------------
# load libraries

library(usdm)
library(dplyr)

# Separate into categorical and continuous variables
designX <- cbind(X_approved[, -which(names(X_approved) %in% colnames(X_categ4))], X_categ4)

# Calculates VIF for variables excluding those that are highly correlated 
vif.data <- usdm::vif(designX)
var.vif <- as.character(dplyr::filter(vif.data, vif.data[,2] < 4)$Variables)

designX2_cont <- dplyr::select(designX, var.vif)
designX2contcat <- cbind(designX2_cont, X_categ4)
designX3 <- designX2contcat