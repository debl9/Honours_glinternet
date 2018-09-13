
# VARIANCE INFLATION FACTOR (VIF) -----------------------------------------
# load libraries

library(usdm)
library(dplyr)

designX <- cbind(X_approved[, -c(100:115)], 
                 X_categ4[, -which(names(X_categ4) %in% 
                                     c("Final_Decision_Summary", "VEHICLE_NEW_USED_IND", 
                                       "ORIG_DECSN"))])

vif.data <- usdm::vif(designX)
var.vif <- as.character(dplyr::filter(vif.data, vif.data[,2] < 4)$Variables)

designX2_cont <- dplyr::select(designX, var.vif)
designX2contcat <- cbind(designX2_cont, X_categ4[, 
                                                 -which(names(X_categ4) %in% 
                                                          c("Final_Decision_Summary", 
                                                            "VEHICLE_NEW_USED_IND", "ORIG_DECSN"))])
designX3 <- designX2contcat