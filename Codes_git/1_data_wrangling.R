
# Loading required libraries and files ------------------------------------

load(file = "westpac_cl.rda")

# setwd("~/Desktop/Honours/R_scripts/")
# westpac1 <- read.csv("westpac_raw_data.csv", header = T)

require(dplyr)      # data manipulation
require(lubridate)  # structuring time
require(zoo)        # indexing observations
require(stringr)    # character manipulation
require(dmm)        # used for factoring data
require(glinternet) # fitting model 

# testing packages
require(cleandata)
require(mice)

# Data Cleaning/Functions ------------------------------------------------


# Converting date - take the date portion and include slashes to convert as.date

extract.date <- function(data) {
  new.date <- as.character(data)
  new.date2 <- substr(new.date, 1,9)
  # new.date3 <- gsub("^([[:digit:]]{2})([[:upper:]]{3})([[:digit:]]{4})$", 
  #                   "\\1\\/\\2\\/\\3", new.date2)
  return(as.Date(new.date2, "%d%b%Y"))
}

replace_99NA <- function(data) {
  data[data == "99"] <- NA
  data[data == "999"] <- NA
  data[data == "9999"] <- NA
  data[data == "999999999"] <- NA
  data[data == "99999999999"] <- NA
  data
}

### Subset of data ###
west <- dplyr::select(westpac1, AGE_OF_APP_1, APPL_DATE:DEFAULT_FLAG, OPENED_DATE, CLOSED_DATE)

# Replacing data 999's with NA

west2 <- replace_99NA(west)
west2$APPL_DATE <- extract.date(west2$APPL_DATE)

# Target variable Y 
Y <- west2$DEFAULT_FLAG

# predictor matrix X
X <- west2[,-133] # (removing default indicator)


# change additional variables into factors
factor.cols = c("CURR_RESIDENCY_POSTCODE_1", "NUM_APPS", "NUM_BNKRPT_1",
                "NUM_EXIST_WBC_ACCTS", "NUM_EXIST_WBC_MTGE_ACCTS",
                "SECURED_LOAN_PURPOSE_CODE", "WORST_APPL_DELQ_STATUS")
                
X[factor.cols] <- lapply(X[factor.cols], factor)


str(X, list.len=ncol(X))

  # Drop these variables with mostly 0 entries
  X %>% 
    dplyr::select(-c(INV_ASSETS_AMT_1, INV_ASSETS_AMT_2, OWN_FUNDS_TOT_COST, 
                     PERS_LN_LIABILITY_1, 
                     PERS_LN_LIABILITY_2, VEHICLE_YEAR, LEND_VAL_1,
                     NUM_EXIST_WBC_MTGE_ACCTS, TIME_AT_PREV_ADDR_2, 
                     AMT_BEING_REFINANCED_3, AMT_BEING_REFINANCED_4,
                     AMT_BEING_REFINANCED_5)) -> X2

is.na(X2$PHONE_APPL_IND) <- X2$PHONE_APPL_IND == ''

# Imputing values as 0 
replace.zero <- function (data) {
  data.new = ifelse(is.na(data), 0, data)
  return(data.new)
}

# Re-code occupation groups
conv.occup <- function (data) {
  
  data.new = ifelse(data == 95011, "I", ifelse((data == 95021 | 
                                                      data == 95925), "J",
                                                    ifelse(data == 95031, "K",
                                                           ifelse((data == 92001 | data ==92501), "L",
                                                                  ifelse(data ==98001, "M", data)))))
  
  data.new2 = ifelse(data.new  >= 10000 & data.new < 20000, "A",
                    ifelse(data.new >= 20000 & data.new < 30000, "B",
                           ifelse(data.new >= 30000 & data.new < 40000, "C",
                                  ifelse(data.new >= 40000 & data.new < 50000, "D", 
                                         ifelse(data.new >= 50000 & data.new < 60000, "E",
                                                ifelse(data.new >= 60000 & data.new < 70000, "F",
                                                       ifelse(data.new >= 70000 & data.new < 80000, "G",
                                                              ifelse(data.new >= 80000 & data.new < 90000, "H", "N"))))))))
  return(data.new2)
}

OCCUPATION_NEW = conv.occup(dmm::unfactor(X2$OCCUPATION_CODE_1))

   # New covariate dataset
  X3 = X2
  X3$OCCUPATION_CODE_1 <- factor(OCCUPATION_NEW)
  X3$OPENED_DATE <- as.Date(X3$OPENED_DATE, "%d%b%Y")
  X3$CLOSED_DATE <- as.Date(X3$CLOSED_DATE, "%d%b%Y")

  # Combine the default into X3
  X3$DEFAULT_FLAG <- Y

# Imputation using mean/mode
  X4 <- X3
  
  X4 <- impute_mean(X3, cols = c("WBC_MTGE_LIABILITY_1", "CC_LIABILITY_1", 
                                 "MTH_PAYE_INC_2", "MTH_MTGE_PYMT_2", 
                                 "MTH_CC_PAY_2", "WORST_DAYS_IN_EXCESS_AT_APPL",
                                 "TOT_MTH_CC_STORE_CARD_PAY", "MTH_CC_PAY_1",
                                 "TOT_MTH_DISCOUNTED_INC",
                                 "LN_AMT_2", "TOT_LN_AMT_APPROVED", "LN_AMT_3",
                                 "LN_AMT_1", "MTH_OTH_INC_2", "ASSETS_VAL_1", "LIABILITY_AMT_1",
                                 "TOT_ASSETS", "TOT_LIABILITY", "CR_FUNDS_WITH_OTH_1",
                                 "MTH_SURP_AMT_2", "TOT_CR_FUNDS_WITH_OTH",
                                 "MTH_PAYE_INC_1", "TIME_AT_PREV_EMPLOYMENT_2",
                                 "LN_TO_SCTY_VAL", "TIME_AT_CURR_EMPLOYMENT_1",
                                 "MTH_PYMTS_AMT_1", "TIME_AT_CURR_ADDR_1",
                                 "CR_FUNDS_WITH_WBC_1", "TOT_CR_FUNDS_WITH_WBC",
                                 "MTH_EXPENSE_AMT_1", "TOT_MTH_OUTGOINGS_INCL_LN_PYMT",
                                 "TIME_WITH_BANK_2", "TIME_AT_PREV_ADDR_1",
                                 "TIME_AT_PREV_EMPLOYMENT_1", "MTH_OTH_LN_PYMTS_1",
                                 "TOT_MTH_OTH_PYMTS", "TIME_AT_CURR_EMPLOYMENT_2",
                                 "TOT_CURR_WBC_EXP", "MTH_SURP_AMT_1",
                                 "AMT_OF_LN_PROTECTION_INS", "MTH_RENT_BOARD_EXPENSE_1",
                                 "TOT_MTH_RENT_BOARD_PYMTS", "MTH_MTGE_PYMT_1", 
                                 "MTH_EXPENSE_AMT_2", "TOT_MTH_MTGE_PYMTS",
                                 "OTH_LN_LIABILITY_1", "MTH_OTH_INC_1",
                                 "NET_ASSETS_VAL_1", "TOT_NET_ASSETS", "TIME_WITH_BANK_1"))
  
  X5 <- impute_mode(X4, cols = c("CURR_RESIDENCY_STATE_1", "OCCUPATION_CODE_1",
                                 "RESIDNTL_STATUS_1", "EMPLOYMENT_STATUS_1"))
  
  View(inspect_na(X5))
  # still have the variables with A LOT OF missing values 
  # REMOVE THESE THAT SEEM IRRELEVANT FROM 
  X_removed_var <- dplyr::select(X5, -c(TIME_AT_CURR_ADDR_2, VEHICLE_NEW_USED_IND, 
                                        NUM_DISHON_CHQS_L6M, AMT_BEING_REFINANCED_2, 
                                        PHONE_APPL_IND,TOT_LIABILITY_OTH_FIN_INSTITUT, 
                                        CLOSED_DATE, AMT_BEING_REFINANCED_1, 
                                        OPENED_DATE, NUM_OTH_FIN_INSTITUTE_ACCTS))
  
  
  # MULTIPLE IMPUTATION - remaining missing data variables
  
  # http://www.columbia.edu/~sjm2186/EPIC_R/EPIC_R_MultipleImputationShort.pdf
  
  X6 <- dplyr::select(X5, -c(TIME_AT_CURR_ADDR_2, PHONE_APPL_IND))
  
  X_impute <- mice(X6[, c("VEHICLE_NEW_USED_IND", 
                          "NUM_DISHON_CHQS_L6M", "AMT_BEING_REFINANCED_2", 
                          "TOT_LIABILITY_OTH_FIN_INSTITUT", 
                          "AMT_BEING_REFINANCED_1", 
                          "NUM_OTH_FIN_INSTITUTE_ACCTS")], m=2, maxit = 3)
  
  # mice::complete() exports the imputed data and we replace the X6 dataframe
  X_out_impute <- complete(X_impute)
  
  X_impute_final <- X6

  X_impute_final$VEHICLE_NEW_USED_IND <- X_out_impute$VEHICLE_NEW_USED_IND
  X_impute_final$NUM_DISHON_CHQS_L6M <- X_out_impute$NUM_DISHON_CHQS_L6M
  X_impute_final$AMT_BEING_REFINANCED_2 <- X_out_impute$AMT_BEING_REFINANCED_2
  X_impute_final$TOT_LIABILITY_OTH_FIN_INSTITUT <- X_out_impute$TOT_LIABILITY_OTH_FIN_INSTITUT
  X_impute_final$AMT_BEING_REFINANCED_1 <- X_out_impute$AMT_BEING_REFINANCED_1
  X_impute_final$NUM_OTH_FIN_INSTITUTE_ACCTS <- X_out_impute$NUM_OTH_FIN_INSTITUTE_ACCTS
  
  
X_impute_final2 <- dplyr::filter(X_impute_final, Final_Decision_Summary == "APPROVE")

# Removing the variables with high percentage of missing values 

# SAVE - data cleaning ----------------------------------------------------

# save(westpac1, west, west2, X, X2, X3, X4, X5, X6, X_removed_var, X_impute,
#      X_out_impute, X_impute_final, Y, extract.date, replace_99NA, replace.zero, conv.occup,
#      file = "westpac_cl.rda")

save(X_removed_var, X_impute_final, X_impute_final2, file = "model_data.rda")

# Sample Model -----------------------------------------------------------

# Bootstrapping for fitting the model


# scraps
n = 4000
sample.data = sample(x = 1:nrow(X3), size = n, replace = T)
x.var = dplyr::select(X3, -DEFAULT_FLAG)
y = X3$DEFAULT_FLAG

# num.levels = c(1, 0, 1, 1, 2470, 9, 2, 12, rep(1, 7), 3, 3, rep(1,6), 30, 1, 9,
#                7, 3, 1, 9, rep(1,20), 2, 1, 1, 2, 1, 1, 2, rep(1, 36), 5, rep(1,16),
#                3, rep(1,5), 10, 1, 3, 2, 0, 0)

x.var2 = x.var[,1:3]
x.var2[is.na(x.var2)] = 0

x.test = x.var2[sample.data,]
x.test = x.test[,c(1,3)]
y.test = as.numeric(y[sample.data])-1 # changing it to numeric

glint.model <- glinternet(x.test, y.test, numLevels = c(1,1),
                                      family = "binomial")

