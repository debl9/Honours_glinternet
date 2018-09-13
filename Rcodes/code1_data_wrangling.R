# DATA WRANGLING ----------------------------------------------------------

require(dplyr)      # data manipulation
require(lubridate)  # structuring dates and times
require(zoo)        # indexing observations
require(stringr)    # character manipulation
require(dmm)        # used for factoring data
require(glinternet) # fitting glinternet model 
require(cleandata)  # inspecting NAs in data
require(mice)       # multiple imputation

# Data Cleaning/Functions ------------------------------------------------
# Read in the data
westpac1 <- read.csv("westpac_data.csv", header = T)

# Function to convert to date
extract.date <- function(data) {
  new.date <- as.character(data)
  new.date2 <- substr(new.date, 1,9)
  return(as.Date(new.date2, "%d%b%Y"))
}

# Function to eplace with missing values
replace_99NA <- function(data) {
  data[data == "99"] <- NA
  data[data == "999"] <- NA
  data[data == "9999"] <- NA
  data[data == "999999999"] <- NA
  data[data == "99999999999"] <- NA
  data
}

# Remove variables related to post-default
west <- dplyr::select(westpac1, AGE_OF_APP_1, APPL_DATE:DEFAULT_FLAG, OPENED_DATE, CLOSED_DATE)
west2 <- replace_99NA(west)
west2$APPL_DATE <- extract.date(west2$APPL_DATE)

# predictor matrix X
X <- west2[,-133] # (removing default indicator)


# change additional variables into factors
factor.cols = c("CURR_RESIDENCY_POSTCODE_1", "NUM_APPS", "NUM_BNKRPT_1",
                "NUM_EXIST_WBC_ACCTS", "NUM_EXIST_WBC_MTGE_ACCTS",
                "SECURED_LOAN_PURPOSE_CODE", "WORST_APPL_DELQ_STATUS")

X[factor.cols] <- lapply(X[factor.cols], factor)

# Drop these variables with mostly 0 entries
X %>% 
  dplyr::select(-c(INV_ASSETS_AMT_1, INV_ASSETS_AMT_2, OWN_FUNDS_TOT_COST, 
                   PERS_LN_LIABILITY_1, 
                   PERS_LN_LIABILITY_2, VEHICLE_YEAR, LEND_VAL_1,
                   NUM_EXIST_WBC_MTGE_ACCTS, TIME_AT_PREV_ADDR_2, 
                   AMT_BEING_REFINANCED_3, AMT_BEING_REFINANCED_4,
                   AMT_BEING_REFINANCED_5)) -> X2

is.na(X2$PHONE_APPL_IND) <- X2$PHONE_APPL_IND == ''

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

# Select observations that have been APPROVE[D]
X3A <- dplyr::filter(X3, Final_Decision_Summary == "APPROVE")

# Observe the number of NA in dataset
cleandata::inspect_na(X3A)/nrow(X3A)

# Imputation using mean/mode
X4 <- impute_mean(X3A, cols = c("WBC_MTGE_LIABILITY_1", "CC_LIABILITY_1", 
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

inspect_na(X5)

# Multiple imputation for these variables
X6 <- dplyr::select(X5, -c(TIME_AT_CURR_ADDR_2, PHONE_APPL_IND))
X_impute <- mice(X6[, c("VEHICLE_NEW_USED_IND", 
                        "NUM_DISHON_CHQS_L6M", "AMT_BEING_REFINANCED_2", 
                        "TOT_LIABILITY_OTH_FIN_INSTITUT", 
                        "AMT_BEING_REFINANCED_1", 
                        "NUM_OTH_FIN_INSTITUTE_ACCTS")], m=2, maxit = 3)

# mice::complete() exports the imputed columns to obtain the final dataframe
X_out_impute <- complete(X_impute)
X_impute_final <- X6

X_impute_final$VEHICLE_NEW_USED_IND <- X_out_impute$VEHICLE_NEW_USED_IND
X_impute_final$NUM_DISHON_CHQS_L6M <- X_out_impute$NUM_DISHON_CHQS_L6M
X_impute_final$AMT_BEING_REFINANCED_2 <- X_out_impute$AMT_BEING_REFINANCED_2
X_impute_final$TOT_LIABILITY_OTH_FIN_INSTITUT <- X_out_impute$TOT_LIABILITY_OTH_FIN_INSTITUT
X_impute_final$AMT_BEING_REFINANCED_1 <- X_out_impute$AMT_BEING_REFINANCED_1
X_impute_final$NUM_OTH_FIN_INSTITUTE_ACCTS <- X_out_impute$NUM_OTH_FIN_INSTITUTE_ACCTS


# CONTINUOUS/CATEGORICAL STRUCTURE ----------------------------------------

Y <- X_impute_final2$DEFAULT_FLAG
X_impute_final2 <- dplyr::select(X_impute_final2, -DEFAULT_FLAG)

# separate continous and categorical columns
colnums <- unlist(lapply(X_impute_final2, is.factor))
X_categ <- X_impute_final2[, colnums] 
X_cont <- X_impute_final2[, which(colnums == F)]

# Remove postcode columns
X_categ2 <- X_categ[,-1]

# RECODING CATEGORICAL DATA SO THAT IT STARTS FROM 0
X_categ2 %>%
  mutate(CURR_RESIDENCY_STATE_1 =
           dplyr::recode_factor(CURR_RESIDENCY_STATE_1,
                                `ACT` = "0", `NSW` = "1", `NT` = "2",
                                `QLD` = "3", `SA` = "4", `TAS` = "5",
                                `VIC` = "6", `WA` = "7"),
         DRIVERS_LICENCE_IND_1 = dplyr::recode_factor(DRIVERS_LICENCE_IND_1, `Y`="0", `N` = "1"),
         EMPLOYMENT_STATUS_1 = dplyr::recode_factor(EMPLOYMENT_STATUS_1,
                                                    `CO` = "0", `FS` = "1", `FT` = "2",
                                                    `GP` = "3", `HD`="4", `PT`="5",
                                                    `RE`="6", `SE`="7", `ST`="8",
                                                    `TE`="9", `UN`="10"),
         NUM_APPS = dplyr::recode_factor(NUM_APPS, `0` = "0", `1`="1", `2`="2"),
         NUM_BNKRPT_1 = dplyr::recode_factor(NUM_BNKRPT_1, `0` = "0", `1`="1", `2`="2"),
         NUM_EXIST_WBC_ACCTS = dplyr::recode_factor(NUM_EXIST_WBC_ACCTS, `0` = "0", `1`="1", `2`="2",
                                                    `3` = "3", `4`="4", `5`="5",
                                                    `6` = "6", `7`="7", `8`="8",
                                                    `9` = "9", `130`="130", `11`="11",
                                                    `12` = "12", `13`="13", `14`="14",
                                                    `15` = "15", `16`="16", `17`="17",
                                                    `18` = "18", `19`="19", `20`="20",
                                                    `21` = "21", `22`="22", `23`="23",
                                                    `24` = "24", `25`="25", `26`="26",
                                                    `27` = "27", `28`="28", `29`="29"),
         OCCUPATION_CODE_1 = dplyr::recode_factor(OCCUPATION_CODE_1,
                                                  `A`="0", `B`="1", `C`="2", `D`="3",
                                                  `E`="4", `F`="5", `G`="6", `H`="7",
                                                  `N`="8"),
         ORIG_DECSN = dplyr::recode_factor(ORIG_DECSN, `APP`="0", `BRA`="1", `DEC`="2",
                                           `INC`="3", `PEN`="4", `PLR`="5", `VER`="6"),
         RESIDNTL_STATUS_1 = dplyr::recode_factor(RESIDNTL_STATUS_1,
                                                  `B`="0", `C`="1", `D`="2", `H`="3",
                                                  `O`="4", `P`="5", `R`="6", `S`="7"),
         DRIVERS_LICENCE_IND_2 = dplyr::recode_factor(DRIVERS_LICENCE_IND_2, `Y`="0", `N` = "1"),
         LN_PROTECTION_INS_IND = dplyr::recode_factor(LN_PROTECTION_INS_IND, `Y`="0", `N` = "1"),
         MOBILE_PHONE_IND_1 = dplyr::recode_factor(MOBILE_PHONE_IND_1, `Y`="0", `N` = "1"),
         SECURED_LOAN_PURPOSE_CODE = dplyr::recode_factor(SECURED_LOAN_PURPOSE_CODE,
                                                          `1`="0", `2`="1", `4`="2",
                                                          `7`="3", `8`="4"),
         VEHICLE_NEW_USED_IND = dplyr::recode_factor(VEHICLE_NEW_USED_IND, `N`="0", `U` = "1"),
         Final_Decision_Summary = dplyr::recode_factor(Final_Decision_Summary,
                                                       `APPROVE` = "0", `DECLINE` = "1",
                                                       `IN PROCESS` = "2"),
         SEC_FLAG = dplyr::recode_factor(SEC_FLAG, `SEC`="0", `UNSEC`="1")) -> X_categ3

X_categ4 <- droplevels(X_categ3) # clears unused factors
X_categ5 <- sapply(X_categ4, function(x) as.numeric(as.character(x)))  

# continuous first then categorical variables
numLevels <- c(rep(1, ncol(X_cont2)), 
               unname(sapply(X_categ4[,sapply(X_categ4, is.factor)], nlevels)))


# Filter out declined loans 
X_pred <- cbind(X_cont2, X_categ5)

# Remove parameter - Final Decision Summary
X_approved <- X_pred[,-115]
numLevels <- numLevels[-115]

# Remove NA variables -----------------------------------------------------

X_impute_final2 <- dplyr::select(X_impute_final, -c(VEHICLE_NEW_USED_IND, 
                                                    TIME_AT_CURR_ADDR_2, 
                                                    VEHICLE_NEW_USED_IND))