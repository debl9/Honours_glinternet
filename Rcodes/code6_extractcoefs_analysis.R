
# Script for Extracting the Coefficients from Models ----------------------

# The following codes are adapted from:
# https://github.com/CBDRH/antidepressants/blob/master/README.Rmd

# Load necessary files and libraries

library(plyr)
library(glinternet) # imports the coef method
library(parallel)
library(reshape2)
library(magrittr)
library(purrr)
library(dplyr)
library(tibble)
library(ggplot2)

load("glint_varselect.rda")
load("bootstrapped_glint.rda")

# Obtains the names of variables ------------------------------------------

catNames <- colnames(glmnet.matrix2)[12:18]
contNames <- colnames(glmnet.matrix2)[1:11]

# Functions to extract and reshape coefficient results --------------------

# The effect = categorical variable, levels = levels within each cat variable
get_catcoef <- function (coefs) {
  id <- coefs$mainEffects$cat
  cat_val <- coefs$mainEffectsCoef$cat
  ldply(map2(.x = id, .y = cat_val,
             .f = function(i,jj) data.frame(value=jj, level=0:(length(jj)-1),
                                            catEffect=i)))
}

get_contcoef <- function (coefs) {
  id <- coefs$mainEffects$cont
  cont_val <- coefs$mainEffectsCoef$cont
  ldply(map2(.x = id, .y = cont_val, .f = function(i, jj) data.frame(value = jj,
                                                                     contEffect = i)))
}

get_Intcontcontcoef <- function (coefs) {
  id <- coefs$interactions$contcont
  contcont_val <- coefs$interactionsCoef$contcont
  data.frame(value = unlist(contcont_val), contEffect1 = id[,1], contEffect2 = id[,2])
}

get_Intcatcontcoef <- function (coefs) {
  id <- coefs$interactions$catcont
  catcont_val <- coefs$interactionsCoef$catcont
  ldply(1:length(catcont_val), function (a) {
    i <- coefs$interactions$catcont[a, ]
    jj <- coefs$interactionsCoef$catcont[[a]]
    data.frame(value = jj, level=0:(length(jj)-1)) %>% 
      mutate(catEffect = i[1], contEffect = i[2])
  })
}

get_Intcatcatcoef <- function (coefs) {
  id <- coefs$interactions$catcat
  catcat_val <- coefs$interactionsCoef$catcat
  ldply(1:length(catcat_val), .fun = function(a) {
    i <- coefs$interactions$catcat[a,]
    jj <- coefs$interactionsCoef$catcat[[a]]
    dimnames(jj)[[1]] <- 0:(dim(jj)[1]-1)
    dimnames(jj)[[2]] <- 0:(dim(jj)[2]-1)
    melt(jj, varnames = c("level1", "level2")) %>% 
      mutate(catEffect1 = i[1], catEffect2 = i[2])
  })
}


# Extract coefficients according to each bootstrap sample -----------------

all_coefs <- mclapply(output, function(bootOut){
  coefs <- coef(bootOut$model)
  lam <- 19 # 19th lambda value that maximised the AUC
  catEffTable <- get_catcoef(coefs[[lam]])
  contEffTable <- get_contcoef(coefs[[lam]]) 
  catcatInterTable <- get_Intcatcatcoef(coefs[[lam]]) 
  catcontInterTable <- get_Intcatcontcoef(coefs[[lam]]) 
  contcontInterTable <- get_Intcontcontcoef(coefs[[lam]]) 
  return(list(catEffTable=catEffTable, contEffTable=contEffTable,
              catcatInterTable=catcatInterTable,
              catcontInterTable=catcontInterTable, 
              contcontInterTable = contcontInterTable))
}, mc.cores = detectCores())


# Combine main effects and interactions into separate dataframes ----------

catEffTable <- ldply(all_coefs, function(model) model$catEffTable)
contEffTable <- ldply(all_coefs, function(model) model$contEffTable)
catcatInterTable <- ldply(all_coefs, function(model) model$catcatInterTable)
catcontInterTable <- ldply(all_coefs, function(model) model$catcontInterTable)
contcontInterTable <- ldply(all_coefs, function(model) model$contcontInterTable)

# Bootstrap Confidence Intervals for Coefficients -------------------------

# Function calculating the bootstrapped quantiles

quantile_update <- function (x, n, p){quantile(c(x, integer(n)), p)}

# Extract Signicant Variables
B = 50
# Categorical main effects
catEffTable %>% 
  group_by(catEffect, level) %>% 
  summarise(CIlo = quantile_update(value, B-length(value), p = 0.05),
            CIhi = quantile_update(value, B-length(value), p = 0.95),
            mean = sum(value)/B,
            median = quantile_update(value, B-length(value), p = 0.5),
            Q1 = quantile_update(value, B-length(value), p = 0.25),
            Q3 = quantile_update(value, B-length(value), p = 0.75)) %>%
  mutate(id=paste(catNames[catEffect], level, sep="_")) %>% 
  arrange(desc(abs(mean))) -> sigcatEff

# Continuous main effects
contEffTable %>% 
  group_by(contEffect) %>% 
  summarise(CIlo = quantile_update(value, B-length(value), p = 0.05),
            CIhi = quantile_update(value, B-length(value), p = 0.95),
            mean = sum(value)/B,
            median = quantile_update(value, B-length(value), p = 0.5),
            Q1 = quantile_update(value, B-length(value), p = 0.25),
            Q3 = quantile_update(value, B-length(value), p = 0.75)) %>%
  mutate(id = paste(contNames[contEffect])) %>% 
  arrange(desc(abs(mean))) -> sigcontEff

# Interactions
# Continuous x Continuous

contcontInterTable %>% 
  group_by(contEffect1, contEffect2) %>% 
  summarise(CIlo = quantile_update(value, B-length(value), p = 0.05),
            CIhi = quantile_update(value, B-length(value), p = 0.95),
            mean = sum(value)/B,
            median = quantile_update(value, B-length(value), p = 0.5),
            Q1 = quantile_update(value, B-length(value), p = 0.25),
            Q3 = quantile_update(value, B-length(value), p = 0.75)) %>% 
  mutate(id1 = paste(contNames[contEffect1]), 
         id2 = paste(contNames[contEffect2]), 
         id = paste(id1, id2, sep=" x ")) %>% 
  arrange(desc(abs(mean))) -> sigcontcontEff

# Categorical x Categorical

catcatInterTable %>% 
  group_by(catEffect1, catEffect2, level1, level2) %>% 
  summarise(CIlo = quantile_update(value, B-length(value), p = 0.05),
            CIhi = quantile_update(value, B-length(value), p = 0.95),
            mean = sum(value)/B,
            median = quantile_update(value, B-length(value), p = 0.5),
            Q1 = quantile_update(value, B-length(value), p = 0.25),
            Q3 = quantile_update(value, B-length(value), p = 0.75)) %>% 
  mutate(id1 = paste(catNames[catEffect1], level1, sep = "_"),
         id2 = paste(catNames[catEffect2], level2, sep = "_"),
         id = paste(id1, id2, sep = " x ")) %>% 
  arrange(desc(abs(mean))) -> sigcatcatEff

# Categorical x Continuous

catcontInterTable %>% 
  group_by(catEffect, contEffect, level) %>% 
  summarise(CIlo = quantile_update(value, B-length(value), p = 0.05),
            CIhi = quantile_update(value, B-length(value), p = 0.95),
            mean = sum(value)/B,
            median = quantile_update(value, B-length(value), p = 0.5),
            Q1 = quantile_update(value, B-length(value), p = 0.25),
            Q3 = quantile_update(value, B-length(value), p = 0.75)) %>% 
  mutate(id1 = paste(catNames[catEffect], level, sep = "_"),
         id2 = paste(catNames[contEffect]),
         id = paste(id1, id2, sep = " x ")) %>% 
  arrange(desc(abs(mean))) -> sigcatcontEff

### Select the same columns

sigcatcatEff %>% 
  ungroup() %>% 
  dplyr::select(c(id, mean, median, CIlo, CIhi, Q1, Q3)) -> sigcatcatEff2

sigcatcontEff %>% 
  ungroup() %>% 
  dplyr::select(c(id, mean, median, CIlo, CIhi, Q1, Q3)) -> sigcatcontEff2

sigcontcontEff %>% 
  ungroup() %>% 
  dplyr::select(c(id, mean, median, CIlo, CIhi, Q1, Q3)) -> sigcontcontEff2

sigcatEff %>% 
  ungroup() %>% 
  dplyr::select(c(id, mean, median, CIlo, CIhi, Q1, Q3)) -> sigcatEff2

sigcontEff %>% 
  ungroup() %>% 
  dplyr::select(c(id, mean, median, CIlo, CIhi, Q1, Q3)) -> sigcontEff2

### Combine and arrange in descending order for the most significant interactions

all_bootstrap_coefsdf <- rbind(sigcatcatEff2, sigcontcontEff2, sigcatcontEff2, 
                               sigcatEff2, sigcontEff2)
new_all_bootstrap_coefsdf <- dplyr::arrange(all_bootstrap_coefsdf, desc(abs(mean)))

# Full fit ----------------------------------------------------------------
load("full_glint_new.rda")

full_fit_coef <- coef(glm_glint)[[19]]

# Main effects: categorical, continuous
full_fit_catEff <- get_catcoef(full_fit_coef) 
full_fit_catEff %>% 
  mutate(id = paste(catNames[catEffect], level, sep = "_")) %>% 
  select(id, value) -> full_fit_catEff2

full_fit_contEff <- get_contcoef(full_fit_coef)
full_fit_contEff %>% 
  mutate(id = paste(contNames[contEffect], sep = "_")) %>% 
  select(id, value) -> full_fit_contEff2

# Interaction terms
full_fit_contcontEff <- get_Intcontcontcoef(full_fit_coef)
full_fit_contcontEff %>% 
  mutate(id1 = paste(contNames[contEffect1]), 
         id2 = paste(contNames[contEffect2]), 
         id = paste(id1, id2, sep=" x ")) %>% 
  select(id, value) -> full_fit_contcontEff2

full_fit_catcontEff <- get_Intcatcontcoef(full_fit_coef)
full_fit_catcontEff %>% 
  mutate(id1 = paste(catNames[catEffect], level, sep = "_"),
         id2 = paste(catNames[contEffect]),
         id = paste(id1, id2, sep = " x ")) %>% 
  select(id, value) -> full_fit_catcontEff2

full_fit_cattcatEff <- get_Intcatcatcoef(full_fit_coef)
full_fit_cattcatEff %>% 
  mutate(id1 = paste(catNames[catEffect1], level1, sep = "_"),
         id2 = paste(catNames[catEffect2], level2, sep = "_"),
         id = paste(id1, id2, sep = " x ")) %>% 
  select(id, value) -> full_fit_catcatEff2

all_full_fitcoefsdf <- rbind(full_fit_catEff2, full_fit_contEff2, 
                             full_fit_catcatEff2, full_fit_catcontEff2,
                             full_fit_contcontEff2)
all_full_fitcoefsdf <- arrange(all_full_fitcoefsdf, desc(abs(mean)))


# Merge the full fit and bootstrap models ---------------------------------

# Match the name of the variables
id_location_bs_in_full <- match(new_all_bootstrap_coefsdf$id, all_full_fitcoefsdf$id)
merge_full_fit <- all_full_fitcoefsdf[id_location_bs_in_full, ]
merge_full_fit2 <- na.omit(merge_full_fit)

# Merge both dataframes
full_bs_coefs <- left_join(merge_full_fit2, new_all_bootstrap_coefsdf, by = "id")
colnames(full_bs_coefs) <- c("id", "mean_fullfit", "mean_bootstrap", colnames(full_bs_coefs[4:8]))

# Plot Bootstrap Percentiles  ---------------------------------------------

# Plotting the significant variables
full_bs_coefs2 <- full_bs_coefs[abs(full_bs_coefs$mean_bootstrap) > 0.05,]

ggplot(data = full_bs_coefs2, aes(id)) + 
  scale_x_discrete("Interactions + Levels") +
  scale_y_continuous("Effect Size") +
  coord_flip() +
  geom_linerange(aes(ymin=CIlo, ymax=CIhi), size=0.5, colour='navy') +
  geom_linerange(aes(ymin=Q1, ymax=Q3), size=1.1, colour='navy') +
  geom_point(aes(y=median, colour='bootstrap median')) + 
  geom_point(aes(y=mean_fullfit, colour = 'full data coefficients')) +
  geom_hline(aes(yintercept=0), lty=3) +
  guides(colour = FALSE) + 
  ggtitle("Bootstrap Percentiles")

# Save --------------------------------------------------------------------

save(catNames, contNames, all_coefs, sigcatcatEff, sigcatcontEff, sigcatEff,
     sigcontcontEff, sigcontEff, new_all_bootstrap_coefsdf,
     all_full_fitcoefsdf, full_bs_coefs, file = "sigEff.rda")



