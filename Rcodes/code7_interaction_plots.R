
# Graphing Interactions ---------------------------------------------------
library(igraph)
library(visNetwork)
library(dplyr) 

load("sigEff.rda")
load("glint_varselect.rda")

catNames <- colnames(glmnet.matrix2)[14:20]
contNames <- colnames(glmnet.matrix2)[1:13]

sigcatcatEff %>% 
  dplyr::group_by(catEffect1, catEffect2) %>% 
  mutate(catname1 = catNames[catEffect1], catname2 = catNames[catEffect2]) ->catcat2

sigcatcontEff %>% 
  mutate(catname = catNames[catEffect], contname = contNames[contEffect]) -> catcont2

sigcontcontEff %>% 
  mutate(cont1name = contNames[contEffect1], contname2 = contNames[contEffect2]) -> contcont2

# catcat ------------------------------------------------------------------
g1 <- graph.data.frame(catcat2[,14:15], directed = F)
# plot(g1)
largest1 <- largest.cliques(g1)

visIgraph(g1) %>%
  visNodes(size = 5, shape = "circle") %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)
# catcont -----------------------------------------------------------------



g2 <- graph.data.frame(catcont2[,13:14], directed = F)
plot(g2)
largest2 <- largest.cliques(g2)
visIgraph(g2) %>%
  visNodes(size = 5, shape = "circle") %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)
# contcont ----------------------------------------------------------------


g3 <- graph.data.frame(contcont2[,12:13], directed = F)
plot(g3)
largest3 <- largest.cliques(g3)

visIgraph(g3) %>%
  visNodes(size = 5, shape = "circle") %>%
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visInteraction(keyboard = TRUE)


# Plotting the size of interactions ---------------------------------------

library(corrplot)
# Function to plot the individual categorical interactions
catpairint_plots <- function (num1, num2) {
  subcatcat1 <- dplyr::filter(catcat2, catEffect1 == num1 & catEffect2 == num2)
  subcatcat1 %>% 
    dplyr::arrange(level1, level2) -> subcatcat2
  
  subcatcat3 <- dplyr::select(subcatcat2, c(id1, id2, mean))
  
  cat_matrix <- matrix(subcatcat3$mean, nrow = length(unique(subcatcat3$id1)),
                     byrow = T, dimnames = list(unique(subcatcat3$id1), unique(subcatcat3$id2)))
  
  corrplot(cat_matrix, is.corr = F, method = "color", tl.cex = 0.65, tl.srt = 45)
}

# NUM_APPS, SECURED_LOAN_PURPOSE_CODE (2,6)
catpairint_plots(2, 6)
catpairint_plots(3,4)
# DRIVERS_LICENCE_IND_2, SECURED_LOAN_PURPOSE (4,6)
catpairint_plots(4,6)
catpairint_plots(3,6) #flip
# NUM_APPS, LN_PROTECTION_INS_IND
catpairint_plots(2,5)
catpairint_plots(2,3) 
# NUM_EXIST_WBC_ACCTS, DRIVERS_LICENCE_IND (3,4)
catpairint_plots(3,4)
catpairint_plots(3,5) # flip
catpairint_plots(1,5) # maybe not include
catpairint_plots(5,7) 
catpairint_plots(1,3)
catpairint_plots(4,5)
catpairint_plots(2,7)

# Continuous x continuous

contcont2 %>% 
  arrange(contEffect1, contEffect2) -> contcont3

contcont4 <- dplyr::select(contcont3, c(contEffect1, contEffect2, id1, id2, mean))

cont_matrix <- matrix(nrow=12, ncol = 13)
for (i in 1:nrow(contcont4)) {
  x = contcont4$contEffect1[i]
  y = contcont4$contEffect2[i]
  cont_matrix[x,y] = contcont4$mean[i]
}

cont_matrix[is.na(cont_matrix)] <- 0
cont_matrix2 <- cont_matrix[,-1]
rownames(cont_matrix2) = contNames[unique(contcont4$contEffect1)]
colnames(cont_matrix2) = contNames[unique(contcont4$contEffect2)]
corrplot(cont_matrix2, is.corr = F, method = "color", tl.cex = 0.65, tl.srt = 45)
