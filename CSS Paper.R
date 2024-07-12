library(tidyverse)
library(tinytex)
library(dbplyr)
library(class)
library(caret)
library(rpart)
library(readr)
library(rsample)
library(xgboost)
library(gbm)
library(tree)
library(ROSE)
library(e1071)
library(randomForest)

# agea, gndr, edulvlb, cntr, marsts, chldhhe, health (demography)
# trstprl, trstlgl, trstplc, trstplt, trstprt, trstep, trstun, trstsci(trust in istitution)
# polintr, prtdgcl, stfdem, implvdm, nwspol, freehms, hmsfmlsh, hmsacld, lrscale (politics)
# ppltrst, pplfair, sclmeet, stflife, happy, aesfdrk, crmvct (social factors)
# stfeco, gincdif, hinctnta, emplrel, wkhtot (economic factors)
# rlgblg, rlgdnm, pray, (religion)
# imdfetn, imbgeco, imueclt, imwbcnt (immigration)
# dscrgrp, dscrrce, dscrntn, dscrrlg, dscrlng (discrimination)


data <- read.csv("C:/Users/39348/Downloads/Computational Social Science/ESS10.csv", header = TRUE)
data <- as.data.frame(data)


# Demography
data$vote <- as.factor(data$vote)
data$gndr <- as.factor(data$gndr)
data$eisced <- as.factor(data$eisced)
data$cntry <- as.factor(data$cntry)
data$region <- as.factor(data$region)
data$marsts <- as.factor(data$marsts)
data$chldhhe <- as.factor(data$chldhhe)
data$health <- as.factor(data$health)

data <- data %>% filter(!vote %in% c(3, 7, 8, 9))
data <- data %>% filter(!eisced %in% c(55, 77, 88, 99))
data <- data %>% filter(!marsts %in% c(77, 88, 99))
data <- data %>% filter(!chldhhe %in% c(7, 8, 9))
data <- data %>% filter(!health %in% c(7, 8, 9))

summary(data$vote)
summary(data$gndr)
summary(data$eisced)
summary(data$cntry)
summary(data$region)
summary(data$marsts)
summary(data$chldhhe)
summary(data$health)


# Trust in institution
data <- data %>% filter(!trstprl %in% c(77, 88, 99))
data <- data %>% filter(!trstlgl %in% c(77, 88, 99))
data <- data %>% filter(!trstplc %in% c(77, 88, 99))
data <- data %>% filter(!trstplt %in% c(77, 88, 99))
data <- data %>% filter(!trstprt %in% c(77, 88, 99))
data <- data %>% filter(!trstep %in% c(77, 88, 99))
data <- data %>% filter(!trstun %in% c(77, 88, 99))
data <- data %>% filter(!trstsci %in% c(77, 88, 99))

summary(data$trstprl)
summary(data$trstlgl)
summary(data$trstplc)
summary(data$trstplt)
summary(data$trstprt)
summary(data$trstep)
summary(data$trstun)
summary(data$trstsci)

# Politics
data$polintr <- as.factor(data$polintr)
data$prtdgcl <- as.factor(data$prtdgcl)

data <- data %>% filter(!polintr %in% c(7, 8, 9))
data <- data %>% filter(!prtdgcl %in% c(6, 7, 8, 9))
data <- data %>% filter(!stfdem %in% c(77, 88, 99))
data <- data %>% filter(!implvdm %in% c(77, 88, 99))
data <- data %>% filter(!lrscale %in% c(77, 88, 99))
data <- data %>% filter(!nwspol %in% c(7777, 8888, 9999))

summary(data$polintr)
summary(data$prtdgcl)
summary(data$stfdem)
summary(data$implvdm)
summary(data$lrscale)
summary(data$nwspol)



# Social factors
data$freehms <- as.factor(data$freehms)
data$hmsfmlsh <- as.factor(data$hmsfmlsh)
data$hmsacld <- as.factor(data$hmsacld)
data$sclmeet <- as.factor(data$sclmeet)
data$aesfdrk <- as.factor(data$aesfdrk)
data$crmvct <- as.factor(data$crmvct)
data$dscrgrp <- as.factor(data$dscrgrp) 
data$netusoft <- as.factor(data$netusoft)

data <- data %>% filter(!freehms %in% c(7, 8, 9))
data <- data %>% filter(!hmsfmlsh %in% c(7, 8, 9))
data <- data %>% filter(!hmsacld %in% c(7, 8, 9))
data <- data %>% filter(!sclmeet %in% c(77, 88, 99))
data <- data %>% filter(!stflife %in% c(77, 88, 99))
data <- data %>% filter(!aesfdrk %in% c(7, 8, 9))
data <- data %>% filter(!crmvct %in% c(7, 8, 9))
data <- data %>% filter(!dscrgrp %in% c(7, 8, 9))
data <- data %>% filter(!netusoft %in% c(7, 8, 9))

summary(data$freehms)
summary(data$hmsfmlsh)
summary(data$hmsacld)
summary(data$sclmeet)
summary(data$stflife)
summary(data$aesfdrk)
summary(data$crmvct)
summary(data$dscrgrp)
summary(data$netusoft)


# Economic factors
data$gincdif <- as.factor(data$gincdif)
data$hmsfmlsh <- as.factor(data$hmsfmlsh)
data$emplrel <- as.factor(data$emplrel)

data <- data %>% filter(!stfeco %in% c(77, 88, 99))
data <- data %>% filter(!gincdif %in% c(7, 8, 9))
data <- data %>% filter(!hinctnta %in% c(77, 88, 99))
data <- data %>% filter(!emplrel %in% c(7, 8, 9))
data <- data %>% filter(!wkhtot %in% c(777, 888, 999))

summary(data$stfeco)
summary(data$gincdif)
summary(data$hinctnta)
summary(data$emplrel)
summary(data$wkhtot)

# Religion
data$rlgblg <- as.factor(data$rlgblg)
data$rlgdnm <- as.factor(data$rlgdnm)
data$pray <- as.factor(data$pray)

data <- data %>% filter(!rlgblg %in% c(7, 8, 9))
data <- data %>% filter(!rlgdnm %in% c(77, 88, 99))
data <- data %>% filter(!pray %in% c(77, 88, 99))

summary(data$rlgblg)
summary(data$rlgdnm)
summary(data$pray)

# Immigration
data$imdfetn <- as.factor(data$imdfetn)
data$pray <- as.factor(data$pray)
data$pray <- as.factor(data$pray)
data$pray <- as.factor(data$pray)

data <- data %>% filter(!imdfetn %in% c(7, 8, 9))
data <- data %>% filter(!imbgeco %in% c(77, 88, 99))
data <- data %>% filter(!imueclt %in% c(77, 88, 99))
data <- data %>% filter(!imwbcnt %in% c(77, 88, 99))

summary(data$imdfetn)
summary(data$imbgeco)
summary(data$imueclt)
summary(data$imwbcnt)


# Discrimination
data$dscrgrp <- as.factor(data$dscrgrp)

data <- data %>% filter(!dscrgrp %in% c(7, 8, 9))

summary(data$dscrgrp)

variables <- c("dscrrce", "dscrntn", "dscrrlg", "dscrlng", "dscretn",
               "dscrage", "dscrgnd", "dscrsex", "dscrdsb")

# Convert each variable to a factor and summarize
for (variable in variables) {
  data[[variable]] <- as.factor(data[[variable]])
  cat("Summary of", variable, ":\n")
  print(summary(data[[variable]]))
  cat("\n")
}

data <- data[!data$vote %in% c(3, 7, 8, 9), ]
data$vote <- droplevels(data$vote)
summary(data$vote)

set.seed(123)
trainIndex <- createDataPartition(data$vote, p = .7, list = FALSE, times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

summary(train_data$vote)
summary(test_data$vote)

train_data_balanced <- ROSE(vote ~ agea + gndr + eisced + cntry + marsts + chldhhe + health + 
                              trstprl + trstlgl + trstplc + trstplt + trstprt + trstep + 
                              trstun + trstsci + polintr + prtdgcl + stfdem + implvdm + lrscale + nwspol + 
                              freehms + hmsfmlsh + hmsacld + sclmeet + stflife + aesfdrk + 
                              crmvct + netusoft + stfeco + gincdif + hinctnta + emplrel + 
                              wkhtot + rlgblg + rlgdnm + pray + imdfetn + imbgeco + imueclt + 
                              imwbcnt + dscrgrp + dscrrce + dscrntn + dscrrlg + dscrlng, 
                            data = train_data, seed = 42)$data

summary(train_data_balanced$vote)


# Theory-driven logistic regression model
glm_model <- glm(vote ~ agea + gndr + eisced + hinctnta + polintr + prtdgcl + nwspol + cntry, data = train_data_balanced, family = binomial)

summary(glm_model)

glm_predictions <- predict(glm_model, newdata = test_data, type = "response")
glm_pred_class <- ifelse(glm_predictions > 0.5, 2, 1)

test_data$vote <- factor(test_data$vote)
glm_pred_class <- factor(glm_pred_class, levels = levels(test_data$vote))

glm_conf_matrix <- confusionMatrix(glm_pred_class, test_data$vote)
print(glm_conf_matrix)


# Data-driven logistic regression model
glm_model <- glm(vote ~ agea + gndr + eisced + cntry + marsts + chldhhe + health + 
                   trstprl + trstlgl + trstplc + trstplt + trstprt + trstep + 
                   trstun + trstsci + polintr + prtdgcl + stfdem + implvdm + lrscale + nwspol + 
                   freehms + hmsfmlsh + hmsacld + sclmeet + stflife + aesfdrk + 
                   crmvct + netusoft + stfeco + gincdif + hinctnta + emplrel + 
                   wkhtot + rlgblg + rlgdnm + pray + imdfetn + imbgeco + imueclt + 
                   imwbcnt + dscrgrp + dscrrce + dscrntn + dscrrlg + dscrlng, 
                 data = train_data_balanced, family = binomial())

summary(glm_model)

glm_predictions <- predict(glm_model, newdata = test_data, type = "response")
glm_pred_class <- ifelse(glm_predictions > 0.5, 2, 1)

test_data$vote <- factor(test_data$vote)
glm_pred_class <- factor(glm_pred_class, levels = levels(test_data$vote))

glm_conf_matrix <- confusionMatrix(glm_pred_class, test_data$vote)
print(glm_conf_matrix)


# Random forest model
rf_model <- randomForest(vote ~ agea + gndr + eisced + cntry + marsts + chldhhe + health + 
                           trstprl + trstlgl + trstplc + trstplt + trstprt + trstep + 
                           trstun + trstsci + polintr + prtdgcl + stfdem + implvdm + lrscale + nwspol + 
                           freehms + hmsfmlsh + hmsacld + sclmeet + stflife + aesfdrk + 
                           crmvct + netusoft + stfeco + gincdif + hinctnta + emplrel + 
                           wkhtot + rlgblg + rlgdnm + pray + imdfetn + imbgeco + imueclt + 
                           imwbcnt + dscrgrp + dscrrce + dscrntn + dscrrlg + dscrlng, 
                           data = train_data_balanced, ntree = 500, mtry = 3, importance = TRUE)

importance(rf_model)
varImpPlot(rf_model)

rf_predictions <- predict(rf_model, newdata = test_data)

rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$vote)
print(rf_conf_matrix)



train_data$vote <- as.numeric(train_data$vote) - 1
test_data$vote <- as.numeric(test_data$vote) - 1
train_data_balanced$vote <- as.numeric(train_data_balanced$vote) - 1

# Gradient boosting model
gbm_model <- gbm(vote ~ agea + gndr + eisced + cntry + marsts + chldhhe + health + 
                   trstprl + trstlgl + trstplc + trstplt + trstprt + trstep + 
                   trstun + trstsci + polintr + prtdgcl + stfdem + implvdm + lrscale + nwspol + 
                   freehms + hmsfmlsh + hmsacld + sclmeet + stflife + aesfdrk + 
                   crmvct + netusoft + stfeco + gincdif + hinctnta + emplrel + 
                   wkhtot + rlgblg + rlgdnm + pray + imdfetn + imbgeco + imueclt + 
                   imwbcnt + dscrgrp + dscrrce + dscrntn + dscrrlg + dscrlng, 
                 data = train_data_balanced, distribution = "bernoulli", n.trees = 500, interaction.depth = 3, 
                 shrinkage = 0.01, cv.folds = 5)

summary(gbm_model)

gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = gbm_model$n.trees, type = "response")

gbm_pred_class <- ifelse(gbm_predictions > 0.5, 1, 0)

gbm_conf_matrix <- confusionMatrix(factor(gbm_pred_class, levels = c(0, 1)), factor(test_data$vote, levels = c(0, 1)))
print(gbm_conf_matrix)

