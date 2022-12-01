###Full model###
#calculate AUC of model using all 32 predictors - the same 10 folds are used.
results_full_model<-list()

#logistic regression SMOTE
for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  #select features in the validation set
  validation_data <- random_forest_data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
                   paed_sepsis_indicator___3 + paed_sepsis_indicator___4 + paed_sepsis_indicator___5 + 
                   paed_sepsis_indicator___6 + paed_sepsis_indicator___7 + paed_sepsis_indicator___8 + 
                   paed_sepsis_indicator___9 + pead_sepsis_factors___1 + pead_sepsis_factors___2 + 
                   pead_sepsis_factors___3 + pead_sepsis_factors___4 + pead_sepsis_factors___5 + 
                   pead_sepsis_factors___6 + severe_illness_feature___1 + severe_illness_feature___2 + 
                   severe_illness_feature___3 + severe_illness_feature___4 + 
                   severe_illness_feature___5 + severe_illness_feature___6 + 
                   severe_illness_feature___7 + severe_illness_feature___8 + 
                   moderate_illness_feature___1 + moderate_illness_feature___2 + 
                   moderate_illness_feature___3 + moderate_illness_feature___4 + 
                   moderate_illness_feature___5 + moderate_illness_feature___6 + 
                   moderate_illness_feature___7 + moderate_illness_feature___8 + 
                   moderate_illness_feature___9,
                 data = training_data_formula,
                 family = binomial)
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_full_model[[i]] <-glm.probs
}

#calculate AUC
truth<-unlist(truth)
results_full_model<-unlist(results_full_model)

results_roc<-cbind(truth, results_full_model )
results_roc<-as.data.frame(results_roc)

library(pROC)
roc_result_full_model <- roc(results_roc$truth,results_roc$results_full_model,smoothed = TRUE,
                             # arguments for ci
                             ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                             # arguments for plot
                             plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                             print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

#Backward selection

#Start with full model and then perform backwards selection
full_model <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 +
                    paed_sepsis_indicator___3 + paed_sepsis_indicator___4 +
                    paed_sepsis_indicator___5 + paed_sepsis_indicator___6 +
                    paed_sepsis_indicator___7 + paed_sepsis_indicator___8 +
                    paed_sepsis_indicator___9 + pead_sepsis_factors___1 +
                    pead_sepsis_factors___2 +   pead_sepsis_factors___3 +
                    pead_sepsis_factors___4 +   pead_sepsis_factors___5 +
                    pead_sepsis_factors___6 +   severe_illness_feature___1 +
                    severe_illness_feature___2 +severe_illness_feature___3 + 
                    severe_illness_feature___4 +severe_illness_feature___5 + 
                    severe_illness_feature___6 +severe_illness_feature___7 +
                    severe_illness_feature___8 +moderate_illness_feature___1 + 
                    moderate_illness_feature___2+moderate_illness_feature___3+
                    moderate_illness_feature___4+moderate_illness_feature___5+
                    moderate_illness_feature___6+ moderate_illness_feature___7+
                    moderate_illness_feature___8+moderate_illness_feature___9,
                  data = data,
                  family = "binomial")

summary(full_model)
backwards <- step(full_model)
#show selected features from backward selection
formula(backwards)
#display results of final model
summary(backwards)
#table of odds ratios and confidence intervals 
tbl_regression(backwards, intercept = TRUE, estimate_fun = function(x) style_ratio(x, digits = 2))

#Evaluate model using 10-fold cross validation
#The same folds are used for all cross validation procedures
results_lr_2<-list()

for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  #select  validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis~  paed_sepsis_indicator___2 + paed_sepsis_indicator___4 +
                   paed_sepsis_indicator___5 + pead_sepsis_factors___1 +
                   pead_sepsis_factors___4 +  pead_sepsis_factors___6 +
                   severe_illness_feature___1 + severe_illness_feature___2 +
                   severe_illness_feature___3 + severe_illness_feature___4 +
                   severe_illness_feature___5 +severe_illness_feature___6 +
                   severe_illness_feature___7 + severe_illness_feature___8 +
                   moderate_illness_feature___3 + moderate_illness_feature___7+
                   moderate_illness_feature___9,
                 data = data,
                 family = "binomial")
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr_2[[i]] <-glm.probs
}

#Produce ROC curve for predictions
results_lr_2<-unlist(results_lr_2)
truth<-unlist(truth)

results_roc<-cbind(truth, results_lr_2 )
results_roc<-as.data.frame(results_roc)

roc_result_backward <- roc(results_roc$truth,results_roc$results_lr_2,smoothed = TRUE,
                           # arguments for ci
                           ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                           # arguments for plot
                           plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                           print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

#Forward selection
M1 <- glm(Pop_B ~ 1, data = random_forest_data, family= "binomial")
check<-add1(M1, scope =~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 +
              paed_sepsis_indicator___3 + paed_sepsis_indicator___4 +
              paed_sepsis_indicator___5 + paed_sepsis_indicator___6 +
              paed_sepsis_indicator___7 + paed_sepsis_indicator___8 +
              paed_sepsis_indicator___9 + pead_sepsis_factors___1 +  
              pead_sepsis_factors___2 +   pead_sepsis_factors___3 +                                     
              pead_sepsis_factors___4 +   pead_sepsis_factors___5 +
              pead_sepsis_factors___6 +   severe_illness_feature___1 +
              severe_illness_feature___2 +severe_illness_feature___3 +                                  
              severe_illness_feature___4 +severe_illness_feature___5 +                                  
              severe_illness_feature___6 +severe_illness_feature___7 +                                  
              severe_illness_feature___8 +moderate_illness_feature___1 + 
              moderate_illness_feature___2+moderate_illness_feature___3+
              moderate_illness_feature___4+moderate_illness_feature___5+
              moderate_illness_feature___6+ moderate_illness_feature___7+
              moderate_illness_feature___8+moderate_illness_feature___9, 
            data = random_forest_data, test = "LRT")


min(check$`Pr(>Chi)`, na.rm=TRUE)
max(check$LRT, na.rm= TRUE)
min(check$AIC, na.rm=TRUE)

#repeat by adding variable with smallest AIC until adding vars does not reduce the AIC.
#calcuate final predictions using code above for 10-fold cross validation and calculation of AUC.