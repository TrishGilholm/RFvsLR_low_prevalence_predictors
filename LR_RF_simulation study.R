#simulations correlated data LR
#libraries
library(tidyverse)
library(randomForest)
library(bindata)
random_forest_data<-read.csv("random_forest_data.csv")
correlations<-select(random_forest_data, -Pop_A, -Pop_B)
cor_matrix<-cor(correlations)

#original probabilities
#marginal probabilities
#generate p
p<-c(.55, .34, .65, .46, .14, .36, .13, .06, .16,
     .11, .02, .05, .03, .02, .04, 
     .06, .10, .15, .01, .09, .04, .04, .01,
     .08, .14, .04, .07, .01, .12, .03, .08, .20)
output_original_x<-list()

cor_matrix[cor_matrix <0]<-0
#simulate x variables
for(i in 1:1000){
data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                   "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                   "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                   "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                   "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                   "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                   "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                   "pead_sepsis_factors___6", "severe_illness_feature___1",
                   "severe_illness_feature___2", "severe_illness_feature___3",
                   "severe_illness_feature___4", "severe_illness_feature___5",
                   "severe_illness_feature___6", "severe_illness_feature___7",
                   "severe_illness_feature___8", "moderate_illness_feature___1",
                   "moderate_illness_feature___2", "moderate_illness_feature___3",
                   "moderate_illness_feature___4", "moderate_illness_feature___5",
                   "moderate_illness_feature___6", "moderate_illness_feature___7",
                   "moderate_illness_feature___8", "moderate_illness_feature___9")

output_original_x[[i]]<-data
}

#save X variables for simulations
saveRDS(output_original_x, "output_original_x_1000.rds")


output_original_x<-readRDS("output_original_x_1000.rds")

output_original<-list()


#generate y variables using LR
for(i in 1:1000){
  data<-output_original_x[[i]]
  data<-as.data.frame(data)
#Use regression equation to get Y values
xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
  0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
  0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
  0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
  0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
  0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
  1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
  0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
  -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
  0.35*data$moderate_illness_feature___9

#convert to probabilities
p<-1/(1+exp(-xb))
summary(p)

#convert to y
y<-rbinom(n=3500, size = 1, prob=p)
output_original[[i]]<-cbind(y,data)
}

#fit LR model to new simulated data sets and calculate how many simulations each variable is statistically significant

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_original[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)


#min 10% probabilities
#marginal probabilities
#generate p
p<-c(.55, .34, .65, .46, .14, .36, .13, .10, .16,
     .11, .10, .10, .10, .10, .10, 
     .10, .10, .15, .10, .10, .10, .10, .10,
     .10, .14, .10, .10, .10, .12, .10, .10, .20)
output_10_x<-list()


for(i in 1:1000){
  data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
  colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                    "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                    "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                    "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                    "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                    "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                    "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                    "pead_sepsis_factors___6", "severe_illness_feature___1",
                    "severe_illness_feature___2", "severe_illness_feature___3",
                    "severe_illness_feature___4", "severe_illness_feature___5",
                    "severe_illness_feature___6", "severe_illness_feature___7",
                    "severe_illness_feature___8", "moderate_illness_feature___1",
                    "moderate_illness_feature___2", "moderate_illness_feature___3",
                    "moderate_illness_feature___4", "moderate_illness_feature___5",
                    "moderate_illness_feature___6", "moderate_illness_feature___7",
                    "moderate_illness_feature___8", "moderate_illness_feature___9")
  
  output_10_x[[i]]<-data
}


saveRDS(output_10_x, "output_10_x_1000.rds")

output_10_x<-readRDS("output_10_x_1000.rds")


output_10<-list()



for(i in 1:1000){
  data<-output_10_x[[i]]
  data<-as.data.frame(data)
  #Use regression equation to get Y values
  xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
    0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
    0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
    0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
    0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
    0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
    1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
    0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
    -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
    0.35*data$moderate_illness_feature___9
  
  #convert to probabilities
  p<-1/(1+exp(-xb))
  summary(p)
  
  #convert to y
  y<-rbinom(n=3500, size = 1, prob=p)
  output_10[[i]]<-cbind(y,data)
}

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_10[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)

#min 20% probabilities
#marginal probabilities
#generate p
p<-c(.55, .34, .65, .46, .20, .36, .20, .20, .20,
     .20, .20, .20, .20, .20, .20, 
     .20, .20, .20, .20, .20, .20, .20, .20,
     .20, .20, .20, .20, .20, .20, .20, .20, .20)
output_20_x<-list()


for(i in 1:1000){
  data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
  colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                    "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                    "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                    "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                    "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                    "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                    "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                    "pead_sepsis_factors___6", "severe_illness_feature___1",
                    "severe_illness_feature___2", "severe_illness_feature___3",
                    "severe_illness_feature___4", "severe_illness_feature___5",
                    "severe_illness_feature___6", "severe_illness_feature___7",
                    "severe_illness_feature___8", "moderate_illness_feature___1",
                    "moderate_illness_feature___2", "moderate_illness_feature___3",
                    "moderate_illness_feature___4", "moderate_illness_feature___5",
                    "moderate_illness_feature___6", "moderate_illness_feature___7",
                    "moderate_illness_feature___8", "moderate_illness_feature___9")
  
  output_20_x[[i]]<-data
}


saveRDS(output_20_x, "output_20_x_1000.rds")


output_20<-list()

output_20_x<-readRDS("output_20_x_1000.rds")

for(i in 1:1000){
  data<-output_20_x[[i]]
  data<-as.data.frame(data)
  #Use regression equation to get Y values
  xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
    0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
    0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
    0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
    0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
    0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
    1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
    0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
    -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
    0.35*data$moderate_illness_feature___9
  
  #convert to probabilities
  p<-1/(1+exp(-xb))
  summary(p)
  
  #convert to y
  y<-rbinom(n=3500, size = 1, prob=p)
  output_20[[i]]<-cbind(y,data)
}

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_20[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)

#min 30% probabilities
#marginal probabilities
#generate p
p<-c(.55, .34, .65, .46, .30, .36, .30, .30, .30,
     .30, .30, .30, .30, .30, .30, 
     .30, .30, .30, .30, .30, .30, .30, .30,
     .30, .30, .30, .30, .30, .30, .30, .30, .30)
output_30_x<-list()


for(i in 1:1000){
  data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
  colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                    "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                    "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                    "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                    "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                    "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                    "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                    "pead_sepsis_factors___6", "severe_illness_feature___1",
                    "severe_illness_feature___2", "severe_illness_feature___3",
                    "severe_illness_feature___4", "severe_illness_feature___5",
                    "severe_illness_feature___6", "severe_illness_feature___7",
                    "severe_illness_feature___8", "moderate_illness_feature___1",
                    "moderate_illness_feature___2", "moderate_illness_feature___3",
                    "moderate_illness_feature___4", "moderate_illness_feature___5",
                    "moderate_illness_feature___6", "moderate_illness_feature___7",
                    "moderate_illness_feature___8", "moderate_illness_feature___9")
  
  output_30_x[[i]]<-data
}


saveRDS(output_30_x, "output_30_x_1000.rds")

output_30_x<-readRDS("output_30_x_1000.rds")


output_30<-list()



for(i in 1:1000){
  data<-output_30_x[[i]]
  data<-as.data.frame(data)
  #Use regression equation to get Y values
  xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
    0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
    0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
    0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
    0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
    0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
    1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
    0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
    -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
    0.35*data$moderate_illness_feature___9
  
  #convert to probabilities
  p<-1/(1+exp(-xb))
  summary(p)
  
  #convert to y
  y<-rbinom(n=3500, size = 1, prob=p)
  output_30[[i]]<-cbind(y,data)
}

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_30[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)

#min 40% probabilities
#marginal probabilities
#generate p
p<-c(.55, .40, .65, .46, .40, .40, .40, .40, .40,
     .40, .40, .40, .40, .40, .40, 
     .40, .40, .40, .40, .40, .40, .40, .40,
     .40, .40, .40, .40, .40, .40, .40, .40, .40)
output_40_x<-list()


for(i in 1:1000){
  data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
  colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                    "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                    "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                    "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                    "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                    "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                    "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                    "pead_sepsis_factors___6", "severe_illness_feature___1",
                    "severe_illness_feature___2", "severe_illness_feature___3",
                    "severe_illness_feature___4", "severe_illness_feature___5",
                    "severe_illness_feature___6", "severe_illness_feature___7",
                    "severe_illness_feature___8", "moderate_illness_feature___1",
                    "moderate_illness_feature___2", "moderate_illness_feature___3",
                    "moderate_illness_feature___4", "moderate_illness_feature___5",
                    "moderate_illness_feature___6", "moderate_illness_feature___7",
                    "moderate_illness_feature___8", "moderate_illness_feature___9")
  
  output_40_x[[i]]<-data
}


saveRDS(output_40_x, "output_40_x_1000.rds")


output_40_x<-readRDS("output_40_x_1000.rds")

output_40<-list()



for(i in 1:1000){
  data<-output_40_x[[i]]
  data<-as.data.frame(data)
  #Use regression equation to get Y values
  xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
    0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
    0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
    0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
    0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
    0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
    1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
    0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
    -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
    0.35*data$moderate_illness_feature___9
  
  #convert to probabilities
  p<-1/(1+exp(-xb))
  summary(p)
  
  #convert to y
  y<-rbinom(n=3500, size = 1, prob=p)
  output_40[[i]]<-cbind(y,data)
}

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_40[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)

#min 50% probabilities
#marginal probabilities
#generate p
p<-c(.55, .50, .65, .5, .50, .50, .50, .50, .50,
     .50, .50, .50, .50, .50, .50, 
     .50, .50, .50, .50, .50, .50, .50, .50,
     .50, .50, .50, .50, .50, .50, .50, .50, .50)
output_50_x<-list()


for(i in 1:1000){
  data<-rmvbin(n=3500, margprob = p, bincorr=cor_matrix)
  colnames(data)<-c("paed_sepsis_indicator___1", "paed_sepsis_indicator___2",
                    "paed_sepsis_indicator___3", "paed_sepsis_indicator___4",
                    "paed_sepsis_indicator___5", "paed_sepsis_indicator___6",
                    "paed_sepsis_indicator___7", "paed_sepsis_indicator___8", 
                    "paed_sepsis_indicator___9", "pead_sepsis_factors___1",
                    "pead_sepsis_factors___2", "pead_sepsis_factors___3",
                    "pead_sepsis_factors___4", "pead_sepsis_factors___5",
                    "pead_sepsis_factors___6", "severe_illness_feature___1",
                    "severe_illness_feature___2", "severe_illness_feature___3",
                    "severe_illness_feature___4", "severe_illness_feature___5",
                    "severe_illness_feature___6", "severe_illness_feature___7",
                    "severe_illness_feature___8", "moderate_illness_feature___1",
                    "moderate_illness_feature___2", "moderate_illness_feature___3",
                    "moderate_illness_feature___4", "moderate_illness_feature___5",
                    "moderate_illness_feature___6", "moderate_illness_feature___7",
                    "moderate_illness_feature___8", "moderate_illness_feature___9")
  
  output_50_x[[i]]<-data
}


saveRDS(output_50_x, "output_50_x_1000.rds")

output_50_x<-readRDS("output_50_x_1000.rds")
output_50<-list()



for(i in 1:1000){
  data<-output_50_x[[i]]
  data<-as.data.frame(data)
  #Use regression equation to get Y values
  xb<- -3.27 + 0.03*data$paed_sepsis_indicator___1 + 0.36*data$paed_sepsis_indicator___2 + -0.05*data$paed_sepsis_indicator___3 +
    0.40*data$paed_sepsis_indicator___4 + 0.30*data$paed_sepsis_indicator___5 + 0.12*data$paed_sepsis_indicator___6 + 0.18*data$paed_sepsis_indicator___7 +
    0.10*data$paed_sepsis_indicator___8 + -0.02*data$paed_sepsis_indicator___9 + 1.53*data$pead_sepsis_factors___1 + 0.16*data$pead_sepsis_factors___2 + 
    0.04*data$pead_sepsis_factors___3 + 1.11*data$pead_sepsis_factors___4 + 0.43*data$pead_sepsis_factors___5 + 0.61*data$pead_sepsis_factors___6 +
    0.63*data$severe_illness_feature___1 + 0.51*data$severe_illness_feature___2 + 0.66*data$severe_illness_feature___3 +
    0.76*data$severe_illness_feature___4 + 1.84*data$severe_illness_feature___5 + 0.69*data$severe_illness_feature___6+0.94*data$severe_illness_feature___7+
    1.93*data$severe_illness_feature___8 + -0.19*data$moderate_illness_feature___1 + 0.11*data$moderate_illness_feature___2+
    0.52*data$moderate_illness_feature___3+ -0.20*data$moderate_illness_feature___4 + 0.32*data$moderate_illness_feature___5 +
    -0.12*data$moderate_illness_feature___6+0.95*data$moderate_illness_feature___7 +-0.14*data$moderate_illness_feature___8 +
    0.35*data$moderate_illness_feature___9
  
  #convert to probabilities
  p<-1/(1+exp(-xb))
  summary(p)
  
  #convert to y
  y<-rbinom(n=3500, size = 1, prob=p)
  output_50[[i]]<-cbind(y,data)
}

count_sig<-list()

for(i in 1:1000){
  glm_data<-output_50[[i]]
  glm_data<-as.data.frame(glm_data)
  
  mod_sim<-glm(y ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
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
                 moderate_illness_feature___9, family = "binomial", data = glm_data)
  
  summary(mod_sim)
  
  check<-coef(summary(mod_sim))
  
  check<-as.data.frame(check)
  colnames(check)
  checks<-dplyr::filter(check,  `Pr(>|z|)` <.05)
  
  count_sig[[i]]<-checks
}


#extract row names for each significant predictor


rownames_list<-list()

for(i in 1:length(count_sig)){
  rownames_list[[i]]<-rownames(count_sig[[i]])
}


#count how many occurances of each variable and store in a readable format
rownames_list<-unlist(rownames_list)
rownames_list<-as.data.frame(rownames_list)
rownames_list$rownames_list<-as.factor(rownames_list$rownames_list)
store<-summary(rownames_list$rownames_list)
store<-as.data.frame(store)



####Generate data for random forest model 
#random forest 
library(tidyverse)
library(randomForest)
random_forest_data<-read.csv("random_forest_data.csv")
random_forest_data<-select(random_forest_data, -Pop_A)
colnames(random_forest_data)
random_forest_data$Pop_B<-as.factor(random_forest_data$Pop_B)

rf <- randomForest(Pop_B ~.,
                   ntree= 10000,
                   mtry=3,
                   data = random_forest_data, 
                   importance = TRUE)

importance_rf<-randomForest::importance(rf, type = 1)
importance_rf<-as.data.frame(importance_rf)
importance_rf<-arrange(importance_rf, MeanDecreaseAccuracy)

pred<-predict(rf, random_forest_data, type = "prob")
y_prob<-pred[,2]

library(pROC)
roc_result_K4 <- roc(random_forest_data$Pop_B,y_prob,smoothed = TRUE,
                     # arguments for ci
                     ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                     # arguments for plot
                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                     print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

#read in sim data x's
output_original_x<-readRDS("output_original_x.rds")
output_10_x<-readRDS("output_10_x.rds")
output_20_x<-readRDS("output_20_x.rds")
output_30_x<-readRDS("output_30_x.rds")
output_40_x<-readRDS("output_40_x.rds")
output_50_x<-readRDS("output_50_x.rds")

output_original<-list()

for(i in 1:1000){
data<-output_original_x[[i]]
pred_data<-predict(rf, data, type = "prob")

y_prob<-pred_data[,2]
y<-rbinom(n=3500, size = 1, prob=y_prob)
data<-cbind(data,y)

output_original[[i]]<-data
}

#data for random forest models
saveRDS(output_original, "output_original_1000.rds")

#get y 10%
output_10<-list()

for(i in 1:1000){
  data<-output_10_x[[i]]
  pred_data<-predict(rf, data, type = "prob")
  
  y_prob<-pred_data[,2]
  y<-rbinom(n=3500, size = 1, prob=y_prob)
  data<-cbind(data,y)
  
  
  
  output_10[[i]]<-data
}

saveRDS(output_10, "output_10_1000.rds")

#get y 20%
output_20<-list()

for(i in 1:1000){
  data<-output_20_x[[i]]
  pred_data<-predict(rf, data, type = "prob")
  
  y_prob<-pred_data[,2]
  y<-rbinom(n=3500, size = 1, prob=y_prob)
  data<-cbind(data,y)
  

  
  output_20[[i]]<-data
}

saveRDS(output_20, "output_20_1000.rds")

#get y 30%
output_30<-list()

for(i in 1:1000){
  data<-output_30_x[[i]]
  pred_data<-predict(rf, data, type = "prob")
  
  y_prob<-pred_data[,2]
  y<-rbinom(n=3500, size = 1, prob=y_prob)
  data<-cbind(data,y)
  

  
  output_30[[i]]<-data
}

saveRDS(output_30, "output_30_1000.rds")

#get y 40%
output_40<-list()

for(i in 1:1000){
  data<-output_40_x[[i]]
  pred_data<-predict(rf, data, type = "prob")
  
  y_prob<-pred_data[,2]
  y<-rbinom(n=3500, size = 1, prob=y_prob)
  data<-cbind(data,y)

  
  output_40[[i]]<-data
}

saveRDS(output_40, "output_40_1000.rds")

#get y 50%
output_50<-list()

for(i in 1:1000){
  data<-output_50_x[[i]]
  pred_data<-predict(rf, data, type = "prob")
  
  y_prob<-pred_data[,2]
  y<-rbinom(n=3500, size = 1, prob=y_prob)
  data<-cbind(data,y)
  
  
  output_50[[i]]<-data
}

saveRDS(output_50, "output_50_1000.rds")





#load HPC results
sim_original<-readRDS("importance_rf_sim-original_1000.rds")
sim_10<-readRDS("importance_rf_sim-10_1000.rds")
sim_20<-readRDS("importance_rf_sim-20_1000.rds")
sim_30<-readRDS("importance_rf_sim-30_1000.rds")
sim_40<-readRDS("importance_rf_sim-40_1000.rds")
sim_50<-readRDS("importance_rf_sim-50_1000.rds")


library(tidyverse)

#change to dataframe and compute row means
sim_original<-as.data.frame(sim_original)

sim_original_rank<-apply(sim_original, 2, function(x) dense_rank(desc(x)))
sim_original_rank<-as.data.frame(sim_original_rank)
sim_original_rank_mean<-mutate(sim_original_rank, avg_imp = rowMeans(sim_original_rank))
sim_original_rank_mean<-select(sim_original_rank_mean, avg_imp)

sim_original_rank_median<-apply(sim_original_rank, 1, median)
sim_original_rank_mean<-apply(sim_original_rank, 1, mean)

sim_original_rank<-t(sim_original_rank)
sim_original_rank<-as.data.frame(sim_original_rank)

summary(sim_original_rank$V32)

sim_original_rank$sim<-rep(1,1000)
sim_original_rank_indicators <-select(sim_original_rank, sim, V1:V9)
sim_original_rank_factors <-select(sim_original_rank, sim, V10:V15)
sim_original_rank_severe <-select(sim_original_rank, sim, V16:V23)
sim_original_rank_moderate <-select(sim_original_rank, sim, V24:V32)

#change to dataframe and compute row means
sim_10<-as.data.frame(sim_10)

sim_10_rank<-apply(sim_10, 2, function(x) dense_rank(desc(x)))
sim_10_rank<-as.data.frame(sim_10_rank)
sim_10_rank_mean<-mutate(sim_10_rank, avg_imp = rowMeans(sim_10_rank))
sim_10_rank_mean<-select(sim_10_rank_mean, avg_imp)

sim_10_rank_median<-apply(sim_10_rank, 1, median)

sim_10_rank<-t(sim_10_rank)
sim_10_rank<-as.data.frame(sim_10_rank)

summary(sim_10_rank$V32)

sim_10_rank$sim<-rep(2,1000)
sim_10_rank_indicators <-select(sim_10_rank, sim, V1:V9)
sim_10_rank_factors <-select(sim_10_rank, sim, V10:V15)
sim_10_rank_severe <-select(sim_10_rank, sim, V16:V23)
sim_10_rank_moderate <-select(sim_10_rank, sim, V24:V32)

#change to dataframe and compute row means
sim_20<-as.data.frame(sim_20)

sim_20_rank<-apply(sim_20, 2, function(x) dense_rank(desc(x)))
sim_20_rank<-as.data.frame(sim_20_rank)
sim_20_rank_mean<-mutate(sim_20_rank, avg_imp = rowMeans(sim_20_rank))
sim_20_rank_mean<-select(sim_20_rank_mean, avg_imp)

sim_20_rank_median<-apply(sim_20_rank, 1, median)

sim_20_rank<-t(sim_20_rank)
sim_20_rank<-as.data.frame(sim_20_rank)

summary(sim_20_rank$V32)

sim_20_rank$sim<-rep(3,1000)
sim_20_rank_indicators <-select(sim_20_rank, sim, V1:V9)
sim_20_rank_factors <-select(sim_20_rank, sim, V10:V15)
sim_20_rank_severe <-select(sim_20_rank, sim, V16:V23)
sim_20_rank_moderate <-select(sim_20_rank, sim, V24:V32)

#change to dataframe and compute row means
sim_30<-as.data.frame(sim_30)

sim_30_rank<-apply(sim_30, 2, function(x) dense_rank(desc(x)))
sim_30_rank<-as.data.frame(sim_30_rank)
sim_30_rank_mean<-mutate(sim_30_rank, avg_imp = rowMeans(sim_30_rank))
sim_30_rank_mean<-select(sim_30_rank_mean, avg_imp)

sim_30_rank_median<-apply(sim_30_rank, 1, median)

sim_30_rank<-t(sim_30_rank)
sim_30_rank<-as.data.frame(sim_30_rank)

summary(sim_30_rank$V32)

sim_30_rank$sim<-rep(4,1000)
sim_30_rank_indicators <-select(sim_30_rank, sim, V1:V9)
sim_30_rank_factors <-select(sim_30_rank, sim, V10:V15)
sim_30_rank_severe <-select(sim_30_rank, sim, V16:V23)
sim_30_rank_moderate <-select(sim_30_rank, sim, V24:V32)

#change to dataframe and compute row means
sim_40<-as.data.frame(sim_40)

sim_40_rank<-apply(sim_40, 2, function(x) dense_rank(desc(x)))
sim_40_rank<-as.data.frame(sim_40_rank)
sim_40_rank_mean<-mutate(sim_40_rank, avg_imp = rowMeans(sim_40_rank))
sim_40_rank_mean<-select(sim_40_rank_mean, avg_imp)

sim_40_rank_median<-apply(sim_40_rank, 1, median)

sim_40_rank<-t(sim_40_rank)
sim_40_rank<-as.data.frame(sim_40_rank)

summary(sim_40_rank$V1)

sim_40_rank$sim<-rep(5,1000)
sim_40_rank_indicators <-select(sim_40_rank, sim, V1:V9)
sim_40_rank_factors <-select(sim_40_rank, sim, V10:V15)
sim_40_rank_severe <-select(sim_40_rank, sim, V16:V23)
sim_40_rank_moderate <-select(sim_40_rank, sim, V24:V32)

#change to dataframe and compute row means
sim_50<-as.data.frame(sim_50)

sim_50_rank<-apply(sim_50, 2, function(x) dense_rank(desc(x)))
sim_50_rank<-as.data.frame(sim_50_rank)
sim_50_rank_mean<-mutate(sim_50_rank, avg_imp = rowMeans(sim_50_rank))
sim_50_rank_mean<-select(sim_50_rank_mean, avg_imp)

sim_50_rank_median<-apply(sim_50_rank, 1, median)

sim_50_rank<-t(sim_50_rank)
sim_50_rank<-as.data.frame(sim_50_rank)

summary(sim_50_rank$V32)

sim_50_rank$sim<-rep(6,1000)
sim_50_rank_indicators <-select(sim_50_rank, sim, V1:V9)
sim_50_rank_factors <-select(sim_50_rank, sim, V10:V15)
sim_50_rank_severe <-select(sim_50_rank, sim, V16:V23)
sim_50_rank_moderate <-select(sim_50_rank, sim, V24:V32)

#bind dataframes and change to long form indicators
all_indicators<-bind_rows(sim_original_rank_indicators, sim_10_rank_indicators)
all_indicators<-bind_rows(all_indicators, sim_20_rank_indicators)
all_indicators<-bind_rows(all_indicators, sim_30_rank_indicators)
all_indicators<-bind_rows(all_indicators, sim_40_rank_indicators)
all_indicators<-bind_rows(all_indicators, sim_50_rank_indicators)

all_indicators_long<- pivot_longer(all_indicators, cols = V1:V9, names_to = "Variable", values_to = "rank")
all_indicators_long$sim<-as.factor(all_indicators_long$sim)

all_indicators_long$Variable<-as.factor(all_indicators_long$Variable)
levels(all_indicators_long$Variable)<-c("Indicator 1", "Indicator 2", "Indicator 3", "Indicator 4", "Indicator 5", "Indicator 6", "Indicator 7", "Indicator 8",
                                        "Indicator 9")
all_indicators_long$Variable <- factor(all_indicators_long$Variable, levels = c("Indicator 9", "Indicator 8", "Indicator 7", "Indicator 6", "Indicator 5", 
                                                                                "Indicator 4", "Indicator 3", "Indicator 2", "Indicator 1"))


levels(all_indicators_long$sim)<-c("Original %", "Minimum 10%", "Minimum 20%", "Minimum 30%", "Minimum 40%", "Minimum 50%")


indicators_plot<-ggplot(all_indicators_long, aes(x = Variable, y = rank))+
  geom_boxplot(aes(fill=sim))+
  theme_bw()+
  coord_flip()+
  labs(fill = "Simulation", y = "", x = "")

#bind dataframes and change to long form factors
all_factors<-bind_rows(sim_original_rank_factors, sim_10_rank_factors)
all_factors<-bind_rows(all_factors, sim_20_rank_factors)
all_factors<-bind_rows(all_factors, sim_30_rank_factors)
all_factors<-bind_rows(all_factors, sim_40_rank_factors)
all_factors<-bind_rows(all_factors, sim_50_rank_factors)

all_factors_long<- pivot_longer(all_factors, cols = V10:V15, names_to = "Variable", values_to = "rank")
all_factors_long$sim<-as.factor(all_factors_long$sim)

all_factors_long$Variable<-as.factor(all_factors_long$Variable)
levels(all_factors_long$Variable)<-c("Factor 1", "Factor 2", "Factor 3", "Factor 4", "Factor 5", "Factor 6")
all_factors_long$Variable <- factor(all_factors_long$Variable, levels = c("Factor 6", "Factor 5", "Factor 4", "Factor 3", "Factor 2", "Factor 1"))


levels(all_factors_long$sim)<-c("Original %", "Minimum 10%", "Minimum 20%", "Minimum 30%", "Minimum 40%", "Minimum 50%")

factors_plot<-ggplot(all_factors_long, aes(x = Variable, y = rank))+
  geom_boxplot(aes(fill=sim))+
  theme_bw()+
  coord_flip()+
  labs(fill = "Simulation", y = "", x = "")

#bind dataframes and change to long form severe
all_severe<-bind_rows(sim_original_rank_severe, sim_10_rank_severe)
all_severe<-bind_rows(all_severe, sim_20_rank_severe)
all_severe<-bind_rows(all_severe, sim_30_rank_severe)
all_severe<-bind_rows(all_severe, sim_40_rank_severe)
all_severe<-bind_rows(all_severe, sim_50_rank_severe)

all_severe_long<- pivot_longer(all_severe, cols = V16:V23, names_to = "Variable", values_to = "rank")
all_severe_long$sim<-as.factor(all_severe_long$sim)

all_severe_long$Variable<-as.factor(all_severe_long$Variable)
levels(all_severe_long$Variable)<-c("Severe 1", "Severe 2", "Severe 3", "Severe 4", "Severe 5", "Severe 6","Severe 7", "Severe 8")
all_severe_long$Variable <- factor(all_severe_long$Variable, levels = c("Severe 8", "Severe 7", "Severe 6", "Severe 5", "Severe 4",
                                                                        "Severe 3","Severe 2", "Severe 1"))


levels(all_severe_long$sim)<-c("Original %", "Minimum 10%", "Minimum 20%", "Minimum 30%", "Minimum 40%", "Minimum 50%")


severe_plot<-ggplot(all_severe_long, aes(x = Variable, y = rank))+
  geom_boxplot(aes(fill=sim))+
  theme_bw()+
  coord_flip()+
  labs(fill = "Simulation", y = "", x = "")

#bind dataframes and change to long form moderate
all_moderate<-bind_rows(sim_original_rank_moderate, sim_10_rank_moderate)
all_moderate<-bind_rows(all_moderate, sim_20_rank_moderate)
all_moderate<-bind_rows(all_moderate, sim_30_rank_moderate)
all_moderate<-bind_rows(all_moderate, sim_40_rank_moderate)
all_moderate<-bind_rows(all_moderate, sim_50_rank_moderate)




all_moderate_long<- pivot_longer(all_moderate, cols = V24:V32, names_to = "Variable", values_to = "rank")
all_moderate_long$sim<-as.factor(all_moderate_long$sim)
all_moderate_long$Variable<-as.factor(all_moderate_long$Variable)

levels(all_moderate_long$Variable)<-c("Moderate 1", "Moderate 2", "Moderate 3", "Moderate 4", "Moderate 5", "Moderate 6","Moderate 7",
                                      "Moderate 8", "Moderate 9")
all_moderate_long$Variable <- factor(all_moderate_long$Variable, levels = c("Moderate 9", "Moderate 8", "Moderate 7", "Moderate 6", 
                                                                            "Moderate 5", "Moderate 4","Moderate 3",
                                                                            "Moderate 2", "Moderate 1"))


levels(all_moderate_long$sim)<-c("Original %", "Minimum 10%", "Minimum 20%", "Minimum 30%", "Minimum 40%", "Minimum 50%")

moderate_plot<-ggplot(all_moderate_long, aes(x = Variable, y = rank))+
  geom_boxplot(aes(fill=sim))+
  theme_bw()+
  coord_flip()+
  labs(fill = "Simulation", y = "", x = "")

library(patchwork)

add_global_label <- function(pwobj, Xlab = NULL, Ylab = NULL, Xgap = 0.03, Ygap = 0.03, ...) {
  ylabgrob <- patchwork::plot_spacer()
  if (!is.null(Ylab)) {
    ylabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Ylab, angle = 90, ...) +
      theme_void()
  }
  if (!is.null(Xlab)) {
    xlabgrob <- ggplot() +
      geom_text(aes(x = .5, y = .5), label = Xlab, ...) +
      theme_void()
  }
  if (!is.null(Ylab) & is.null(Xlab)) {
    return((ylabgrob + patchworkGrob(pwobj)) + 
             patchwork::plot_layout(widths = 100 * c(Ygap, 1 - Ygap)))
  }
  if (is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = c(0, 100),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  if (!is.null(Ylab) & !is.null(Xlab)) {
    return((ylabgrob + pwobj) + 
             (xlabgrob) +
             patchwork::plot_layout(heights = 100 * c(1 - Xgap, Xgap),
                                    widths = 100 * c(Ygap, 1 - Ygap),
                                    design = "
                                   AB
                                   CC
                                   "
             ))
  }
  return(pwobj)
}


(indicators_plot + factors_plot +severe_plot+ moderate_plot+plot_layout(ncol = 4))%>%
  add_global_label(Xlab = "Rank")+ plot_layout(guides = 'collect') & theme(legend.position = 'bottom')