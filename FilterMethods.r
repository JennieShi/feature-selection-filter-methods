#libraries
install.packages("FSelector")
install.packages("mlbench")
install.packages("rpart")
install.packages("caret")

library(FSelector)
library(randomForest)
library(RWeka)
library(digest)
library(entropy)
library(mlbench)
library(rpart)
library(caret)
library(data.table)

#setting working directionary
project_directory = "C:/Users/shijie/Dropbox/APA Seminar - Feature Selection/Jie/codes"
setwd(project_directory)

#loading data 
load("C:/Users/shijie/Dropbox/APA Seminar - Feature Selection/Jie/codes/data_2.10_full.rda")

# converting target to a factor
df$returnQuantity <- as.factor(df$returnQuantity)
df <- df[!is.na(df$returnQuantity),]

rm(df)

# CFS Filter method
df_cfs_subset <- cfs(returnQuantity~., df)
f_df_cfs_subset <- as.simple.formula(df_cfs_subset, "returnQuantity")
print(f_df_cfs_subset)

#CHI-SQUARED Filter 
weight_chis <- chi.squared(returnQuantity~., df)
print(weight_chis)
df_chis_subset <- cutoff.k.percent(weight_chis,0.2)
f_df_chis_subset <- as.simple.formula(df_chis_subset,"returnQuantity")
print(f_df_chis_subset)

#CONSISTENCY Filter
df_consistency_subset <- consistency(returnQuantity~., df)
f_df_consistency_subset <- as.simple.formula(df_consistency_subset, "returnQuantity")
print(f_df_consistency_subset)

#INFORMATION GAIN Filter
weight_ig <- information.gain(returnQuantity~., df)
print(weight_ig)
df_ig_subset <- cutoff.k.percent(weight_ig,0.2) 
f_df_ig_subset <- as.simple.formula(df_ig_subset, "returnQuantity")
print(f_df_ig_subset)

#GAIN RATIO Filter
weight_gr <- gain.ratio(returnQuantity~., df)
print(weight_gr)
df_gr_subset <- cutoff.k.percent(weight_gr,0.2) 
f_df_gr_subset <- as.simple.formula(df_gr_subset, "returnQuantity")
print(f_df_gr_subset)

#SYMMETRICAL UNCERTAINTY
weight_su <- symmetrical.uncertainty(returnQuantity~., df)
print(weight_su)
df_su_subset <- cutoff.k.percent(weight_su,0.2) 
f_df_su_subset <- as.simple.formula(df_su_subset, "returnQuantity")
print(f_df_su_subset)

#OneR Filter
weight_oneR <- oneR(returnQuantity~., df)
print(weight_oneR)
df_oneR_subset <- cutoff.k.percent(weight_oneR,0.2) 
f_df_oneR_subset <- as.simple.formula(df_oneR_subset, "returnQuantity")
print(f_df_oneR_subset)

#Random forest importance Filter
weight_rfi <- random.forest.importance(returnQuantity~., df, importance.type = 1)
print(weight_rfi)
df_rfi_subset <- cutoff.k.percent(weight_rfi,0.2) 
f_df_rfi_subset <- as.simple.formula(df_rfi_subset, "returnQuantity")
print(f_df_rfi_subset)

#RELIEF Filter
weight_relief <- relief(returnQuantity~., df, neighbours.count = 5, sample.size = 20)
print(weight_relief)
df_relief_subset <- cutoff.k.percent(weight_relief,0.2)
f_df_relief_subset <- as.simple.formula(df_relief_subset, "returnQuantity")
print(f_df_relief_subset)

