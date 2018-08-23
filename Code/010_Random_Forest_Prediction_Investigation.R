# Random Forest Model build

table(halo_spatial$After)

table(is.na(halo_spatial$After))

# Check for any Date Issues

# datetime_issues = halo_spatial[is.na(halo_spatial$Datetime) == TRUE, ]

# Remove all rows with 0 for all averages.

nrow(halo_spatial[halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0, ])

halo_spatial = halo_spatial[!(halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0), ]




# Identify any Troublesome NAs

# sapply(halo_spatial, function(x) sum(is.na(x)))

# After Data, 3 lanes and After Column

variables = c("AveSpeed", "AveOccupancy", "AveHeadway", "TotalFlow")

outputs = c("Before", "After")

# Create Training Datasets

mydata = halo_spatial
mydata = as.data.frame(mydata)
mydata$After = as.factor(mydata$After)
mydata$Before = as.factor(mydata$Before)

validation  = mydata[month(mydata$Datetime) == 1, ]
mydata = mydata[month(mydata$Datetime) != 1, ]
other = mydata[mydata$Before != 1 & mydata$After != 1, ]
mydata = mydata[mydata$Before == 1 | mydata$After == 1, ]

# idx = sample(nrow(mydata), 2750000) # Greater than Half of sample

# Create Outputs - Run Number of Variables Individually

winner_train_a = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))
winner_test_a = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))
winner_validation_a = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))
winner_train_b = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))
winner_test_b = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))
winner_validation_b = as.data.frame(matrix(ncol = 4, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula", "Precision"))))

# Timer

start_time <- Sys.time()

# One Variable

for (iteration in 1:20) {

  idx = sample(nrow(mydata), 70000)
  random_train = sample(nrow(other), 100000)
  other2 = other[-random_train, ]
  random_test = sample(nrow(other2), 100000)

  train_data = mydata[idx, ]
  train_extra = other[random_train, ]
  train_data = rbind(train_data, train_extra)
  table(train_data$After)

  test_data = mydata[-idx, ]
  test_extra = other2[random_test, ]
  test_data = rbind(test_data, test_extra)
  table(test_data$After)

  formulas_after = rep(NA, length(variables))
  formulas_before = rep(NA, length(variables))
  for (a in 1:length(variables)){
    formulas_after[a] = paste("After ~", variables[a])
    formulas_before[a] = paste("Before ~", variables[a])
  }

  best_train_a = c(0,0,"",0)
  best_test_a = c(0,0,"",0)
  best_validation_a = c(0,0,"",0)

  best_train_b = c(0,0,"",0)
  best_test_b = c(0,0,"",0)
  best_validation_b = c(0,0,"",0)


  for (i in 1:length(formulas_after)){
  # Print Variable to identify loop location
    print(variables[i])

    current_Forest_after = randomForest(formula = as.formula(formulas_after[i]), data = train_data, ntree = 500)
    current_Forest_before = randomForest(formula = as.formula(formulas_before[i]), data = train_data, ntree = 500)

  
  # Predict Probability using model based on Test Data
    pred_a = predict(current_Forest_after, newdata=train_data, type = "response")
    pred_test_a = predict(current_Forest_after, newdata=test_data, type = "response")
    pred_scale_a = predict(current_Forest_after, newdata=validation, type = "response")
  
    pred_b = predict(current_Forest_before, newdata=train_data, type = "response")
    pred_test_b = predict(current_Forest_before, newdata=test_data, type = "response")
    pred_scale_b = predict(current_Forest_before, newdata=validation, type = "response")
 
  # After
  
    cm = table(train_data$After, pred_a)
    print("Confusion Matrix Train")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_a, train_data$After)$byClass[11])
    result = confusionMatrix(pred_a, train_data$After)$byClass[11]
    if (result > best_train_a[2]) {best_train_a = c(Sensitivity,result, formulas_after[i], Precision)} 
    print("---------------------")
  
    cm = table(test_data$After, pred_test_a)
    print("Confusion Matrix Test")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
  
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
    result = confusionMatrix(pred_test_a, test_data$After)$byClass[11]
    if (result > best_test_a[2]) {best_test_a = c(Sensitivity,result, formulas_after[i], Precision)}

    print("---------------------")
  
    cm = table(validation$After, pred_scale_a)
    print("Confusion Matrix Scale")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
  
    # Print Results
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
    result = confusionMatrix(pred_scale_a, validation$After)$byClass[11]
    if (result > best_validation_a[2]) {best_validation_a = c(Sensitivity,result, formulas_after[i], Precision)}
  
    print("---------------------")
    
    # Before Testing - Not suitable approach
  
    # # Before 
    # 
    # cm = table(train_data$After, pred_b)
    # print("Confusion Matrix Train")
    # print(cm)
    # Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    # Specificity = cm[1,1]/sum(cm[1,])
    # Sensitivity = cm[2,2]/sum(cm[2,])
    # Precision = cm[2,2]/sum(cm[,2])
    # 
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_b, train_data$After)$byClass[11])
    # result = confusionMatrix(pred_b, train_data$After)$byClass[11]
    # if (result > best_train_b[2]) {best_train_b = c(Sensitivity,result, formulas_before[i], Precision)} 
    # print("---------------------")
    # 
    # cm = table(test_data$After, pred_test_b)
    # print("Confusion Matrix Test")
    # print(cm)
    # Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    # Specificity = cm[1,1]/sum(cm[1,])
    # Sensitivity = cm[2,2]/sum(cm[2,])
    # Precision = cm[2,2]/sum(cm[,2])
    # 
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_test_b, test_data$After)$byClass[11])
    # result = confusionMatrix(pred_test_b, test_data$After)$byClass[11]
    # if (result > best_test_b[2]) {best_test_b = c(Sensitivity,result, formulas_before[i])}
    # 
    # print("---------------------")
    # 
    # cm = table(validation$After, pred_scale_b)
    # print("Confusion Matrix Scale")
    # print(cm)
    # Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    # Specificity = cm[1,1]/sum(cm[1,])
    # Sensitivity = cm[2,2]/sum(cm[2,])
    # Precision = cm[2,2]/sum(cm[,2])
    # 
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_scale_b, validation$After)$byClass[11])
    # result = confusionMatrix(pred_scale_b, validation$After)$byClass[11]
    # if (result > best_validation_b[2]) {best_validation_b = c(Sensitivity,result, formulas_before[i])}
    # 
    # print("---------------------")

  }

  print(paste("Best After Train: ",best_train_a))
  print(paste("Best After Test: ",best_test_a))
  print(paste("Best After Scaled Validation: ",best_validation_a))

  winner_train_a[iteration,1] = best_train_a[1]
  winner_train_a[iteration,2] = best_train_a[2]
  winner_train_a[iteration,3] = best_train_a[3]
  winner_train_a[iteration,4] = best_train_a[4]

  winner_test_a[iteration,1] = best_test_a[1]
  winner_test_a[iteration,2] = best_test_a[2]
  winner_test_a[iteration,3] = best_test_a[3]
  winner_test_a[iteration,4] = best_test_a[4]
  
  winner_validation_a[iteration,1] = best_validation_a[1]
  winner_validation_a[iteration,2] = best_validation_a[2]
  winner_validation_a[iteration,3] = best_validation_a[3]
  winner_validation_a[iteration,4] = best_validation_a[4]
  

# 
#   winner_train_b[iteration,1] = best_train_b[1]
#   winner_train_b[iteration,2] = best_train_b[2]
#   winner_train_b[iteration,3] = best_train_b[3]
# 
#   winner_test_b[iteration,1] = best_test_b[1]
#   winner_test_b[iteration,2] = best_test_b[2]
#   winner_test_b[iteration,3] = best_test_b[3]
# 
#   winner_validation_b[iteration,1] = best_validation_b[1]
#   winner_validation_b[iteration,2] = best_validation_b[2]
#   winner_validation_b[iteration,3] = best_validation_b[3]

}

end_time <- Sys.time()
  
end_time - start_time

write.csv(winner_train_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_after_1VAR.csv",row.names=FALSE)
write.csv(winner_test_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_after_1VAR.csv",row.names=FALSE)
write.csv(winner_validation_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_after_1VAR.csv",row.names=FALSE)
write.csv(winner_train_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_before_1VAR.csv",row.names=FALSE)
write.csv(winner_test_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_before_1VAR.csv",row.names=FALSE)
write.csv(winner_validation_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_before_1VAR.csv",row.names=FALSE)


# Print Winner Results

# table(winner_train_a[3])
# 
# table(winner_test_a[3])
# 
# table(winner_validation_a[3])
# 
# table(winner_train_b[3])
# 
# table(winner_test_b[3])
# 
# table(winner_validation_b[3])

# 2nd Variable After ################################################################################################

winner_train_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_train_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))

variables = c("AveOccupancy", "AveHeadway", "TotalFlow")

start_time <- Sys.time()

for (iteration in 1:20) {
  
  idx = sample(nrow(mydata), 70000)
  random_train = sample(nrow(other), 100000)
  other2 = other[-random_train, ]
  random_test = sample(nrow(other2), 100000)
  
  train_data = mydata[idx, ]
  train_extra = other[random_train, ]
  train_data = rbind(train_data, train_extra)
  table(train_data$After)
  
  test_data = mydata[-idx, ]
  test_extra = other2[random_test, ]
  test_data = rbind(test_data, test_extra)
  table(test_data$After)
  
  formulas_after = rep(NA, length(variables))

  for (a in 1:length(variables)){
    formulas_after[a] = paste("After ~ AveSpeed + ", variables[a])

  }
  best_train_a = c(0,0,"")
  best_test_a = c(0,0,"")
  best_validation_a = c(0,0,"")
  
  
  for (i in 1:length(formulas_after)){
    # Print Variable to identify loop location
    print(variables[i])
    
    current_Forest_after = randomForest(formula = as.formula(formulas_after[i]), data = train_data, ntree = 500)
    
    # Predict Probability using model based on Test Data
    pred_a = predict(current_Forest_after, newdata=train_data, type = "response")
    pred_test_a = predict(current_Forest_after, newdata=test_data, type = "response")
    pred_scale_a = predict(current_Forest_after, newdata=validation, type = "response")

    # After
    
    cm = table(train_data$After, pred_a)
    print("Confusion Matrix Train")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_a, train_data$After)$byClass[11])
    result = confusionMatrix(pred_a, train_data$After)$byClass[11]
    if (result > best_train_a[2]) {best_train_a = c(Sensitivity,result, formulas_after[i])} 
    print("---------------------")
    
    cm = table(test_data$After, pred_test_a)
    print("Confusion Matrix Test")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
    result = confusionMatrix(pred_test_a, test_data$After)$byClass[11]
    if (result > best_test_a[2]) {best_test_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
    cm = table(validation$After, pred_scale_a)
    print("Confusion Matrix Scale")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])    
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
    result = confusionMatrix(pred_scale_a, validation$After)$byClass[11]
    if (result > best_validation_a[2]) {best_validation_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
  }
  
  print(paste("Best After Train: ",best_train_a))
  print(paste("Best After Test: ",best_test_a))
  print(paste("Best After Scaled Validation: ",best_validation_a))
  
  winner_train_a[iteration,1] = best_train_a[1]
  winner_train_a[iteration,2] = best_train_a[2]
  winner_train_a[iteration,3] = best_train_a[3]
  
  winner_test_a[iteration,1] = best_test_a[1]
  winner_test_a[iteration,2] = best_test_a[2]
  winner_test_a[iteration,3] = best_test_a[3]
  
  winner_validation_a[iteration,1] = best_validation_a[1]
  winner_validation_a[iteration,2] = best_validation_a[2]
  winner_validation_a[iteration,3] = best_validation_a[3]
  
}

end_time <- Sys.time()

end_time - start_time

write.csv(winner_train_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_after_2VAR.csv",row.names=FALSE)
write.csv(winner_test_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_after_2VAR.csv",row.names=FALSE)
write.csv(winner_validation_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_after_2VAR.csv",row.names=FALSE)


# After 3rd Variable ##############################################################################################

winner_train_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_train_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))


formulas_after = c("After ~ AveSpeed + TotalFlow + AveOccupancy", "After ~ AveSpeed + AveHeadway + TotalFlow", "After ~ AveSpeed + AveHeadway + AveOccupancy")

start_time <- Sys.time()

for (iteration in 1:20) {
  
  idx = sample(nrow(mydata), 70000)
  random_train = sample(nrow(other), 100000)
  other2 = other[-random_train, ]
  random_test = sample(nrow(other2), 100000)
  
  train_data = mydata[idx, ]
  train_extra = other[random_train, ]
  train_data = rbind(train_data, train_extra)
  table(train_data$After)
  
  test_data = mydata[-idx, ]
  test_extra = other2[random_test, ]
  test_data = rbind(test_data, test_extra)
  table(test_data$After)
  
  best_train_a = c(0,0,"")
  best_test_a = c(0,0,"")
  best_validation_a = c(0,0,"")
  
  
  for (i in 1:length(formulas_after)){
    # Print Variable to identify loop location
    print(formulas_after[i])
    
    current_Forest_after = randomForest(formula = as.formula(formulas_after[i]), data = train_data, ntree = 500)
    
    # Predict Probability using model based on Test Data
    pred_a = predict(current_Forest_after, newdata=train_data, type = "response")
    pred_test_a = predict(current_Forest_after, newdata=test_data, type = "response")
    pred_scale_a = predict(current_Forest_after, newdata=validation, type = "response")
    
    # After
    
    cm = table(train_data$After, pred_a)
    print("Confusion Matrix Train")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    # 
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_a, train_data$After)$byClass[11])
    result = confusionMatrix(pred_a, train_data$After)$byClass[11]
    if (result > best_train_a[2]) {best_train_a = c(Sensitivity,result, formulas_after[i])} 
    print("---------------------")
    
    cm = table(test_data$After, pred_test_a)
    print("Confusion Matrix Test")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
    result = confusionMatrix(pred_test_a, test_data$After)$byClass[11]
    if (result > best_test_a[2]) {best_test_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
    cm = table(validation$After, pred_scale_a)
    print("Confusion Matrix Scale")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    # 
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
    result = confusionMatrix(pred_scale_a, validation$After)$byClass[11]
    if (result > best_validation_a[2]) {best_validation_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
  }
  
  print(paste("Best After Train: ",best_train_a))
  print(paste("Best After Test: ",best_test_a))
  print(paste("Best After Scaled Validation: ",best_validation_a))
  
  winner_train_a[iteration,1] = best_train_a[1]
  winner_train_a[iteration,2] = best_train_a[2]
  winner_train_a[iteration,3] = best_train_a[3]
  
  winner_test_a[iteration,1] = best_test_a[1]
  winner_test_a[iteration,2] = best_test_a[2]
  winner_test_a[iteration,3] = best_test_a[3]
  
  winner_validation_a[iteration,1] = best_validation_a[1]
  winner_validation_a[iteration,2] = best_validation_a[2]
  winner_validation_a[iteration,3] = best_validation_a[3]
  
}

end_time <- Sys.time()

end_time - start_time

write.csv(winner_train_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_after_3VAR.csv",row.names=FALSE)
write.csv(winner_test_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_after_3VAR.csv",row.names=FALSE)
write.csv(winner_validation_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_after_3VAR.csv",row.names=FALSE)

# After 4th Variable ####################################################################################################

winner_train_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_train_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))


formulas_after = c("After ~ AveSpeed + TotalFlow + AveOccupancy + AveHeadway")
i = 1
start_time <- Sys.time()

for (iteration in 1:20) {
  
  idx = sample(nrow(mydata), 70000)
  random_train = sample(nrow(other), 100000)
  other2 = other[-random_train, ]
  random_test = sample(nrow(other2), 100000)
  
  train_data = mydata[idx, ]
  train_extra = other[random_train, ]
  train_data = rbind(train_data, train_extra)
  table(train_data$After)
  
  test_data = mydata[-idx, ]
  test_extra = other2[random_test, ]
  test_data = rbind(test_data, test_extra)
  table(test_data$After)
  
  best_train_a = c(0,0,"")
  best_test_a = c(0,0,"")
  best_validation_a = c(0,0,"")
  
  

    # Print Variable to identify loop location
    print(formulas_after[i])
    
    current_Forest_after = randomForest(formula = as.formula(formulas_after[i]), data = train_data, ntree = 500)
    
    # Predict Probability using model based on Test Data
    pred_a = predict(current_Forest_after, newdata=train_data, type = "response")
    pred_test_a = predict(current_Forest_after, newdata=test_data, type = "response")
    pred_scale_a = predict(current_Forest_after, newdata=validation, type = "response")
    
    # After
    
    cm = table(train_data$After, pred_a)
    print("Confusion Matrix Train")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_a, train_data$After)$byClass[11])
    result = confusionMatrix(pred_a, train_data$After)$byClass[11]
    if (result > best_train_a[2]) {best_train_a = c(Sensitivity,result, formulas_after[i])} 
    print("---------------------")
    
    cm = table(test_data$After, pred_test_a)
    print("Confusion Matrix Test")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
    result = confusionMatrix(pred_test_a, test_data$After)$byClass[11]
    if (result > best_test_a[2]) {best_test_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
    cm = table(validation$After, pred_scale_a)
    print("Confusion Matrix Scale")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    # Print Results
    
    # print(paste("Accuracy:", Accuracy)) # How right is it
    # print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    # print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    # print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    # print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
    result = confusionMatrix(pred_scale_a, validation$After)$byClass[11]
    if (result > best_validation_a[2]) {best_validation_a = c(Sensitivity,result, formulas_after[i])}
    
    print("---------------------")
    
  
  print(paste("Best After Train: ",best_train_a))
  print(paste("Best After Test: ",best_test_a))
  print(paste("Best After Scaled Validation: ",best_validation_a))
  
  winner_train_a[iteration,1] = best_train_a[1]
  winner_train_a[iteration,2] = best_train_a[2]
  winner_train_a[iteration,3] = best_train_a[3]
  
  winner_test_a[iteration,1] = best_test_a[1]
  winner_test_a[iteration,2] = best_test_a[2]
  winner_test_a[iteration,3] = best_test_a[3]
  
  winner_validation_a[iteration,1] = best_validation_a[1]
  winner_validation_a[iteration,2] = best_validation_a[2]
  winner_validation_a[iteration,3] = best_validation_a[3]
  
}

end_time <- Sys.time()

end_time - start_time

write.csv(winner_train_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_after_4VAR.csv",row.names=FALSE)
write.csv(winner_test_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_after_4VAR.csv",row.names=FALSE)
write.csv(winner_validation_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_after_4VAR.csv",row.names=FALSE)

###############################################################

# Predict 28/01/2016 Collision

###############################################################

mydata = halo_spatial
mydata = as.data.frame(mydata)
mydata$After = as.factor(mydata$After)
mydata$Before = as.factor(mydata$Before)

Collision_Comparision  = mydata[month(mydata$Datetime) == 1 &
                                  day(mydata$Datetime) == 28 & 
                                  mydata$Geographic_Address == "M1/2519B", ]

mydata = mydata[month(mydata$Datetime) != 1 & day(mydata$Datetime) != 28, ]
other = mydata[mydata$Before != 1 & mydata$After != 1, ]
mydata = mydata[mydata$Before == 1 | mydata$After == 1, ]
random_train = sample(nrow(other), 100000)

train_extra = other[random_train, ]
train_data = rbind(mydata, 
                   train_extra)


formula_test = "After ~ AveSpeed + TotalFlow + AveOccupancy"

Test_Forest = randomForest(formula = as.formula(formula_test), 
                           data = train_data, ntree = 500)

prediction = predict(Test_Forest, 
                     newdata=Collision_Comparision, 
                     type = "response")

table(Collision_Comparision$After)

confusionMatrix(prediction, 
                Collision_Comparision$After, 
                positive = "1")

# Test Result
#             Reference
# Prediction    0    1
# 0          1244    0
# 1           132   61
# 
# Accuracy : 0.9081         
# 95% CI : (0.892, 0.9226)
# No Information Rate : 0.9576         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.4445         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 1.00000        
# Specificity : 0.90407        
# Pos Pred Value : 0.31606        
# Neg Pred Value : 1.00000        
# Prevalence : 0.04245        
# Detection Rate : 0.04245        
# Detection Prevalence : 0.13431        
# Balanced Accuracy : 0.95203        
# 
# 'Positive' Class : 1

pred= as.data.frame(prediction)

Collision_results = cbind(Collision_Comparision, pred)

# Visualisations

# # Create Graphs
# stat19 = as.data.frame(readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Stat19_2016_2km_Subset.csv")))
# 
# TC1A = stat19[stat19$Accident_Index == "2016406CA0222",]
# 
# TC1TD = Collision_results[Collision_results$Geographic_Address == "M1/2519B",]
# 
# # Speed
# 
# Prediction = ggplot(data = TC1TD, aes(x = Time_GMT)) +
#   geom_point(mapping = aes(y = TC1TD$prediction), colour = "black", alpha = 0.1) +
#   geom_vline(xintercept= TC1A$Time, colour = "red") +
#   geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "green") + 
#   geom_vline(xintercept= hms(TC1A$Time) + hours(1), colour = "orange") + 
#   ylab("Prediction After a Collision")
# 
# Speed_pl = ggplot(data = TC1TD, aes(x = Time_GMT)) +
#   geom_point(mapping = aes(y = AveSpeed), colour = "blue") +
#   geom_vline(xintercept= TC1A$Time, colour = "red") +
#   geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "green") + 
#   geom_vline(xintercept= hms(TC1A$Time) + hours(1), colour = "orange")
#   
# Occupancy_pl = ggplot(data = TC1TD, aes(x = Time_GMT)) +
#   geom_point(mapping = aes(y = AveOccupancy), colour = "green") +
#   geom_vline(xintercept= TC1A$Time, colour = "red") +
#   geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "green") + 
#   geom_vline(xintercept= hms(TC1A$Time) + hours(1), colour = "orange")
# 
# 
# # Not used in Formula
# # AveHeadway_pl = ggplot(data = TC1TD, aes(x = Time_GMT)) +
# #   geom_point(mapping = aes(y = AveHeadway), colour = "purple") +
# #   geom_vline(xintercept= TC1A$Time, colour = "red") +
# #   geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "green") + 
# #   geom_vline(xintercept= hms(TC1A$Time) + hours(1), colour = "orange")
# #   
# TotalFlow_pl = ggplot(data = TC1TD, aes(x = Time_GMT)) +
#   geom_point(mapping = aes(y = TotalFlow), colour = "darkblue") +
#   geom_vline(xintercept= TC1A$Time, colour = "red") +
#   geom_vline(xintercept= hms(TC1A$Time) + hours(3) + minutes(24), colour = "green") + 
#   geom_vline(xintercept= hms(TC1A$Time) + hours(1), colour = "orange")
#   
# grid.arrange(Prediction,Speed_pl,Occupancy_pl,TotalFlow_pl, nrow = 1, ncol = 4, top = "M1/2519B Collision Prediction")
# 
# grid.newpage()
# pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 3, heights = unit(c(0.5, 6, 6), "null"))))
# grid.text("Detection of Collision Example Using Chosen Model: AveSpeed + TotalFlow + AveOccupancy", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))
# print(Prediction, vp=viewport(layout.pos.row = 3, layout.pos.col = 1:3))
# print(Speed_pl, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(Occupancy_pl, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
# print(TotalFlow_pl, vp=viewport(layout.pos.row = 2, layout.pos.col = 3))
# 

# C = ggplot(data = halo_spatial, aes(AveSpeed, AveOccupancy), colour = After) + geom_point()
# 
# After = halo_spatial[halo_spatial$After == 1, ]
# 
# Before = halo_spatial[halo_spatial$Before == 1, ]
# 
# 
# A = ggplot(data = After, aes(AveSpeed, AveOccupancy)) + geom_point()
# B = ggplot(data = Before, aes(AveSpeed, AveOccupancy)) + geom_point()
# 
# 
# grid.arrange(A, B, C, nrow = 1, ncol = 3, top = "Ave Occupancy Vs Speed Within Hour of Major Crash")
# 
# 
# After[After$AveSpeed == 0 & After$AveOccupancy == 0 & After$AveHeadway == 0, ] = NA
# 
# AveDens = table(cut(After$AveSpeed, 50), cut(After$AveOccupancy,50))
# image2D(z = AveDens, border = "black")
# AveDens = table(cut(After$AveSpeed, 50), cut(After$AveHeadway,50))
# image2D(z = AveDens, border = "black")
# 
# grid.arrange(pl, im, nrow = 1, ncol = 2, top = "Ave Occupancy Vs Speed Within Hour of Major Crash")
# 
# all = halo_spatial
# all[all$AveSpeed == 0, ] = NA
# AveDens = table(cut(all$AveSpeed, 50), cut(all$AveOccupancy,50))
# image2D(z = AveDens, border = "black")
# AveDens = table(cut(all$AveSpeed, 50), cut(all$AveHeadway,50))
# image2D(z = AveDens, border = "black")
# 
# ggplot(data = halo_spatial, aes(AveSpeed, AveHeadway), col = After) + geom_point()
# ggplot(data = halo_spatial, aes(AveSpeed, AveOccupancy), col = After) + geom_point()
# 
# 
# 
# 
# 
# 
# Before[Before$AveSpeed == 0 & Before$AveOccupancy == 0 & Before$AveHeadway == 0, ] = NA
# AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveOccupancy,50))
# image2D(z = AveDens, border = "black")
# AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveHeadway,50))
# image2D(z = AveDens, border = "black")
# AveDens = table(cut(Before$AveSpeed, 50), cut(Before$TotalFlow,50))
# image2D(z = AveDens, border = "black")
# 
# # Ave Speed
# 
# A1 = qplot(all$AveSpeed, binwidth = 1, xlim = c(1,200))
# all2 = all[all$Before ==0 & all$Before ==0, ]
# A2 = qplot(all2$AveSpeed, binwidth = 1)
# 
# B1 = qplot(Before$AveSpeed, binwidth = 1, xlim = c(1,200))
# AF1 = qplot(After$AveSpeed, binwidth = 1, xlim = c(1,200))
# 
# 
# grid.arrange(A1, B1, AF1, nrow = 1, ncol = 3, top = "Ave Speed Within Hour of Major Crash")
# 
# plot_grid()
# 
# ggplot(df, aes(x=count,group=year,fill=as.factor(year))) + 
#   geom_histogram(position="identity", alpha=0.5, breaks=seq(0,1600, by=200),right=TRUE) +
#   scale_fill_discrete(name="Year") +
#   theme_bw(base_size=20) +
#   xlab("values")