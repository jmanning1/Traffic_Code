# Load Data with After before

halo_spatial = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Halogen_2016_With_After_Before.csv"))
halo_spatial = as.data.frame(halo_spatial)
colnames(halo_spatial) = c("Control_Office","Geographic_Address","Year",
                     "Month","Day","Day_of_Week",
                     "Type_of_Day","Days_After_Nearest_Bank_Holiday","Time_GMT",
                     "Number_of_Lanes","Flow_Category_1","Flow_Category_2",
                     "Flow_Category_3","Flow_Category_4","Average_Speed_Lane_1",
                     "Total_Flow_Lane_1","Occupancy_Lane_1","Average_Headway_Lane_1",
                     "Average_Speed_Lane_2","Total_Flow_Lane_2","Occupancy_Lane_2",
                     "Average_Headway_Lane_2","Average_Speed_Lane_3","Total_Flow_Lane_3",
                     "Occupancy_Lane_3","Average_Headway_Lane_3","Average_Speed_Lane_4",
                     "Total_Flow_Lane_4","Occupancy_Lane_4","Average_Headway_Lane_4",
                     "Average_Speed_Lane_5","Total_Flow_Lane_5","Occupancy_Lane_5",
                     "Average_Headway_Lane_5","Datetime",
                     "After","Before", "X", "Y")

# Random Forest Model build

table(halo_spatial$After)

table(is.na(halo_spatial$After))

halo_spatial = halo_spatial[is.na(halo_spatial$Datetime) == FALSE, ]

# Run Average Column Code in 008

# Remove all rows with 0 for all averages.
nrow(halo_spatial[halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0, ])

halo_spatial = halo_spatial[!(halo_spatial$AveSpeed == 0 & halo_spatial$AveOccupancy == 0 & halo_spatial$AveHeadway == 0 & halo_spatial$TotalFlow == 0), ]


# Identify any Troublesome NAs

sapply(halo_spatial, function(x) sum(is.na(x)))

# After Data, 3 lanes and After Column

variables = c("AveSpeed", "AveOccupancy", "AveHeadway", "TotalFlow")

outputs = c("Before", "After")

mydata = halo_spatial
mydata = as.data.frame(mydata)
mydata$After = as.factor(mydata$After)
mydata$Before = as.factor(mydata$Before)

validation  = mydata[month(mydata$Datetime) == 12, ]
mydata = mydata[month(mydata$Datetime) != 12, ]
other = mydata[mydata$Before != 1 & mydata$After != 1, ]
mydata = mydata[mydata$Before == 1 | mydata$After == 1, ]

coor = st_coordinates(mydata$geometry)

mydata = cbind(mydata, coor)

mydata$geometry = NULL
rm(coor)


colnames(mydata) = c("Control_Office","Geographic_Address","Year",
                     "Month","Day","Day_of_Week",
                     "Type_of_Day","Days_After_Nearest_Bank_Holiday","Time_GMT",
                     "Number_of_Lanes","Flow_Category_1","Flow_Category_2",
                     "Flow_Category_3","Flow_Category_4","Average_Speed_Lane_1",
                     "Total_Flow_Lane_1","Occupancy_Lane_1","Average_Headway_Lane_1",
                     "Average_Speed_Lane_2","Total_Flow_Lane_2","Occupancy_Lane_2",
                     "Average_Headway_Lane_2","Average_Speed_Lane_3","Total_Flow_Lane_3",
                     "Occupancy_Lane_3","Average_Headway_Lane_3","Average_Speed_Lane_4",
                     "Total_Flow_Lane_4","Occupancy_Lane_4","Average_Headway_Lane_4",
                     "Average_Speed_Lane_5","Total_Flow_Lane_5","Occupancy_Lane_5",
                     "Average_Headway_Lane_5","Datetime",
                     "After","Before", "X", "Y", "AveSpeed")

idx = sample(nrow(mydata), 2750000) # Greater than Half of sample

winner_train_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_train_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))

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
formulas_before = rep(NA, length(variables))
for (a in 1:length(variables)){
formulas_after[a] = paste("After ~", variables[a])
formulas_before[a] = paste("Before ~", variables[a])
}
best_train_a = c(0,0,"")
best_test_a = c(0,0,"")
best_validation_a = c(0,0,"")

best_train_b = c(0,0,"")
best_test_b = c(0,0,"")
best_validation_b = c(0,0,"")


for (i in 1:length(formulas_after)){
  # Print Variable to identify loop location
  print(variables[i])
  # Random Forest Model
  #table(is.na(halo_spatial$After))
  # train_data_form = data.frame(Y = train_data$After, X = train_data[,variables[i]])
  # colnames(train_data_form) = c("Y", "X")
  # Create Random Forest Model on Training Data
  # current_Forest = randomForest(formula = Y ~ X, data = train_data_form, ntree = 10)
  # for (k in 1:49) {
  #   next_Forest = randomForest(formula = Y ~ X, data = train_data_form, ntree = 10)
  #   current_Forest = combine(current_Forest, next_Forest)
  # }
  # pred = predict(current_Forest, newdata=train_data_form, type = "response")
  
  # current_Forest = randomForest(formula = After ~ Average_Speed_Lane_1 + Average_Speed_Lane_2 + Average_Speed_Lane_3, data = train_data, ntree = 10)
  # for (k in 1:49) {
  #   next_Forest = randomForest(formula = After ~ Average_Speed_Lane_1 + Average_Speed_Lane_2 + Average_Speed_Lane_3, data = train_data, ntree = 10)
  #   current_Forest = combine(current_Forest, next_Forest)
  # }

  current_Forest_after = randomForest(formula = as.formula(formulas_after[i]), data = train_data, ntree = 500)
  current_Forest_before = randomForest(formula = as.formula(formulas_before[i]), data = train_data, ntree = 500)
  
  
  #current_Forest = randomForest(formula = After ~ AveSpeed + AveOccupancy, data = train_data, ntree = 500)

  #names(train_data)
  # for (k in 1:24) {
  #   next_Forest = randomForest(formula = as.formula(formulas), data = train_data, ntree = 20)
  #   current_Forest = combine(current_Forest, next_Forest)
  # }

  
  # Predict Probability using model based on Test Data
  pred_a = predict(current_Forest_after, newdata=train_data, type = "response")
  pred_test_a = predict(current_Forest_after, newdata=test_data, type = "response")
  pred_scale_a = predict(current_Forest_after, newdata=validation, type = "response")
  
  pred_b = predict(current_Forest_before, newdata=train_data, type = "response")
  pred_test_b = predict(current_Forest_before, newdata=test_data, type = "response")
  pred_scale_b = predict(current_Forest_before, newdata=validation, type = "response")
  #pred = predict(current_Forest, newdata=test_data, type = "response")
  #end_time <- Sys.time()
  
  # end_time - start_time
  # # Calculate number of trees
  # numtrees = current_Forest$ntree
  # # Add one correct and incorrect value to stop prediction being 0 or 1 exactly
  # pred = (numtrees*pred + 1)/(numtrees +2)
  # # Use Edible to compare
  # pred = pred[ ,1]
  # # Calculate log likelihood
  # testLL = sum(log(pred[which(mydata$After == "1")]), na.rm=T) + sum(1 - log(pred[which(mydata$After == "0")]), na.rm=T)
  # # Print Results - Log Likelihood closest to 0 is the best factor
  # print(paste("Predictive Log-Likelihood = ", testLL))
  # results = round(pred, 0)
  # print(paste("Correct Number of Predictions: ", length(which(test_data$After == pred))))
  # print(paste("Correct Number of Predictions where 1: ", length(which(test_data$After == pred & test_data$After == 1))))
  # cm = table(test_data$After, pred)
  # cm = table(train_data_form$Y, pred)
  # print("Confusion Matrix Train")
  # print(cm)
  # print("---------------------")
  
  # After
  
  cm = table(train_data$After, pred_a)
  print("Confusion Matrix Train")
  print(cm)
  Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
  Specificity = cm[1,1]/sum(cm[1,])
  Sensitivity = cm[2,2]/sum(cm[2,])
  Precision = cm[2,2]/sum(cm[,2])
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_a, train_data$After)$byClass[11])
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
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
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
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
  result = confusionMatrix(pred_scale_a, validation$After)$byClass[11]
  if (result > best_validation_a[2]) {best_validation_a = c(Sensitivity,result, formulas_after[i])}
  
  print("---------------------")
  
  # Before 
  
  cm = table(train_data$After, pred_b)
  print("Confusion Matrix Train")
  print(cm)
  Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
  Specificity = cm[1,1]/sum(cm[1,])
  Sensitivity = cm[2,2]/sum(cm[2,])
  Precision = cm[2,2]/sum(cm[,2])
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_b, train_data$After)$byClass[11])
  result = confusionMatrix(pred_b, train_data$After)$byClass[11]
  if (result > best_train_b[2]) {best_train_b = c(Sensitivity,result, formulas_before[i])} 
  print("---------------------")
  
  cm = table(test_data$After, pred_test_b)
  print("Confusion Matrix Test")
  print(cm)
  Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
  Specificity = cm[1,1]/sum(cm[1,])
  Sensitivity = cm[2,2]/sum(cm[2,])
  Precision = cm[2,2]/sum(cm[,2])
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_test_b, test_data$After)$byClass[11])
  result = confusionMatrix(pred_test_b, test_data$After)$byClass[11]
  if (result > best_test_b[2]) {best_test_b = c(Sensitivity,result, formulas_before[i])}
  
  print("---------------------")
  
  cm = table(validation$After, pred_scale_b)
  print("Confusion Matrix Scale")
  print(cm)
  Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
  Specificity = cm[1,1]/sum(cm[1,])
  Sensitivity = cm[2,2]/sum(cm[2,])
  Precision = cm[2,2]/sum(cm[,2])
  
  print(paste("Accuracy:", Accuracy)) # How right is it
  print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
  print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
  print(paste("Precision:", Precision)) # when predicts 1 how often is it right
  print(confusionMatrix(pred_scale_b, validation$After)$byClass[11])
  result = confusionMatrix(pred_scale_b, validation$After)$byClass[11]
  if (result > best_validation_b[2]) {best_validation_b = c(Sensitivity,result, formulas_before[i])}
  
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


winner_train_b[iteration,1] = best_train_b[1]
winner_train_b[iteration,2] = best_train_b[2]
winner_train_b[iteration,3] = best_train_b[3]

winner_test_b[iteration,1] = best_test_b[1]
winner_test_b[iteration,2] = best_test_b[2]
winner_test_b[iteration,3] = best_test_b[3]

winner_validation_b[iteration,1] = best_validation_b[1]
winner_validation_b[iteration,2] = best_validation_b[2]
winner_validation_b[iteration,3] = best_validation_b[3]

}

end_time <- Sys.time()
  
end_time - start_time

write.csv(winner_train_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_after_1VAR.csv",row.names=FALSE)
write.csv(winner_test_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_after_1VAR.csv",row.names=FALSE)
write.csv(winner_validation_a, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_after_1VAR.csv",row.names=FALSE)
write.csv(winner_train_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_before_1VAR.csv",row.names=FALSE)
write.csv(winner_test_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_before_1VAR.csv",row.names=FALSE)
write.csv(winner_validation_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_before_1VAR.csv",row.names=FALSE)

table(winner_train_a[3])

table(winner_test_a[3])

table(winner_validation_a[3])

table(winner_train_b[3])

table(winner_test_b[3])

table(winner_validation_b[3])

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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_a, train_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
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


# After 3rd Variable

winner_train_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_a = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_train_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_test_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))
winner_validation_b = as.data.frame(matrix(ncol = 3, nrow = 20, dimnames = list(1:20,c("Sensitivity", "Balanced_Accuracy", "Winning_Formula"))))


formulas_after = c("After ~ AveSpeed + TotalFlow + AveOccupancy", "After ~ AveSpeed + TotalFlow + AveHeadway", "After ~ AveSpeed + AveOccupancy + AveHeadway")

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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_a, train_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
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

# After 4th Variable

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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_a, train_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_test_a, test_data$After)$byClass[11])
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
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_scale_a, validation$After)$byClass[11])
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


  # Before 2nd Variable ###############################################################################
  
  formulas_before = rep(NA, length(variables) +  length(variables))
  for (a in 1:length(variables)){
    formulas_before[a] = paste("Before ~ AveSpeed + ", variables[a])
    variables = c("AveHeadway", "TotalFlow")
    formulas_before[a+length(variables)] = paste("Before ~ AveOccupancy + ", variables[a])
  }
  formulas_before = formulas_before[-1]

  best_train_b = c(0,0,"")
  best_test_b = c(0,0,"")
  best_validation_b = c(0,0,"")
  
  
  for (i in 1:length(formulas_before)){
    # Print Variable to identify loop location
    print(formulas_before[i])
    
    current_Forest_before = randomForest(formula = as.formula(formulas_before[i]), data = train_data, ntree = 500)
    
    # Predict Probability using model based on Test Data
    
    pred_b = predict(current_Forest_before, newdata=train_data, type = "response")
    pred_test_b = predict(current_Forest_before, newdata=test_data, type = "response")
    pred_scale_b = predict(current_Forest_before, newdata=validation, type = "response")
    
    cm = table(train_data$After, pred_b)
    print("Confusion Matrix Train")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_b, train_data$After)$byClass[11])
    result = confusionMatrix(pred_b, train_data$After)$byClass[11]
    if (result > best_train_b[2]) {best_train_b = c(Sensitivity,result, formulas_before[i])} 
    print("---------------------")
    
    cm = table(test_data$After, pred_test_b)
    print("Confusion Matrix Test")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_test_b, test_data$After)$byClass[11])
    result = confusionMatrix(pred_test_b, test_data$After)$byClass[11]
    if (result > best_test_b[2]) {best_test_b = c(Sensitivity,result, formulas_before[i])}
    
    print("---------------------")
    
    cm = table(validation$After, pred_scale_b)
    print("Confusion Matrix Scale")
    print(cm)
    Accuracy = (cm[1,1]+ cm[2,2])/sum(cm)
    Specificity = cm[1,1]/sum(cm[1,])
    Sensitivity = cm[2,2]/sum(cm[2,])
    Precision = cm[2,2]/sum(cm[,2])
    
    print(paste("Accuracy:", Accuracy)) # How right is it
    print(paste("Specificity:", Specificity)) # When 0 how often does it predict 0
    print(paste("Sensitivity:", Sensitivity)) # when 1 how often does it predict 1
    print(paste("Precision:", Precision)) # when predicts 1 how often is it right
    print(confusionMatrix(pred_scale_b, validation$After)$byClass[11])
    result = confusionMatrix(pred_scale_b, validation$After)$byClass[11]
    if (result > best_validation_b[2]) {best_validation_b = c(Sensitivity,result, formulas_before[i])}
    
    print("---------------------")
    
  }
  
  
  winner_train_b[iteration,1] = best_train_b[1]
  winner_train_b[iteration,2] = best_train_b[2]
  winner_train_b[iteration,3] = best_train_b[3]
  
  winner_test_b[iteration,1] = best_test_b[1]
  winner_test_b[iteration,2] = best_test_b[2]
  winner_test_b[iteration,3] = best_test_b[3]
  
  winner_validation_b[iteration,1] = best_validation_b[1]
  winner_validation_b[iteration,2] = best_validation_b[2]
  winner_validation_b[iteration,3] = best_validation_b[3]
  
}

end_time <- Sys.time()

end_time - start_time

write.csv(winner_train_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/train_before_2VAR.csv",row.names=FALSE)
write.csv(winner_test_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/test_before_2VAR.csv",row.names=FALSE)
write.csv(winner_validation_b, file = "D:/Documents/5872M-Dissertation/Data/RandomForest/scale_before_2VAR.csv",row.names=FALSE)



###########################################################################################

mydata = halo_spatial
geom = mydata$geometry
mydata$geometry = NULL
nrow(mydata)/ 2

# Plots of Relationships

C = ggplot(data = halo_spatial, aes(AveSpeed, AveOccupancy), colour = After) + geom_point()

After = halo_spatial[halo_spatial$After == 1, ]

Before = halo_spatial[halo_spatial$Before == 1, ]


A = ggplot(data = After, aes(AveSpeed, AveOccupancy)) + geom_point()
B = ggplot(data = Before, aes(AveSpeed, AveOccupancy)) + geom_point()


grid.arrange(A, B, C, nrow = 1, ncol = 3, top = "Ave Occupancy Vs Speed Within Hour of Major Crash")


After[After$AveSpeed == 0 & After$AveOccupancy == 0 & After$AveHeadway == 0, ] = NA

AveDens = table(cut(After$AveSpeed, 50), cut(After$AveOccupancy,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(After$AveSpeed, 50), cut(After$AveHeadway,50))
image2D(z = AveDens, border = "black")

grid.arrange(pl, im, nrow = 1, ncol = 2, top = "Ave Occupancy Vs Speed Within Hour of Major Crash")

all = halo_spatial
all[all$AveSpeed == 0, ] = NA
AveDens = table(cut(all$AveSpeed, 50), cut(all$AveOccupancy,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(all$AveSpeed, 50), cut(all$AveHeadway,50))
image2D(z = AveDens, border = "black")

ggplot(data = halo_spatial, aes(AveSpeed, AveHeadway), col = After) + geom_point()
ggplot(data = halo_spatial, aes(AveSpeed, AveOccupancy), col = After) + geom_point()






Before[Before$AveSpeed == 0 & Before$AveOccupancy == 0 & Before$AveHeadway == 0, ] = NA
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveOccupancy,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveHeadway,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$TotalFlow,50))
image2D(z = AveDens, border = "black")

# Ave Speed

A1 = qplot(all$AveSpeed, binwidth = 1, xlim = c(1,200))
all2 = all[all$Before ==0 & all$Before ==0, ]
A2 = qplot(all2$AveSpeed, binwidth = 1)

B1 = qplot(Before$AveSpeed, binwidth = 1, xlim = c(1,200))
AF1 = qplot(After$AveSpeed, binwidth = 1, xlim = c(1,200))


grid.arrange(A1, B1, AF1, nrow = 1, ncol = 3, top = "Ave Speed Within Hour of Major Crash")

plot_grid()

ggplot(df, aes(x=count,group=year,fill=as.factor(year))) + 
  geom_histogram(position="identity", alpha=0.5, breaks=seq(0,1600, by=200),right=TRUE) +
  scale_fill_discrete(name="Year") +
  theme_bw(base_size=20) +
  xlab("values")


# 
# colnames(mydata) = c("Location","Time","Speed1", "Flow1", "Occupancy1", "Headway1","Speed2", "Flow2", "Occupancy2", "Headway2","Speed3", "Flow3", "Occupancy3", "Headway3","After")
# names(mydata) <- make.names(names(mydata))
# mydata$After = as.factor(mydata$After)
# 
# sapply(mydata, function(x) sum(is.na(x)))
# 
# table(is.na(mydata$After))
# mydata$After[is.na(mydata$After)] = 0
# table(mydata$After)
# 
# 
# #mydata = rbind(mydata, mydata) # To multiply
# 
# #formula = "After ~ Average Speed Lane 1 + Total Flow Lane 1 + Occupancy Lane 1 + Average Headway Lane 1 + Average Speed Lane 2 + Total Flow Lane 2 + Occupancy Lane 2 + Average Headway Lane 2 + Average Speed Lane 3 + Total Flow Lane 3 + Occupancy Lane 3 + Average Headway Lane 3"
# 
# #formula 
# 
# 
# idx = sample(nrow(mydata), 2500000) # Greater than Half of sample
# 
# train_data = mydata[idx, ]
# 
# test_data = mydata[-idx, ]
# 
# table(train_data$After)
# 
# start_time <- Sys.time()
# 
# current_Forest = randomForest(After ~ Speed1 + Flow1 + Occupancy1 + Headway1 + Speed2 + Flow2 + Occupancy2 + Headway2, data=train_data, ntree = 20)
# 
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# start_time <- Sys.time()
# 
# for (k in 1:24) {
#   next_Forest = randomForest(After ~ Speed1 + Flow1 + Occupancy1 + Headway1 + Speed2 + Flow2 + Occupancy2 + Headway2, data=train_data, ntree = 20)
#   current_Forest = combine(current_Forest, next_Forest)
# }
# 
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# start_time <- Sys.time()
# 
# prediction = predict(current_Forest, newdata = test_data, type="response")
# 
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# tree = current_Forest$ntree
# 
# prediction = (tree*prediction + 1)/(tree + 2)
# 
# testLL = sum(log(prediction[which(test_data$After==1)])) + sum(log(1-prediction[which(test_data$After==0)]))
# print(testLL)
# 
# aic = 2*k - 2*testLL
# 
# plot(test_data$Time, test_data$After)
# points(test_data$Time, prediction, col='blue')
# 
# length(which(test_data$After == 1 & test_data$After == prediction))
# 
# length(which(test_data$After == 1))
# 
# head(prediction)

Occupancy_Request = halo_spatial[halo_spatial$AveOccupancy > 100, 1:34]
nrow(Occupancy_Request)

write.csv(Occupancy_Request, file = "D:/Documents/5872M-Dissertation/Data/Geometries/Occupancy_Over_100.csv",row.names=FALSE)
