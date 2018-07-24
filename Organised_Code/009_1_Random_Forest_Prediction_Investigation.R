# Load Data with After before

halo_spatial = readr::read_csv(file.path("D:/Documents/5872M-Dissertation/Data/Geometries/", "Halogen_2016_With_After_Before.csv"))
halo_spatial = as.data.frame(halo_spatial)
# Random Forest Model build

table(halo_spatial$After)

table(is.na(halo_spatial$After))

halo_spatial = halo_spatial[is.na(halo_spatial$Datetime) == FALSE, ]

# Identify any Troublesome NAs

sapply(halo_spatial, function(x) sum(is.na(x)))

# After Data, 3 lanes and After Column

variables = c("AveSpeed", "AveOccupancy", "AveHeadway", "AveTotalFlow")

outputs = c("Before", "After")

mydata = halo_spatial
mydata = as.data.frame(mydata)

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

idx = sample(nrow(mydata), 70000) 
mydata$After = as.factor(mydata$After)
mydata$Before = as.factor(mydata$Before)


train_data = mydata[idx, ]
table(train_data$After)

test_data = mydata[-idx, ]

start_time <- Sys.time()

formulas = rep(NA, length(variables))
for (a in 1:length(variables)){
formulas[a] = paste("After ~", variables[a])
}
for (i in 1:length(formulas)){
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

  current_Forest = randomForest(formula = as.formula(formulas[i]), data = train_data, ntree = 500)
  
  current_Forest = randomForest(formula = After ~ AveSpeed + AveOccupancy, data = train_data, ntree = 500)

  names(train_data)
  # for (k in 1:24) {
  #   next_Forest = randomForest(formula = as.formula(formulas), data = train_data, ntree = 20)
  #   current_Forest = combine(current_Forest, next_Forest)
  # }

  
  # Predict Probability using model based on Test Data
  pred = predict(current_Forest, newdata=train_data, type = "response")
  pred_test = predict(current_Forest, newdata=test_data, type = "response")
  pred_scale = predict(current_Forest, newdata=halo_spatial, type = "response")
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
  
  cm = table(train_data$After, pred)
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
  print("---------------------")
  cm = table(test_data$After, pred_test)
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
  print("---------------------")
  cm = table(halo_spatial$After, pred_scale)
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
  print("---------------------")

}

end_time <- Sys.time()
  
end_time - start_time


mydata = halo_spatial
geom = mydata$geometry
mydata$geometry = NULL
nrow(mydata)/ 2

# Plots of Relationships

ggplot(data = halo_spatial, aes(AveSpeed, AveOccupancy), colour = After) + geom_point()

After = halo_spatial[halo_spatial$After == 1, ]

Before = halo_spatial[halo_spatial$Before == 1, ]


pl = ggplot(data = After, aes(AveSpeed, AveOccupancy), colour = After) + geom_point()


After[After$AveSpeed == 0, ] = NA
After[After$AveOccupancy == 0, ] = NA
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



Before[Before$AveSpeed == 0, ] = NA
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveOccupancy,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveHeadway,50))
image2D(z = AveDens, border = "black")
AveDens = table(cut(Before$AveSpeed, 50), cut(Before$AveTotalFlow,50))
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
