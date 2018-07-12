# Random Forest Model build

table(halo_spatial$After)

# Identify any Troublesome NAs

sapply(halo, function(x) sum(is.na(x)))

# After Data, 3 lanes and After Column

mydata = halo_spatial[,c(2,9,15:26,37)]
geom = mydata$geometry
mydata$geometry = NULL
nrow(mydata)/ 2

colnames(mydata) = c("Location","Time","Speed1", "Flow1", "Occupancy1", "Headway1","Speed2", "Flow2", "Occupancy2", "Headway2","Speed3", "Flow3", "Occupancy3", "Headway3","After")
names(mydata) <- make.names(names(mydata))
mydata$After = as.factor(mydata$After)

sapply(mydata, function(x) sum(is.na(x)))

table(is.na(mydata$After))
mydata$After[is.na(mydata$After)] = 0
table(mydata$After)


#mydata = rbind(mydata, mydata) # To multiply

#formula = "After ~ Average Speed Lane 1 + Total Flow Lane 1 + Occupancy Lane 1 + Average Headway Lane 1 + Average Speed Lane 2 + Total Flow Lane 2 + Occupancy Lane 2 + Average Headway Lane 2 + Average Speed Lane 3 + Total Flow Lane 3 + Occupancy Lane 3 + Average Headway Lane 3"

#formula 


idx = sample(nrow(mydata), 2500000) # Greater than Half of sample

train_data = mydata[idx, ]

test_data = mydata[-idx, ]

table(train_data$After)

start_time <- Sys.time()

current_Forest = randomForest(After ~ Speed1 + Flow1 + Occupancy1 + Headway1 + Speed2 + Flow2 + Occupancy2 + Headway2, data=train_data, ntree = 20)

end_time <- Sys.time()

end_time - start_time

start_time <- Sys.time()

for (k in 1:24) {
  next_Forest = randomForest(After ~ Speed1 + Flow1 + Occupancy1 + Headway1 + Speed2 + Flow2 + Occupancy2 + Headway2, data=train_data, ntree = 20)
  current_Forest = combine(current_Forest, next_Forest)
}

end_time <- Sys.time()

end_time - start_time

start_time <- Sys.time()

prediction = predict(current_Forest, newdata = test_data, type="response")

end_time <- Sys.time()

end_time - start_time

tree = myForest$ntree

#prediction = (tree*prediction + 1)/(tree + 2)

testLL = sum(log(prediction[which(test_data$After==1)])) + sum(log(1-prediction[which(test_data$After==0)]))
print(testLL)

plot(test_data$Time, test_data$After)
points(test_data$Time, prediction, col='blue')

length(which(test_data$After == 1 & test_data$After == prediction))

length(which(test_data$After == 1))

head(prediction)
