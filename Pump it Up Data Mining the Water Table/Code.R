##
setwd("C:/Users/AJKoma/Desktop/Pump it Up Data Mining the Water Table")
X = read.csv("Training Set Values.csv")
Y = read.csv("Training Set Labels.csv")
test = read.csv("Test Set Values.csv")

####################################################################################
### Mice
subtest <- test[c('id', 'amount_tsh', 'population',  
               'gps_height', 'longitude', 'latitude',
               'wpt_name', 'basin', 
               'subvillage', 'region', 'region_code', 'district_code', 'lga', 'ward',
               'construction_year')]

subtest$population[subtest$population==0] <- NA
subtest$amount_tsh[subtest$amount_tsh==0] <- NA
subtest$gps_height[subtest$gps_height==0] <- NA
subtest$longitude[subtest$longitude==0] <- NA
subtest$latitude[subtest$latitude==-2e-08] <- NA
subtest$subvillage[subtest$subvillage==''] <- NA
subtest$construction_year[subtest$construction_year==0] <- NA

set.seed(1)
m <- subtest[c(#'id', 
               'amount_tsh', 'population', 
               'gps_height', 'longitude', 'latitude',
               'basin', 'region', 'region_code', 'district_code',
               'construction_year')]
testimp <- mice(m)

cplttest <- complete(testimp,5)

test[c('amount_tsh', 'population', 
       'gps_height', 'longitude', 'latitude',
       'basin', 'region', 'region_code', 'district_code', 
       'construction_year')]<- cplttest
#write.csv(test, 'cpltXt.csv')

year_recorded <- format(as.Date(test$date_recorded),format = "%Y")
years <- as.integer(year_recorded)-test$construction_year
testX <- cbind(test[c('amount_tsh', 'population', 
                    'gps_height', 'longitude', 'latitude',
                    'basin', 'region', 'region_code', 'district_code',  
                    'extraction_type_class', 'management_group', 'payment_type', 
                    'quality_group', 'quantity_group', 'source_class')],years)

result <- data.frame(predict(rf1,testX))
result <- cbind(test['id'],result)
colnames(result) <- c('id','status_group')
#write.csv(result, 'result1.csv', row.names = FALSE)

##
library('mice')
md.pattern(X)

subX <- X[c('id', 'amount_tsh', 'population',  
           'gps_height', 'longitude', 'latitude',
           'wpt_name', 'basin', 
           'subvillage', 'region', 'region_code', 'district_code', 'lga', 'ward',
           'construction_year')]

subX$population[subX$population==0] <- NA
subX$amount_tsh[subX$amount_tsh==0] <- NA
subX$gps_height[subX$gps_height==0] <- NA
subX$longitude[subX$longitude==0] <- NA
subX$latitude[subX$latitude==-2e-08] <- NA
subX$subvillage[subX$subvillage==''] <- NA
subX$construction_year[subX$construction_year==0] <- NA


set.seed(1)
M <- subX[c(#'id', 
           'amount_tsh', 'population', 
           'gps_height', 'longitude', 'latitude',
           'basin', 'region', 'region_code', 'district_code',
           'construction_year')]
imp <- mice(M)

#######################################################################################
cplt <- complete(imp,5)
#fit <- with(imp,randomForest(Y))
#pooled <- pool(fit)  
#summary(pooled)  

X[c('amount_tsh', 'population', 
    'gps_height', 'longitude', 'latitude',
    'basin', 'region', 'region_code', 'district_code', 
    'construction_year')]<- cplt
#write.csv(X, 'cpltX.csv')

year_recorded <- format(as.Date(X$date_recorded, format='%m/%d/%Y'),format = "%Y")
years <- as.integer(year_recorded)-X$construction_year
trainX <- cbind(X[c('amount_tsh', 'population', 
                    'gps_height', 'longitude', 'latitude',
                    'basin', 'region', 'region_code', 'district_code',  
                    'extraction_type_class', 'management_group', 'payment_type', 
                    'quality_group', 'quantity_group', 'source_class')],years)

##
set.seed(999)
#library('randomForest')
rf1 <- randomForest(trainX,Y$status_group,type='classification')
print(rf1)

##
set.seed(666)
for (i in 1:5){
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c('amount_tsh', 'population', 
                      'gps_height', 'longitude', 'latitude',
                      'basin', 'region', 'region_code', 'district_code',  
                      'extraction_type_class', 'management_group', 'payment_type', 
                      'quality_group', 'quantity_group', 'source_class')],years)
  rf <- randomForest(trainX,Y$status_group,type='classification')
  print(rf)
}

##
library('party')
data <- cbind(trainX,Y['status_group'])
cf <- cforest(status_group~.,data = data)


#####################################################################################
install.packages("Boruta")
library(Boruta)

X[c('amount_tsh', 'population', 
    'gps_height', 'longitude', 'latitude',
    'basin', 'region', 'region_code', 'district_code', 
    'construction_year')]<- cplt
years <- as.integer(year_recorded)-X$construction_year
train <- cbind(X[c('amount_tsh','gps_height','longitude','latitude','wpt_name','basin',
                   'region','region_code','lga','ward','population',
                   'extraction_type','extraction_type_group','extraction_type_class',
                   'management','management_group','payment','payment_type',
                   'water_quality','quality_group','quantity','quantity_group',
                   'source','source_type','source_class',
                   'waterpoint_type','waterpoint_type_group')],years,Y['status_group'])

set.seed(123)
boruta.train <- Boruta(status_group~., data = train, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)


###################################################################################
### Single
for (i in 1:5){
  set.seed(999)
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c('amount_tsh', 'population', 
                      'gps_height', 'longitude', 'latitude',
                      'basin', 'region', 'region_code', 'district_code',  
                      'extraction_type', 'management', 'payment_type', 
                      'water_quality', 'quantity_group', 'source', 'waterpoint_type')],years)
  rf <- randomForest(trainX,Y$status_group,type='classification')
  print(rf)
}

cplt <- complete(imp,5)
#fit <- with(imp,randomForest(Y))
#pooled <- pool(fit)  
#summary(pooled)  

X[c('amount_tsh', 'population', 
    'gps_height', 'longitude', 'latitude',
    'basin', 'region', 'region_code', 'district_code', 
    'construction_year')]<- cplt
#write.csv(X, 'cpltX.csv')

year_recorded <- format(as.Date(X$date_recorded, format='%m/%d/%Y'),format = "%Y")
years <- as.integer(year_recorded)-X$construction_year
trainX <- cbind(X[c('amount_tsh', 'population', 
                    'gps_height', 'longitude', 'latitude',
                    'basin', 'region', 'region_code', 'district_code',  
                    'extraction_type_class', 'management', 'payment_type', 
                    'water_quality', 'quantity_group', 'source', 'waterpoint_type')],years)

##
set.seed(999)
#library('randomForest')
rf1 <- randomForest(trainX,Y$status_group,type='classification')
print(rf1)

##
cplttest <- complete(testimp,5)

test[c('amount_tsh', 'population', 
    'gps_height', 'longitude', 'latitude',
    'basin', 'region', 'region_code', 'district_code', 
    'construction_year')]<- cplttest
#write.csv(X, 'cpltX.csv')

year_recorded <- format(as.Date(test$date_recorded),format = "%Y")
years <- as.integer(year_recorded)-test$construction_year
testX <- cbind(test[c('amount_tsh', 'population', 
                      'gps_height', 'longitude', 'latitude',
                      'basin', 'region', 'region_code', 'district_code',  
                      'extraction_type_class', 'management', 'payment_type', 
                      'water_quality', 'quantity_group', 'source', 'waterpoint_type')],years)

result <- data.frame(predict(rf1,testX))
result <- cbind(test['id'],result)
colnames(result) <- c('id','status_group')
#write.csv(result, 'result2.csv', row.names = FALSE)

######################################################################################
### Multi1
tyear_recorded <- format(as.Date(test$date_recorded),format = "%Y")

results <- data.frame(test['id'])

for (i in 1:5){
  set.seed(999)
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c('longitude','latitude','basin',
                      'region','region_code','district_code',
                      'extraction_type_group','extraction_type_class',
                      'management','payment','payment_type',
                      'water_quality','quality_group','quantity','quantity_group',
                      'source','source_type',
                      'waterpoint_type','waterpoint_type_group')],years)
  rf <- randomForest(trainX,Y$status_group,type='classification')
  print(rf)
  
  cplttest <- complete(testimp,i)
  
  test[c('amount_tsh', 'population', 
         'gps_height', 'longitude', 'latitude',
         'basin', 'region', 'region_code', 'district_code', 
         'construction_year')]<- cplttest

  years <- as.integer(tyear_recorded)-test$construction_year
  testX <- cbind(test[c('longitude','latitude','basin',
                        'region','region_code','district_code',
                        'extraction_type_group','extraction_type_class',
                        'management','payment','payment_type',
                        'water_quality','quality_group','quantity','quantity_group',
                        'source','source_type',
                        'waterpoint_type','waterpoint_type_group')],years)
  result <- data.frame(predict(rf,testX))
  
  results[,i+1] <- result
  
}

results

for (k in 1:nrow(results)){
  row <- results[k,]
  u <- unique(row)
  results[k,7] <- u[which.max(tabulate(match(row, u)))]
}

result3 <- data.frame(test$id,results[,7])
colnames(result3) <- c('id','status_group')
#write.csv(result3, 'result3.csv', row.names = FALSE)

######################################################################################
### Multi2
tyear_recorded <- format(as.Date(test$date_recorded),format = "%Y")

results <- data.frame(test['id'])

for (i in 1:5){
  set.seed(999)
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c('amount_tsh','gps_height','longitude','latitude','basin',
                      'region','region_code','district_code','population',
                      'extraction_type_group','extraction_type_class',
                      'management','management_group','payment','payment_type',
                      'water_quality','quality_group','quantity','quantity_group',
                      'source','source_type','source_class',
                      'waterpoint_type','waterpoint_type_group')],years)
  rf <- randomForest(trainX,Y$status_group,type='classification')
  print(rf)
  
  cplttest <- complete(testimp,i)
  
  test[c('amount_tsh', 'population', 
         'gps_height', 'longitude', 'latitude',
         'basin', 'region', 'region_code', 'district_code', 
         'construction_year')]<- cplttest
  
  years <- as.integer(tyear_recorded)-test$construction_year
  testX <- cbind(test[c('amount_tsh','gps_height','longitude','latitude','basin',
                        'region','region_code','district_code','population',
                        'extraction_type_group','extraction_type_class',
                        'management','management_group','payment','payment_type',
                        'water_quality','quality_group','quantity','quantity_group',
                        'source','source_type','source_class',
                        'waterpoint_type','waterpoint_type_group')],years)
  result <- data.frame(predict(rf,testX))
  
  results[,i+1] <- result
  
}

results1 <- results

for (k in 1:nrow(results1)){
  row <- results1[k,]
  u <- unique(row)
  results1[k,7] <- u[which.max(tabulate(match(row, u)))]
}

result5 <- data.frame(test$id,results1[,7])
colnames(result5) <- c('id','status_group')
#write.csv(result5, 'result5.csv', row.names = FALSE)

##################################################################################
### C5.0
library(C50)

tyear_recorded <- format(as.Date(test$date_recorded),format = "%Y")

C5.0.results <- data.frame(test['id'])

for (i in 1:5){
  set.seed(999)
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c('longitude','latitude','basin','lga',
                      'region','region_code',
                      'extraction_type_group','extraction_type_class',
                      'management','payment',
                      'water_quality','quality_group','quantity','quantity_group',
                      'source','source_type',
                      'waterpoint_type','waterpoint_type_group')],years)
  C50 <- C5.0(trainX,Y$status_group)
  print(summary(C50))
  
  cplttest <- complete(testimp,i)
  
  test[c('amount_tsh', 'population', 
         'gps_height', 'longitude', 'latitude',
         'basin', 'region', 'region_code', 'district_code', 
         'construction_year')]<- cplttest
  
  years <- as.integer(tyear_recorded)-test$construction_year
  testX <- cbind(test[c('longitude','latitude','basin','lga',
                        'region','region_code',
                        'extraction_type_group','extraction_type_class',
                        'management','payment',
                        'water_quality','quality_group','quantity','quantity_group',
                        'source','source_type',
                        'waterpoint_type','waterpoint_type_group')],years)
  result <- data.frame(predict(C50,testX))
  
  C5.0.results[,i+1] <- result
  
}

C5.0.results

for (k in 1:nrow(C5.0.results)){
  row <- C5.0.results[k,]
  u <- unique(row)
  C5.0.results[k,7] <- u[which.max(tabulate(match(row, u)))]
}

result6 <- data.frame(test$id,C5.0.results[,7])
colnames(result6) <- c('id','status_group')
write.csv(result6, 'result6.csv', row.names = FALSE)

#################################################################################
### XGboost
library(xgboost)

cplt <- complete(imp,1)
X[c('amount_tsh', 'population', 
    'gps_height', 'longitude', 'latitude',
    'basin', 'region', 'region_code', 'district_code', 
    'construction_year')]<- cplt
years <- as.integer(year_recorded)-X$construction_year
trainX <- cbind(X[c('longitude','latitude','basin','lga',
                    'region','region_code','district_code',
                    'extraction_type_group','extraction_type_class',
                    'management','payment',
                    'water_quality','quality_group','quantity','quantity_group',
                    'source','source_type',
                    'waterpoint_type','waterpoint_type_group')],years)


Xmatrix <- Matrix::sparse.model.matrix(~.-1,data=trainX)
data <- xgb.DMatrix(Xmatrix,label=Y$status_group)

set.seed(999)
xgb <-  xgb.cv(data = data, objective = "multi:softmax", booster = "gbtree",
                 nrounds = 500, nfold = 5, early.stop.round = 10, num_class = 4, maximize = FALSE,
                 evaluation = "merror", eta = .2, max_depth = 12, colsample_bytree = .4)
min.error.idx = which.min(xgb$evaluation_log$test_merror_mean)

model <- xgboost(data = data, objective = "multi:softmax", booster = "gbtree",
                 eval_metric = "merror", nrounds = min.error.idx, 
                 num_class = 4,eta = .2, max_depth = 14, colsample_bytree = .4)

#############################################################################

tyear_recorded <- format(as.Date(test$date_recorded),format = "%Y")

results <- data.frame(test['id'])

for (i in 1:5){
  set.seed(999)
  cplt <- complete(imp,i)
  X[c('amount_tsh', 'population', 
      'gps_height', 'longitude', 'latitude',
      'basin', 'region', 'region_code', 'district_code', 
      'construction_year')]<- cplt
  years <- as.integer(year_recorded)-X$construction_year
  trainX <- cbind(X[c(#'amount_tsh','gps_height',
                      'longitude','latitude','basin','lga',
                      'region','region_code','district_code',#'population',
                      'extraction_type_group','extraction_type_class',
                      'management','management_group','payment','payment_type',
                      'water_quality','quality_group','quantity','quantity_group',
                      'source','source_type','source_class',
                      'waterpoint_type','waterpoint_type_group')],years)
  
  data <- as.matrix(as.data.frame(lapply(trainX, as.numeric)))
  label<-as.numeric(Y$status_group)
  data <- xgb.DMatrix(data,label=label)
  
  xgb <-  xgb.cv(data = data, objective = "multi:softmax", booster = "gbtree",
                 nrounds = 500, nfold = 5, early.stop.round = 10, num_class = 4, maximize = FALSE,
                 evaluation = "merror", eta = .2, max_depth = 12, colsample_bytree = .4)
  min.error.idx = which.min(xgb$evaluation_log$test_merror_mean)
  
  model <- xgboost(data = data, objective = "multi:softmax", booster = "gbtree",
                   eval_metric = "merror", nrounds = min.error.idx, 
                   num_class = 4,eta = .2, max_depth = 14, colsample_bytree = .4)
  
  cplttest <- complete(testimp,i)
  
  test[c('amount_tsh', 'population', 
         'gps_height', 'longitude', 'latitude',
         'basin', 'region', 'region_code', 'district_code', 
         'construction_year')]<- cplttest
  
  years <- as.integer(tyear_recorded)-test$construction_year
  testX <- cbind(test[c('amount_tsh','gps_height',
                        'longitude','latitude','basin','lga',
                        'region','region_code','district_code','population',
                        'extraction_type_group','extraction_type_class',
                        'management','management_group','payment','payment_type',
                        'water_quality','quality_group','quantity','quantity_group',
                        'source','source_type','source_class',
                        'waterpoint_type','waterpoint_type_group')],years)
  
  testM <- as.matrix(as.data.frame(lapply(testX, as.numeric)))
  
  result <- predict(model,testM)
  result[result==1] <- 'functional'
  result[result==2] <- 'functional needs repair'
  result[result==3] <- 'non functional'
  
  results[,i+1] <- result
  
}

results

for (k in 1:nrow(results)){
  row <- results[k,]
  u <- unique(row)
  results[k,7] <- u[which.max(tabulate(match(row, u)))]
}

result9 <- data.frame(test$id,results[,7])#7
colnames(result9) <- c('id','status_group')#7
#write.csv(result9, 'result9.csv', row.names = FALSE)

##############################################################3
combination <- read.csv('Results.csv')
colnames(combination) <- c('R2','R3','R4','R5','R8','R9')
combinedresult <- data.frame(test$id,rep(NA,nrow(combination)))
for (k in 1:nrow(combination)){
  row <- combination[k,]
  u <- unique(row)
  combinedresult[k,2] <- u[which.max(tabulate(match(row, u)))]
}

result10 <- data.frame(test$id,combinedresult[,2])
colnames(result10) <- c('id','status_group')
result10$status_group[result10$status_group==1] <- 'functional'
result10$status_group[result10$status_group==2] <- 'functional needs repair'
result10$status_group[result10$status_group==3] <- 'non functional'
#write.csv(result10, 'result10.csv', row.names = FALSE)

