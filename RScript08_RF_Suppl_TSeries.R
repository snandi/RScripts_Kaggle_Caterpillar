rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script imputes cost data for qty = 1 and uses RF to predict
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/RScripts_Caterpillar/'
Filename.Header <- paste(RScriptPath, 'HeaderFile_Caterpillar.R', sep = '')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Caterpillar.R', sep = ''))
RPlotPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Plots/'
RDataPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Data/'
########################################################################
Today <- Sys.Date()

#fn_loadAllData(RDataPath=RDataPath)
fn_loadData(RDataPath=RDataPath, File='train_set.csv')
fn_loadData(RDataPath=RDataPath, File='test_set.csv')

options(scipen = 10)

########################################################################
## Build train and test db
########################################################################
test <- test_set
train <- train_set

train$id <- - (1:nrow(train))
test$cost <- 0

length(unique(train$tube_assembly_id))
length(unique(test$tube_assembly_id))

########################################################################
## Load MinQty data with imputed log_ai
########################################################################
Filename <- paste(RDataPath, 'Data_MinQty.RData', sep = '')
load(Filename) ## The last column is the imputed data, for qty 1
str(Data_MinQty)

#Data_MinQty_test <- fn_prepData_MinQty(trainORtest = 'test_set')
Filename <- paste(RDataPath, 'Data_MinQty_test.RData', sep = '')
#save(Data_MinQty_test, file = Filename)
load(Filename) 

fn_prepData_forRF <- function(Data){
  ########################################################################
  ## Add Year and Quarter factors
  ########################################################################
  Data$quote_date <- as.Date(Data$quote_date)
  Data$Year <- format(as.yearqtr(Data$quote_date), '%Y')
  Data$Year2010 <- as.factor(Data$Year == '2010')
  Data$Year2012 <- as.factor(Data$Year == '2012')
  Data$Year2013 <- as.factor(Data$Year == '2013')
  Data$Year2014 <- as.factor(Data$Year == '2014')
  
  Data$Qtr <- format(as.yearqtr(Data$quote_date), '%q')
  Data$Qtr1 <- as.factor(Data$Qtr == '1')
  
  ########################################################################
  ## Add Supplier factors
  ########################################################################
  BulkSuppliers <- c("S-0013", "S-0014", "S-0026", "S-0030", "S-0041", "S-0054", "S-0058", "S-0062", "S-0064", "S-0066", 
                     "S-0072", "S-0081", "S-0104")
  Data$supplier <- as.vector(Data$supplier)
  for(Supp in BulkSuppliers){
    Data$NewCol <- as.factor(Data$supplier == Supp)
    names(Data)[names(Data) == 'NewCol'] <- paste0('Supplier_', Supp)
  }
  # str(Data)
  
  Colnames.Keep <- colnames(Data) %w/o% c('tube_assembly_id', 'supplier', 'quote_date', 'cost', 'train_id', 'log_ai', 
                                                 'Year', 'Qtr', 'quantity')
  Data_forRF <- Data[, Colnames.Keep]
  colnames(Data_forRF) <- gsub(pattern='-', replacement='', x=colnames(Data_forRF))
  
  for(Col in colnames(Data_forRF)){
    if(is.numeric(Data_forRF[,Col])){
      Data_forRF[,Col] <- na.is.zero(Data_forRF[,Col])
    }
  }

  Data_forRF$material_id <- as.vector(Data_forRF$material_id)
  Data_forRF[is.na(Data_forRF[,'material_id']),'material_id'] <- 'None'
  Data_forRF$material_id[Data_forRF$material_id == 'NA'] <- 'None'
  Data_forRF$material_id <- as.factor(Data_forRF$material_id)

  Data_forRF$end_x <- NULL
  return(Data_forRF) 
}

Data_forRF_Train <- fn_prepData_forRF(Data=Data_MinQty)
Data_forRF_Train$id <- -(1:nrow(Data_forRF_Train))
Data_forRF_Test <- fn_prepData_forRF(Data=Data_MinQty_test)
Data_forRF_Test$log_ai_qty1 <- 0

Data_forRF_TestTrain<- rbind(Data_forRF_Train, Data_forRF_Test)
for(col in colnames(Data_forRF_TestTrain)){
  if(is.factor(Data_forRF_TestTrain[,col])){
    print(col)
    Data_forRF_TestTrain[,col] <- as.factor(as.vector(Data_forRF_TestTrain[,col]))
  }
}

Data_forRF_Train <- subset(Data_forRF_TestTrain, id < 0)
rownames(Data_forRF_Train) <- -Data_forRF_Train$id

Data_forRF_Test <- subset(Data_forRF_TestTrain, id > 0)
rownames(Data_forRF_Test) <- Data_forRF_Test$id
########################################################################
## Linear Model
########################################################################
column_id <- which(colnames(Data_forRF_Train) == 'id')
Model1 <- lm(log_ai_qty1 ~ ., data = Data_forRF_Train[,-column_id])
summary(Model1)
# plot(Model1)

column_id <- which(colnames(Data_forRF_Test) %in% c('id', 'log_ai_qty1'))
Prediction <- as.data.frame(cbind(id = Data_forRF_Test$id,
                                      Model1=predict(Model1, Data_forRF_Test[,-column_id])))
rownames(Prediction) <- NULL
summary(Prediction)

########################################################################
## Random Forest
########################################################################
column_id <- which(colnames(Data_forRF_Train) %in% c('id', 'log_ai_qty1'))

RF1 <- randomForest(formula = Data_forRF_Train$log_ai_qty1 ~ ., data = Data_forRF_Train[,-column_id], 
                    ntree = 100, do.trace = 2, importance = TRUE)
round(importance(RF1))

column_id <- which(colnames(Data_forRF_Test) %in% c('id', 'log_ai_qty1'))

Prediction$RF1 <- predict(RF1, Data_forRF_Test[-column_id])

########################################################################
## Estimate the costs of different quantities
########################################################################
submitted <- read.csv('submit.csv', header=T)

test.pred <- merge(x = Prediction, y = test, all.y = T)
test.pred_split <- split(x = test.pred, f = as.factor(test.pred$tube_assembly_id))

test.pred <- do.call(what=rbind, lapply(X=test.pred_split, FUN=fn_predictCostFromQty))
test.pred <- merge(x=test.pred, y=submitted, by='id')
test.pred$cost_Model1[test.pred$cost_Model1 < 1.5] <- test.pred$cost.y[test.pred$cost_Model1 < 1.5]

submitFile <- test.pred[,c('id', 'cost_RF1')]
submitFile <- test.pred[,c('id', 'cost_Model1')]

colnames(submitFile) <- c('id', 'cost')
summary(submitFile)
write.csv(submitFile, 'submitFile_lm.csv', row.names = FALSE, quote = FALSE)

# qplot() + geom_point(aes(x = log(cost.y), y = log(cost_RF1)), data = test.pred)
# qplot() + geom_point(aes(x = log(cost.y), y = log(cost_Model1)), data = test.pred)
# 
# summary(test.pred)

