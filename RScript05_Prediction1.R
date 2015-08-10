
rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
# This script explores the relationship between basis price (at quantity=1) and tubes properties

########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/RScripts_Caterpillar/'
Filename.Header <- paste(RScriptPath, 'HeaderFile_Caterpillar.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Caterpillar.R', sep=''))
RPlotPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Plots/'
RDataPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Data/'
########################################################################
Today <- Sys.Date()







TrainData <-  fn_prepData_tubeComp(trainORtest = 'train_set')
dim (TrainData)
# remove tube_assembly_id, which works like id index/ row.names
TrainData <- TrainData[,-which(names(TrainData)=='cost'
                                 |names(TrainData)=="tube_assembly_id"
                                 |names(TrainData)=='train_id')] 
TrainData$quote_date <- as.Date(TrainData$quote_date)
TrainData$quote_date <- as.integer( TrainData$quote_date - min(TrainData$quote_date) )
dim(TrainData)

library(randomForest)
fit1.rf <- randomForest(log_ai~.,  data=TrainData, mtry=10, ntree=10, importance=TRUE)
#You can set na.action=na.roughfix which fills NAs with the mean or mode of the missing variable. 
# Other option is to impute missing values using rfImpute, then run randomForest on the complete data set

fit1.rf <- randomForest(log_ai~.,  data=TrainData, mtry=10, ntree=10, importance=TRUE,na.action=na.roughfix )
str(TrainData)

TestData <-  fn_prepData_tubeComp(trainORtest = 'test_set')
dim (TestData)
TestData <- TestData[,-which(names(TestData)=='cost'
                                           |names(TestData)=="tube_assembly_id"
                                           |names(TestData)=='log_ai'
                                           |names(TestData)=='train_id')] 
TestData$quote_date <- as.Date(TestData$quote_date)

basePrice_te <- predict(lm_MinQty,TestData_MinQty)

