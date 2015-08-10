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



########################################################################
## Prepare data for qty = 1 only, NOT good, give up this procedure.
########################################################################
TrainData <- fn_prepData_tubeComp(trainORtest='train_set')
TrainData_Qty1 = subset(TrainData,quantity==1)
TrainData_Qty1 <- TrainData_Qty1[,-which(names(TrainData_Qty1)=='cost'|names(TrainData_Qty1)=="tube_assembly_id"
                                        |names(TrainData_Qty1)=='train_id')] 
TrainData_Qty1$quote_date <- as.Date(TrainData_Qty1$quote_date)
ncol(TrainData_Qty1)

lm_Qty1 <- lm( log_ai ~ . ,data =TrainData_Qty1)
summary(lm_Qty1)

TrainData_MinQty <-  fn_prepData_MinQty(trainORtest = 'train_set')
dim (TrainData_MinQty)
# remove tube_assembly_id, which works like id index/ row.names
TrainData_MinQty <- TrainData_MinQty[,-which(names(TrainData_MinQty)=='cost'|
                                               names(TrainData_MinQty)=='log_ai'
                                             |names(TrainData_MinQty)=="tube_assembly_id"
                                             |names(TrainData_MinQty)=='train_id')] 
TrainData_MinQty$quote_date <- as.Date(TrainData_MinQty$quote_date)
dim(TrainData_MinQty)
basePrice <- predict(lm_Qty1, newdata = TrainData_MinQty)
#Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
factor supplier has new levels S-0022, S-0024, S-0029, S-0043, S-0050, S-0051, S-0059, S-0061, S-0068, S-0078, S-0080, S-0106, S-0108, S-0109


###############################################################################################################
### Build a linear model on the TrainData_MinQty, include the quantity in the linear model.
################################################################################################################


TrainData_MinQty <-  fn_prepData_MinQty(trainORtest = 'train_set')
dim (TrainData_MinQty)
# remove tube_assembly_id, which works like id index/ row.names
TrainData_MinQty <- TrainData_MinQty[,-which(names(TrainData_MinQty)=='cost'
                                             |names(TrainData_MinQty)=="tube_assembly_id"
                                             |names(TrainData_MinQty)=='train_id')] 
TrainData_MinQty$quote_date <- as.Date(TrainData_MinQty$quote_date)
dim(TrainData_MinQty)

lm_MinQty <- lm( log_ai ~ . ,data =TrainData_MinQty)
summary(lm_MinQty)


TestData_MinQty <-  fn_prepData_MinQty(trainORtest = 'test_set')
dim (TestData_MinQty)
TestData_MinQty <- TestData_MinQty[,-which(names(TestData_MinQty)=='cost'
                                             |names(TestData_MinQty)=="tube_assembly_id"
                                             |names(TestData_MinQty)=='log_ai'
                                             |names(TestData_MinQty)=='train_id')] 
TestData_MinQty$quote_date <- as.Date(TestData_MinQty$quote_date)

basePrice_te <- predict(lm_MinQty,TestData_MinQty)

# Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
#   factor supplier has new levels S-0028, S-0033, S-0036, S-0039, S-0040, S-0043, S-0051, S-0061, 
#                                   S-0069, S-0073, S-0076, S-0077, S-0078, S-0080, S-0088, S-0091
## setting the new supplier as NA 

id <- which(!(TestData_MinQty$supplier %in% levels(TrainData_MinQty$supplier)))
TestData_MinQty$supplier[id] <- NA
basePrice_te <- predict(lm_MinQty,TestData_MinQty)

# Have quite figured out how to fit yet
## issues:
#  1, We want to get a data set with price corresponding to quantity =1, but some tube_assembly_id/ instance has prices from a higher quantity.
#  2, some factor variables have a lot of levels:  supplier: Factor w/ 57 levels , end_a, end_x have round 30 levesls. Difficult to intepret.
#  3, Missing issues, it seems that There are 10% missing for component type. 1 % missing for material_id.


missing <- integer(ncol(TrainData_MinQty))
for ( i  in 1:ncol(TrainData_MinQty)){
  missing[i] <- sum(is.na(TrainData_MinQty[,i]))
}
plot (missing)
names(TrainData_MinQty)[which(missing > 50)]

#####!!!!! because of missingness, many observations are removed from the linear model, 
#          and for those new suppliers, we have to predict with mean value.
###        Think about the imputation.





##processing on Data_MInQty, #remove Data_MinQty$train_id, tube_assembly_id
str(Data_MinQty$quote_date <- as.Date(Data_MinQty$quote_date))
Data_MinQty<- Data_MinQty[,-which(names(Data_MinQty)=="train_id")]
Data_MinQty<- Data_MinQty[,-which(names(Data_MinQty)=="tube_assembly_id")]
ncol(Data_MinQty)



############################################################################################################################
### predcition by combining thethe two parts togehter basis price and decline over quantity
### Need to creat corresponding Data_CostQty_Mult_D1//Data_MinQty for the test set, 
################################################################################################################################
#Construct a data set consists of quantity =1 and price, We will construct a model on this data set to predict the price for different product 
# at quantity =1;


TestData <- fn_prepData_tubeComp(trainORtest='test_set')

Data_MinQty_Train <- fn_prepData_MinQty(trainORtest = 'train_set')
Data_MinQty_Test <- fn_prepData_MinQty(trainORtest = 'test_set')

BasePrice <- predict (lm1,  Data_MinQty.te)
deltaPrice <- predict (lm1,  Data_CostQty.te)
PredPrice <- BasePrcie + PredPrice
