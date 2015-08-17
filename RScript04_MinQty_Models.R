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


############################################### Prediction ##################################################################
# there are new levels for factor variables in test set 
####                          ISSUES : !!!!!!
## 1, some new suppliers
## 2, in regression model, some suppliers are revoved due to missing data, which makes it difficult to predict the value for TestData
     # 1016/8856 deleted
## 3. end_a has some new factors --  EF-006, EF-011, EF-013, 
## 4, end_x
#
# solving methods, 1, do some imputation, 2, using average to predict the new supplier





## for each variables
missing <- integer(ncol(TrainData_MinQty))
for ( i  in 1:ncol(TrainData_MinQty)){
  missing[i] <- sum(is.na(TrainData_MinQty[,i]))
}
plot (missing)
names(TrainData_MinQty)[which(missing > 50)]

## for each observation
rowMissing <- integer(nrow(TrainData_MinQty))
for ( i  in 1:nrow(TrainData_MinQty)){
  rowMissing[i] <- any(is.na(TrainData_MinQty[i,]))
}
summary(as.factor(rowMissing))

TrainData_MinQty_inModel <- TrainData_MinQty[(1-rowMissing)*(1:nrow(TrainData_MinQty)),]

TestData_MinQty_nonMissing <- subset(TestData_MinQty,supplier%in%TrainData_MinQty_inModel$supplier
                                       &end_a %in% TrainData_MinQty_inModel$end_a
                                       & end_x %in% TrainData_MinQty_inModel$end_x)
TestData_MinQty_nonMissing$supplier <- droplevels(TestData_MinQty_MissingSuppliers$supplier)
TestData_MinQty_Missing <- subset(TestData_MinQty,!(supplier%in%TrainData_MinQty_inModel$supplier
                                       &end_a %in% TrainData_MinQty_inModel$end_a
                                       & end_x %in% TrainData_MinQty_inModel$end_x ))
TestData_MinQty_Missing$supplier <- droplevels(TestData_MinQty_Missing$supplier)

idx_Missing <- which (!(TestData_MinQty$ supplier%in%TrainData_MinQty_inModel$supplier
                        & TestData_MinQty$end_a %in% TrainData_MinQty_inModel$end_a
                        & TestData_MinQty$end_x %in% TrainData_MinQty_inModel$end_x ) )
odx_nonMissing <- which ((TestData_MinQty$ supplier%in%TrainData_MinQty_inModel$supplier
                           & TestData_MinQty$end_a %in% TrainData_MinQty_inModel$end_a
                           & TestData_MinQty$end_x %in% TrainData_MinQty_inModel$end_x ) )

pred_oldSuppliers <- predict(lm_MinQty,TestData_MinQty_oldSuppliers)


suppliers <- unique( droplevels(TrainData_MinQty_inModel$supplier))
#using average of existing Suppliers
pred_Missing <- NULL
for ( i in 1:nrow(TestData_MinQty_Missing)){
  obs <- NULL
  for( j in 1:length(suppliers))
    obs <-rbind(obs,cbind(suppliers[j], TestData_MinQty[idx_Missing[i],2:ncol(TestData_MinQty)]))
  names(obs)[1]<-"supplier"
  values  <- mean(predict(lm_MinQty, obs))
  pred_Missing[i]<-mean(values)  
}
  
  
TestData_MinQty$supplier[id] <- NA
basePrice_te <- predict(lm_MinQty,TestData_MinQty)


############################################################################################################################
### scheme / big picture
################################################################################################################################
#Construct a data set consists of quantity =1 and price, We will construct a model on this data set to predict the price for different product 
# at quantity =1;

BasePrice <- predict (lm1,  Data_MinQty.te)
deltaPrice <- predict (lm1,  Data_CostQty.te)
PredPrice <- BasePrcie + PredPrice
