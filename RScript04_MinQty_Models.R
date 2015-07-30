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
## Prepare data for qty = 1 only
########################################################################
TrainData <- fn_prepData_tubeComp(trainORtest='train_set')
Data_Split <- split(x=TrainData, f=TrainData$tube_assembly_id)

DF <- Data_Split[[10]]
fn_returnMinQty <- function(DF){
  DF <- DF[order(DF$quantity),]
  return(DF[1,])
}
Data_MinQty <- do.call(what=rbind, lapply(X=Data_Split, FUN=fn_returnMinQty))

# Have quite figured out how to fit yet
## issues:
#  1, We want to get a data set with price corresponding to quantity =1, but some tube_assembly_id/ instance has prices from a higher quantity.
#  2, some factor variables have a lot of levels:  supplier: Factor w/ 57 levels , end_a, end_x have round 30 levesls. Difficult to intepret.
#  3, Missing issues, it seems that There are 10% missing for component type. 1 % missing for material_id.


plot(table(TrainData$material_id))
length(unique(TrainData$tube_assembly_id)) 
plot(table(TrainData$tube_assembly_id))


nrow(TrainData)
plot(TrainData$quantity)
nrow(Data_MinQty) 
plot(Data_MinQty$quantity)


##processing on Data_MInQty, #remove Data_MinQty$train_id, tube_assembly_id
str(Data_MinQty$quote_date <- as.Date(Data_MinQty$quote_date))
Data_MinQty<- Data_MinQty[,-which(names(Data_MinQty)=="train_id")]
Data_MinQty<- Data_MinQty[,-which(names(Data_MinQty)=="tube_assembly_id")]
ncol(Data_MinQty)

## missing values 
missing <- integer(ncol(Data_MinQty))
for ( i  in 1:ncol(Data_MinQty)){
  missing[i] <- sum(is.na(Data_MinQty[,i]))
}
plot (missing)
names(Data_MinQty)[which(missing > 50)]
# all from component_type variales



lm1 <- lm(log_ai ~ ., data =Data_MinQty)
summary(lm1)
anova(lm1)
# end_a, end_x have 25,27 levels respectively, suppliers have 57 levels. 
#result looks weird. But We don't need go too much into the intepretation.
nlevels(Data_MinQty$supplier)
nlevels(Data_MinQty$end_a)
nlevels(Data_MinQty$end_x)



############################################################################################################################
### predcition by combining thethe two parts togehter basis price and decline over quantity
### Need to creat corresponding Data_CostQty_Mult_D1//Data_MinQty for the test set, 
################################################################################################################################
BasePrice <- predict (lm1,  Data_MinQty.te)
deltaPrice <- predict (lm1,  Data_CostQty.te)
PredPrice <- BasePrcie + PredPrice
