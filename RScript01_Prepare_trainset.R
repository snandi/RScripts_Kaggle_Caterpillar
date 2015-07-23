rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script merges the train_set data, with other relevant tables
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

#fn_loadAllData(RDataPath=RDataPath)
fn_loadData(RDataPath=RDataPath, File='train_set.csv')
fn_loadData(RDataPath=RDataPath, File='tube.csv')

dim(train_set)
train_set$train_id <- row(train_set)
train_tube_common <- intersect(names(train_set), names(tube))
train_tube <- merge(x=train_set, y=tube, by=train_tube_common, all.x=T) 
dim(train_tube)
head(train_tube)

tube_by_component_type <- fn_merge_tube_comp_type(File1='tube.csv', File2='bill_of_materials.csv')

train_tube_comptype <- merge(x=train_tube, y=tube_by_component_type, by='tube_assembly_id',
                             all.x=T, all.y=F)


########################################################################
## Extract data for analyzing cost & quantity relationship
########################################################################
Data <- train_tube_comptype
Data$log_ai <- log(Data$cost + 1)

########################################################################
## Prepare the data for cost and quantity relationship
########################################################################
Data_CostQty <- Data[,c('tube_assembly_id', 'bracket_pricing', 'quantity', 'log_ai')]

Data_Split <- split(x=Data_CostQty, f=Data_CostQty$tube_assembly_id)

## Function to add a column of the first difference of log(cost), by levels
## of the factor tube_assembly_id
fn_logcostD1 <- function(DF){
  DF <- DF[order(DF$quantity),]
  DF$log_ai_d1 <- c(0, diff(DF$log_ai))
  return(DF)
}
Data_CostQty_D1 <- do.call(what=rbind, lapply(X=Data_Split, FUN=fn_logcostD1))
