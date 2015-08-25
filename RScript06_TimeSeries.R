rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script imputes cost data for qty = 1 and uses RF to predict
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
## Load MinQty data with imputed log_ai
########################################################################
Filename <- paste(RDataPath, 'Data_MinQty.RData', sep='')
load(Filename) ## The last column is the imputed data, for qty 1

TSData <- Data_MinQty[,c('quote_date', 'train_id', 'log_ai_qty1')]
TSData$quote_date <- as.Date(as.vector(TSData$quote_date))
str(TSData)

qplot(x=quote_date, y=log_ai_qty1, data=TSData) + geom_line()
