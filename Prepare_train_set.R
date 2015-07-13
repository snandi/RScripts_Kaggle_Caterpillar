rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script reads in all the data, using the functions
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
## Diagnostics
########################################################################
Data <- train_tube_comptype
Data$log_ai <- log(Data$cost + 1)

qplot(log_ai, data=Data, geom="histogram", binwidth=0.2) +
  ggtitle(label = expression(paste('Histogram of log', (a[i])))) +
  xlab(label = expression(log(a[i])))

lattice::xyplot(log_ai ~ quantity, data=Data)

lattice::xyplot(log_ai ~ quantity | bracket_pricing, data=Data)

lattice::xyplot(log_ai ~ quantity | bracket_pricing, data=subset(Data, quantity <=500))

