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

fn_loadAllData(RDataPath=RDataPath)

str(tube)
str(train_set)
str(test_set)
str(bill_of_materials)
str(components)
str(comp_adaptor)
str(comp_boss)
str(comp_elbow)


