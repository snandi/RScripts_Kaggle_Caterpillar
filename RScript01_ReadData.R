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

Filename <- paste(RDataPath, 'tube.csv', sep='')
tube <- read.csv(Filename, header=T)
str(tube)

Filename <- paste(RDataPath, 'train_set.csv', sep='')
train_set <- read.csv(Filename, header=T)
str(train_set)

Filename <- paste(RDataPath, 'test_set.csv', sep='')
test_set <- read.csv(Filename, header=T)
str(test_set)

Filename <- paste(RDataPath, 'bill_of_materials.csv', sep='')
bill_of_materials <- read.csv(Filename, header=T)
str(bill_of_materials)

Filename <- paste(RDataPath, 'components.csv', sep='')
components <- read.csv(Filename, header=T)
str(components)

Filename <- paste(RDataPath, 'comp_adaptor.csv', sep='')
comp_adaptor <- read.csv(Filename, header=T)
str(comp_adaptor)

Filename <- paste(RDataPath, 'comp_boss.csv', sep='')
comp_boss <- read.csv(Filename, header=T)
str(comp_boss)

Filename <- paste(RDataPath, 'comp_elbow.csv', sep='')
comp_elbow <- read.csv(Filename, header=T)
str(comp_elbow)

