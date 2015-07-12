rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script does Visual exploration of costs vs tubes, written by 
## ad2 on Kaggle
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
fn_loadData(RDataPath=RDataPath, File='train_set.csv')

tube2 <- subset(tube, select = -c(end_a_1x,end_a_2x,end_x_1x, end_x_2x,
                                    end_a, end_x, num_boss, num_bracket, other))
# cut down version 1st 1000 observations and merge by intersect 
# primary key 
newtrain <- train_set[1:1000,1:8]
t <- intersect(names(newtrain), names(tube2))
bm <- intersect(names(bill_of_materials), names(tube2))
length(t)
#cuttrain <- train_set[1:100,1:8]
cuttrain1 <- NULL
tubbom <- NULL
# cut down version of tube2
cuttube <- tube2[1:1000,1:7]

# for(i in seq(along=cuttube[,1])){
#   tubbom = merge(cuttube, bill_of_materials, by=bm, all.x = TRUE)
#   #  print(i)
# }
tubbom <- merge(cuttube, bill_of_materials, by=bm, all.x = TRUE)

# merge together cut down version of tube + bill_of_materials + train set
# for(i in seq(along=newtrain[,1])){
#   cuttrain1 = merge(newtrain, tubbom, by=t, all.x = TRUE)
#   #  print(i)
#   
# }
cuttrain1 <- merge(x=newtrain, y=tubbom, by='tube_assembly_id', all.x=T)

ggplot(cuttrain1, aes(x = cost, y=num_bends, title="Cost v Number of Bends")) + 
  geom_point()+ stat_smooth(method="glm", level=0.95,
                            color="orange")

ggplot(cuttrain1, aes(x = supplier, y= num_bends,title="Supplier v Bends")) + 
  geom_boxplot(color="red")

ggplot(cuttrain1, aes(x = cost, y=num_bends ,title="Cost v Number of Bends")) + 
  geom_point()

ggplot(cuttrain1, aes(x=cost, y=num_bends, 
                      title="Cost v Bends"))+ stat_bin2d(bins=25)
