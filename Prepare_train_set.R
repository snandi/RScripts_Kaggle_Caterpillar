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

Filename.pdf <- paste(RPlotPath, 'DiagnosticPlots_', Today, '.pdf', sep='')
#pdf(file = Filename.pdf, onefile=T)
## Histogram of log of cost
Plot1 <- qplot(log_ai, data=Data, geom="histogram", binwidth=0.2) +
  ggtitle(label = expression(paste('Histogram of log', (a[i])))) +
  xlab(label = expression(log(a[i])))

## xyplot of log_ai & quantity, to visualize the nonlinearity
Plot2 <- qplot(x=log(quantity), y=log_ai, data=Data) +
  geom_point(aes(color=bracket_pricing), data=Data)
## xyplot of log_ai & quantity, to visualize the nonlinearity
Plot3 <- qplot(x=log(quantity), y=log_ai, data=Data) +
  geom_point() +
  facet_grid(.~bracket_pricing) +
  ggtitle('By Bracket Pricing')

## Hist of log_ai by bracket_pricing
Plot4 <- qplot(log_ai, data=Data) +
  geom_histogram(aes(col=bracket_pricing), bandwidth=0.2) +
  facet_grid(.~bracket_pricing) +
  ggtitle('By Bracket Pricing')

## How many tube_assembly_id
length(unique(Data$tube_assembly_id))     ## 8855

## xyplot of log_ai & diameter, to visualize the nonlinearity
Plot5 <- qplot(x=diameter, y=log_ai, data=Data) +
  geom_point() +
  ggtitle('Cost vs diamter')

## xyplot of log_ai & wall, to visualize the nonlinearity
Plot6 <- qplot(x=wall, y=log_ai, data=Data) +
  geom_point() +
  ggtitle('Cost vs wall')

## xyplot of log_ai & length, to visualize the nonlinearity
Plot7 <- qplot(x=length, y=log_ai, data=Data) +
  geom_point() +
  ggtitle('Cost vs length')

## xyplot of log_ai & num_bends, to visualize the nonlinearity
Plot8 <- qplot(x=num_bends, y=log_ai, data=Data) +
  geom_point() +
  ggtitle('Cost vs Number of bends')

## xyplot of log_ai & bend_radius, to visualize the nonlinearity
Plot9 <- qplot(x=bend_radius, y=log_ai, data=subset(Data, bend_radius < 550)) +
  geom_point() +
  ggtitle('Cost vs bend radius')

## xyplot of length & num_bends, to visualize the nonlinearity
Data.tmp <- unique(Data[,c('num_bends','length')])
Plot10 <- qplot(x=num_bends, y=length, data=Data.tmp) +
  geom_point() +
  ggtitle('Length vs Number of bends')
rm(Data.tmp)

## xyplot of log_ai & quantity, for each Tube assembly id
Data.tmp <- subset(Data, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:100] & quantity < 500)
Plot11 <- qplot(x=quantity, y=log_ai, data=Data.tmp) +
  geom_line(aes(group=tube_assembly_id))
rm(Data.tmp)
## Plot11 shows that the cost & quantity relationship becomes a straight line after 50

NumTubes <- 500
Data.tmp <- subset(Data, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:NumTubes] & quantity < 100)
Plot12a <- qplot(x=quantity, y=log_ai, data=Data.tmp) +
  geom_line(aes(group=tube_assembly_id)) + 
  ggtitle(paste('Cost vs quantity for', NumTubes, 'tube assemblies'))
#Plot12a

Plot12b <- qplot(x=quantity, y=log_ai, data=Data.tmp) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Cost vs quantity for', NumTubes, 'tube assemblies'))
#Plot12b

Plot12c <- qplot(x=quantity, y=log_ai, data=Data.tmp) +
  geom_point() + 
  ggtitle(paste('Cost vs quantity for', NumTubes, 'tube assemblies'))
#Plot12c

Data.tmp <- subset(Data, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:NumTubes] & quantity < 400)
Plot12d <- qplot(x=log(quantity), y=log_ai, data=Data.tmp) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

Data.tmp <- subset(Data, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:NumTubes] & 
                     quantity < 400 & quantity > 1)
Plot12e <- qplot(x=log(quantity), y=log_ai, data=Data.tmp) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

rm(Data.tmp)
## Plot12 confirms that the cost & quantity relationship between 1 and 50 needs to be modeled carefully

# Plot1
# Plot2
# Plot3
# Plot4
# Plot5
# Plot6
# Plot7
# Plot8
# Plot9
# Plot10
# Plot11
# Plot12a
# Plot12b
# Plot12c
# Plot12d
# dev.off()


