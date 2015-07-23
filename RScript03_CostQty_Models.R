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

source(paste(RScriptPath, 'Prepare_train_set.R', sep=''))

########################################################################
## Discard tubes with only qty=1
########################################################################
Data_Split <- split(x=Data_CostQty, f=Data_CostQty$tube_assembly_id)

fn_OneQty <- function(DF){
  if(nrow(DF) == 1) {
    if(DF$quantity == 1) Ans <- TRUE
  } else Ans <- FALSE
  #names(Ans) <- as.vector(DF$tube_assembly_id[1])
  return(Ans)
}
Tubes.Drop <- do.call(what=c, lapply(X=Data_Split, FUN=fn_OneQty))


########################################################################
## Plot first diff of log(cost) & quantity
########################################################################
head(Data_CostQty_D1)
NumTubes <- 500
Data.tmp <- subset(Data_CostQty_D1, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:NumTubes] & 
                     quantity < 400 & quantity > 1)

Filename.pdf <- paste(RPlotPath, 'CostQtyPlots_', Today, '.pdf', sep='')
#pdf(file = Filename.pdf, onefile=T)

Plot1 <- qplot(x=log(quantity), y=log_ai, data=Data_CostQty_D1) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

#Plot1

Plot2 <- qplot(x=log(quantity), y=log_ai_d1, data=Data_CostQty_D1) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Diff of Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

#Plot2

Plot3 <- qplot(x=log(quantity), y=log_ai, data=Data_CostQty_D1) +
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))
Plot3

dev.off()

########################################################################
## Model the cost & quantity
########################################################################
Model1 <- lm()
