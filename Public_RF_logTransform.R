rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script got 0.2748 with Random Forest & log transformation, 
## written by arnaud demytt on Kaggle
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
fn_loadData(RDataPath=RDataPath, File='test_set.csv')

options(scipen = 10)

###
### Build train and test db
###

### Load train and test
test = test_set
train = train_set

train$id = - (1:nrow(train))
test$cost = 0

train = rbind(train, test)

### Merge datasets if only 1 variable in common
continueLoop = TRUE
while(continueLoop){
  continueLoop = FALSE
  for(f in dir(RDataPath)){
    print(f)
    filename <- paste0(RDataPath, f)
    if(substrRight(filename, 3) != 'csv') next      ## Because the folder contains file_descriptions
    d = read.csv(filename)
    commonVariables = intersect(names(train), names(d))
    if(length(commonVariables) == 1){
      train = merge(train, d, by = commonVariables, all.x = TRUE)
      continueLoop = TRUE
      print(dim(train))
    }
  }
}
#compents.csv something wierd, deleted one row

#   f =  dir(RDataPath)[i]
#   d =read.csv(paste0(RDataPath,f))
#   i=i+1

### Clean NA values
for(i in 1:ncol(train)){
  if(is.numeric(train[,i])){
    train[is.na(train[,i]),i] = -1
  }else{
    train[,i] = as.character(train[,i])
    train[is.na(train[,i]),i] = "NAvalue"
    train[,i] = as.factor(train[,i])
  }
}


### Clean variables with too many categories
for(i in 1:ncol(train)){
  if(!is.numeric(train[,i])){
    freq = data.frame(table(train[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    train[,i] = as.character(match(train[,i], freq$Var1[1:30]))
    train[is.na(train[,i]),i] = "rareValue"
    train[,i] = as.factor(train[,i])
  }
}

test = train[which(train$id > 0),]
train = train[which(train$id < 0),]

###
### Evaluate RF predictions by splitting the train db in 80%/20%
###

# dtrain_cv = train[which(train$id %% 5 > 0),]
# dtest_cv = train[which(train$id %% 5 == 0),]
# 
# ### Train randomForest on dtrain_cv and evaluate predictions on dtest_cv
# set.seed(123)
# rf1 = randomForest(dtrain_cv$cost~., dtrain_cv[,-match(c("id", "cost"), names(dtrain_cv))], ntree = 10, do.trace = 2)
# 
# pred = predict(rf1, dtest_cv)
# sqrt(mean((log(dtest_cv$cost + 1) - log(pred + 1))^2)) # 0.2589951
# 
# ### With log transformation trick
# set.seed(123)
# rf2 = randomForest(log(dtrain_cv$cost + 1)~., dtrain_cv[,-match(c("id", "cost"), names(dtrain_cv))], ntree = 10, do.trace = 2)
# pred = exp(predict(rf2, dtest_cv)) - 1
# 
# sqrt(mean((log(dtest_cv$cost + 1) - log(pred + 1))^2)) # 0.2410004

### Train randomForest on the whole training set
rf = randomForest(log(train$cost + 1)~., train[,-match(c("id", "cost"), names(train))], ntree = 20, do.trace = 2)

pred = exp(predict(rf, test)) - 1

submitDb = data.frame(id = test$id, cost = pred)
submitDb = aggregate(data.frame(cost = submitDb$cost), by = list(id = submitDb$id), mean)

write.csv(submitDb, "submit.csv", row.names = FALSE, quote = FALSE)
