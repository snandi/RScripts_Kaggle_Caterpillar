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

# TrainData <- fn_prepData_tubeComp(trainORtest='train_set')
Data_CostQty_Mult_D1 <- fn_prepData_CostQty(trainORtest = 'train_set')
str(Data_CostQty_Mult_D1)

Data_MinQty <- fn_prepData_MinQty(trainORtest = 'train_set')
str(Data_MinQty)
########################################################################
## Plot first diff of log(cost) & quantity
########################################################################
head(Data_CostQty_Mult_D1)
Data <- Data_CostQty_Mult_D1
NumTubes <- 500

Data.tmp <- subset(Data, tube_assembly_id %in% levels(Data$tube_assembly_id)[1:NumTubes] & 
                     quantity < 400 & quantity > 1)

Filename.pdf <- paste(RPlotPath, 'CostQtyPlots_', Today, '.pdf', sep='')
#pdf(file = Filename.pdf, onefile=T)
NumTubes <- length(unique(as.vector(Data$tube_assembly_id)))

Plot1 <- qplot(x=log(quantity), y=log_ai, data=Data) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

#Plot1

Plot2 <- qplot(x=log(quantity), y=log_ai_d1, data=Data) +
  geom_line(aes(color=tube_assembly_id)) + 
  ggtitle(paste('Diff of Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))

#Plot2

Plot3 <- qplot(x=log_qty, y=log_ai, data=Data) +
  ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))
#Plot3

dev.off()

########################################################################
## Model the cost & quantity
########################################################################
head(Data_CostQty_Mult_D1)
Data <- Data_CostQty_Mult_D1
Data$log_qty <- log(Data$quantity)

Model1 <- lm(log_ai ~ quantity, data=Data)
summary(Model1)

Model2 <- lm(log_ai ~ log_qty, data=Data)
summary(Model2)

Model3 <- lm(log_ai ~ log_qty + I((log_qty)^2), data=Data)
summary(Model3)
anova(Model2, Model3)

Model4 <- lm(log_ai ~ log_qty + I((log_qty)^2) + I((log_qty)^3), 
             data=Data)
summary(Model4)
anova(Model3, Model4)

Model5 <- lm(log_ai ~ log_qty + I((log_qty)^2) + I((log_qty)^3) + 
               I((log_qty)^4), data=Data)
summary(Model5)
anova(Model4, Model5)

# Model4b <- lm(Data$log_ai ~ poly(x=log(Data$quantity), degree=3, raw=F))
# summary(Model4b)
# 
# Model4.lo <- loess(log_ai ~ log_qty, data=Data, model=TRUE, degree=2)
# summary(Model4.lo)
# DD <- predict(Model4.lo, Data$log_qty, se=T)
# 
# Plot4 <- Plot3 +
#  geom_line(aes(x=Data$log_qty, y=predict(Model4.lo, Data$log_qty, se.fit=T)), color='red')
## Don't use loess for the relationship between cost & qty

########################################################################
## FINAL MODEL with all quantities
## Should be Model 3, with quadratic coefficients
########################################################################
Fitted.3 <- fitted.values(Model3)   # quadratic
Fitted.4 <- fitted.values(Model4)   # cubic
Fitted.5 <- fitted.values(Model5)   # quartic

Plot3 + geom_line(aes(y = Fitted.3), col = 'red') + geom_line(aes(y = Fitted.4), col = 'blue') + 
  geom_line(aes(y = Fitted.5), col = 'orange', size=1)

CostQtyModel <- Model3
Resid <- residuals(CostQtyModel)

ResidPlots <- grid.arrange(qplot(y=Resid) + geom_point() + xlab(label=''), 
                           qplot() + geom_histogram(aes(x = Resid), binwidth=0.1), 
                           ncol = 2)
coef(CostQtyModel)
### (Intercept)     log_qty       I((log_qty)^2) 
### 3.18632061      -0.60453927   0.05378612 
Beta0 <- coef(CostQtyModel)[[1]]
Beta1 <- coef(CostQtyModel)[[2]]
Beta2 <- coef(CostQtyModel)[[3]]

Data_MinQty$log_ai_qty1 <- Data_MinQty$log_ai - Beta1*log(Data_MinQty$quantity) - Beta2*(log(Data_MinQty$quantity))^2
Filename <- paste(RDataPath, 'Data_MinQty.RData', sep='')
save(Data_MinQty, file=Filename)
########################################################################
## Model the cost & quantity, with quantity strictly greater than 1
########################################################################
# Data1 <- subset(Data, log_qty > 0)
# 
# Model1a <- lm(log_ai ~ quantity, data=Data1)
# summary(Model1a)
# 
# Model2a <- lm(log_ai ~ log_qty, data=Data1)
# summary(Model2a)
# 
# Model3a <- lm(log_ai ~ log_qty + I((log_qty)^2), data=Data1)
# Fitted.3a <- fitted.values(Model3a)
# summary(Model3a)
# anova(Model2a, Model3a)
# 
# Plot3a <- qplot(x=log_qty, y=log_ai, data=Data1) +
#   ggtitle(paste('Log(Cost) vs log(quantity) for', NumTubes, 'tube assemblies'))
# #Plot3a
# 
# Plot3a + geom_line(aes(y = Fitted.3a), col = 'red')
# 
# Model4a <- lm(log_ai ~ log_qty + I((log_qty)^2) + I((log_qty)^3), data=Data1)
# Fitted.4a <- fitted.values(Model4a)
# summary(Model4a)
# anova(Model3a, Model4a)
# 

