rm(list = ls(all.names = TRUE))
rm(list = objects(all.names = TRUE))
#dev.off()

########################################################################
## This script imputes cost data for qty = 1 and uses RF to predict
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/RScripts_Caterpillar/'
Filename.Header <- paste(RScriptPath, 'HeaderFile_Caterpillar.R', sep = '')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Caterpillar.R', sep = ''))
RPlotPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Plots/'
RDataPath <- '~/Stat/Stat_Competitions/Kaggle_Caterpillar_2015July/Data/'
########################################################################
Today <- Sys.Date()

########################################################################
## Load MinQty data with imputed log_ai
########################################################################
Filename <- paste(RDataPath, 'Data_MinQty.RData', sep = '')
load(Filename) ## The last column is the imputed data, for qty 1

TSData <- Data_MinQty[,c('quote_date', 'train_id', 'log_ai_qty1')]
TSData$quote_date <- as.Date(as.vector(TSData$quote_date))
str(TSData)

Plot1 <- qplot(x = quote_date, y = log_ai_qty1, data = TSData) + geom_point()

TSData.xts <- as.xts(x = TSData$log_ai_qty1, order.by = TSData$quote_date)

MonthlyMedians <- apply.monthly(x = TSData.xts, FUN = median)
MonthlyMeans <- apply.monthly(x = TSData.xts, FUN = mean)
MonthlyMin <- apply.monthly(x = TSData.xts, FUN = min)
MonthlyMax <- apply.monthly(x = TSData.xts, FUN = max)
MonthlySD <- apply.monthly(x = TSData.xts, FUN = sd)
MonthlyCount <- apply.monthly(x = TSData.xts, FUN = length)

MonthlyData <- merge.xts(MonthlyMedians, MonthlyMeans, MonthlyMin, MonthlyMax, MonthlySD, MonthlyCount)
MonthlyData.df <- na.is.zero(as.data.frame(MonthlyData))
MonthlyData.df$Date <- as.Date(rownames(MonthlyData.df))
MonthlyData.df$Year <- as.numeric(format(MonthlyData.df$Date, '%Y'))
MonthlyData.long <- melt(data = MonthlyData.df, 
                         id.vars = c('Date', 'Year'),
                         measure.vars = c("MonthlyMedians", "MonthlyMeans")
)
# qplot() + geom_line(aes(x = Date, y = MonthlyCount), data = subset(MonthlyData.df, Year <= 2010))
# qplot() + geom_line(aes(x = Date, y = MonthlyCount), data = subset(MonthlyData.df, Year > 2010))

Monthly_MeanMedian_before2010 <- qplot() + 
  geom_line(aes(x = Date, y = value, col = variable), data = subset(MonthlyData.long, Year <= 2010), ) + 
  facet_grid(variable ~ .) + 
  ylab(label = '') +
  theme(legend.position = 'none') + 
  ggtitle(label = 'Mean & Median before 2010')

Monthly_MeanMedian_after2010 <- qplot() + 
  geom_line(aes(x = Date, y = value, col = variable), data = subset(MonthlyData.long, Year > 2010), ) + 
  facet_grid(variable ~ .) + 
  ylab(label = '') +
  theme(legend.position = 'none') + 
  ggtitle(label = 'Mean & Median after 2010')

MonthlyData.long <- melt(data = MonthlyData.df, 
                         id.vars = c('Date', 'Year'),
                         measure.vars = c("MonthlyMedians", "MonthlyMeans", "MonthlyMin", "MonthlyMax")
)

MonthlyStats_before2010 <- qplot() + geom_line(aes(x = Date, y = value, col = variable), 
                                               data = subset(MonthlyData.long, Year <= 2010), ) + 
  facet_grid(variable ~ .) + 
  ylab(label = '') +
  theme(legend.position = 'none') +
  ggtitle(label = 'Monthly Stats before 2010')


MonthlyStats_after2010 <- qplot() + geom_line(aes(x = Date, y = value, col = variable), 
                                               data = subset(MonthlyData.long, Year > 2010), ) + 
  facet_grid(variable ~ .) + 
  ylab(label = '') +
  theme(legend.position = 'none') +
  ggtitle(label = 'Monthly Stats after 2010')


QtrlyBreaks <- endpoints(x = TSData.xts, on = 'quarters', k = 1)

QtrlyMedians <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = median)
QtrlyMeans <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = mean)
QtrlyMin <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = min)
QtrlyMax <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = max)
QtrlySD <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = sd)
QtrlyCount <- period.apply(x = TSData.xts, INDEX = QtrlyBreaks, FUN = length)

QtrlyData <- merge.xts(QtrlyMedians, QtrlyMeans, QtrlyMin, QtrlyMax, QtrlySD, QtrlyCount)
QtrlyData.df <- na.is.zero(as.data.frame(QtrlyData))
QtrlyData.df$Date <- as.Date(rownames(QtrlyData.df))
QtrlyData.df$Year <- as.numeric(format(QtrlyData.df$Date, '%Y'))
QtrlyData.long <- melt(data = QtrlyData.df, 
                         id.vars = c('Date', 'Year'),
                         measure.vars = c("QtrlyMedians", "QtrlyMeans")
)

Qtrly_MeanMedian_after2010 <- qplot() + 
  geom_line(aes(x = Date, y = value, col = variable), data = subset(QtrlyData.long, Year > 2010), ) + 
  facet_grid(variable ~ .) + 
  ylab(label = '') +
  theme(legend.position = 'none') + 
  ggtitle(label = 'Mean & Median before 2010')

########################################################################
## CONCLUSION: There doesnt seem to be a time trend or seasonality. 
## Wasted 3 hrs.
########################################################################

QtrlyData.df$Qtr <- format(as.yearqtr(QtrlyData.df$Date), '%q')

TSData$Year <- format(as.yearqtr(TSData$quote_date), '%Y')
TSData$Qtr <- format(as.yearqtr(TSData$quote_date), '%q')

Molel_Yr <- lm(log_ai_qty1 ~ as.factor(Year), data = subset(TSData, Year >= 2010))
summary(Molel_Yr)
anova(Molel_Yr)

Molel_Qtr <- lm(log_ai_qty1 ~ as.factor(Qtr), data = subset(TSData, Year >= 2010))
summary(Molel_Qtr)
anova(Molel_Qtr)

Molel_YrQtr <- lm(log_ai_qty1 ~ as.factor(Year) + as.factor(Qtr)  , data = subset(TSData, Year >= 2010))
summary(Molel_YrQtr)
anova(Molel_Yr, Molel_YrQtr)

Tukey_YrQtr <- TukeyHSD(aov(log_ai_qty1 ~ as.factor(Year) + as.factor(Qtr), data=subset(TSData, Year >= 2010)), 
                            conf.level = 0.95)
str(Tukey_YrQtr)

pValues_Yr <- round(Tukey_YrQtr[[1]], 4)
pValues_Qtr <- round(Tukey_YrQtr[[2]], 4)

########################################################################
## Looks like 2012 is significantly different from 2010, 2013, 2014
ggplot(aes(y = log_ai_qty1, x = factor(Year)), data = subset(TSData, Year >= 2010)) + 
  geom_boxplot(fill = 'royalblue1') + geom_jitter(col = 'gray40')
pValues_Yr
########################################################################

########################################################################
## Looks like Qtr1 is significantly different from 2, 3 & 4
ggplot(aes(y = log_ai_qty1, x = factor(Qtr)), data = subset(TSData, Year >= 2010)) + 
  geom_boxplot(fill = 'royalblue1') + geom_jitter(col = 'gray40')
pValues_Qtr
########################################################################

