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

Model1 <- lm(log_ai_qty1 ~ supplier, data = Data_MinQty)
anova(Model1)
summary(Model1)

Tukey_Supplier <- TukeyHSD(aov(log_ai_qty1 ~ supplier, data=Data_MinQty), 
                        conf.level = 0.95)
str(Tukey_Supplier)

pValues_Supplier <- Tukey_Supplier$supplier
colnames(pValues_Supplier)
pValues_Supplier_05 <- pValues_Supplier[pValues_Supplier[,'p adj'] < 0.05,]
pValues_Supplier_01 <- pValues_Supplier[pValues_Supplier[,'p adj'] < 0.01,]

pValueMatrix_Supplier <- fn_return_pValueTukeyMatrix(TukeyObj = Tukey_Supplier, factorName = 'supplier', 
                                                     ReturnObj = 'pValues')

Plot_pValue <- levelplot(pValueMatrix_Supplier, cex.axis=2, cex.lab=2, 
                   col.regions=colorRampPalette(c("red", "green")), 
                   scales=list(x=list(rot=90)),  
                   main="Tukey test of pairwise difference in suppliers")

BoxPlot <- ggplot(aes(y = log_ai_qty1, x = supplier), data = Data_MinQty) + 
  geom_boxplot(fill = 'royalblue1') + geom_jitter(col = 'gray40')

########################################################################
## Now look at bulk suppliers (at least 5 entries) only
########################################################################
supplierAgg <- aggregate(x = Data_MinQty$log_ai_qty1, by = list(Data_MinQty$supplier), FUN = length)
Bulk_Suppliers <- as.vector(subset(supplierAgg, x >= 5)[,'Group.1'])

Data_MinQty_Bulk <- subset(Data_MinQty, supplier %in% Bulk_Suppliers)
Tukey_BulkSupplier <- TukeyHSD(aov(log_ai_qty1 ~ supplier, data=Data_MinQty_Bulk), 
                           conf.level = 0.95)
pValues_BulkSupplier <- Tukey_BulkSupplier$supplier
pValues_BulkSupplier_01 <- pValues_BulkSupplier[pValues_BulkSupplier[,'p adj'] < 0.01,]

pValueMatrix_BulkSupplier <- fn_return_pValueTukeyMatrix(TukeyObj = Tukey_BulkSupplier, factorName = 'supplier', 
                                                     ReturnObj = 'pValues')

Plot_pValue_Bulk <- levelplot(pValueMatrix_BulkSupplier, cex.axis=2, cex.lab=2, 
                         col.regions=colorRampPalette(c("red", "green")), 
                         scales=list(x=list(rot=90)),  
                         main="Tukey test of pairwise difference in suppliers")

BoxPlot_Bulk <- ggplot(aes(y = log_ai_qty1, x = supplier), data = Data_MinQty_Bulk) + 
  geom_boxplot(fill = 'royalblue1') + geom_jitter(col = 'gray40')


########################################################################
## There is definitely a supplier effect. 
## Include indicator variable columns for certain suppliers lower or 
## higher than the rest of them
########################################################################
