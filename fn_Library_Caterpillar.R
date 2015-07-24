######################### Convert NAs to Zero ##########################
na.is.zero <- function(X)
{
  X1 <- X
  X1[is.na(X)] <- 0.0
  return(X1)
}
########################################################################

########################################################################
"%notin%" <- function(x, y){
  if(x %in% y){
    return(FALSE)
  } else{
    return(TRUE)
  }
}
########################################################################

########################################################################
"%w/o%" <- function(x, y){
  return(x[!x %in% y])
}
########################################################################

########################################################################
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
########################################################################

is.integer0 <- function(x)
{
  is.integer(x) && !length(x)
}

## Function library for Curve registration

fn_get_pValue <- function (lmobject) {
  if (class(lmobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(lmobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail = F)
  attributes(p) <- NULL
  return(round(p, 6))
}

########################################################################
## Returns the SE of mean of each row of a dataset              
########################################################################
rowSE <- function(Data){
  SE <- apply(X = Data, MARGIN = 1, FUN = function(Row){sd(Row)/sqrt(length(Row))})
  return(SE)
}
########################################################################

########################################################################
## Load all datasets, except test & train                       
########################################################################
fn_loadAllData <- function(RDataPath){
  csvFiles <- list.files(path = RDataPath, pattern = '.csv')
  csvFiles <- csvFiles %w/o% c('train_set.csv', 'test_set.csv')
#   File <- csvFiles[1]
  for(File in csvFiles){
    print(paste('Loading', File))
    Filename <- paste(RDataPath, File, sep = '')
    Data <- read.csv(Filename, header = T, quote = '')
    File_trunc <- substr(x = File, start = 1, stop = (nchar(File) - 4))
    assign(x = File_trunc, value = Data, envir = .GlobalEnv)
  }
}

########################################################################
## Load atasets individually, like test & train
########################################################################
fn_loadData <- function(RDataPath, File = 'test_set.csv', Return = FALSE){
    print(paste('Loading', File))
    Filename <- paste(RDataPath, File, sep = '')
    Data <- read.csv(Filename, header = T, quote = '')
    File_trunc <- substr(x = File, start = 1, stop = (nchar(File) - 4))
    assign(x = File_trunc, value = Data, envir = .GlobalEnv)
    if(Return) return(Data)
}

########################################################################
## Reshaped bill_of_materials, by component_type_id & their 
## quantities, for each tube_assembly_id
########################################################################
fn_reshapeBOM <- function(File = 'bill_of_materials.csv'){
  bom <- fn_loadData(RDataPath = RDataPath, File = File, Return = T)
  
  bom_long.c <- melt(data = bom, id.vars = 'tube_assembly_id', 
                     measure.vars = c(grep(pattern = 'component', x = names(bom)))
  )
  bom_long.c <- na.omit(bom_long.c[order(bom_long.c$tube_assembly_id),])
  names(bom_long.c) <- c('tube_assembly_id', 'component_num', 'component_id')
  
  bom_long.q <- melt(data = bom, id.vars = 'tube_assembly_id', 
                     measure.vars = c(grep(pattern = 'quantity', x = names(bom)))
  )
  bom_long.q <- na.omit(bom_long.q[order(bom_long.q$tube_assembly_id),])
  names(bom_long.q) <- c('tube_assembly_id', 'component_num', 'quantity')
  bom_long.q$component_num <- gsub(pattern = 'quantity_', replacement = 'component_id_', 
                                   x = bom_long.q$component_num)
  
  bom_long <- merge(x = bom_long.c, y = bom_long.q, by = intersect(names(bom_long.c), names(bom_long.q)), all = T)
  bom_long <- na.omit(bom_long)  ## There are 8 components without any valid component_id
  bom_long$component_num <- NULL
  return(bom_long)
}

########################################################################
## Merge tube_assembly_id with the quantity of each component 
########################################################################
fn_merge_tube_comp_type <- function(File1 = 'tube.csv', File2 = 'bill_of_materials.csv'){
  tube <- fn_loadData(RDataPath = RDataPath, File = File1, Return = T)
  components <- fn_loadData(RDataPath = RDataPath, File = 'components.csv', Return = T)
  bom_long <- fn_reshapeBOM(File = File2)
  bom_comp <- merge(x = bom_long, y = components, by = 'component_id', all.x = T)
  
  component_type_qty <- aggregate(x = bom_comp$quantity, by = list(bom_comp$tube_assembly_id, bom_comp$component_type_id), 
                                  FUN = sum)
  names(component_type_qty) <- c('tube_assembly_id', 'component_type_id', 'comp_type_qty')
  
  tube_by_component_type <- reshape2::dcast(data = component_type_qty, 
                                            formula = tube_assembly_id ~ component_type_id, 
                                            value.var = 'comp_type_qty')
  tube_by_component_type <- na.is.zero(X = tube_by_component_type)
  return(tube_by_component_type)  
}

########################################################################

########################################################################
## Prepares train or test data, by merging it with tubes and tube
## component types. Use this function in subsequent data preparation
## functions
########################################################################
fn_prepData_tubeComp <- function(trainORtest = 'train_set'){
  
  if(substr(x = trainORtest, start = (nchar(trainORtest) - 4), stop = nchar(trainORtest)) != '.csv'){
    trainORtest <- paste(trainORtest, '.csv', sep = '')
  }
  
  TT <- fn_loadData(RDataPath = RDataPath, File = trainORtest, Return = T)
  tube <- fn_loadData(RDataPath = RDataPath, File = 'tube.csv', Return = T)
  
  TT$train_id <- row(TT)
  TT_tube_common <- intersect(names(TT), names(tube))
  TT_tube <- merge(x = TT, y = tube, by = TT_tube_common, all.x = T) 
  
  tube_by_component_type <- fn_merge_tube_comp_type(File1 = 'tube.csv', File2 = 'bill_of_materials.csv')
  
  TT_tube_comptype <- merge(x = TT_tube, y = tube_by_component_type, by = 'tube_assembly_id',
                               all.x = T, all.y = F)
  TT_tube_comptype$log_ai <- log(TT_tube_comptype$cost + 1)  
  return(TT_tube_comptype)  
}
########################################################################

########################################################################
## Function to prepare the data for cost and quantity model
########################################################################
fn_prepData_CostQty <- function(trainORtest = 'train_set'){
  TT <- fn_prepData_tubeComp(trainORtest=trainORtest)
  
  Data_CostQty <- TT[,c('tube_assembly_id', 'bracket_pricing', 'quantity', 'log_ai')]
  
  ###############
  ## Discard tubes with only qty=1
  ###############
  Data_Split <- split(x = Data_CostQty, f = Data_CostQty$tube_assembly_id)
  
  fn_OneQty <- function(DF){
    #print(DF$tube_assembly_id[1])
    DF <- DF[order(DF$quantity),]
    Ans <- FALSE
    if(nrow(DF) == 1 & DF$quantity[1] == 1) Ans <- TRUE
    return(Ans)
  }
  
  Tubes.Drop <- do.call(what = c, lapply(X = Data_Split, FUN = fn_OneQty))
  Data_CostQty$tube_assembly_id <- as.vector(Data_CostQty$tube_assembly_id)
  Data_CostQty <- merge(x = Data_CostQty, y = cbind(tube_assembly_id = names(Tubes.Drop), Drop = Tubes.Drop), 
                        by = 'tube_assembly_id', all.x = T)
  
  ###############
  ## Add first difference of log(cost), by tube id
  ###############
  Data_CostQty_Mult <- subset(Data_CostQty, Drop==FALSE)
  Data_CostQty_Mult$Drop <- NULL
  
  Data_CostQty_Mult$tube_assembly_id <- as.factor(Data_CostQty_Mult$tube_assembly_id)
  Data_Split <- split(x = Data_CostQty_Mult, f = Data_CostQty_Mult$tube_assembly_id)
  
  ## Function to add a column of the first difference of log(cost), by levels
  ## of the factor tube_assembly_id
  fn_logcostD1 <- function(DF){
    DF <- DF[order(DF$quantity),]
    DF$log_ai_d1 <- c(0, diff(DF$log_ai))
    return(DF)
  }
  Data_CostQty_Mult_D1 <- do.call(what = rbind, lapply(X = Data_Split, FUN = fn_logcostD1))
  Data_CostQty_Mult_D1$log_qty <- log(Data_CostQty_Mult_D1$quantity)
  return(Data_CostQty_Mult_D1)  
}
########################################################################

########################################################################
## Prepare data for qty = 1 only
## This dataset should be used for modeling other factors
########################################################################
fn_prepData_MinQty <- function(trainORtest = 'train_set'){
  TrainData <- fn_prepData_tubeComp(trainORtest = trainORtest)
  Data_Split <- split(x=TrainData, f=TrainData$tube_assembly_id)
  
  fn_returnMinQty <- function(DF){
    DF <- DF[order(DF$quantity),]
    return(DF[1,])
  }
  Data_MinQty <- do.call(what=rbind, lapply(X=Data_Split, FUN=fn_returnMinQty))
  return(Data_MinQty)  
}
