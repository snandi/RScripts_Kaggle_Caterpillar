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
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(round(p, 6))
}

################################################################## 
## Returns the SE of mean of each row of a dataset              ##
##################################################################
rowSE <- function(Data){
  SE <- apply(X = Data, MARGIN = 1, FUN=function(Row){sd(Row)/sqrt(length(Row))})
  return(SE)
}
################################################################## 

################################################################## 
## Load all datasets, except test & train                       ##
##################################################################
fn_loadAllData <- function(RDataPath){
  csvFiles <- list.files(path=RDataPath, pattern='.csv')
  csvFiles <- csvFiles %w/o% c('train_set.csv', 'test_set.csv')
#   File <- csvFiles[1]
  for(File in csvFiles){
    print(paste('Loading', File))
    Filename <- paste(RDataPath, File, sep='')
    Data <- read.csv(Filename, header=T)
    File_trunc <- substr(x=File, start=1, stop=(nchar(File) - 4))
    assign(x=File_trunc, value=Data, envir=.GlobalEnv)
  }
}

################################################################## 
## Load atasets individually, like test & train                 ##
##################################################################
fn_loadData <- function(RDataPath, File='test_set.csv'){
    print(paste('Loading', File))
    Filename <- paste(RDataPath, File, sep='')
    Data <- read.csv(Filename, header=T)
    File_trunc <- substr(x=File, start=1, stop=(nchar(File) - 4))
    assign(x=File_trunc, value=Data, envir=.GlobalEnv)
}
