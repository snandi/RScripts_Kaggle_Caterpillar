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
## Identifies positions of minimum curvature in a vector        ##
## Needs further refinement. This is just the second difference ##
##################################################################
fn_returnMinCurvaturePositions <- function(Vector, NumberOfMins){
  V <- na.remove(Vector)
  L <- length(V)
  
  D1 <- diff(x=V)
  D2 <- diff(x=D1)
  
  Data <- cbind(V, c(0, D1), c(0, 0, D2))
  Positions <- which(abs(D2) %in% sort(abs(D2))[1:NumberOfMins]) + 2
  return(Positions[1:NumberOfMins])
}
##################################################################

################################################################## 
## Inserts pixel intensity based on the Position numbers estim- ##
## -ated by the function MinCurvaturePositions                  ##
##################################################################
fn_insertPixelPositions <- function(Curve1, Positions){
  Pos1 <- Positions
  L1 <- length(Pos1)
  Pos2 <- Pos1 + 0:(L1-1)
  for(i in 1:L1){
    Pos <- Pos2[i]
    Curve2 <- c(Curve1[1:(Pos - 1)], 
                mean(Curve1[(Pos - 1):Pos]), 
                Curve1[Pos:length(Curve1)])
    Curve1 <- Curve2
    rm(Curve2)
  }
  return(Curve1)
}
##################################################################

################################################################## 
## Stretches all the curves passed as a matrix, to the desired  ##
## length (consensusPixelLength). PixelNum is a vector of pixel ##
## numbers, passed from the original dataset                    ##
##################################################################
fn_stretchAllcurves <- function(Curves, ConsensusPixelLength, PixelNum){
  Vector <- rep(0, times=ConsensusPixelLength)
  PixelNum <- PixelNum[1:ConsensusPixelLength]
  Curves_SameLength <- as.data.frame(cbind(Vector, PixelNum=PixelNum))
  Col <- 1
  for(Col in 1:ncol(Curves)){
    Curve1 <- na.remove(Curves[,Col])
    CurveName <- names(Curves)[Col]
    Discrepancy <- ConsensusPixelLength - length(Curve1)
    if(Discrepancy > 0){
      MinPos <- fn_returnMinCurvaturePositions(Vector=Curve1, 
                                               NumberOfMins=Discrepancy)
      Curve2 <- fn_insertPixelPositions(Curve1=Curve1, Positions=MinPos)
      Curves_SameLength <- cbind(Curves_SameLength, Curve2)
      colnames(Curves_SameLength)[colnames(Curves_SameLength)=='Curve2'] <- CurveName
      rm(Curve1, Curve2)      
    } else if(Discrepancy < 0){
      MinPos <- fn_returnMinCurvaturePositions(Vector=Curve1, 
                                               NumberOfMins=(-Discrepancy))
      Curve2 <- Curve1[-c(MinPos)]
      Curves_SameLength <- cbind(Curves_SameLength, Curve2)
      colnames(Curves_SameLength)[colnames(Curves_SameLength)=='Curve2'] <- CurveName
      rm(Curve1, Curve2)      
    } else{
      Curves_SameLength <- cbind(Curves_SameLength, Curve1)
      colnames(Curves_SameLength)[colnames(Curves_SameLength)=='Curve1'] <- CurveName
    }
    if(Col == 1){
      Curves_SameLength$Vector = NULL
    }
  }
  return(as.data.frame(Curves_SameLength))
}

################################################################## 
########## Functions used for RScript02_reg_MF_perFrag.R #########
fn_returnManualRSq <- function(InputFD, RegisteredData){
  AmpPhaseList <- AmpPhaseDecomp(xfd=InputFD, 
                                 yfd=RegisteredData$regfd, 
                                 hfd=RegisteredData$warpfd
  )
  MS.Amp <- AmpPhaseList$MS.amp
  MS.Phase <- AmpPhaseList$MS.pha
  RSq <- AmpPhaseList$RSQR
  RSq.Manual <- MS.Phase*(MS.Phase > 0)/(MS.Phase + MS.Amp)
  return(RSq.Manual)
}
################################################################## 

################################################################## 
## To plot multiple curves in a single plot                     ##
################################################################## 
plotCurves <- function(Data, ColsToPlot, XVar, Xlab='PixelPosition', 
                       Ylab='Intensity', MainTitle='', Main.cex=2, 
                       Colors=''){
  ## Make sure the X-axis column is named PixelNum. If not, pass that 
  ## argument
  Data <- as.data.frame(Data)
  X.long <- reshape(data=Data, varying=ColsToPlot, 
                    idvar='PixelPosition',
                    ids=XVar,
                    timevar='Curve', 
                    v.names='Intensity', 
                    direction='long')
  
  X.long <- within(data=X.long,{
    Curve <- factor(Curve)
  })
  
  if(Colors[1]==''){
    Plot <- xyplot(Intensity ~ PixelPosition, groups=Curve , data=X.long, 
                   type=c("l", "g"), lwd=1.25, 
                   scales=list(x=list(rot=1, col='orange', cex=1.2), 
                               y=list(col='orange', cex=1.2)), 
                   xlab=list(label=Xlab, col='red', cex=1.2), 
                   ylab=list(label=Ylab, col='red', cex=1.2),
                   main=list(label=MainTitle, col='red', cex=Main.cex),
                   panel = function(...) { 
                     panel.fill(col = 'black') 
                     panel.xyplot(...) 
                   }
    )
  } else{
    Plot <- xyplot(Intensity ~ PixelPosition, groups=Curve , data=X.long, 
                   type=c("l", "g"), lwd=1.25, 
                   col=Colors,
                   scales=list(x=list(rot=1, col='orange', cex=1.2), 
                               y=list(col='orange', cex=1.2)), 
                   xlab=list(label=Xlab, col='red', cex=1.2), 
                   ylab=list(label=Ylab, col='red', cex=1.2),
                   main=list(label=MainTitle, col='red', cex=Main.cex),
                   panel = function(...) { 
                     panel.fill(col = 'black') 
                     panel.xyplot(...) 
                   }
    )
  }
  return(Plot)
}
################################################################## 

plotMeanCurves <- function(Data, ColsToPlot, XVar, Xlab='PixelNum', 
                           Ylab='Intensity', MainTitle='', Main.cex=2, 
                           Colors=NULL){
  Data <- as.data.frame(Data)
  X.long <- reshape(data=Data, varying=ColsToPlot, 
                    idvar='PixelPosition',
                    ids=XVar,
                    timevar='Curve', 
                    v.names='Intensity', 
                    direction='long')
  
  X.long <- within(data=X.long,{
    Curve <- factor(Curve)
    levels(Curve) <- ColsToPlot
  })
  
  if(is.null(Colors)){
    my.Colors=c('olivedrab1', 'orange', 'gray90')
    #  my.Colors=c('olivedrab1', 'orange', 'gray30', 'gray35', 'gray40', 'gray45', 
    #              'gray50', 'gray55', 'gray60', 'gray65', 'gray70', 'gray90')
  } else{
    my.Colors <- Colors
  }
  Plot <- xyplot(Intensity ~ PixelPosition, groups=Curve , data=X.long, 
                 type=c("l", "g"), lwd=rep(1.5, 7), 
                 col=my.Colors,
                 scales=list(x=list(rot=1, col='orange'), 
                             y=list(col='orange', cex=1.5)), 
                 xlab=list(label=Xlab, col='orange', cex=1.2), 
                 ylab=list(label=Ylab, col='orange', cex=1.2),
                 main=list(label=MainTitle, col='red', cex=Main.cex),
                 key = list(text=list(levels(X.long$Curve)), 
                                 space='top', col=my.Colors,
                                 lines=list(col=my.Colors, lwd=1.1), 
                                 columns = 3
                 ),
                 panel = function(...) { 
                   panel.fill(col = 'black') 
                   #panel.grid(h=2, v=5, col='red', col.line='red', lwd=2, identifier = "grid")
                   panel.xyplot(...) 
                 }
  )
  return(Plot)
}
################################################################## 

################################################################## 
## To use kma.similarity for a matrix                           ##
################################################################## 
fn_kma.similarity_mat <- function(Mat, Xaxis,
                                  similarity.method=c("d0.pearson", "d1.pearson", 
                                                      "d0.L2", "d1.L2", "d0.L2.centered", "d1.L2.centered")){
  ## Assume that similarity measure is to be estimated between columns
  ## of the matrix. 
  ## Assumed that the x-axis of all the functions will be the same
  ## Return the average of the similarity index, between all the columns
  ## d1 corresponds to first derivatives. Make sure the matrix Mat is that of
  ## first derivatives and not just the function values
  Vector <- c(1:ncol(Mat))
  ColCombs <- as.data.frame(combinations(n=length(Vector), r=2, v=Vector))
  ColCombs$Similarity <- 0
  for(Row in 1:nrow(ColCombs)){
    ColCombs[Row,'Similarity'] <- kma.similarity (x.f=Xaxis, y1.f=Mat[,ColCombs[Row,1]], 
                                                  x.g=Xaxis, y1.g=Mat[,ColCombs[Row,2]], 
                                                  similarity.method=similarity.method)
  }
  return(list(ColCombs=ColCombs, AvgSimilarity=mean(ColCombs$Similarity)))
}
################################################################## 

################################################################## 
## Plot a mean curve with SE, using ggplot2
################################################################## 
fn_plotMean_wSE <- function(Mean, SE, Xaxis, Color='steelblue2', 
                            MainTitle='', YLim_MeanPlot=NULL){

  Data <- as.data.frame(cbind(Mean, LL=Mean - 2*SE, UU=Mean + 2*SE, Xaxis))
  
  if(is.null(YLim_MeanPlot)){
    YLim_MeanPlot <- range(Data$LL, Data$UU)*1.1
  }
  
  MeanPlot <- qplot(x=Xaxis, y=Mean, data=Data) + 
    ylim(YLim_MeanPlot) +
    geom_line(data=Data, size=2, color=Color) + 
    geom_smooth(aes(ymin = LL, ymax = UU), data=Data, stat="identity") +
    xlab(label = 'Pixel Position') + ylab('Mean +/- 2 SE') +
    ggtitle(MainTitle) +
    theme(plot.title=element_text(face="bold", size=12, colour="white"),
          panel.background = element_rect(fill = 'black'), 
          plot.background = element_rect(color='black', fill = "gray10"), 
          axis.text = element_text(colour = "white", size=10), 
          axis.title.x = element_text(colour = "white", size=10), 
          axis.title.y = element_text(colour = "white", size=10), 
          panel.grid.major = element_line(colour="gray30", size=0.35), 
          panel.grid.minor = element_line(colour="gray20", size=0.25)
    )
  return(MeanPlot)  
}
################################################################## 

################################################################## 
## Create a fd object of a curve, after smoothing it with bsplies
################################################################## 
fn_createCurve_FDObject <- function(lambdas, Curve=Median, Xaxis=PixelPos, 
                                    pbasis){
  gcvs <- rep(0,length(lambdas))
  for(i in 1:length(lambdas)){ 
    pPar <- fdPar(pbasis,int2Lfd(2),lambdas[i])
    gcvs[i] <- mean(smooth.basis(argvals=Xaxis,y=Curve,fdParobj=pPar)$gcv)
  }
  best <- which.min(gcvs)
  Lambda_forMedian <- lambdas[best]
  pPar <- fdPar(pbasis,int2Lfd(2),lambda=Lambda_forMedian)
  Median_ToReg_fd <- smooth.basis(argvals=Xaxis,y=Curve,fdParobj=pPar)
  return(Median_ToReg_fd)
}
