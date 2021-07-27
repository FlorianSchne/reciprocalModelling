## Modified version of Hanna Meyer's CAST::aoa function
# https://github.com/HannaMeyer/CAST/blob/master/R/aoa.R
# Meyer, H. and Pebesma, E. (2021), Predicting into unknown space? 
# Estimating the area of applicability of spatial prediction models. 
# Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13650

aoa1=function (newData = NULL,
               train = NULL,
               weight = NULL,
               variables = NULL,
               threshold = 0.95,
               clusterTrain = NULL)
{
  if(is.null(newData)|is.null(train)|is.null(variables)){
    out=NA
  }

  ## 1. Standardize
  trainScaled=scale(train[,variables])
  trainScaled[is.na(trainScaled)]=0
  
  scaleparam=attributes(trainScaled)
  newDataScaled=scale(newData[,variables], 
                   center=scaleparam$`scaled:center`,
                   scale=scaleparam$`scaled:scale`)
  newDataScaled[is.na(newDataScaled)|newDataScaled==Inf]=0
  
  ## 2. Weight
  for(i in 1:nrow(weight)){
    var=weight$name[i]
    value=weight$value[i]
    trainScaled[,var]=trainScaled[,var]*value
    newDataScaled[,var]=newDataScaled[,var]*value
  }
  
  trainScaled[is.na(trainScaled)]=0
  newDataScaled[is.nan(newDataScaled)]=0
  
  ## 3. Distance
  # ... in parallel
  if (getDoParWorkers()==1) {
    if (.Platform$OS.type == "unix") {
      library(doMC)
      try(registerDoMC(cores))
    } else {
      cl = try(makePSOCKcluster(cores))
      try(registerDoParallel(cl))
    }
    mindist = foreach(z=1:nrow(newDataScaled),
                      .combine = 'rbind',
                      .packages = c("FNN")) %dopar% {
                        tmp=knnx.dist(t(matrix(newDataScaled[z,])), 
                                      trainScaled, k=1)
                        return(min(tmp))
                      }
    try(registerDoSEQ(),silent=T)
    try(stopCluster(cl),silent=T)
    
    mindist=unlist(mindist,use.names=F)
    
    } else {
      
      mindist=data.frame()
      for(z in 1:nrow(newDataScaled)){
        tmp=FNN::knnx.dist(t(matrix(newDataScaled[z,])), trainScaled, k=1)
        mindist=rbind(mindist,min(tmp))
      }
  }
  print("mindist done")
  
  trainDist <- as.matrix(dist(trainScaled))
  diag(trainDist) <- NA
  print("trainDist done")

  # Account for clusters (only really relevant for target oriented CV)
  if(!is.null(clusterTrain)){
    for (i in 1:nrow(trainDist)) {
      trainDist[i, clusterTrain == clusterTrain[i]] <- NA
    }
  }

  ## Dissimilarity index (DI)
  trainDist_mean <- apply(trainDist, 1, FUN = function(x) {
    mean(x, na.rm = T)
  })
  trainDist_avrgmean <- mean(trainDist_mean)
  DI <- mindist/trainDist_avrgmean
  
  ## Area of applicability (AOA)
  trainDist_min <- apply(trainDist, 1, FUN = function(x) {
    min(x, na.rm = T)
  })
  thres <- quantile(trainDist_min/trainDist_avrgmean,
                    probs = threshold, 
                    na.rm = TRUE)
  AOA <- rep(1, length(DI))
  AOA[DI > thres] <- 0
  
  out=tibble(DI=as.numeric(unlist(DI)),
             AOA=AOA)
  return(out)
  }

## Function to process EUROSTAT data
dasMittel=function (
  newdata,
  groups = NULL)
{
  DT=newdata[, keyby = c("Point_ID",
                         "nutLevel",
                         groups),
             .(values=mean(values,na.rm=T))]
  
  # Keep only maximum nutLevel
  vars=names(DT)[!(names(DT)%in%c("nutLevel","values"))]
  DT=DT[DT[, .I[which.max(nutLevel)], key = vars]$V1]
  print(paste0("Mean NUTS level: ",round(mean(DT$nutLevel),2)))
  
  DT=DT[,nutLevel:=NULL]
  return(DT)
}



