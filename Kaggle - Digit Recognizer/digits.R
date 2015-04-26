setwd("D:/GitHub/Repo_R/Kaggle - Digit Recognizer/")

install.packages("darch")
library(darch)

inputs <- matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
outputs <- matrix(c(0,1,1,0),nrow=4)

# Generating the darch
darch <- newDArch(c(2,4,1),batchSize=2)

# Pre-Train the darch
darch <- preTrainDArch(darch,inputs,maxEpoch=1000)

layers <- getLayers(darch)
for(i in length(layers):1){
  layers[[i]][[2]] <- sigmoidUnitDerivative
}

setLayers(darch) <- layers
rm(layers)

# Setting and running the Fine-Tune function
setFineTuneFunction(darch) <- backpropagation
darch <- fineTuneDArch(darch,inputs,outputs,maxEpoch=1000)

# Running the darch
darch <- darch <- getExecuteFunction(darch)(darch,inputs)
outputs <- getExecOutputs(darch)
cat(outputs[[length(outputs)]])
