library(rpart)

trainForest <- function(trainingData, size) {
  forest <- vector("list",forestSize)
  forestAUC <- vector("list",forestSize)
  
  for (i in 1:forestSize){
    cat(sprintf("Training tree %d\n", i))
    #split
    in.training <- createDataPartition(trainingData$objective, p = .6, list = FALSE)
    testing <- trainingData[-in.training, ]
    train <- trainingData[ in.training, ]
    
    #train
    forest[[i]] <- rpart(objective ~ ., data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
  }
}

###########################################

selectTrees <- function(model, modelSelectionData, size){
  selectedForest <- vector("list",size)
  selectedForestIndex <- vector("list",size)
  
  previousAUC <- 0
  
  for(i in 1:size){
    highestAUC <- 0
    selectIndex <- NULL
    
    for(j in 1:size){
      if(is.null(selectedForestIndex[[j]])){
        selectedForest[[i]] <- forest[[j]]
        AUC <- testForestVetoVote(selectedForest, modelSelectionData, i)
        if(AUC > highestAUC){
          highestAUC <- AUC
          selectIndex <- j
        }
        cat(sprintf("Interaction %d tree %d AUC: %f\n", i,j,AUC))
      }
    }
    cat(sprintf("Highest AUC is %f, previous AUC was %f\n", highestAUC,previousAUC))
    if(highestAUC-previousAUC > 0.001){
      previousAUC <- highestAUC
      selectedForest[[i]] <- forest[[selectIndex]]
      selectedForestIndex[[selectIndex]] <- TRUE
    }
    else{
      finalSize <- i-1
      break
    }
  }
  
  finalForest <- vector("list",finalSize)
  
  finalForest[[1:finalSize]] <- selectedForest[[1:finalSize]]
  
  selectedForest
}

testForestVetoVote <- function(model, testingData, size){
  
  solution <- matrix(nrow = nrow(testingData), ncol = size)
  for(i in 1:size){
    prediction <- predict(model[[i]], testingData, type = "class")
    solution[,i] <- prediction
  }
  
  finalsolution <- matrix(nrow = nrow(testingData), ncol = 1)
  for(i in 1:nrow(testingData)){
    finalsolution[i,] <- mean(solution[i,])
  }
  
  solution <- data.frame(objective = testingData$objective, predicted = finalsolution)
  
  solution$predictedAbs <- NA
  solution$predictedAbs[solution$predicted == 1] <- 1
  solution$predictedAbs[solution$predicted > 1] <- 2
  
  truth_table <- table(testingData$objective, solution$predictedAbs)
  if(ncol(truth_table)==2){
    Vp <- truth_table[1,1]
    Fp <- truth_table[1,2]
    Fn <- truth_table[2,1]
    Vn <- truth_table[2,2]
    
    Sen <- Vp/(Vp+Fp)
    Esp <- Vn/(Vn+Fn)
    AUC <- (Esp+Sen)/2
    ACC <- (Vp+Vn)/(Vp+Vn+Fp+Fn)
    truth_table
    AUC
  }
  else{
    AUC <- 0.5
    AUC
  }
}

###########################################

predictForest <- function(model, testingData, size){
  
  solution <- matrix(nrow = nrow(testingData), ncol = size)
  for(i in 1:size){
    prediction <- predict(model[[i]], testingData, type = "class")
    solution[,i] <- prediction
  }
  
  finalsolution <- matrix(nrow = nrow(testingData), ncol = 1)
  for(i in 1:nrow(testingData)){
    finalsolution[i,] <- mean(solution[i,])
  }
  
  solution <- data.frame(predicted = finalsolution)
  
  solution$predictedAbs <- NA
  solution$predictedAbs[solution$predicted == 1] <- 1
  solution$predictedAbs[solution$predicted > 1] <- 2
  
  solution$predictedAbs
}