#################################################################

# This is for applying svm to testing data, since svm beat nb in all t tests

# Function for SVM classification on testing data for given parameters and compute performance
classifySVM <- function(train_corpus,train_class,test_corpus,test_class,fSelectFunc,nFeats){
  # Create Document-Term Matrix for documents in training set.
  train_dtm <- DocumentTermMatrix(train_corpus,control=list(weighting=weightTfIdf))
  
  # Create Document-Term Matrix for documents in testing set.
  test_dtm <- DocumentTermMatrix(test_corpus,control=list(weighting=weightTfIdf))
  
  # First dimensionality reduction based on ranking via TfIdf - takes 'top' 5% of term-summed TdIdf
  # some assumptions naturally but a good start, and neccessary for FSelector to handle in RAM! 
  dtm_col_sums <- as.vector(rollup(train_dtm, 1, na.rm=TRUE, FUN = sum))
  reduced_train_dtm <- train_dtm[,sort(match(sort(dtm_col_sums,decreasing=TRUE)
                                             [1:floor(0.05*length(train_dtm$dimnames$Terms))],dtm_col_sums))]
  
  # Turn into form that FSelector package works well with
  d1 <- as.data.frame(as.matrix(reduced_train_dtm))
  d1$class <- train_class
  
  # Compute feature relevance measure for each term
  spisok <- fSelectFunc(class~.,d1)
  
  # (Sort? and) Select the most highly relevant features (in some defined way...)
  #sorted <- spisok[order(spisok[,"attr_importance"]), , drop=FALSE]
  subset <- cutoff.k(spisok,nFeats)
  
  # Find features not in testing dtm - v. important to do first for indexing out others!
  missingFeatureIndicies <- which(is.na(match(subset,Terms(test_dtm)))==1)
  if(length(missingFeatureIndicies) >0){
    subset <- subset[-missingFeatureIndicies]
  }
  
  # SVM must train on (potentially) updated subset, so the space dimensions are consistent
  # with the testing set, and the prediction function can work.
  svm_model <- svm(d1[,subset],as.factor(d1[,"class"]))
  
  # Subset the terms to look for in the testing set
  featureSelectedTestDtm <- as.data.frame(as.matrix(test_dtm[,subset]))
  
  # Predict if the documents in the test set contain the topic or not
  svm_results <- predict(svm_model,featureSelectedTestDtm)
  
  # Compute classification performance statistics      
  svm_CM <- confusionMatrix(svm_results,test_class,positive="1")
  svm_accuracy <- as.numeric(svm_CM$overall["Accuracy"])
  svm_precision <- as.numeric(svm_CM$byClass["Pos Pred Value"])
  svm_recall <- as.numeric(svm_CM$byClass["Sensitivity"])
  svm_f1 <- as.numeric((2*svm_recall*svm_precision)/(svm_recall+svm_precision))
  
  mainOutput <- list()
  s_v_m <- list()
  
  s_v_m[[paste("Accuracy")]] <- svm_accuracy
  s_v_m[[paste("Precision")]] <- svm_precision
  s_v_m[[paste("Recall")]] <- svm_recall
  s_v_m[[paste("F1 Measure")]] <- svm_f1
  s_v_m[[paste("Confusion Matrix")]] <- svm_CM

  mainOutput[[paste("Best performing features")]] <- subset
  mainOutput[[paste("SVM")]] <- s_v_m
  mainOutput[[paste("Full feature list and ranking")]] <- spisok[order(spisok[,"attr_importance"])
                                                                 , , drop=FALSE]
  
  return(mainOutput)
}

ptm <- proc.time()
testing1=classifySVM(trainingCorpus,trainingClasses[,classNames[1]]
                     ,testingCorpus,testingClasses[,classNames[1]],chi.squared,50)
testing2=classifySVM(trainingCorpus,trainingClasses[,classNames[2]]
                     ,testingCorpus,testingClasses[,classNames[2]],information.gain,49)
testing3=classifySVM(trainingCorpus,trainingClasses[,classNames[3]]
                     ,testingCorpus,testingClasses[,classNames[3]],information.gain,34)
testing4=classifySVM(trainingCorpus,trainingClasses[,classNames[4]]
                     ,testingCorpus,testingClasses[,classNames[4]],information.gain,11)
testing5=classifySVM(trainingCorpus,trainingClasses[,classNames[5]]
                     ,testingCorpus,testingClasses[,classNames[5]],chi.squared,18)
ptm <- proc.time()
testing6=classifySVM(trainingCorpus,trainingClasses[,classNames[6]]
                     ,testingCorpus,testingClasses[,classNames[6]],information.gain,23)
testing7=classifySVM(trainingCorpus,trainingClasses[,classNames[7]]
                     ,testingCorpus,testingClasses[,classNames[7]],information.gain,28)
testing8=classifySVM(trainingCorpus,trainingClasses[,classNames[8]]
                     ,testingCorpus,testingClasses[,classNames[8]],information.gain,10)
testing9=classifySVM(trainingCorpus,trainingClasses[,classNames[9]]
                     ,testingCorpus,testingClasses[,classNames[9]],chi.squared,2)
testing10=classifySVM(trainingCorpus,trainingClasses[,classNames[10]]
                     ,testingCorpus,testingClasses[,classNames[10]],chi.squared,5)
proc.time() - ptm

# Combine the best performing features resulting from applying the optimal model for each class
bestPerfFeats <- c(testing1$'Best performing features',testing2$'Best performing features',
                   testing3$'Best performing features',testing4$'Best performing features',
                   testing5$'Best performing features',testing6$'Best performing features',
                   testing7$'Best performing features',testing8$'Best performing features',
                   testing9$'Best performing features',testing10$'Best performing features')
bestPerfFeats <- unique(bestPerfFeats)
