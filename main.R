#################################################################
#################### EX8 - TEXT CLASSIFICATION ##################

require(tm)
require(tm.corpus.Reuters21578)
require(slam)
require(NLP)
require(openNLP)
require(openNLPdata)
require(rJava)
require(SnowballC)
require(modeltools)
require(topicmodels)
require(e1071)
require(caret)
require(FSelector)


#################################################################
### -> SPLIT DATA INTO TRAINING AND TESTING (IGNORE IF NO TOPIC)

data(Reuters21578)
rt <- Reuters21578

# ModApte split w/ zero Topics screened out (see readme section VIII.B.)
trainingIndicies=NULL
testingIndicies=NULL
for (i in 1:length(rt)){
  if(tm::meta(rt[[i]],tag="TOPICS")=="YES"){  #indexers suggest topics list non-empty*
    if(length(tm::meta(rt[[i]],tag="Topics")) >0){    #topics list non-empty (*readme)
      if(tm::meta(rt[[i]],tag="LEWISSPLIT")=="TRAIN"){    #tagged as training data
        trainingIndicies=c(trainingIndicies,i)
      }
      if(tm::meta(rt[[i]],tag="LEWISSPLIT")=="TEST"){     #tagged as testing data
        testingIndicies=c(testingIndicies,i)
      }
    }
  }
}

# There are some empty documents; as suggested we will not use these.
# Finds the ID numbers of empty documents
dtm <- DocumentTermMatrix(rt)
dtmRowSums <- as.vector(rollup(dtm, 2, na.rm=TRUE, FUN = sum))
emptyDocIndicies <- which(dtmRowSums==0)

# Subsets the empty documents out
trainingIndicies=trainingIndicies[-sort(unique(match(emptyDocIndicies,trainingIndicies)))]
testingIndicies=testingIndicies[-sort(unique(match(emptyDocIndicies,testingIndicies)))]

# Builds training and testing corpus'
trainingCorpus <- rt[trainingIndicies]
testingCorpus <- rt[testingIndicies]

# Builds combined training and testing corpus'
combinedIndices <- c(trainingIndicies,testingIndicies)
combinedCorpus <- rt[combinedIndices]
combinedCorpus<-preprocessCorpus(combinedCorpus)


#################################################################
### -> PREPROCESSING

# Generalised preprocessing function with the following steps
preprocessCorpus <- function(corpus) {
  # Step 1: Remove numeric characters [0-9]
  corpus <- tm_map(corpus, removeNumbers)
  
  # Step 2: Make all letters lowercase
  corpus <- tm_map(corpus, tolower)
  
  # Step 3: Remove stopwords (EN) from SMART (coincides with MC_tk list)
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  
  # Step 4: Replace punctuation with space (to handle intraword punct.)
  removePunctuationCustom <- function(x){    
    x <- gsub("[[:punct:]]+", " ", x)
  }
  corpus <- tm_map(corpus, removePunctuationCustom)
  
  # Step 5: Remove excess whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Step 6: Stemming and Part of Speech (PoS) tagging
  posTagStem <- function(x, language = Language(x)) {
    mainFunc <- function(x) {
      z1 <- as.String(x)
      
      a2 <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
      a3 <- annotate(x, Maxent_POS_Tag_Annotator(), a2) 
      gc()   #R garbage collection to fix Maxent_..._Annotator() memory problems
      
      a3w <- subset(a3,type=="word")
      tags <- sapply(a3w$features,`[[`,"POS")
      a3wstem <- stemDocument(z1[a3w])   #using SnowballC
      
      paste(sprintf("%s/%s",a3wstem,tags), collapse=" ")
    }
    
    s <- unlist(lapply(x, mainFunc))
    Content(x) <- if (is.character(s)) s else ""
    x
  }
  corpus <- tm_map(corpus, posTagStem)
}

# Preprocess training and testing corpus' as above
trainingCorpus <- preprocessCorpus(trainingCorpus)
testingCorpus <- preprocessCorpus(testingCorpus)
combinedCorpus <- preprocessCorpus(combinedCorpus)


#################################################################
### -> CROSSVALIDATION ON TRAINING CORPUS
### Aim is to do xval w diff classifiers to find best performing
### classifiers and also feature selection methods.
### After this we can apply the best to testing data and cluster.

# Build class vector for training/testing data for 10 most populous classes
classNames=c("earn","acq","money-fx","grain","crude",
             "trade","interest","ship","wheat","corn")

trainingClasses=matrix(data=0,nrow=length(trainingCorpus),ncol=length(classNames),
                       dimnames=list(c(1:length(trainingCorpus)),classNames))
for (i in 1:length(trainingCorpus)){
  trainingClasses[i,match(tm::meta(trainingCorpus[[i]],tag="Topics"),classNames)]=1
}

testingClasses=matrix(data=0,nrow=length(testingCorpus),ncol=length(classNames),
                      dimnames=list(c(1:length(testingCorpus)),classNames))
for (i in 1:length(testingCorpus)){
  testingClasses[i,match(tm::meta(testingCorpus[[i]],tag="Topics"),classNames)]=1
}

# Function to perform x-val for given classifier and given feature selection method and class
evalClassifier <- function(train_corpus,train_class,fSelectFunc,nFolds,nMaxFeats){
  # Function to compute matrix column standard deviations
  colSd <- function(x, na.rm=TRUE) {
    if (na.rm) {
      n <- colSums(!is.na(x))
    } else {
      n <- nrow(x)
    }
    colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
    return(sqrt(colVar * n/(n-1)))
  }
  
  nb_accuracy <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  nb_precision <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  nb_recall <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  nb_f1 <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  
  svm_accuracy <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  svm_precision <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  svm_recall <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  svm_f1 <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  
  nb_TP <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  nb_TPplusFP <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  nb_TPplusFN <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  
  svm_TP <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  svm_TPplusFP <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  svm_TPplusFN <- matrix(data=0,nrow=nFolds,ncol=nMaxFeats,dimnames=list(1:nFolds,1:nMaxFeats))
  
  numDocs <- length(train_corpus)
  reordered <- sample(numDocs)
  folds <- cut(1:numDocs,breaks=nFolds,labels=F)
  
  # Looping over number of folds
  for (i in 1:nFolds){
    # Output current progress
    print(i)
    flush.console()
    
    # Create Document-Term Matrix for documents not in this fold - training set.
    train_dtm <- DocumentTermMatrix(train_corpus[reordered[folds!=i]]
                                    ,control=list(weighting=weightTfIdf))
    
    # Create Document-Term Matrix for documents in this fold only - testing set.
    test_dtm <- DocumentTermMatrix(train_corpus[reordered[folds==i]]
                                   ,control=list(weighting=weightTfIdf))
    
    # Simple frequency based dimensionality reduction ... terms must be in > ~1% of documents
    # FSelector needs dataframe input ... would kill ram if dtm not weakly reduced first!
    #reduced_train_dtm <- train_dtm[,findFreqTerms(train_dtm,
    #                                              ceiling(0.01*dim(train_dtm)[1]))]
    
    # First dimensionality reduction based on ranking via TfIdf - takes 'top' 5% of term-summed TdIdf
    # some assumptions naturally but a good start, and neccessary for FSelector to handle in RAM! 
    dtm_col_sums <- as.vector(rollup(train_dtm, 1, na.rm=TRUE, FUN = sum))
    reduced_train_dtm <- train_dtm[,sort(match(sort(dtm_col_sums,decreasing=TRUE)
                              [1:floor(0.05*length(train_dtm$dimnames$Terms))],dtm_col_sums))]
    
    # Turn into form that FSelector package works well with
    d1 <- as.data.frame(as.matrix(reduced_train_dtm))
    d1$class <- train_class[reordered[folds!=i]]
    
    # Compute feature relevance measure for each term
    spisok <- fSelectFunc(class~.,d1)
    
    for (j in 2:nMaxFeats){
      # (Sort? and) Select the most highly relevant features (in some defined way...)
      #sorted <- spisok[order(spisok[,"attr_importance"]), , drop=FALSE]
      subset <- cutoff.k(spisok,j)
            
      # Train model for class j using train_dtm and jth column of train_class
      nb_model <- naiveBayes(d1[,subset],as.factor(d1[,"class"]))
      
      # Find features not in testing dtm - v. important to do first for indexing out others!
      missingFeatureIndicies <- which(is.na(match(subset,Terms(test_dtm)))==1)
      if(length(missingFeatureIndicies) >0){
        subset <- subset[-missingFeatureIndicies]
      }
      
      # SVM must train on (potentially) updated subset, so the space dimensions are consistent
      # with the testing set, and the prediction function can work - no such problem for NB.
      svm_model <- svm(d1[,subset],as.factor(d1[,"class"]))
        
      # Subset the terms to look for in the testing set
      featureSelectedTestDtm <- as.data.frame(as.matrix(test_dtm[,subset]))
      
      # Predict if the documents in the test set contain the ith topic or not
      nb_results <- predict(nb_model,featureSelectedTestDtm)
      svm_results <- predict(svm_model,featureSelectedTestDtm)
      
      # Compute classification performance statistics
      nb_CM <- confusionMatrix(nb_results,train_class[reordered[folds==i]],positive="1")
      nb_accuracy[i,j] <- as.numeric(nb_CM$overall["Accuracy"])
      nb_precision[i,j] <- as.numeric(nb_CM$byClass["Pos Pred Value"])
      nb_recall[i,j] <- as.numeric(nb_CM$byClass["Sensitivity"])
      nb_f1[i,j] <- as.numeric((2*nb_recall[i,j]*nb_precision[i,j])/
                                 (nb_recall[i,j]+nb_precision[i,j]))
      nb_TP[i,j] <- nb_CM$table["1","1"]
      nb_TPplusFP[i,j] <- nb_TP[i,j] + nb_CM$table["1","0"]
      nb_TPplusFN[i,j] <- nb_TP[i,j] + nb_CM$table["0","1"]
      
      svm_CM <- confusionMatrix(svm_results,train_class[reordered[folds==i]],positive="1")
      svm_accuracy[i,j] <- as.numeric(svm_CM$overall["Accuracy"])
      svm_precision[i,j] <- as.numeric(svm_CM$byClass["Pos Pred Value"])
      svm_recall[i,j] <- as.numeric(svm_CM$byClass["Sensitivity"])
      svm_f1[i,j] <- as.numeric((2*svm_recall[i,j]*svm_precision[i,j])/
                                  (svm_recall[i,j]+svm_precision[i,j]))
      svm_TP[i,j] <- svm_CM$table["1","1"]
      svm_TPplusFP[i,j] <- svm_TP[i,j] + svm_CM$table["1","0"]
      svm_TPplusFN[i,j] <- svm_TP[i,j] + svm_CM$table["0","1"]
    }
  }
  
  mainOutput <- list()
  naive_bayes <- list()
  s_v_m <- list()
  
  naive_bayes[[paste("Accuracy per Fold")]] <- nb_accuracy
  naive_bayes[[paste("Mean Accuracy")]] <- colMeans(nb_accuracy)
  naive_bayes[[paste("St Dev Accuracy")]] <- colSd(nb_accuracy)
  naive_bayes[[paste("Precisions per Fold")]] <- nb_precision
  naive_bayes[[paste("Mean Precisions")]] <- colMeans(nb_precision)
  naive_bayes[[paste("St Dev Precisions")]] <- colSd(nb_precision)
  nb_micro_precision <- colSums(nb_TP)/colSums(nb_TPplusFP)
  naive_bayes[[paste("Micro Ave Precisions")]] <- nb_micro_precision
  naive_bayes[[paste("Recalls per Fold")]] <- nb_recall
  naive_bayes[[paste("Mean Recalls")]] <- colMeans(nb_recall)
  naive_bayes[[paste("St Dev Recalls")]] <- colSd(nb_recall)
  nb_micro_recall <- colSums(nb_TP)/colSums(nb_TPplusFN)
  naive_bayes[[paste("Micro Ave Recalls")]] <- nb_micro_recall
  naive_bayes[[paste("F1 Measure")]] <- nb_f1
  naive_bayes[[paste("Mean F1 Measure")]] <- colMeans(nb_f1)
  naive_bayes[[paste("St Dev F1 Measure")]] <- colSd(nb_f1)
  naive_bayes[[paste("Micro Ave F1 Measure")]] <- (2*nb_micro_precision*nb_micro_recall)/
                                                    (nb_micro_precision+nb_micro_recall)
  
  s_v_m[[paste("Accuracy per Fold")]] <- svm_accuracy
  s_v_m[[paste("Mean Accuracy")]] <- colMeans(svm_accuracy)
  s_v_m[[paste("St Dev Accuracy")]] <- colSd(svm_accuracy)
  s_v_m[[paste("Precisions per Fold")]] <- svm_precision
  s_v_m[[paste("Mean Precisions")]] <- colMeans(svm_precision)
  s_v_m[[paste("St Dev Precisions")]] <- colSd(svm_precision)
  svm_micro_precision <- colSums(svm_TP)/colSums(svm_TPplusFP)
  s_v_m[[paste("Micro Ave Precisions")]] <- svm_micro_precision
  s_v_m[[paste("Recalls per Fold")]] <- svm_recall
  s_v_m[[paste("Mean Recalls")]] <- colMeans(svm_recall)
  s_v_m[[paste("St Dev Recalls")]] <- colSd(svm_recall)
  svm_micro_recall <- colSums(svm_TP)/colSums(svm_TPplusFN)
  s_v_m[[paste("Micro Ave Recalls")]] <- svm_micro_recall
  s_v_m[[paste("F1 Measure")]] <- svm_f1
  s_v_m[[paste("Mean F1 Measure")]] <- colMeans(svm_f1)
  s_v_m[[paste("St Dev F1 Measure")]] <- colSd(svm_f1)
  s_v_m[[paste("Micro Ave F1 Measure")]] <- (2*svm_micro_precision*svm_micro_recall)/
                                              (svm_micro_precision+svm_micro_recall)
  
  mainOutput[[paste("NAIVE BAYES")]] <- naive_bayes
  mainOutput[[paste("SVM")]] <- s_v_m
  
  return(mainOutput)
}

ptm <- proc.time()
x=evalClassifier(trainingCorpus,trainingClasses[,classNames[1]],chi.squared,10,50)
proc.time() - ptm
