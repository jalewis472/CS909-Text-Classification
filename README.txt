  .g8"""bgd  .M"""bgd                               
.dP'     `M ,MI    "Y                               
dM'       ` `MMb.      .d*"*bg.  ,pP""Yq.   .d*"*bg.
MM            `YMMNq. 6MP    Mb 6W'    `Wb 6MP    Mb
MM.         .     `MM YMb    MM 8M      M8 YMb    MM
`Mb.     ,' Mb     dM  `MbmmdM9 YA.    ,A9  `MbmmdM9
  `"bmmmd'  P"Ybmmd"        .M'  `Ybmmd9'        .M'
                          .d9                  .d9  
                        m"'                  m"'    
Text-Classification
=========================
CS909 Assignment 8 Week 10 Text Classification, Clustering, and Topic Models


.-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.   
     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.
.-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.     .-"-.   
     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.-"     "-.

----------------------------------------------------------------------
| INSTRUCTIONS TO RUN HERE - GO TO BOTTOM FOR DATA FILE DESCRIPTION. |
----------------------------------------------------------------------


1) DATA-SELECTION AND PREPROCESSING:
------------------------------------

Begin with an fresh session of R. Run lines 1-106 of main.R for a preprocessed version of 
ModApte split for Reuters-21578 dataset (preprocessing steps in comments). Beware long run-
time due to POS-tagging step. Omit if time limited!

NOTE: do not require package "caret" before doing this as it masks the "NLP" function 
"annotate" required for POS tagging.



2) BUILDING MODELS ON TRAINING SET:
-----------------------------------

First run lines 114-135 of main.R to obtain the required variables. Now run lines 137-298 
of main.R to load in the main model building and evaluation function: evalClassifier

      -> Input: training corpus, training class required, desired feature selection function 
                from FSelector package, number of folds, and number of features to evaluate 
                the model for in steps of 1 (from 2 to nMaxFeats).
      -> Output: Accuracy, Precision, Recall, F1 Measure
                   - Reported for each fold.
                   - Macro and micro averages.
                   - Standard deviation of statistic.
          -> These values are also reported for each feature numner from 2 to nMaxFeats, 
             computed for Naive Bayes and SVM classifiers from e1017 package.

This can be run by adjusting line 300 of main.R for your required parameters.

Plots can be generated from this output using the plotting functions in plotting.R.
Output can be tested for significance via functions in ttest.R to determine optimal model.



3) RUNNING OPTIMAL MODEL ON TESTING DATA:
-----------------------------------------

As SVM was best in this study, function classifySVM created to test this as optimal model.
Load in by running lines 5-69 of testingClassifier.R:

      -> Input: training corpus, training class required, desired feature selection function 
                from FSelector package, testing corpus, testing class required, and number 
                of features to evaluate the model for based on outcome of best feature number
                from previous section on training corpus.
      -> Output: Accuracy, Precision, Recall, F1 Measure computed.
                 Confusion matrix for additional data, generated via caret package.
                 List of all features and their importance scores from feature selection 
                 function.
                 List of best performing features for this classification for later use.

Running lines 70-92 of testingClassifier.R for comparisons relevant to accompanied report.
Running lines 70-92 of testingClassifier.R for total list of best performing features.



4) TOPIC MODELS:
----------------

File tpcMods.R contains very similar code to evalClassifier function from main.R, but with 
the feature selection part FSelector functions are replaced by a version with topic models
as LDA with Gibbs sampling method used via the topicmodels package. A combination of feature 
types is also easy to do in this way.



5) CLUSTERING:
--------------

Three methods implemented in file clusteringTests.R: Hierarchical Agglomerative Clustering, 
K-Means, and E-M clusting to fit Gaussian finite mixture model. This is where the best
performing features from application of the optimal model to testing data come in useful, 
and code for each of these is partitioned in an obvious fashion by comments. Plotting 
prototypes are also provided for each to plot using prinicpal components, and also generate
HAC dendrogram.

To evaulate and check correspondence between clusters and Topics tags use evalClust function.
Builds matrix of clusters vs how much of each topic is in them.



6) DATA FILES:
--------------

For raw data files refered to in report, the files are as follows:

  - IG#.txt, where # is 1 to 10, corresponding to most populous classes as in report.
       -> these are raw outputs of classifier and feature selection evaluation for Information 
          Gain.
  - CS#.txt, where # is 1 to 10, corresponding to most populous classes as in report.
       -> these are raw outputs of classifier and feature selection evaluation for Chi-Squared.
       
  - t_tests.txt
       -> raw results of performing t-tests on pairs described in report, to determine SVM>NB.
  
  - bestfeatures.txt
       -> listing of best features for each of the 10 most populous classes, obtained by
          performing the optimal model on each of them - used for clustering.



