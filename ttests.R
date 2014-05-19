# Tests if first arguement is significantly (to confidence level) higher than second (ie SVM > NB ?)

t.test(chisq_1$"SVM"$"Accuracy"[,50],
       chisq_1$"NAIVE BAYES"$"Accuracy"[,49],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_2$"SVM"$"Accuracy"[,49],
       chisq_2$"NAIVE BAYES"$"Accuracy"[,49],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_3$"SVM"$"Accuracy"[,34],
       ig_3$"NAIVE BAYES"$"Accuracy"[,9],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_4$"SVM"$"Accuracy"[,11],
       chisq_4$"NAIVE BAYES"$"Accuracy"[,2],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(chisq_5$"SVM"$"Accuracy"[,18],
       chisq_5$"NAIVE BAYES"$"Accuracy"[,4],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_6$"SVM"$"Accuracy"[,23],
       ig_6$"NAIVE BAYES"$"Accuracy"[,4],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_7$"SVM"$"Accuracy"[,28],
       ig_7$"NAIVE BAYES"$"Accuracy"[,12],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(ig_8$"SVM"$"Accuracy"[,10],
       chisq_8$"NAIVE BAYES"$"Accuracy"[,5],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(chisq_9$"SVM"$"Accuracy"[,2],
       ig_9$"NAIVE BAYES"$"Accuracy"[,2],
       alternative="greater",paired=TRUE,conf.level=0.95)
t.test(chisq_10$"SVM"$"Accuracy"[,5],
       ig_10$"NAIVE BAYES"$"Accuracy"[,2],
       alternative="greater",paired=TRUE,conf.level=0.95)
