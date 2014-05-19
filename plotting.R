## PLOTTING

plot(1:50, chisq_1$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.95))
lines(1:50, ig_1$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_1$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_1$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("earn")),bty="n")

plot(1:50, chisq_2$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.9))
lines(1:50, ig_2$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_2$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_2$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("acq")),bty="n")

plot(1:50, chisq_3$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.9))
lines(1:50, ig_3$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_3$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_3$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("money-fx")),bty="n")

plot(1:50, chisq_4$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.9))
lines(1:50, ig_4$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_4$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_4$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("grain")),bty="n")

plot(1:50, chisq_5$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.8))
lines(1:50, ig_5$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_5$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_5$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("crude")),bty="n")

plot(1:50, chisq_6$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.8))
lines(1:50, ig_6$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_6$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_6$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("trade")),bty="n")

plot(1:50, chisq_7$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.8))
lines(1:50, ig_7$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_7$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_7$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("interest")),bty="n")

plot(1:50, chisq_8$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.8))
lines(1:50, ig_8$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_8$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_8$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("topright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("ship")),bty="n")

plot(1:50, chisq_9$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.9))
lines(1:50, ig_9$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_9$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_9$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("right",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("wheat")),bty="n")

plot(1:50, chisq_10$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean F1 Score",xlim=c(1,50),ylim=c(0,0.9))
lines(1:50, ig_10$"NAIVE BAYES"$"Mean F1 Measure",type="o",pch=1,col="blue")
lines(1:50, chisq_10$"SVM"$"Mean F1 Measure",type="o",pch=4,col="red")
lines(1:50, ig_10$"SVM"$"Mean F1 Measure",type="o",pch=1,col="red")
legend("right",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("corn")),bty="n")

###

plot(1:50, chisq_1$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,0.95))
lines(1:50, ig_1$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_1$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_1$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("earn")),bty="n")

plot(1:50, chisq_2$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,0.95))
lines(1:50, ig_2$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_2$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_2$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("acq")),bty="n")

plot(1:50, chisq_3$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_3$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_3$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_3$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("money-fx")),bty="n")

plot(1:50, chisq_4$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_4$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_4$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_4$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("grain")),bty="n")

plot(1:50, chisq_5$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_5$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_5$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_5$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("crude")),bty="n")

plot(1:50, chisq_6$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_6$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_6$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_6$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("trade")),bty="n")

plot(1:50, chisq_7$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_7$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_7$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_7$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("interest")),bty="n")

plot(1:50, chisq_8$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_8$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_8$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_8$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottomright",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("ship")),bty="n")

plot(1:50, chisq_9$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_9$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_9$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_9$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("bottom",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("wheat")),bty="n")

plot(1:50, chisq_10$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=4,col="blue",
     xlab="Number of Features", ylab="Mean Accuracy",xlim=c(1,50),ylim=c(0,1))
lines(1:50, ig_10$"NAIVE BAYES"$"Mean Accuracy",type="o",pch=1,col="blue")
lines(1:50, chisq_10$"SVM"$"Mean Accuracy",type="o",pch=4,col="red")
lines(1:50, ig_10$"SVM"$"Mean Accuracy",type="o",pch=1,col="red")
legend("right",inset=.05,c("NB CS","NB IG","SVM CS","SVM IG"),
       col=c("blue","blue","red","red"),pch=c(4,1,4,1), lty=1,
       title=expression(italic("corn")),bty="n")
