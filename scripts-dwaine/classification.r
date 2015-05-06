

library(class)
library(SDMTools)

caretrf <- FALSE
rf <- TRUE
nnet <- TRUE
nb <- FALSE
knn <- TRUE

iteration = c()
if(rf){ 
  rf.accuracy = c()
  rf.fprate = c()
}
if(nnet){ 
  nnet.accuracy = c()
  nnet.fprate = c()
}
if(knn){ 
  knn.accuracy = c()
  knn.fprate = c()
}

#Cross ten validation loop
for (i in 0:9 ) {
  
  cat("Starting iteration: ",i,"\n")
  
  iteration <- c(iteration,i)
  
  #read in dgadata, factorize as necessary, then sample.
  trainfilename = paste(c('crosstenvalidationdatasets/DGA_data_train_',i,'_ngram.csv'),collapse='')
  train <- read.csv(trainfilename, stringsAsFactors=FALSE)
  train <- train[1:300000,]
  train <- train[,c("isDGA","sldlen","dwordratio","bigramisDGAratio","bigramnonDGAratio","trigramisDGAratio","trigramnonDGAratio","fourgramisDGAratio","fourgramnonDGAratio","fivegramisDGAratio","fivegramnonDGAratio")]
  train$isDGA <- factor(train$isDGA)
  #cl <- train$isDGA
  
  testfilename = paste(c('crosstenvalidationdatasets/DGA_data_test_',i,'_ngram.csv'),collapse='')
  test <- read.csv(testfilename, stringsAsFactors=FALSE)
  test <- test[1:50000,]
  test <- test[,c("isDGA","sldlen","dwordratio","bigramisDGAratio","bigramnonDGAratio","trigramisDGAratio","trigramnonDGAratio","fourgramisDGAratio","fourgramnonDGAratio","fivegramisDGAratio","fivegramnonDGAratio")]
  test$isDGA <- factor(test$isDGA)
  #truth <- test$isDGA
  
  ptm <- proc.time()
  
  
  
  if(caretrf){
    cat("\ncaret Random Forest\n")
    library(caret)
    model <- train(isDGA ~ ., train, method="rf", tuneGrid = data.frame(.mtry = 4), trControl=trainControl(method="none"), preProcess = c("center","scale"))
    predicted <- predict(model, test, type="raw")
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    cat("False positive rate: ", fp, "\n")
    print(accuracy(test$isDGA, predicted, threshold = 0.5))
    rm(model,predicted,cm,fp)
    # train 200k, test 20k
    #  threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
    #1       0.5 0.8483407     0.2492938   0.7507062   0.9459751       0.9045 0.7093646
    # fp   [1] 0.04255
  }
  
  
  
  if(rf){
    cat("\nRandom Forest\n")
    library(randomForest)
    model <- randomForest(isDGA ~ ., train, mtry=4, ntree=200)
    predicted <- predict(model, test, type="class")
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    #cat("False positive rate: ", fp, "\n"
    acc<-accuracy(test$isDGA, predicted, threshold = 0.5)
    rf.accuracy <- c(rf.accuracy,acc$prop.correct)
    rf.fprate <- c(rf.fprate,fp)
    #print(accuracy(test$isDGA, predicted, threshold = 0.5))
    rm(model,predicted,cm,fp,acc)
    # train 200k, test 20k
    #  threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
    #1       0.5 0.8483407     0.2492938   0.7507062   0.9459751       0.9045 0.7093646
    # fp   [1] 0.04255
  }
  
  
  
  if(nnet){
    cat("\nNeural Network\n")
    # 7 nodes
    #library("neuralnet")
    #model <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
    #print(model)
    library(nnet)
    #train <- train[,c("isDGA","sldlen","dwordratio","bigramisDGAratio","bigramnonDGAratio","trigramisDGAratio","trigramnonDGAratio","fourgramisDGAratio","fourgramnonDGAratio","fivegramisDGAratio","fivegramnonDGAratio")]
    #test <- test[,c("isDGA","sldlen","dwordratio","bigramisDGAratio","bigramnonDGAratio","trigramisDGAratio","trigramnonDGAratio","fourgramisDGAratio","fourgramnonDGAratio","fivegramisDGAratio","fivegramnonDGAratio")]
    model = nnet(isDGA~., train, size = 7, maxit=10000, decay = .04)
    predicted <- predict(model, test,type="class")
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    #cat("False positive rate: ", fp, "\n")
    acc<-accuracy(test$isDGA, predicted, threshold = 0.5)
    nnet.accuracy <- c(nnet.accuracy,acc$prop.correct)
    nnet.fprate <- c(nnet.fprate,fp)
    rm(model,predicted,cm,fp,acc)
    #  threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
    #1       0.5 0.8773217      0.218972    0.781028   0.9736154    0.9332852 0.789271
    #> fp 0.02085937
  }
  
  
  if(nb){
    cat("Naive Bayes\n")
    library(e1071)
    model <- naiveBayes(subset(train, select = -c(isDGA)), train$isDGA)     
    predicted <- predict(model, test)   
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    cat("False positive rate: ", fp, "\n")
    print(accuracy(test$isDGA, predicted, threshold = 0.5))
    rm(model,predicted,cm,fp,acc)
    # threshold       AUC omission.rate sensitivity specificity prop.correct     Kappa
    # 1       0.5 0.8689666     0.1724036   0.8275964   0.9103368    0.8930099 0.6954735
    # fp:  0.07088666
  }  
    
  
  ###
  # kNN 
  # k = 13 neighbors
  ###
  if(knn){
    cat("\nk Nearest Neighbor (kNN)\n")
    fit <- knn(subset(train, select = -c(isDGA)), subset(test, select = -c(isDGA)), train$isDGA, 13, 0, FALSE, use.all = FALSE)  
    cm <- table(Truth = test$isDGA, Predicted = fit)
    fp <- cm[1,2]/length(test$isDGA)
    acc <- accuracy(test$isDGA, fit, threshold = 0.5)
    knn.accuracy <- c(knn.accuracy,acc$prop.correct)
    knn.fprate <- c(knn.fprate,fp)
    rm(fit,predicted,cm,fp,acc)
  }
  
  
}  #end cross ten



if(rf){ 
  cat("RF accuracy mean and fprate mean: ",mean(rf.accuracy),",",mean(rf.fprate),"\n")
}
if(nnet){ 
  cat("NNET accuracy mean and fprate mean: ",mean(nnet.accuracy),",",mean(nnet.fprate),"\n")
}
if(knn){ 
  cat("kNN 13 neighbors accuracy mean and fprate mean: ",mean(knn.accuracy),",",mean(knn.fprate),"\n")
}


proc.time() - ptm

rm(test,train,ptm,rf,nnet)
rm(i,iteration,rf.accuracy,rf.fprate,nnet.accuracy,nnet.fprate,knn.accuracy,knn.fprate)
rm(testfilename,trainfilename)
rm(caretrf,knn,nb,rf,nnet)
