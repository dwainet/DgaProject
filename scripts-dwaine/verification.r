

library(class)
library(SDMTools)

dt <- FALSE
gbm <- FALSE
nnet <- FALSE
rf <- TRUE

if(dt){ 
  dt.accuracy = c()
  dt.fprate = c()
}
if(gbm){ 
  gbm.accuracy = c()
  gbm.fprate = c()
}
if(nnet){ 
  nnet.accuracy = c()
  nnet.fprate = c()
}
if(rf){ 
  rf.accuracy = c()
  rf.fprate = c()
}

fulldatasetfilename = 'james_plus_5_linfea_enchant.csv'
fulldatasetasread <- read.csv(fulldatasetfilename, stringsAsFactors=FALSE)
rm (fulldatasetfilename)

fulldataset <- fulldatasetasread
#fulldataset <- fulldatasetasread[1:100000,]
fulldataset <- fulldataset[sample(nrow(fulldataset)),]
fulldataset$isDGA <- ifelse(fulldataset$class == 'DGA', 1, 0)
fulldataset <- fulldataset[,c("isDGA","X2nd_domain_2gram_nor_score","X2nd_domain_3gram_nor_score","domain_meaningful_score","pairwise_score","subdomain_length","domain_word_brokendown_count","dot_count","subdomain_entropy","domain_length","suffix_length","domain_entropy","url_length","url_entropy")]
fulldataset$isDGA <- factor(fulldataset$isDGA)

#Create 10 equally size folds
folds <- cut(seq(1,nrow(fulldataset)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){

  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- fulldataset[testIndexes, ]
  train <- fulldataset[-testIndexes, ]
  
  cat("Starting iteration: ",i,"\n")

  
  if(dt){
    cat("\nDecision Tree\n")
    library(rpart)
    model <- rpart(isDGA ~ ., data=train, method="class")
    predicted <- predict(model, test ,type="class")
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    acc<-accuracy(test$isDGA, predicted, threshold = 0.5)
    dt.accuracy <- c(dt.accuracy,acc$prop.correct)
    dt.fprate <- c(dt.fprate,fp)
    rm(model,predicted,cm,fp,acc)
  }
  
 
  if(gbm){
    #install.packages("gbm")
    library(gbm)
    cat("\nGeneralized Boosted Modeling a.k.a. Boosted Decision Tree?\n")
    #model = gbm(isDGA~., data=train, n.trees = 200, distribution="adaboost", train.fraction = .1)  
    # convert to numeric not factor: as.numeric(levels(train$isDGA))[train$isDGA]
    #model = gbm(as.numeric(levels(train$isDGA))[train$isDGA]~., data=train, n.trees = 200, distribution="bernoulli", train.fraction = .1, verbose = TRUE)
    model <- gbm.fit(x = train[,-(names(train) %in% c("isDGA"))], y = as.numeric(levels(train$isDGA))[train$isDGA], n.trees = 10000, distribution="bernoulli", verbose = TRUE, nTrain = 6000 )   
    summary(model)
    predicted <- predict.gbm(model, test, n.trees=10000, type="response")
    #plogis(predicted)
    #gbm.perf(model)
    #plot(model,i=3,type="response")
    #gbm_predicted<-plogis(2*predict.gbm(model, test, n.trees=2000))
    cm <- table(Truth = test$isDGA, Predicted = round(predicted))
    fp <- cm[1,2]/length(test$isDGA)
    #cat("False positive rate: ", fp, "\n")
    acc<-accuracy(test$isDGA, predicted, threshold = 0.5)
    gbm.accuracy <- c(gbm.accuracy,acc$prop.correct)
    gbm.fprate <- c(gbm.fprate,fp)
    rm(model,predicted,cm,fp,acc,confusion)
  }
  
  
  if(nnet){
    cat("\nNeural Network\n")
    # 7 nodes
    library(nnet)
    model = nnet(isDGA~., train, size = 100, maxit=100, decay = .020156, MaxNWts = 2000)
    #model = nnet(isDGA~., train, size = 7, maxit=10000, decay = .04)
    predicted <- predict(model, test,type="class")
    cm <- table(Truth = test$isDGA, Predicted = predicted)
    fp <- cm[1,2]/length(test$isDGA)
    #cat("False positive rate: ", fp, "\n")
    acc<-accuracy(test$isDGA, predicted, threshold = 0.5)
    nnet.accuracy <- c(nnet.accuracy,acc$prop.correct)
    nnet.fprate <- c(nnet.fprate,fp)
    rm(model,predicted,cm,fp,acc)
  }
  
  
  if(rf){
    cat("\nRandom Forest\n")
    library(randomForest)
    #model <- randomForest( x=train[,-(names(train) %in% c("isDGA"))], y=train$isDGA, xtest=test[,-(names(test) %in% c("isDGA"))], ytest=test$isDGA, mtry=4, ntree=200)    
    model <- randomForest( x=train[,-(names(train) %in% c("isDGA"))], y=train$isDGA, xtest=test[,-(names(test) %in% c("isDGA"))], ytest=test$isDGA)   
    cm <- model$test$confusion
    fp <- cm[1,2]/length(test$isDGA)
    acc <- (cm[1,1]+cm[2,2])/length(test$isDGA)
    rf.accuracy <- c(rf.accuracy,acc)
    rf.fprate <- c(rf.fprate,fp)
    rm(model,cm,fp,acc)
  }
  
  
}  #end cross ten


if(dt){
  cat("DT accuracy mean and fprate mean: ",mean(dt.accuracy),",",mean(dt.fprate),"\n")
  rm(dt.accuracy,dt.fprate)
}
if(gbm){
  cat("GBM accuracy mean and fprate mean: ",mean(gbm.accuracy),",",mean(gbm.fprate),"\n")
  rm(gbm.accuracy,gbm.fprate)
}
if(nnet){ 
  cat("NNET accuracy mean and fprate mean: ",mean(nnet.accuracy),",",mean(nnet.fprate),"\n")
  rm(nnet.accuracy,nnet.fprate)
}
if(rf){
  cat("RF accuracy mean and fprate mean: ",mean(rf.accuracy),",",mean(rf.fprate),"\n")
  rm(rf.accuracy,rf.fprate)
}

rm(fulldataset,test,train,dt,gbm,rf,nnet,folds,testIndexes,i,fulldatasetasread)
