###
#kNN with caret

library(ISLR)
library(caret)

#read in dgadata, factorize as necessary, then sample.
dgatestdata <- read.csv("dgatestdata5.csv", stringsAsFactors=FALSE)
dgatestdata$isDGA <- factor(dgatestdata$isDGA)
dgatestdata$family <- factor(dgatestdata$family)



## DGA vs non classification

ptm <- proc.time()
traindgadata <- dgatestdata[1:137036,]
traincolumns <- c("isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio","trigramnondgaratio","fourgramnondgaratio","fivegramnondgaratio","trigramCryptolockerratio","fourgramCryptolockerratio","fivegramCryptolockerratio","trigramConfickerCratio","fourgramConfickerCratio","fivegramConfickerCratio","trigramConfickerAratio","fourgramConfickerAratio","fivegramConfickerAratio","trigramZeuSGameoverratio","fourgramZeuSGameoverratio","fivegramZeuSGameoverratio","trigramConfickerBratio","fourgramConfickerBratio","fivegramConfickerBratio","trigramPushdoratio","fourgramPushdoratio","fivegramPushdoratio","trigramNewZeuSGameoverratio","fourgramNewZeuSGameoverratio","fivegramNewZeuSGameoverratio","trigramVirutratio","fourgramVirutratio","fivegramVirutratio","trigramBamitalratio","fourgramBamitalratio","fivegramBamitalratio","trigramRunForrestRunratio","fourgramRunForrestRunratio","fivegramRunForrestRunratio","trigramKelihosratio","fourgramKelihosratio","fivegramKelihosratio","trigramRamdoratio","fourgramRamdoratio","fivegramRamdoratio","trigramCoreFloodratio","fourgramCoreFloodratio","fivegramCoreFloodratio","trigramInfostealerShizratio","fourgramInfostealerShizratio","fivegramInfostealerShizratio","trigramRamnitratio","fourgramRamnitratio","fivegramRamnitratio","trigramShizratio","fourgramShizratio","fivegramShizratio","trigramFlashbackratio","fourgramFlashbackratio","fivegramFlashbackratio","trigramSinowalratio","fourgramSinowalratio","fivegramSinowalratio","trigramDorkbotratio","fourgramDorkbotratio","fivegramDorkbotratio","trigramExpiroratio","fourgramExpiroratio","fivegramExpiroratio","trigramZeroAccessratio","fourgramZeroAccessratio","fivegramZeroAccessratio","trigramRansomwareratio","fourgramRansomwareratio","fivegramRansomwareratio","trigramDarkCometratio","fourgramDarkCometratio","fivegramDarkCometratio","trigramFlameratio","fourgramFlameratio","fivegramFlameratio","trigramTinyBankerratio","fourgramTinyBankerratio","fivegramTinyBankerratio","trigramClickerratio","fourgramClickerratio","fivegramClickerratio","trigramExpiroZratio","fourgramExpiroZratio","fivegramExpiroZratio","trigramUrlZoneratio","fourgramUrlZoneratio","fivegramUrlZoneratio","trigramNeverquestratio","fourgramNeverquestratio","fivegramNeverquestratio")
#trainXwithclass <- traindgadata[,c("isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio")]
trainXwithclass <- traindgadata[,traincolumns]
                               
ctrl <- trainControl(method="repeatedcv", repeats = 1 )
tune.grid <- data.frame(k=c(seq(13,19,2)))
knnFit <- train(isDGA ~ ., data = trainXwithclass, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = tune.grid )
knnFit

knnFit.cm <- confusionMatrix(knnFit)
knnFit.fprate <- knnFit.cm$table[2,1]
print("False positive rate: ")
knnFit.fprate
proc.time() - ptm

#knnFit$finalModel
#knnFit$results
#knnFit$perfNames

# notes
#    17  0.9542018  0.8578850  0.001232240  0.003769743 1.565282  full dataset of 137036 rows, more k.
#   try with family ratios added (but not 'nonDGA family' ratios.)
#   15  0.9523629  0.8477963  0.002030424  0.007104624 0.8647359 137036rows, 26258secs

rm(traindgadata,trainXwithclass,ctrl,rfFit,tune.grid,knnFit,isDGAdata)




## family classification

ptm <- proc.time()
isDGAdata <- dgatestdata[with(dgatestdata, isDGA == "TRUE" ),]
traindgadata <- isDGAdata[1:28647,]
traindgadata$family <- factor(traindgadata$family)
traincolumns <- c("family","sldlen","dword","trigramCryptolockerratio","fourgramCryptolockerratio","fivegramCryptolockerratio","trigramConfickerCratio","fourgramConfickerCratio","fivegramConfickerCratio","trigramConfickerAratio","fourgramConfickerAratio","fivegramConfickerAratio","trigramZeuSGameoverratio","fourgramZeuSGameoverratio","fivegramZeuSGameoverratio","trigramConfickerBratio","fourgramConfickerBratio","fivegramConfickerBratio","trigramPushdoratio","fourgramPushdoratio","fivegramPushdoratio","trigramNewZeuSGameoverratio","fourgramNewZeuSGameoverratio","fivegramNewZeuSGameoverratio","trigramVirutratio","fourgramVirutratio","fivegramVirutratio","trigramBamitalratio","fourgramBamitalratio","fivegramBamitalratio","trigramRunForrestRunratio","fourgramRunForrestRunratio","fivegramRunForrestRunratio","trigramKelihosratio","fourgramKelihosratio","fivegramKelihosratio","trigramRamdoratio","fourgramRamdoratio","fivegramRamdoratio","trigramCoreFloodratio","fourgramCoreFloodratio","fivegramCoreFloodratio","trigramInfostealerShizratio","fourgramInfostealerShizratio","fivegramInfostealerShizratio","trigramRamnitratio","fourgramRamnitratio","fivegramRamnitratio","trigramShizratio","fourgramShizratio","fivegramShizratio","trigramFlashbackratio","fourgramFlashbackratio","fivegramFlashbackratio","trigramSinowalratio","fourgramSinowalratio","fivegramSinowalratio","trigramDorkbotratio","fourgramDorkbotratio","fivegramDorkbotratio","trigramExpiroratio","fourgramExpiroratio","fivegramExpiroratio","trigramZeroAccessratio","fourgramZeroAccessratio","fivegramZeroAccessratio","trigramRansomwareratio","fourgramRansomwareratio","fivegramRansomwareratio","trigramDarkCometratio","fourgramDarkCometratio","fivegramDarkCometratio","trigramFlameratio","fourgramFlameratio","fivegramFlameratio","trigramTinyBankerratio","fourgramTinyBankerratio","fivegramTinyBankerratio","trigramClickerratio","fourgramClickerratio","fivegramClickerratio","trigramExpiroZratio","fourgramExpiroZratio","fivegramExpiroZratio","trigramUrlZoneratio","fourgramUrlZoneratio","fivegramUrlZoneratio","trigramNeverquestratio","fourgramNeverquestratio","fivegramNeverquestratio")
trainXwithclass <- traindgadata[,traincolumns]
trainXwithclass$family <- factor(trainXwithclass$family)
ctrl <- trainControl(method="repeatedcv", repeats = 1 )
tune.grid <- data.frame(k=c(seq(21,23,2)))
knnFit <- train(family ~ ., data = trainXwithclass, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = tune.grid )
knnFit
proc.time() - ptm

knnFit$finalModel

#confusionMatrix(knnFit,"average")

#knnPredict <- predict(knnFit, newdata = traindgadata)
#knnCM <- confusionMatrix(knnPredict, traindgadata$family )
#knnCM

#knnCM$table
#knnCM$byClass
#knnCM$overall

rm(traindgadata,trainXwithclass,ctrl,rfFit,tune.grid,knnFit)

## notes
#   k   Accuracy   Kappa      Accuracy SD  Kappa SD 
#   23  0.7351637  0.6566306  0.003408689  0.004533655 28647rows, 538secs


## end family classification ##

rm(dgatestdata)


