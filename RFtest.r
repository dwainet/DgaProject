###
#RF with caret

library(ISLR)
library(caret)


#read in dgadata, factorize as necessary, then sample.
dgatestdata <- read.csv("dgatestdata5.csv", stringsAsFactors=FALSE)
dgatestdata$isDGA <- factor(dgatestdata$isDGA)
dgatestdata$family <- factor(dgatestdata$family)

# isDGA prediction #

ptm <- proc.time()
traindgadata <- dgatestdata[1:137036,]
trainXwithclass <- traindgadata[,c("isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio")]
ctrl <- trainControl(method="repeatedcv", repeats = 1)
#rfFit <- train(isDGA ~ ., data = trainXwithclass, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 4)
rfFit <- train(isDGA ~ ., data = trainXwithclass, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneGrid = data.frame(.mtry = 4), ntree=200 )
rfFit
rfFit$finalModel

rfFit.cm <- confusionMatrix(rfFit)
rfFit.fprate <- rfFit.cm$table[2,1]
print("False positive rate: ")
rfFit.fprate
proc.time() - ptm

#rfFit$finalModel

## notes
#   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   FPrate
#   4    0.9560407  0.8630146  0.001323031  0.00416282  1.357308  137036rows, mtry=5, ntrees=200,829 secs    


rm(traindgadata,trainXwithclass,ctrl,rfFit)







# family prediction

#traincolumns <- append(traincolumns, c("trigramnondgaratio","fourgramnondgaratio","fivegramnondgaratio","trigramCryptolockerratio","fourgramCryptolockerratio","fivegramCryptolockerratio","trigramConfickerCratio","fourgramConfickerCratio","fivegramConfickerCratio","trigramConfickerAratio","fourgramConfickerAratio","fivegramConfickerAratio","trigramZeuSGameoverratio","fourgramZeuSGameoverratio","fivegramZeuSGameoverratio","trigramConfickerBratio","fourgramConfickerBratio","fivegramConfickerBratio","trigramPushdoratio","fourgramPushdoratio","fivegramPushdoratio","trigramNewZeuSGameoverratio","fourgramNewZeuSGameoverratio","fivegramNewZeuSGameoverratio","trigramVirutratio","fourgramVirutratio","fivegramVirutratio","trigramBamitalratio","fourgramBamitalratio","fivegramBamitalratio","trigramRunForrestRunratio","fourgramRunForrestRunratio","fivegramRunForrestRunratio","trigramKelihosratio","fourgramKelihosratio","fivegramKelihosratio","trigramRamdoratio","fourgramRamdoratio","fivegramRamdoratio","trigramCoreFloodratio","fourgramCoreFloodratio","fivegramCoreFloodratio","trigramInfostealerShizratio","fourgramInfostealerShizratio","fivegramInfostealerShizratio","trigramRamnitratio","fourgramRamnitratio","fivegramRamnitratio","trigramShizratio","fourgramShizratio","fivegramShizratio","trigramFlashbackratio","fourgramFlashbackratio","fivegramFlashbackratio","trigramSinowalratio","fourgramSinowalratio","fivegramSinowalratio","trigramDorkbotratio","fourgramDorkbotratio","fivegramDorkbotratio","trigramExpiroratio","fourgramExpiroratio","fivegramExpiroratio","trigramZeroAccessratio","fourgramZeroAccessratio","fivegramZeroAccessratio","trigramRansomwareratio","fourgramRansomwareratio","fivegramRansomwareratio","trigramDarkCometratio","fourgramDarkCometratio","fivegramDarkCometratio","trigramFlameratio","fourgramFlameratio","fivegramFlameratio","trigramTinyBankerratio","fourgramTinyBankerratio","fivegramTinyBankerratio","trigramClickerratio","fourgramClickerratio","fivegramClickerratio","trigramExpiroZratio","fourgramExpiroZratio","fivegramExpiroZratio","trigramUrlZoneratio","fourgramUrlZoneratio","fivegramUrlZoneratio","trigramNeverquestratio","fourgramNeverquestratio","fivegramNeverquestratio"))
traincolumns <- c("family","sldlen","dword","trigramCryptolockerratio","fourgramCryptolockerratio","fivegramCryptolockerratio","trigramConfickerCratio","fourgramConfickerCratio","fivegramConfickerCratio","trigramConfickerAratio","fourgramConfickerAratio","fivegramConfickerAratio","trigramZeuSGameoverratio","fourgramZeuSGameoverratio","fivegramZeuSGameoverratio","trigramConfickerBratio","fourgramConfickerBratio","fivegramConfickerBratio","trigramPushdoratio","fourgramPushdoratio","fivegramPushdoratio","trigramNewZeuSGameoverratio","fourgramNewZeuSGameoverratio","fivegramNewZeuSGameoverratio","trigramVirutratio","fourgramVirutratio","fivegramVirutratio","trigramBamitalratio","fourgramBamitalratio","fivegramBamitalratio","trigramRunForrestRunratio","fourgramRunForrestRunratio","fivegramRunForrestRunratio","trigramKelihosratio","fourgramKelihosratio","fivegramKelihosratio","trigramRamdoratio","fourgramRamdoratio","fivegramRamdoratio","trigramCoreFloodratio","fourgramCoreFloodratio","fivegramCoreFloodratio","trigramInfostealerShizratio","fourgramInfostealerShizratio","fivegramInfostealerShizratio","trigramRamnitratio","fourgramRamnitratio","fivegramRamnitratio","trigramShizratio","fourgramShizratio","fivegramShizratio","trigramFlashbackratio","fourgramFlashbackratio","fivegramFlashbackratio","trigramSinowalratio","fourgramSinowalratio","fivegramSinowalratio","trigramDorkbotratio","fourgramDorkbotratio","fivegramDorkbotratio","trigramExpiroratio","fourgramExpiroratio","fivegramExpiroratio","trigramZeroAccessratio","fourgramZeroAccessratio","fivegramZeroAccessratio","trigramRansomwareratio","fourgramRansomwareratio","fivegramRansomwareratio","trigramDarkCometratio","fourgramDarkCometratio","fivegramDarkCometratio","trigramFlameratio","fourgramFlameratio","fivegramFlameratio","trigramTinyBankerratio","fourgramTinyBankerratio","fivegramTinyBankerratio","trigramClickerratio","fourgramClickerratio","fivegramClickerratio","trigramExpiroZratio","fourgramExpiroZratio","fivegramExpiroZratio","trigramUrlZoneratio","fourgramUrlZoneratio","fivegramUrlZoneratio","trigramNeverquestratio","fourgramNeverquestratio","fivegramNeverquestratio")
ptm <- proc.time()
isDGAdata <- dgatestdata[with(dgatestdata, isDGA == "TRUE" ),]
isDGAdata$family <- factor(isDGAdata$family)
trainXwithclass <- isDGAdata[1:28647,traincolumns]
#trainXwithclass <- isDGAdata[1:1000,traincolumns]
trainXwithclass$family <- factor(trainXwithclass$family)
ctrl <- trainControl(method="cv", number = 10, repeats = 1)
#ctrl <- trainControl(method="none")
rfFit <- train(family ~ ., data = trainXwithclass, method = "rf", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 5)
rfFit
print(proc.time() - ptm)

#rfFit$finalModel

#dm <-confusionMatrix(rfFit,"average")
#dm$table
#write.table(dm$table,"rfFamilydm.csv", sep=",", quote=FALSE)

#rfPredict <- predict(rfFit, newdata = testdgawithclass)
#levels(rfPredict) <- levels(isDGAdata$family)
#levels(testdgawithclass$family) <- levels(isDGAdata$family)
#rfCM <- confusionMatrix(rfPredict, testdgawithclass$family )
#rfCM$table
#rfCM$byClass
#rfCM$overall

rm(traindgadata,trainXwithclass,ctrl,rfFit,ptm,traincolumns)

## notes
# mtry  Accuracy   Kappa      Accuracy SD  Kappa SD 
#  23    0.8034117  0.7524563  0.001812336  0.002356217 28647rows, tl=5, 16734sec


### end family prediction



rm(dgatestdata)

