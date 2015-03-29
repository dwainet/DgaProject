###
# Neural Network
###

library(ISLR)
library(caret)
library(nnet)


#read in dgadata, factorize as necessary, then sample.
dgatestdata <- read.csv("dgatestdata5.csv", stringsAsFactors=FALSE)
dgatestdata$isDGA <- factor(dgatestdata$isDGA)
dgatestdata$family <- factor(dgatestdata$family)


# isDGA prediction #

ptm <- proc.time()
traindgadata <- dgatestdata[1:137036,]
#traindgadata <- dgatestdata[1:64000,]
traindgawithclass <- traindgadata[,c("isDGA","sldlen","dword","bigramTRUEratio","trigramTRUEratio","fourgramTRUEratio","fivegramTRUEratio","bigramFALSEratio","trigramFALSEratio","fourgramFALSEratio","fivegramFALSEratio")]
my.grid <- expand.grid(.decay = c(.02,.04,.08), .size = c(5,7,11))
my.grid <- expand.grid(.decay = c(.04), .size = c(7))
ctrl <- trainControl(method="repeatedcv", repeats = 1)
nn.fit <- train(isDGA ~ ., data = traindgawithclass, method = "nnet", trControl=ctrl, maxit = 5000, tuneGrid = my.grid, trace = F) 
nn.fit

nn.fit.cm <- confusionMatrix(nn.fit)
nn.fit.fprate <- nn.fit.cm$table[2,1]
print("False positive rate: ")
nn.fit.fprate
print(proc.time() - ptm)



##notes
#   decay  size  Accuracy   Kappa      Accuracy SD  Kappa SD    FPrate
#   0.04    7    0.9548951  0.8599485  0.001374887  0.00439346  1.514201  137036rows,decay.04, size=7,1123 secs

rm(nn.fit,ptm,traindgadata,testdgadata,my.grid)




#### family classification ###

traincolumns <- c("family","sldlen","dword","trigramCryptolockerratio","fourgramCryptolockerratio","fivegramCryptolockerratio","trigramConfickerCratio","fourgramConfickerCratio","fivegramConfickerCratio","trigramConfickerAratio","fourgramConfickerAratio","fivegramConfickerAratio","trigramZeuSGameoverratio","fourgramZeuSGameoverratio","fivegramZeuSGameoverratio","trigramConfickerBratio","fourgramConfickerBratio","fivegramConfickerBratio","trigramPushdoratio","fourgramPushdoratio","fivegramPushdoratio","trigramNewZeuSGameoverratio","fourgramNewZeuSGameoverratio","fivegramNewZeuSGameoverratio","trigramVirutratio","fourgramVirutratio","fivegramVirutratio","trigramBamitalratio","fourgramBamitalratio","fivegramBamitalratio","trigramRunForrestRunratio","fourgramRunForrestRunratio","fivegramRunForrestRunratio","trigramKelihosratio","fourgramKelihosratio","fivegramKelihosratio","trigramRamdoratio","fourgramRamdoratio","fivegramRamdoratio","trigramCoreFloodratio","fourgramCoreFloodratio","fivegramCoreFloodratio","trigramInfostealerShizratio","fourgramInfostealerShizratio","fivegramInfostealerShizratio","trigramRamnitratio","fourgramRamnitratio","fivegramRamnitratio","trigramShizratio","fourgramShizratio","fivegramShizratio","trigramFlashbackratio","fourgramFlashbackratio","fivegramFlashbackratio","trigramSinowalratio","fourgramSinowalratio","fivegramSinowalratio","trigramDorkbotratio","fourgramDorkbotratio","fivegramDorkbotratio","trigramExpiroratio","fourgramExpiroratio","fivegramExpiroratio","trigramZeroAccessratio","fourgramZeroAccessratio","fivegramZeroAccessratio","trigramRansomwareratio","fourgramRansomwareratio","fivegramRansomwareratio","trigramDarkCometratio","fourgramDarkCometratio","fivegramDarkCometratio","trigramFlameratio","fourgramFlameratio","fivegramFlameratio","trigramTinyBankerratio","fourgramTinyBankerratio","fivegramTinyBankerratio","trigramClickerratio","fourgramClickerratio","fivegramClickerratio","trigramExpiroZratio","fourgramExpiroZratio","fivegramExpiroZratio","trigramUrlZoneratio","fourgramUrlZoneratio","fivegramUrlZoneratio","trigramNeverquestratio","fourgramNeverquestratio","fivegramNeverquestratio")
ptm <- proc.time()
isDGAdata <- dgatestdata[with(dgatestdata, isDGA == "TRUE" ),]
isDGAdata$family <- factor(isDGAdata$family)
trainXwithclass <- isDGAdata[1:28647,traincolumns] #family,sld,dword,tri-four-fivefamilyratios
trainXwithclass$family <- factor(trainXwithclass$family)
my.grid <- expand.grid(.decay = c(.02,.04), .size = c(7,11))
ctrl <- trainControl(method="cv", number = 10, repeats = 1)
#ctrl <- trainControl(method="none")
nn.fit <- train(family ~ ., data = trainXwithclass, method = "nnet", trControl=ctrl, MaxNWts = 10000,  maxit = 5000, tuneGrid = my.grid, trace = F)
nn.fit
print(proc.time() - ptm)

## mptes
#decay  size  Accuracy   Kappa      Accuracy SD  Kappa SD 
#0.04   11    0.7744150  0.7168500  0.06758661   0.08139651 full 28647 rows, removed nondga columns, 2x2grid, 10xcv 23367s


rm(ptm,traindgadata,dgadata,testdgadata,ctrl,my.grid,isDGAdata,trainXwithclass,traincolumns)
rm(nn.fit)

#### end family classification

rm(dgatestdata)

