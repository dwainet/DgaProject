
library(foreach)
library(tau)


#specify dataframe to parse
dgadata <- read.csv("dgadata2.csv", stringsAsFactors=FALSE)

#start timer after files load
ptm <- proc.time()

dgadata$isDGA <- as.factor(dgadata$isDGA) 

#scrample then split into train and test datasets.
dgadata <- dgadata[sample(1:nrow(dgadata), nrow(dgadata),  replace=FALSE),]

#mine only the training data. Although not yet split, we decide that first 1M records are for 
#training and additional 130k are for testing.
dgadatatrain <- dgadata[1:1000000, ]

#maximum number of rows in ngramlist. 23328 is 1/2 maximum number possible trigrams.
maxnumberofgrams <- 100000


topngramsdf <- data.frame(row.names = c(1:maxnumberofgrams))


dgadatatrain$family <- gsub(" ", "", dgadatatrain$family, fixed = TRUE)
familylevels <- unique(dgadatatrain$family)


#Sum n-gram counts for each class
#foreach(isDGAvalue = c(TRUE,FALSE)) %do% {
foreach(isDGAvalue = familylevels ) %do% {
  
  urldatasource <- dgadatatrain[with(dgadatatrain, family == isDGAvalue ), ]
  
  txt <- urldatasource$sld
  
  r<-textcnt(txt, method="ngram",n=5L,split = "[[:space:][:punct:]+]", decreasing=TRUE)
  
  a<-data.frame(gram = names(r), counts = unclass(r), size = nchar(names(r)))
  
  b<-split(a,a$size)
  
  trigrams <-   b$'3'
  trigrams <- trigrams[grep("[_]",trigrams$gram,invert=TRUE,value=FALSE),]
  trigrams <- trigrams[1:maxnumberofgrams,]
  
  fourgrams <-   b$'4'
  fourgrams <- fourgrams[grep("[_]",fourgrams$gram,invert=TRUE,value=FALSE),]
  fourgrams <- fourgrams[1:maxnumberofgrams,]
  
  fivegrams <- b$'5'
  fivegrams <- fivegrams[grep("[_]",fivegrams$gram,invert=TRUE,value=FALSE),]
  fivegrams <- fivegrams[1:maxnumberofgrams,]
  
  topngramsdf$gram <- trigrams$gram
  topngramsdf$trigramcount <- trigrams$counts
  names(topngramsdf)[names(topngramsdf)=="gram"] <- paste("trigram",isDGAvalue, sep="")
  names(topngramsdf)[names(topngramsdf)=="trigramcount"] <- paste(paste("trigram",isDGAvalue, sep=""),"count",sep="")
  
  topngramsdf$gram <- fourgrams$gram
  topngramsdf$fourgramcount <- fourgrams$counts
  names(topngramsdf)[names(topngramsdf)=="gram"] <- paste("fourgram",isDGAvalue, sep="")
  names(topngramsdf)[names(topngramsdf)=="fourgramcount"] <- paste(paste("fourgram",isDGAvalue, sep=""),"count",sep="")

  topngramsdf$gram <- fivegrams$gram
  topngramsdf$fivegramcount <- fivegrams$counts
  names(topngramsdf)[names(topngramsdf)=="gram"] <- paste("fivegram",isDGAvalue, sep="")
  names(topngramsdf)[names(topngramsdf)=="fivegramcount"] <- paste(paste("fivegram",isDGAvalue, sep=""),"count",sep="")
  
}



write.table(topngramsdf,"familygrams.csv", sep=",", quote=FALSE)

proc.time() - ptm

rm(a,b,r,txt,urldatasource,dgadatatrain,isDGAvalue,dgadata)
rm(trigrams,fourgrams,fivegrams)
rm(topngramsdf)
