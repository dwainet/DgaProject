
library(foreach)
library(tau)


#specify dataframe to parse
dgadata <- read.csv("dgadata2.csv", stringsAsFactors=FALSE)

#start timer after files load
ptm <- proc.time()

dgadata$isDGA <- as.factor(dgadata$isDGA) 


#mine only the training data. Although not yet split, we decide that first 1M records are for 
#training and additional 130k are for testing.
dgadatatrain <- dgadata[1:1000000, ]

#maximum number of rows in ngramlist. 23328 is 1/2 maximum number possible trigrams.
maxnumberofgrams <- 100000
topngramsdf <- data.frame(row.names = c(1:maxnumberofgrams))


#Sum n-gram counts for each class
foreach(isDGAvalue = c(TRUE,FALSE)) %do% {

  #sum n-grams for DGA or nonDGA?
  #isDGAvalue <- TRUE
  
  urldatasource <- dgadatatrain[with(dgadatatrain, isDGA == isDGAvalue ), ]
  
  urls <- urldatasource$URL
  # remove path,tld
  urlsnotld <- gsub("\\/.*$","",urls)
  urlsnotld <- gsub("\\.\\w{2,4}$","",urlsnotld)
  urlsnotld <- gsub("\\.\\w{2,4}$","",urlsnotld)
  
  txt <- urlsnotld
  
  r<-textcnt(txt, method="ngram",n=5L,split = "[[:space:][:punct:]+]", decreasing=TRUE)
  
  a<-data.frame(gram = names(r), counts = unclass(r), size = nchar(names(r)))
  
  b<-split(a,a$size)
  
  ####
  
  igrams <-   b$'1'
  igrams <- igrams[grep("[_]",igrams$gram,invert=TRUE,value=FALSE),]
  igrams <- igrams[1:maxnumberofgrams,]
  
  bigrams <-   b$'2'
  bigrams <- bigrams[grep("[_]",bigrams$gram,invert=TRUE,value=FALSE),]
  bigrams <- bigrams[1:maxnumberofgrams,]

  trigrams <-   b$'3'
  trigrams <- trigrams[grep("[_]",trigrams$gram,invert=TRUE,value=FALSE),]
  trigrams <- trigrams[1:maxnumberofgrams,]
  
  fourgrams <-   b$'4'
  fourgrams <- fourgrams[grep("[_]",fourgrams$gram,invert=TRUE,value=FALSE),]
  fourgrams <- fourgrams[1:maxnumberofgrams,]
  
  fivegrams <- b$'5'
  fivegrams <- fivegrams[grep("[_]",fivegrams$gram,invert=TRUE,value=FALSE),]
  fivegrams <- fivegrams[1:maxnumberofgrams,]

  topngramsdf$gram <- igrams$gram
  topngramsdf$igramcount <- igrams$counts
  names(topngramsdf)[names(topngramsdf)=="gram"] <- paste("igram",isDGAvalue, sep="")
  names(topngramsdf)[names(topngramsdf)=="igramcount"] <- paste(paste("igram",isDGAvalue, sep=""),"count",sep="")
  
  topngramsdf$gram <- bigrams$gram
  topngramsdf$bigramcount <- bigrams$counts
  names(topngramsdf)[names(topngramsdf)=="gram"] <- paste("bigram",isDGAvalue, sep="")
  names(topngramsdf)[names(topngramsdf)=="bigramcount"] <- paste(paste("bigram",isDGAvalue, sep=""),"count",sep="")
  
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


rm(a,b,r,txt,urls,urlsnotld,urldatasource,dgadatatrain,isDGAvalue,dgadata)
rm(igrams,bigrams,trigrams,fourgrams,fivegrams,maxnumberofgrams)

write.table(topngramsdf,"topngramsdf.csv", sep=",", quote=FALSE)

proc.time() - ptm

rm(topngramsdf,ptm)
