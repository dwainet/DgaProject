###
# step_four_aggregate_bgram_cols.R
#
# This script will take the ngram counts and create ratio columns.
#


#read in dgatestdata
dgadata <- read.csv("dgadata4.csv", stringsAsFactors=FALSE)

#Pull out test set that was _not_ used to build ngrams lists. This is the 'test' dataset.
dgatestdata <- dgadata[ 1000001:nrow(dgadata),]


#start timer after files load
ptm <- proc.time()

families <- c("nondga","Cryptolocker","ConfickerC","ConfickerA","ZeuSGameover","ConfickerB","Pushdo","NewZeuSGameover", "Virut","Bamital","RunForrestRun","Kelihos","Ramdo","CoreFlood" , "InfostealerShiz", "Ramnit"  ,"Shiz","Flashback","Sinowal","Dorkbot","Expiro","ZeroAccess","Ransomware","DarkComet","Flame","TinyBanker","Clicker","ExpiroZ","UrlZone","Neverquest")

dgatestdata$igramTRUEratio <- dgatestdata$igramTRUEcount/nchar(dgatestdata$sld)
dgatestdata$bigramTRUEratio <- dgatestdata$bigramTRUEcount/(max(c(nchar(dgatestdata$sld)-1),0))
dgatestdata$trigramTRUEratio <- dgatestdata$trigramTRUEcount/(max(c(nchar(dgatestdata$sld)-2),0))
dgatestdata$fourgramTRUEratio <- dgatestdata$fourgramTRUEcount/(max(c(nchar(dgatestdata$sld)-3),0))
dgatestdata$fivegramTRUEratio <- dgatestdata$fivegramTRUEcount/(max(c(nchar(dgatestdata$sld)-4),0))

dgatestdata$igramFALSEratio <- dgatestdata$igramFALSEcount/nchar(dgatestdata$sld)
dgatestdata$bigramFALSEratio <- dgatestdata$bigramFALSEcount/(max(c(nchar(dgatestdata$sld)-1),0))
dgatestdata$trigramFALSEratio <- dgatestdata$trigramFALSEcount/(max(c(nchar(dgatestdata$sld)-2),0))
dgatestdata$fourgramFALSEratio <- dgatestdata$fourgramFALSEcount/(max(c(nchar(dgatestdata$sld)-3),0))
dgatestdata$fivegramFALSEratio <- dgatestdata$fivegramFALSEcount/(max(c(nchar(dgatestdata$sld)-4),0))


for (family in families) {
  trifamily <- paste("trigram",family,sep="")
  tricount <- dgatestdata[,trifamily]
  triratio <- tricount/(max(c(nchar(dgatestdata$sld)-2),0))
  trifamilyratiolabel <- paste(trifamily,"ratio",sep="")
  dgatestdata[, trifamilyratiolabel] <- triratio; 
  
  trifamily <- paste("fourgram",family,sep="")
  tricount <- dgatestdata[,trifamily]
  triratio <- tricount/(max(c(nchar(dgatestdata$sld)-3),0))
  trifamilyratiolabel <- paste(trifamily,"ratio",sep="")
  dgatestdata[, trifamilyratiolabel] <- triratio; 
  
  trifamily <- paste("fivegram",family,sep="")
  tricount <- dgatestdata[,trifamily]
  triratio <- tricount/(max(c(nchar(dgatestdata$sld)-4),0))
  trifamilyratiolabel <- paste(trifamily,"ratio",sep="")
  dgatestdata[, trifamilyratiolabel] <- triratio;
}


#record resuls to fs
write.table(dgatestdata,"dgatestdata5.csv", sep=",", quote=FALSE)

proc.time() - ptm


rm(dgadata,dgatestdata,ptm,families,family,tricount,trifamily,trifamilyratiolabel,triratio)
