###
# presplit data prep
###
load("dwords.rda")
library("stringr")
ngram.name <- function(instr, n) {
  lapply(instr, function(x) {
    first <- unlist(strsplit(x, NULL))
    lns <- nchar(x)
    unlist(lapply(n[n<=lns], function(i) {
      sapply(seq(i, lns), function(p) paste0(first[(p-(i-1)):p], collapse="") )
    }))
  })
}
wmatch <- function(text, cnum = T) {
  matched <- lapply(text, function(txt) rep(F, nchar(txt)))
  if(cnum) {
    outs <- str_locate_all(text, "[0-9]")
    for(i in seq_along(outs)) {
      matched[[i]][outs[[i]][, 1]] <- TRUE
    }
  }
  top <- min(max(nchar(text)), length(dwords))
  for(tlen in seq(top, 3)) {
    ngs <- ngram.name(text, n=tlen)
    for(i in seq_along(ngs)) {
      pos <- which(ngs[[i]] %in% dwords[[tlen]])
      for(x in pos) {
        matched[[i]][x:(x+tlen-1)] <- T
      }
    }
  }
  sapply(matched, mean)
}

#DGA_classification_data.csv - original
#dgadata.csv - columns renamed
dgadata <- read.csv("dgadata.csv", stringsAsFactors=FALSE)

#scrample rows.
dgadata <- dgadata[sample(1:nrow(dgadata), nrow(dgadata),  replace=FALSE),]

#Add classification 'isDGA' column
dgadata$isDGA <- as.factor(ifelse(dgadata$family == "non dga", "F", "T"))

#Add tld
dgadata$tld <- gsub(".*\\.(\\w+$)","\\1",dgadata$URL)

#Add sld (second level domain)
myfunctionurlstrip <- function(url){
  url <- tolower(url)
  url <- gsub("\\/.*$","",url)
  url <- gsub("\\.\\w{2,4}$","",url)
  url <- gsub("\\.\\w{2,4}$","",url)
  return(url)
}
dgadata$sld <- myfunctionurlstrip(dgadata$URL)

#Add length of second level domain
dgadata$sldlen <- nchar(dgadata$sld)

#Add tld
dgadata$tld <- gsub(".*\\.(\\w+$)","\\1",dgadata$URL)

#Add dwords (dictionary word value)
dgadata$dword <- wmatch(dgadata$sld)


#write to fs. 
write.table(dgadata,"dgadata2.csv", sep=",", quote=FALSE)

rm(dgadata,ngram.name,wmatch)

print("Ready for ngram mining.")


