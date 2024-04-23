# load the necessary packages (install if necessary)
# ==============================================================================
reqpacks <- c("pdftools",   # handling of pdf content
              "tm",         # text mining
              "dplyr",      # smart object handling
              "tibble",
              "openxlsx",
              "tidytext",
              "ggplot2",
              "topicmodels",
              "reshape2",
              "tidyr",
              "tidyverse", 
              "igraph",
              "ggraph",
              "RColorBrewer",
              "readxl",
              "scales",
              "crayon",
              "viridis")     # smart table handling
for (apack in reqpacks) {
  if(!suppressWarnings(require(apack, character.only = TRUE))) {
    install.packages(apack, repos = "http://cran.us.r-project.org")
    library(apack, character.only = TRUE)
  }
}
# ==============================================================================



# ==============================================================================
# Function that reads a pdf, extracts words, removes stopwords and finds the
# frequency of the remaining ones
# ------------------------------------------------------------------------------
freqFullWords <- function(pdffile = NULL, language = NULL) {
  # read pdf 
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  
  # convert text to lowercase
  dt.lower <- tolower(data.pdf)
  
  # remove newlines symbols
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  
  # remove punctuation except scripts and & (joining two words)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  
  # split in words
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  
  # remove actual numbers
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  
  # remove roman numerals
  rom <- paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|C?D",
         "|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|X?L",
         "|L?X{1,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V",
         "|V?I{1,3}))\\b")
  pos.rom <- grep(rom, dt.num, ignore.case = TRUE)
  pos.rom.ok <- pos.rom[-grep('[^[:alnum:]]', dt.num[pos.rom])] # protect punct exceptions
  if(length(pos.rom.ok)>0){
    dt.rom <- dt.num[-pos.rom.ok]
  }else{
    dt.rom <- dt.num[-pos.rom]
  }
  
  # remove individual letters (for numbered bullets case)
  pos.ltr <- grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)
  pos.ltr.ok <- pos.ltr[-grep('[^[:alnum:]]', dt.rom[pos.ltr])] # protect punct exceptions
  if(length(pos.ltr.ok)>0){
    dt.ltr <- dt.rom[-pos.ltr.ok]
  }else{
    dt.ltr <- dt.rom[-pos.ltr]
  }
  
  # remove stopwords
  dt.ltr.temp <- dt.ltr
  sym <- grep('[^[:alnum:]]', dt.ltr.temp) # protect punct exceptions
  if(length(sym) > 0){
    dt.ltr.temp <- dt.ltr.temp[-grep('[^[:alnum:]]', dt.ltr.temp)]
  }
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  if(length(sym) > 0){
    dt.stop <- c(dt.stop, dt.ltr[sym]) # recover the punct exceptions
  }
  
  # remove any remaining symbol
  dt.nohy <- dt.stop
  if(length(which(dt.stop == "-"))>0){ dt.nohy <- dt.stop[-which(dt.stop == "-")]}
  dt.noam <- dt.nohy
  if(length(which(dt.nohy == "&"))>0){ dt.noam <- dt.nohy[-which(dt.nohy == "&")]}
  
  # remove whitespace
  dt.clean <- dt.noam[-which(dt.noam == "")] 
  
  # find the frequency of the remaining words
  freq.all <- table(dt.clean)
  
  # sort the words in descending order of frequency
  freq.sorted <- sort(freq.all, decreasing = TRUE)
  
  #convert to dataframe
  outFull <- data.frame(freq.sorted, 
                      row.names = NULL, stringsAsFactors = FALSE)
  names(outFull) <- c("Word", "Freq")
  
  # save the output in a csv file
  #write.csv(outFull,"outFull.csv", row.names = FALSE, quote = FALSE)
  
  return(outFull)
}
# ==============================================================================

# ==============================================================================
# Function that reads a pdf, removes stopwords, extracts the keywords you
# specify through a csv and finds their frequency
# ------------------------------------------------------------------------------
freqKeywords <- function(pdffile = NULL, csvfile = NULL, language = NULL){
  # read pdf 
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  
  # convert text to lowercase
  dt.lower <- tolower(data.pdf)
  
  # remove newlines symbols
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  
  # remove punctuation except scripts and & (joining two words)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  
  # split in words
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  
  # remove actual numbers
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  
  # remove roman numerals
  rom <- paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|C?D",
                "|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|X?L",
                "|L?X{1,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V",
                "|V?I{1,3}))\\b")
  pos.rom <- grep(rom, dt.num, ignore.case = TRUE)
  pos.rom.ok <- pos.rom[-grep('[^[:alnum:]]', dt.num[pos.rom])] # protect punct exceptions
  if(length(pos.rom.ok)>0){
    dt.rom <- dt.num[-pos.rom.ok]
  }else{
    dt.rom <- dt.num[-pos.rom]
  }
  
  # remove individual letters (for numbered bullets case)
  pos.ltr <- grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)
  pos.ltr.ok <- pos.ltr[-grep('[^[:alnum:]]', dt.rom[pos.ltr])] # protect punct exceptions
  if(length(pos.ltr.ok)>0){
    dt.ltr <- dt.rom[-pos.ltr.ok]
  }else{
    dt.ltr <- dt.rom[-pos.ltr]
  }
  
  # remove stopwords
  dt.ltr.temp <- dt.ltr
  sym <- grep('[^[:alnum:]]', dt.ltr.temp) # protect punct exceptions
  if(length(sym) > 0){
    dt.ltr.temp <- dt.ltr.temp[-grep('[^[:alnum:]]', dt.ltr.temp)]
  }
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  if(length(sym) > 0){
    dt.stop <- c(dt.stop, dt.ltr[sym]) # recover the punct exceptions
  }
  
  # remove any remaining symbol
  dt.nohy <- dt.stop
  if(length(which(dt.stop == "-"))>0){ dt.nohy <- dt.stop[-which(dt.stop == "-")]}
  dt.noam <- dt.nohy
  if(length(which(dt.nohy == "&"))>0){ dt.noam <- dt.nohy[-which(dt.nohy == "&")]}
  
  # remove whitespace
  dt.clean <- dt.stop[-which(dt.stop == "")]
  
  # import data of a csv 
  data.csv <- read.csv(csvfile)
  
  # extract first column with the keywords
  keywords <- tolower(as.vector(data.csv[[1]]))
  
  # match keywords and all words (out of stopwords)
  check.match <- dt.clean %in% keywords
  # leave only the words that match the keywords
  dt.clean <- dt.clean[which(check.match)]
  
  # find the frequency of the keywords
  freq.all <- table(dt.clean)
  new.freq.all <- c()
  for (i in keywords) {
    if(!any(i == names(freq.all))){
      rowdf <- data.frame(dt.clean = i, Freq = 0, row.names = NULL, 
                          stringsAsFactors = FALSE)
    }else{
      rowdf <- data.frame(dt.clean = i, Freq = freq.all[i], row.names = NULL, 
                          stringsAsFactors = FALSE)
    }
    new.freq.all <- rbind(new.freq.all, rowdf)
  }
  
  # extract index of descending order according the frequency of keywords
  ind.ord <- order(new.freq.all[,2], decreasing = TRUE)
  data.csv[[1]] <- tolower(data.csv[[1]])
  
  # add frequency column
  data.csv <- data.csv %>% add_column("Freq" = new.freq.all[,2], .after = colnames(data.csv[1]))
  
  # sort the keywords and the remaining data of the received csv according the index above
  outKey <- data.frame(data.csv[ind.ord,1:2], row.names = NULL, stringsAsFactors = FALSE)
  
  # save the output in a csv file
  #write.csv(outKey,"outKey.csv", row.names = FALSE, quote = FALSE)
  
  return(outKey)
}

# ==============================================================================
# Function that reads a pdf, removes stopwords, extracts the keywords you
# specify through a csv and returns their frequency (without sorting them)
# ------------------------------------------------------------------------------
freqKeywords_NotSorted <- function(pdffile = NULL, csvfile = NULL, language = NULL){
  # read pdf 
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  
  # convert text to lowercase
  dt.lower <- tolower(data.pdf)
  
  # remove newlines symbols
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  
  # remove punctuation except scripts and & (joining two words)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  
  # split in words
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  
  # remove actual numbers
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  
  # remove roman numerals
  rom <- paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|C?D",
                "|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|X?L",
                "|L?X{1,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V",
                "|V?I{1,3}))\\b")
  pos.rom <- grep(rom, dt.num, ignore.case = TRUE)
  pos.rom.ok <- pos.rom[-grep('[^[:alnum:]]', dt.num[pos.rom])] # protect punct exceptions
  if(length(pos.rom.ok)>0){
    dt.rom <- dt.num[-pos.rom.ok]
  }else{
    dt.rom <- dt.num[-pos.rom]
  }
  
  # remove individual letters (for numbered bullets case)
  pos.ltr <- grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)
  pos.ltr.ok <- pos.ltr[-grep('[^[:alnum:]]', dt.rom[pos.ltr])] # protect punct exceptions
  if(length(pos.ltr.ok)>0){
    dt.ltr <- dt.rom[-pos.ltr.ok]
  }else{
    dt.ltr <- dt.rom[-pos.ltr]
  }
  
  # remove stopwords
  dt.ltr.temp <- dt.ltr
  sym <- grep('[^[:alnum:]]', dt.ltr.temp) # protect punct exceptions
  if(length(sym) > 0){
    dt.ltr.temp <- dt.ltr.temp[-grep('[^[:alnum:]]', dt.ltr.temp)]
  }
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  if(length(sym) > 0){
    dt.stop <- c(dt.stop, dt.ltr[sym]) # recover the punct exceptions
  }
  
  # remove any remaining symbol
  dt.nohy <- dt.stop
  if(length(which(dt.stop == "-"))>0){ dt.nohy <- dt.stop[-which(dt.stop == "-")]}
  dt.noam <- dt.nohy
  if(length(which(dt.nohy == "&"))>0){ dt.noam <- dt.nohy[-which(dt.nohy == "&")]}
  
  # remove whitespace
  dt.clean <- dt.stop[-which(dt.stop == "")]
  
  # import data of a csv 
  data.csv <- read.csv(csvfile)
  
  # extract first column with the keywords
  keywords <- tolower(as.vector(data.csv[[1]]))
  
  # match keywords and all words (out of stopwords)
  check.match <- dt.clean %in% keywords
  # leave only the words that match the keywords
  dt.clean <- dt.clean[which(check.match)]
  
  # find the frequency of the keywords
  freq.all <- table(dt.clean)
  new.freq.all <- c()
  for (i in keywords) {
    if(!any(i == names(freq.all))){
      rowdf <- data.frame(dt.clean = i, Freq = 0, row.names = NULL, 
                          stringsAsFactors = FALSE)
    }else{
      rowdf <- data.frame(dt.clean = i, Freq = freq.all[i], row.names = NULL, 
                          stringsAsFactors = FALSE)
    }
    new.freq.all <- rbind(new.freq.all, rowdf)
  }
  
  # extract index of descending order according the frequency of keywords
  #ind.ord <- order(new.freq.all[,2], decreasing = TRUE)
  data.csv[[1]] <- tolower(data.csv[[1]])
  
  # add frequency column
  data.csv <- data.csv %>% add_column("Freq" = new.freq.all[,2], .after = colnames(data.csv[1]))
  
  # sort the keywords and the remaining data of the received csv according the index above
  #outKey <- data.frame(data.csv[ind.ord,1:2], row.names = NULL, stringsAsFactors = FALSE)
  outKey <- data.frame(data.csv[,2], row.names = NULL, stringsAsFactors = FALSE)
  
  # save the output in a csv file
  #write.csv(outKey,"outKey.csv", row.names = FALSE, quote = FALSE)
  
  return(outKey)
}
# ==============================================================================

# ==============================================================================
# Function that reads a pdf, extracts all words, removes stopwords/symbols/numbers
# and returns the remaining ones, without frequecies
# ------------------------------------------------------------------------------
fullCleanWords_stopWRemoved <- function(pdffile = NULL, language = NULL, dataExc = NULL) {
  # read pdf 
  #pdffile <- pdfsPaths[4]
  #language <- "english"
  
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  
  # convert text to lowercase
  dt.lower <- tolower(data.pdf)
  
  # remove newlines symbols
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  
  # remove punctuation except scripts and & (joining two words)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  
  # split in words
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  
  # remove actual numbers
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  
  # remove roman numerals
  rom <- paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|C?D",
                "|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|X?L",
                "|L?X{1,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V",
                "|V?I{1,3}))\\b")
  pos.rom <- grep(rom, dt.num, ignore.case = TRUE)
  pos.rom.ok <- pos.rom[-grep('[^[:alnum:]]', dt.num[pos.rom])] # protect punct exceptions
  if(length(pos.rom.ok)>0){
    dt.rom <- dt.num[-pos.rom.ok]
  }else{
    dt.rom <- dt.num[-pos.rom]
  }
  
  # remove individual letters (for numbered bullets case)
  pos.ltr <- grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)
  pos.ltr.ok <- pos.ltr[-grep('[^[:alnum:]]', dt.rom[pos.ltr])] # protect punct exceptions
  if(length(pos.ltr.ok)>0){
    dt.ltr <- dt.rom[-pos.ltr.ok]
  }else{
    dt.ltr <- dt.rom[-pos.ltr]
  }
  
  # remove stopwords
  dt.ltr.temp <- dt.ltr
  sym <- grep('[^[:alnum:]]', dt.ltr.temp) # protect punct exceptions
  if(length(sym) > 0){
    dt.ltr.temp <- dt.ltr.temp[-grep('[^[:alnum:]]', dt.ltr.temp)]
  }
  dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  if(length(sym) > 0){
    dt.stop <- c(dt.stop, dt.ltr[sym]) # recover and add the punct exceptions
  }
  
  # remove any remaining symbol
  dt.nohy <- dt.stop
  if(length(which(dt.stop == "-"))>0){ dt.nohy <- dt.stop[-which(dt.stop == "-")]}
  dt.noam <- dt.nohy
  if(length(which(dt.nohy == "&"))>0){ dt.noam <- dt.nohy[-which(dt.nohy == "&")]}
  
  # remove anything you want
  dt.youExc <- dt.noam
  #dataExc <- c("b-", "-etaf", "ing")
  if(length(dataExc)>0){
    for(j in dataExc){
      if(length(which(dt.youExc == j))>0){ dt.youExc <- dt.youExc[-which(dt.youExc == j)]}
    }
  }
 
  
  # remove whitespace
  dt.clean <- dt.youExc[-which(dt.youExc == "")] 
  
  # find the frequency of the remaining words
  #freq.all <- table(dt.clean)
  
  # sort the words in descending order of frequency
  #freq.sorted <- sort(freq.all, decreasing = TRUE)
  
  #convert to dataframe
  #outFull <- data.frame(freq.sorted, 
  #                      row.names = NULL, stringsAsFactors = FALSE)
  #names(outFull) <- c("Word", "Freq")
  
  # save the output in a csv file
  #write.csv(outFull,"outFull.csv", row.names = FALSE, quote = FALSE)
  dt.return <- paste(dt.clean,collapse=" ") 
  return(dt.return)
}

# ==============================================================================

# ==============================================================================
# Function that reads a pdf, extracts all words, removes symbols/numbers
# and returns the remaining ones, without frequecies (keeping stopwords)
# ------------------------------------------------------------------------------
fullCleanWords_stopWKept <- function(pdffile = NULL, dataExc = NULL) {
  #pdffile <- pdfsPaths[4]
  # read pdf 
  suppressMessages(data.pdf <- pdftools::pdf_text(pdffile))
  
  # convert text to lowercase
  dt.lower <- tolower(data.pdf)
  
  # remove newlines symbols
  dt.sym <- gsub("\r?\n|\r", " ", dt.lower)
  
  # remove punctuation except scripts and & (joining two words)
  dt.punc <- gsub("[[:punct:]]* *(\\w+[&'-]\\w+)|[[:punct:]]+ *| {2,}", " \\1", dt.sym)
  
  # split in words
  dt.split <- strsplit(dt.punc, " ")
  dt.vec <- unlist(dt.split)
  
  # remove actual numbers
  dt.num <- gsub("\\b\\d+\\b", "", dt.vec)
  
  # remove roman numerals
  rom <- paste0("\\b(M{1,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|C?D",
                "|D?C{1,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|X?L",
                "|L?X{1,3})(IX|IV|V?I{0,3})|M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|I?V",
                "|V?I{1,3}))\\b")
  pos.rom <- grep(rom, dt.num, ignore.case = TRUE)
  pos.rom.ok <- pos.rom[-grep('[^[:alnum:]]', dt.num[pos.rom])] # protect punct exceptions
  if(length(pos.rom.ok)>0){
    dt.rom <- dt.num[-pos.rom.ok]
  }else{
    dt.rom <- dt.num[-pos.rom]
  }
  
  # remove individual letters (for numbered bullets case)
  pos.ltr <- grep("\\b([a-z])\\b", dt.rom, ignore.case = TRUE)
  pos.ltr.ok <- pos.ltr[-grep('[^[:alnum:]]', dt.rom[pos.ltr])] # protect punct exceptions
  if(length(pos.ltr.ok)>0){
    dt.ltr <- dt.rom[-pos.ltr.ok]
  }else{
    dt.ltr <- dt.rom[-pos.ltr]
  }
  
  # remove stopwords
  # dt.ltr.temp <- dt.ltr
  # sym <- grep('[^[:alnum:]]', dt.ltr.temp) # protect punct exceptions
  # if(length(sym) > 0){
  #   dt.ltr.temp <- dt.ltr.temp[-grep('[^[:alnum:]]', dt.ltr.temp)]
  # }
  # dt.stop <- removeWords(dt.ltr.temp, stopwords(language))
  # if(length(sym) > 0){
  #   dt.stop <- c(dt.stop, dt.ltr[sym]) # recover the punct exceptions
  # }
  
  # remove any remaining symbol
  dt.nohy <- dt.ltr
  if(length(which(dt.ltr == "-"))>0){ dt.nohy <- dt.ltr[-which(dt.ltr == "-")]}
  dt.noam <- dt.nohy
  if(length(which(dt.nohy == "&"))>0){ dt.noam <- dt.nohy[-which(dt.nohy == "&")]}
  
  # remove anything you want
  dt.youExc <- dt.noam
  
  #dataExc <- c("b-", "-etaf", "ing")
  if(length(dataExc)>0){
    for(j in dataExc){
      if(length(which(dt.youExc == j))>0){ dt.youExc <- dt.youExc[-which(dt.youExc == j)]}
    }
  }
  
  # remove whitespace
  dt.clean <- dt.youExc[-which(dt.youExc == "")] 
  
  # find the frequency of the remaining words
  #freq.all <- table(dt.clean)
  
  # sort the words in descending order of frequency
  #freq.sorted <- sort(freq.all, decreasing = TRUE)
  
  #convert to dataframe
  #outFull <- data.frame(freq.sorted, 
  #                      row.names = NULL, stringsAsFactors = FALSE)
  #names(outFull) <- c("Word", "Freq")
  
  # save the output in a csv file
  #write.csv(outFull,"outFull.csv", row.names = FALSE, quote = FALSE)
  dt.return <- paste(dt.clean,collapse=" ") 
  return(dt.return)
}
