# cleaning around
# ==============================================================================
rm(list=ls()) # removing variables
graphics.off() # clearing plotting devices
cat("\014") # clearing console
# ==============================================================================

# ==============================================================================
# This function extracts all words in each PDF, removes stop words/symbols/#s,
# finds the freq of the remaining ones, sorts them and generates an excel file 
# with that information. It also generates a bar plot per PDF containing the  
# most common words on it
source("1. Full words freq.R")
fullWordsFreqHist()


source("1. Full words freq 2.R")
fullWordsFreqHist()

# ==============================================================================
# This function generates plots: one per PDF and one as a group showing Zipf’s 
# law. Also, it produces plots: one per PDF and one as a group showing Zipf’s 
# law and their corresponding linear regressions for comparison purposes
source("2. Zipf law.R")
zipfLaw()

# ==============================================================================
# This function builds a LDA model for a given number of topics, and delivers
# plots with the resulting word-topic probabilities and the document-topic 
# probabilities
source("3. LDA.R")
nbtopics <- 10
LDA_analysis(nbtopics)


# ==============================================================================
# This function generates a plot with a network per PDF and also one as a group 
# among all the PDFs containing the most relevant bigrams 
source("4. Bigrams networks.R")
bigramsNetworks()

# ==============================================================================
# This function generates two excel files containing the freq of each keyword 
# per each PDF. This first file with the keywords sorted by freq and the other 
# one with the keywords in the original order to compute some stats per keyword
source("5. Keywords stats.R")
keywordsStats()

# ==============================================================================
# This function generates presence absence data containing 0 and 1 for each 
# custom keyword extracted from all the PDFs in the input folder
# long lat will be added manually after the csv export
source("6. Keywords heatmap prep.R")
keywordsStats()

getwd()
setwd("G:/My Drive/_Spring2024DissertationII/RTextMining v2 April 2024")
