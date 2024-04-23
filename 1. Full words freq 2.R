# Cleaning around
# ==============================================================================
rm(list=ls()) # Removing variables
graphics.off() # Clearing plotting devices
cat("\014") # Clearing console
# ==============================================================================

# Set the working directory
# ==============================================================================
#setwd("C:/Users/ltawk/Documents/Text mining pipeline/Textmining2024")
#getwd()  # Confirm the current working directory
# ==============================================================================

# Load the necessary packages (install if necessary)
# ==============================================================================
reqpacks <- c("pdftools", "tm", "dplyr", "tibble", "openxlsx", "tidytext",
              "ggplot2", "reshape2", "tidyr", "tidyverse", "igraph", "ggraph",
              "RColorBrewer", "readxl", "scales", "crayon")
for (apack in reqpacks) {
  if (!suppressWarnings(require(apack, character.only = TRUE))) {
    install.packages(apack, repos = "http://cran.us.r-project.org")
    library(apack, character.only = TRUE)
  }
}
# ==============================================================================

# Load the extraction function
# ==============================================================================
source("Aux_pdfsExtractor.R")
# ==============================================================================

# Main function to process PDFs and generate outputs
# ==============================================================================
fullWordsFreqHist <- function() {
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old output files
  unlink("1. Full words freq 2/*")
  
  # Indicate output path for the file
  outputPath <- "1. Full words freq 2/AllWords.xlsx"
  
  # Indicate sheet name for the excel file
  sheetName <- "AllWords"
  
  # Create workbook and sheet for the excel file
  wBook <- createWorkbook()
  addWorksheet(wBook, sheetName)
  showGridLines(wBook, 1, showGridLines = FALSE)
  saveWorkbook(wBook, file = outputPath, overwrite = TRUE)
  
  # Process each PDF
  outFullList <- list()
  colCounter <- 1
  for (i in pdfsPaths) {
    # Extract words and frequencies
    outFull <- freqFullWords(i, "english")
    outFullList <- append(outFullList, list(outFull))
    csvOutputPath <- paste0("1. Full words freq 2/", tools::file_path_sans_ext(basename(i)), "_words.csv")
    write.csv(outFull, csvOutputPath, row.names = FALSE)
    
    # Excel styling
    myHeaderStyle <- openxlsx::createStyle(fontSize = 12, fontColour= "#FFFFFF", 
                                           halign = "center",
                                           fgFill = "#D9D9D9", 
                                           border = "TopBottomLeftRight",  
                                           borderStyle ="medium",
                                           valign = "center", 
                                           textDecoration = "bold",
                                           wrapText = FALSE)
    myPdfNamesStyle <- openxlsx::createStyle(fontSize = 12, 
                                             fontColour = "#FFFFFF", 
                                             halign = "center",
                                             fgFill = "#BFBFBF", 
                                             border = "TopBottomLeftRight",  
                                             borderStyle ="medium",
                                             valign = "center", 
                                             textDecoration = "bold",
                                             wrapText = FALSE)
    writeData(wBook, sheetName, outFull, startRow = 2, startCol = colCounter, 
              headerStyle = myHeaderStyle, borders = "surrounding",
              borderStyle = "medium")
    writeData(wBook, sheetName, tools::file_path_sans_ext(basename(i)), 
              startRow = 1, startCol = colCounter, borders = "surrounding",
              borderStyle = "medium")
    
    setColWidths(wBook, sheetName, cols = colCounter + 2, widths = "auto")
    mergeCells(wBook, sheetName, cols = colCounter:colCounter+1, rows=1)
    addStyle(wBook, sheet = sheetName, style = myPdfNamesStyle, rows = 1, 
             cols = colCounter)
    
    colCounter <- colCounter + 2
  }
  saveWorkbook(wBook, file = outputPath, overwrite = TRUE)
  
  # Plotting histograms for each PDF
  index <- 1
  for (i in pdfsPaths) {
    topFreq <- head(unlist(outFullList[[index]][2]),10)
    topWord <- head(unlist(outFullList[[index]][1]),10)
    
    # Create the PNG for each plot
    png(file = paste("1. Full words freq 2/hist_", 
                     tools::file_path_sans_ext(basename(i)), ".png"), 
        width = 3.5, height = 2.5, units = "in", res = 1100, pointsize = 4)
    
    # Plot the bar chart
    par(mar=c(12, 10, 6, 4), mgp=c(8, 0.5, 0), las=1)
    barplot(topFreq, names.arg = topWord, xlab = "Top words", ylab = "Frequency",
            main = tools::file_path_sans_ext(basename(i)), 
            col = "#ADD8E6", # Light blue for bars
            border = "#989898", # Grey for bar borders
            las=2, cex.axis=1.0, cex.names=1.0, cex.main = 1.2, cex.lab = 1.0)
    
    # Save the plot
    dev.off()
    
    index <- index + 1
  }
  cat(green("Done!\n"))
}
# ==============================================================================

# Optionally, call the function to execute
#fullWordsFreqHist()

