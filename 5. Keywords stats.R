# cleaning around
# ==============================================================================
rm(list=ls()) # removing variables
graphics.off() # clearing plotting devices
cat("\014") # clearing console
# ==============================================================================



# load the extraction function
# ==============================================================================
source("Aux_pdfsExtractor.R")
# ==============================================================================

keywordsStats <- function() {
  
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old output files
  unlink("5. Keywords stats/*")
  
  # Indicate output paths for the files
  firstOutputPath <- "5. Keywords stats/1. Sorted Keywords.xlsx"
  secondOutputPath <- "5. Keywords stats/2. Not-Sorted Keywords.xlsx"
  
  # Indicate sheet name for the excel file
  sheetName <- "Keywords"
  
  #-----------------------------------------------------------------------------
  # First analysis - Sorted keywords 
  #-----------------------------------------------------------------------------
  # Create workbook and sheet for the excel file
  wBook <- createWorkbook()
  addWorksheet(wBook, sheetName)
  showGridLines(wBook, 1, showGridLines = FALSE)
  saveWorkbook(wBook, file = firstOutputPath, overwrite = TRUE)
  outKeyList <- list()
  
  # Write results in the excel file and add format to it
  colCounter <- 1
  secondCount <- 0
  myHeaderStyle <- openxlsx::createStyle(fontSize = 12, fontColour = "#FFFFFF", 
                                         halign = "center", fgFill = "#0000FF", 
                                         border = "TopBottomLeftRight",  
                                         borderStyle ="medium",
                                         valign = "center",
                                         textDecoration = "bold",
                                         wrapText = FALSE)
  myPdfNamesStyle <- openxlsx::createStyle(fontSize = 12, fontColour= "#FFFFFF",
                                           halign = "center", fgFill= "#990033",
                                           border = "TopBottomLeftRight",  
                                           borderStyle ="medium",
                                           valign = "center",
                                           textDecoration = "bold",
                                           wrapText = FALSE)
  
  for (i in pdfsPaths) {
    # Use the function called freqKeywords from Aux_pdfsExtractor
    outFull <- freqKeywords(i, "Inputs/Keywords/Keywords.csv", "english")
    outKeyList <- append(outKeyList, list(outFull))
    writeData(wBook, sheetName, outFull, startRow = 2, startCol = colCounter, 
              headerStyle = myHeaderStyle, borders = "surrounding",
              borderStyle = "medium")
    writeData(wBook, sheetName, tools::file_path_sans_ext(basename(i)), 
              startRow = 1, startCol = colCounter, borders = "surrounding",
              borderStyle = "medium")
    
    setColWidths(wBook, sheetName, cols = colCounter + 2, widths = "1")
    secondCount <- colCounter + 1
    mergeCells(wBook, sheetName, cols = colCounter:secondCount, rows=1)
    addStyle(wBook, sheet = sheetName, style = myPdfNamesStyle, rows = 1, 
             cols = colCounter)
    
    
    colCounter <-  colCounter + 3
  }
  # save workbook
  saveWorkbook(wBook, file = firstOutputPath, overwrite = TRUE)
  
  #-----------------------------------------------------------------------------
  # Second analysis - Not sorted keywords
  #-----------------------------------------------------------------------------
  
  # Create workbook and sheet for the excel file
  wBook <- createWorkbook()
  addWorksheet(wBook, sheetName)
  showGridLines(wBook, 1, showGridLines = FALSE)
  saveWorkbook(wBook, file = secondOutputPath, overwrite = TRUE)
  
  # Write results in the excel file and add format to it
  colCounter <- 2
  outKeyList <- list()
  myHeaderStyle <- openxlsx::createStyle(fontSize = 12, fontColour = "#FFFFFF", 
                                         halign = "center", fgFill = "#0000FF", 
                                         border = "TopBottomLeftRight", 
                                         borderStyle ="medium", 
                                         valign = "center", 
                                         textDecoration = "bold",
                                         wrapText = FALSE)
  
  mySecondHeaderStyle <- openxlsx::createStyle(fontSize = 12, 
                                               fontColour = "#FFFFFF", 
                                               halign = "center",
                                               fgFill = "#52A447", 
                                               border = "TopBottomLeftRight",  
                                               borderStyle ="medium",
                                               valign = "center", 
                                               textDecoration = "bold",
                                               wrapText = FALSE)
  
  myStatsTitStyle <- openxlsx::createStyle(fontSize = 12, 
                                           fontColour = "#FFFFFF", 
                                           halign = "center",
                                           fgFill = "#005C29", 
                                           border = "TopBottomLeftRight",  
                                           borderStyle ="medium",
                                           valign = "center", 
                                           textDecoration = "bold",
                                           wrapText = FALSE)
  
  myTableStyle <- openxlsx::createStyle(fontSize = 12, fontColour = "#000000", 
                                        halign = "center")
  
  myPdfNamesStyle <- openxlsx::createStyle(fontSize = 8, fontColour = "#FFFFFF",
                                           halign = "center",fgFill = "#990033", 
                                           border = "TopBottomLeftRight",  
                                           borderStyle ="medium",
                                           valign = "center", 
                                           textDecoration = "bold",
                                           wrapText = FALSE)
  
  for (i in pdfsPaths) {
    outFull <- freqKeywords_NotSorted(i, "Inputs/Keywords/Keywords.csv", 
                                      "english")
    outKeyList <- append(outKeyList, list(outFull))
    
    writeData(wBook, sheetName, outFull, startRow = 2, startCol = colCounter, 
              headerStyle = myHeaderStyle, borders = "surrounding",
              borderStyle = "medium")
    writeData(wBook, sheetName, "Freq", startRow = 2, startCol = colCounter)
    
    writeData(wBook, sheetName, tools::file_path_sans_ext(basename(i)), 
              startRow = 1, startCol = colCounter, borders = "surrounding",
              borderStyle = "medium")
    
    setColWidths(wBook, sheetName, cols = colCounter, widths = "12")
    addStyle(wBook, sheet = sheetName, style = myPdfNamesStyle, rows = 1, 
             cols = colCounter)
    
    colCounter <-  colCounter + 1
  }
  
  # Write keywords
  data.csv <- read.csv("Inputs/Keywords/Keywords.csv")
  keywords <- tolower(as.vector(data.csv[[1]]))
  data.csv[[1]] <- tolower(data.csv[[1]])
  outKey <- data.frame(data.csv[,1], row.names = NULL, stringsAsFactors = FALSE)
  writeData(wBook, sheetName, outKey, startRow = 2, startCol = 1, 
            borders = "surrounding",borderStyle = "medium")
  setColWidths(wBook, sheetName, cols = 1, widths = "20")
  
  writeData(wBook, sheetName, "KEYWORD", startRow = 2, startCol = 1, 
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = myHeaderStyle, rows = 2, cols = 1)
  
  saveWorkbook(wBook, file = secondOutputPath, overwrite = TRUE)
  
  # Compute some stats
  neededRange <- suppressMessages(read_excel(secondOutputPath, sheet="Keywords", 
                                             range = paste0("R2C2:R", length(keywords) + 2, "C", 
                                                            length(pdfsPaths) + 1)))
  
  minRows <- apply(neededRange, 1, FUN = min)
  maxRows <- apply(neededRange, 1, FUN = max)
  medianRows <- apply(neededRange, 1, FUN = median)
  medianDec <- as.numeric(format(round(medianRows, 2), nsmall = 2))
  meanRows <- apply(neededRange, 1, FUN = mean)
  meanDec <- as.numeric(format(round(meanRows, 2), nsmall = 2))
  percMean <- meanRows/sum(meanRows)*100
  percMedian <- medianRows/sum(medianRows)*100
  percMeanDec <- as.numeric(format(round(percMean, 2), nsmall = 2))
  percMedianDec <- as.numeric(format(round(percMedian, 2), nsmall = 2))
  
  # Write stats in the excel file
  colCounter <-  colCounter + 1
  tableEnd <- length(keywords) + 2
  writeData(wBook, sheetName, minRows, startRow = 3, startCol = colCounter, 
            borders = "surrounding",borderStyle = "medium")
  writeData(wBook, sheetName, maxRows, startRow = 3, startCol = colCounter + 1, 
            borders = "surrounding",borderStyle = "medium")
  writeData(wBook, sheetName, medianDec,startRow = 3,startCol = colCounter + 2, 
            borders = "surrounding",borderStyle = "medium")
  writeData(wBook, sheetName, percMedianDec,startRow=3,startCol = colCounter + 3, 
            borders = "surrounding",borderStyle = "medium")
  writeData(wBook, sheetName, meanDec,startRow = 3,startCol = colCounter + 4, 
            borders = "surrounding",borderStyle = "medium")
  writeData(wBook, sheetName, percMeanDec,startRow = 3,startCol = colCounter + 5, 
            borders = "surrounding",borderStyle = "medium")
  
  # Write stats's names in the file
  writeData(wBook, sheetName, "Min", startRow = 2, startCol = colCounter, 
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter)
  
  writeData(wBook, sheetName, "Max", startRow = 2, startCol = colCounter + 1, 
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter + 1)
  
  writeData(wBook, sheetName, "Median", startRow = 2, startCol = colCounter + 2,
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter + 2)
  
  writeData(wBook, sheetName,"% (Median)", startRow = 2,startCol = colCounter + 3, 
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter + 3)
  
  writeData(wBook, sheetName, "Mean", startRow = 2, startCol = colCounter + 4, 
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter + 4)
  
  writeData(wBook, sheetName,"% (Mean)", startRow = 2, startCol = colCounter + 5,
            borders = "surrounding",borderStyle = "medium")
  addStyle(wBook, sheet = sheetName, style = mySecondHeaderStyle, rows = 2, 
           cols = colCounter + 5)
  
  writeData(wBook, sheetName, "Keywords Stats", startRow = 1, 
            startCol = colCounter, borders = "surrounding",
            borderStyle = "medium")
  
  endColCounter <- colCounter + 5
  mergeCells(wBook, sheetName, cols = colCounter:endColCounter, rows=1)
  addStyle(wBook, sheet = sheetName, style = myStatsTitStyle, rows = 1, 
           cols = colCounter)
  
  # save the excel file
  saveWorkbook(wBook, file = secondOutputPath, overwrite = TRUE)
  
  cat(green("Done!\n"))
}
