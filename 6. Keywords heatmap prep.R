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
  csvOutputPathFreq <- "5. Keywords stats/Keywords_Frequencies.csv"  # Correct path for frequency CSV
  csvOutputPathPresAbs <- "5. Keywords stats/Keywords_Presence_Absence.csv"  # Correct path for presence/absence CSV
  
  # Indicate sheet name for the excel file
  sheetName <- "Keywords"
  
  # Excel operations omitted for brevity...
  
  #-----------------------------------------------------------------------------
  # Transform and Export Data to CSV for Keyword Frequencies
  #-----------------------------------------------------------------------------
  data <- read.csv("Inputs/Keywords/Keywords.csv")
  keywords <- tolower(as.vector(data[[1]]))
  
  transformed_data_freq <- lapply(pdfsPaths, function(pdfPath) {
    pdf_data <- freqKeywords_NotSorted(pdfPath, "Inputs/Keywords/Keywords.csv", "english")
    return(pdf_data)  # Return frequency data
  })
  
  pdf_names <- tools::file_path_sans_ext(basename(pdfsPaths))
  transformed_df_freq <- data.frame(PDF_Name = pdf_names, do.call(rbind, transformed_data_freq))
  # Ensure column names assignment is correct
  if (length(keywords) == ncol(transformed_df_freq) - 1) {
    colnames(transformed_df_freq)[-1] <- keywords
  } else {
    warning("Keyword and column count mismatch")
  }
  
  write.csv(transformed_df_freq, csvOutputPathFreq, row.names = FALSE)
  
  #-----------------------------------------------------------------------------
  # Transform and Export Data to CSV for Presence/Absence
  #-----------------------------------------------------------------------------
  transformed_data_pres_abs <- lapply(pdfsPaths, function(pdfPath) {
    pdf_data <- freqKeywords_NotSorted(pdfPath, "Inputs/Keywords/Keywords.csv", "english")
    binary_data <- as.integer(pdf_data > 0)  # Convert frequencies to presence/absence
    return(binary_data)
  })
  
  transformed_df_pres_abs <- data.frame(PDF_Name = pdf_names, do.call(rbind, transformed_data_pres_abs))
  if (length(keywords) == ncol(transformed_df_pres_abs) - 1) {
    colnames(transformed_df_pres_abs)[-1] <- keywords
  } else {
    warning("Keyword and column count mismatch")
  }
  
  write.csv(transformed_df_pres_abs, csvOutputPathPresAbs, row.names = FALSE)
  
  cat("CSV files for keyword frequencies and presence/absence created successfully.\n")
}

# Run the function to process data
keywordsStats()
