# Cleaning around
# ==============================================================================
rm(list=ls()) # Removing variables
graphics.off() # Clearing plotting devices
cat("\014") # Clearing console
# ==============================================================================

# Load necessary packages
# ==============================================================================
reqpacks <- c("pdftools", "tm", "dplyr", "tibble", "tidytext", "ggplot2", "viridis", "crayon", "moments", "scales")
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

# Main function to analyze PDFs according to Zipf's law and generate outputs
# ==============================================================================
zipfLaw <- function() {
  # Define the base output folder path
  baseOutputPath <- file.path(getwd(), "2. Zipf law")
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  
  # Manage output folders
  unlink(file.path(baseOutputPath, "1. Plots"), recursive = TRUE, force = TRUE)
  unlink(file.path(baseOutputPath, "2. Plots vs LReg"), recursive = TRUE, force = TRUE)
  unlink(file.path(baseOutputPath, "Statistics"), recursive = TRUE, force = TRUE)
  dir.create(file.path(baseOutputPath, "1. Plots"), recursive = TRUE)
  dir.create(file.path(baseOutputPath, "2. Plots vs LReg"), recursive = TRUE)
  dir.create(file.path(baseOutputPath, "Statistics"), recursive = TRUE)
  
  # Extract and process PDF data
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  outFullList <- list()
  pdfNames <- c()
  for (i in pdfsPaths) {
    preparedData <- fullCleanWords_stopWKept(i)
    outFullList <- append(outFullList, list(preparedData))
    pdfNames <- c(pdfNames, tools::file_path_sans_ext(basename(i)))
  }
  
  # Prepare data for analysis
  outFullVect <- unlist(outFullList)
  tibble_pdf_text <- tibble(pdfName = pdfNames, text = outFullVect)
  write.csv(tibble_pdf_text, file.path(baseOutputPath, "Statistics/Extracted_Text_Data.csv"), row.names = FALSE)
  
  # Compute term frequency and total words per PDF
  tibble_pdf_word_freq <- tibble_pdf_text %>%
    unnest_tokens(word, text) %>%
    count(pdfName, word, sort = TRUE) %>%
    group_by(pdfName) %>%
    mutate(total = sum(n), `term frequency` = n / total, rank = row_number()) %>%
    ungroup()
  write.csv(tibble_pdf_word_freq, file.path(baseOutputPath, "Statistics/Term_Frequencies.csv"), row.names = FALSE)
  
  # Calculate statistics and generate plots for each PDF
  stats_data <- tibble_pdf_word_freq %>%
    group_by(pdfName) %>%
    do({
      data <- .
      pdf_lm <- lm(log10(`term frequency`) ~ log10(rank), data = data)
      pdf_intercept <- coef(pdf_lm)[1]
      pdf_slope <- coef(pdf_lm)[2]
      r_squared <- summary(pdf_lm)$r.squared
      skewness_val <- moments::skewness(data$`term frequency`, na.rm = TRUE)
      total_words <- sum(data$total)
      
      # Save individual plot without regression
      individual_plot_simple <- ggplot(data, aes(x = log10(rank), y = log10(`term frequency`))) +
        geom_line(color = "#0099f9") +
        labs(title = paste("Zipf's Law - ", data$pdfName[1]), subtitle = "Term Frequency Distribution") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "lightgrey"))
      ggsave(file.path(baseOutputPath, "1. Plots", paste0(data$pdfName[1], "_Zipf.png")), individual_plot_simple, width = 8, height = 6)
      
      # Save individual plot with regression
      individual_plot_regression <- ggplot(data, aes(x = log10(rank), y = log10(`term frequency`))) +
        geom_line(color = "#0099f9") +
        geom_abline(intercept = pdf_intercept, slope = pdf_slope, color = "red", linetype = 2) +
        labs(title = paste("Zipf's Law - ", data$pdfName[1]), subtitle = "With Linear Regression") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "lightgrey"))
      ggsave(file.path(baseOutputPath, "2. Plots vs LReg", paste0(data$pdfName[1], "_Plot_with_Regression.png")), individual_plot_regression, width = 8, height = 6)
      
      # Return a data frame for statistics
      tibble(
        pdfName = data$pdfName[1],
        R_squared = r_squared,
        Skewness = skewness_val,
        Total_Words = total_words
      )
    })
  
  # Save the computed stats to CSV
  write.csv(stats_data, file.path(baseOutputPath, "Statistics/All_Stats.csv"), row.names = FALSE)
  
  # Output completion message
  cat(crayon::green("Done! All PDFs Zipf's law plots with and without regression and CSVs saved in '", baseOutputPath, "'.\n"))
}

# Optionally, call the function to execute
#zipfLaw()

