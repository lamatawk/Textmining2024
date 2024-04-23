# cleaning around
# ==============================================================================
rm(list=ls()) # removing variables
graphics.off() # clearing plotting devices
cat("\014") # clearing console
# ==============================================================================



# load the extraction functions
# ==============================================================================
source("Aux_pdfsExtractor.R")
# ==============================================================================


LDA_analysis <- function(nbtopics = NULL) {
  
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old output files
  unlink("3. LDA/*")
  
  # Initialize vectors and lists
  outFullList <- list()
  pdfNames <- c()
  
  # Use the function called fullCleanWords_stopWRemoved from Aux_pdfsExtractor
  # For this analysis, the data is extracted from the PDFs removing stop words,
  # symbols and numbers
  for (i in pdfsPaths) {
    cleanDt <- fullCleanWords_stopWRemoved(i, "english")
    outFullList <- append(outFullList, list(cleanDt))
    pdfNames <- c(pdfNames, tools::file_path_sans_ext(basename(i)))
  }
  
  # Create a corpus
  allDataCorpus <- Corpus(VectorSource(outFullList))
  
  # Create a document term matrix
  docTerm <- DocumentTermMatrix(allDataCorpus, 
                                control = list(removePunctuation = TRUE,
                                               stopwords = TRUE,
                                               tolower = TRUE,
                                               stemming = TRUE,
                                               removeNumbers = TRUE
                                )) 
  
  # LDA -> finds a mixture of words associated with each topic, also a mixture
  # of topics that describes each document.
  
  # Generate LDA model, where k is the number of topics you want to create the
  # model with
  ldaModel <- LDA(docTerm, k = nbtopics, control = list(seed = 1234))
  
  #-----------------------------------------------------------------------------
  # Compute word-topic probabilities (called beta)-> prob of that work being 
  # generated from a topic
  #-----------------------------------------------------------------------------
  topic_term_beta <- tidy(ldaModel, matrix = "beta")
  
  # Select n terms (ex. 8 terms) that are most common within each topic
  top_n_beta <- topic_term_beta %>%
    group_by(topic) %>%
    slice_max(beta, n = 8) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  # Plot top terms
  plot_n_beta <- top_n_beta %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() + scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) +
    labs(title = "Word-Topic Prob") +
    theme(
      plot.title = element_text(color = "#0099f9", size = 8, face ="bold", 
                                hjust = 0.5),
      axis.text.x=element_text(size=4),
      axis.text.y=element_text(size=4),
      axis.title=element_text(size=4,face="bold"),
      strip.text = element_text(size=5)
    ) 
  
  print(plot_n_beta)
  
  png(file = paste("3. LDA/Word-Topic Prob.png"), width = 3.5, height = 2.5, 
      units = "in", res = 1100, pointsize = 4)
  print(plot_n_beta)
  dev.off()
  
  
  #-----------------------------------------------------------------------------
  # Compute document-topic probabilities (called gamma)-> % of the words in 
  # doc x that are generated from topic y
  #-----------------------------------------------------------------------------
  doc_topic_gamma <- tidy(ldaModel, matrix = "gamma")
  
  # Plot document-topic probabilities
  plot_doc_topic_gamma <-  doc_topic_gamma %>%
    mutate(title = reorder(document, gamma * topic)) %>%
    ggplot(aes(factor(topic), gamma)) +
    geom_boxplot() +
    facet_wrap(~ title) +
    labs(title = "Doc-Topic Prob", x = "topic", y = "Gamma")+
    theme(
      plot.title = element_text(color = "#0099f9", size = 8, face ="bold", 
                                hjust = 0.5),
      axis.text.x=element_text(size=4),
      axis.text.y=element_text(size=4),
      axis.title=element_text(size=4,face="bold"),
      strip.text = element_text(size=5)
    )
  
  print(plot_doc_topic_gamma)
  
  png(file = paste("3. LDA/Doc-Topic Prob.png"), width = 3.5, height = 2.5, 
      units = "in", res = 1100, pointsize = 4)
  print(plot_doc_topic_gamma)
  dev.off()
  
  cat(green("Done!\n"))
}