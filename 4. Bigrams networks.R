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

bigramsNetworks <- function() {
  # Get input files
  pdfsFolderPath <- file.path(getwd(), "Inputs/PDFs")
  pdfsPaths <- list.files(pdfsFolderPath, pattern="*.pdf", full.names=TRUE)
  
  # Delete old output files
  unlink("4. Bigrams networks/*")
  
  # Initialize vectors and lists
  outFullList <- list()
  pdfNames <- c()
  
  # Use the function called fullCleanWords_stopWKept from Aux_pdfsExtractor
  # For the analysis, the data is extracted from the PDFs removing symbols and 
  # numbers but # keeping stop words (required for this procedure) 
  for (i in pdfsPaths) {
    woutStpWData <- fullCleanWords_stopWKept(i)
    outFullList <- append(outFullList, list(woutStpWData))
    pdfNames <- c(pdfNames, tools::file_path_sans_ext(basename(i)))
  }
  
  # Turn extracted data into tibble
  outFullVect <- unlist(outFullList)
  tibble_names_text <- tibble(pdfName = pdfNames, text = outFullVect)
  
  # Bigrams-> Analysis by pairs of adjacent words rather than by individual ones
  
  # Extract all the bigrams
  tibble_names_text_bigr <- tibble_names_text %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram))
  
  #-----------------------------------------------------------------------------
  # All-in-one plot - Bigrams Network
  #-----------------------------------------------------------------------------
  # Count bigrams
  tibble_names_text_bigr %>%
    count(bigram, sort = TRUE)
  
  # Splir bigrams into 2 columns
  tibble_sep_w1_w2 <- tibble_names_text_bigr %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  # Remove bigrams containing any stop word
  tibble_sep_w1_w2_noStopW <- tibble_sep_w1_w2 %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  # Count again bigrams
  tibble_sep_w1_w2_n <- tibble_sep_w1_w2_noStopW %>% 
    count(word1, word2, sort = TRUE)
  
  # Build graph (parameters “from”, “to”, and "n") Use bigrams with freq > n
  # (ex. n > 50)
  bigram_graph <- tibble_sep_w1_w2_n %>%
    filter(n > 50) %>%
    graph_from_data_frame()
  
  # Plot the All-in-one graph
  set.seed(2020)
  arrow_bigrams <- grid::arrow(type = "closed", length = unit(.05, "inches"))
  groupNetw <- ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = arrow_bigrams, end_cap = circle(.06, 'inches')) +
    geom_node_point(color = "#005C29", size = 0.5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 1.4) + 
    labs(
      title = "All PDFs",
      subtitle = "Bigrams network") +
    #theme_void()
    theme(
      
      plot.title = element_text(color = "#0099f9", size = 10, face ="bold", 
                                hjust = 0.5),
      plot.subtitle = element_text(size = 8, face ="bold", hjust = 0.5)
    )
  # save image
  png(file = paste("4. Bigrams networks/All-in-one.png"), width = 4.5, 
      height = 3, units = "in", res = 1100, pointsize = 4)
  suppressWarnings(print(groupNetw))
  dev.off()
  
  # Print the graph in the plot window
  suppressWarnings(print(groupNetw))
  
  #-----------------------------------------------------------------------------
  # Individual plots (one per pdf) - Bigrams Networks
  #-----------------------------------------------------------------------------
  # Count bigrams, per pdf
  tibble_names_bigr_n_perPdf <- tibble_names_text_bigr %>%
    count(pdfName, bigram) %>%
    arrange(desc(n))
  
  # Split bigrams into 2 columns, per pdf
  tibble_sep_w1_w2_perPdf <- tibble_names_bigr_n_perPdf %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  # Remove bigrams containing any stop word, per pdf
  tibble_sep_w1_w2_noStopW_perPdf <- tibble_sep_w1_w2_perPdf %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  # Split tibble per pdf
  tibble_split_perPdf <- split(tibble_sep_w1_w2_noStopW_perPdf, 
                               tibble_sep_w1_w2_noStopW_perPdf$pdfName)
  
  # Remove pdfNames from tibble (to prepare the tibble for the graph functions)
  tibble_remov_nameCol <- lapply(pdfNames, function(i) {
    select(tibble_split_perPdf[[i]],-pdfName)
  })
  
  # Build graph  per pdf (parameters “from”, “to”, and "n") Use bigrams with 
  # freq > n # (ex. n > 25)
  ind_pdfNames <- 1:length(pdfNames)
  table_nodes_perPDF <- lapply(ind_pdfNames, function(i) {
    tibble_remov_nameCol[[i]]%>%
      filter(n > 25)%>%
      graph_from_data_frame()
  })
  
  # Plot the individual networks
  set.seed(2017)
  arrow_bigrams <- grid::arrow(type = "closed", length = unit(.05, "inches"))
  indiv_networks <- lapply(ind_pdfNames, function(j) {
    ggraph(table_nodes_perPDF[[j]], layout = "fr") + {
      if(length(table_nodes_perPDF[[j]])>0) geom_edge_link(aes(edge_alpha = n), 
                                                           show.legend = FALSE,
                                                            arrow = arrow_bigrams, 
                                                           end_cap = circle(.07, 'inches'),
                                                            color='#FF0800')
      }+
      geom_node_point(color = "#0000FF") + {
        if(length(table_nodes_perPDF[[j]])>0) geom_node_text(aes(label = name),
                                                             vjust = 1, 
                                                             hjust = 1, 
                                                             size = 2)
      }+
      labs(
        title = pdfNames[j],
        subtitle = "Bigrams network") +
      #theme_void() +
      theme(
        
        plot.title = element_text(color = "#0099f9", size = 10, face ="bold", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 8, face ="bold", hjust = 0.5),
      )
    # Save the file
    ggsave(paste0("4. Bigrams networks/", pdfNames[j], ".png"), device = "png", 
           width=6,height=3)
  }) 
  
  # Print them in the plot window
  indiv_networks <- lapply(ind_pdfNames, function(j) {
    ggraph(table_nodes_perPDF[[j]], layout = "fr") + {
      if(length(table_nodes_perPDF[[j]])>0) geom_edge_link(aes(edge_alpha = n), 
                                                           show.legend = FALSE,
                                                           arrow = arrow_bigrams, 
                                                           end_cap = circle(.07, 'inches'),
                                                           color='#FF0800')
    }+
      geom_node_point(color = "#0000FF") + {
        if(length(table_nodes_perPDF[[j]])>0) geom_node_text(aes(label = name),  
                                                             vjust = 1, 
                                                             hjust = 1, size = 2)
      }+
      labs(
        title = pdfNames[j],
        subtitle = "Bigrams network") +
      #theme_void() +
      theme(
        
        plot.title = element_text(color = "#0099f9", size = 10, face ="bold", 
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 8, face ="bold", hjust = 0.5),
      )
    # Save the file
    #ggsave(paste0("4. Bigrams networks/", pdfNames[j], ".png"), device = "png", 
    #       width=6,height=3)
  }) 
  
  suppressWarnings(print(indiv_networks))
  
  cat(green("Done!\n"))
}