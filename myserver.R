############################
######## Libraries #########
############################

library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(ggrepel)
library(tibble)
library(tidyr)
library(shinyBS)
library(DT)
library(mailtoR)
library(network)
library(scales)
library(sna)
library(intergraph)
library(ggparty)
library(igraph)
library(visNetwork)
library(shinythemes)
library(viridis)
set.seed(666)
options(stringsAsFactors = FALSE)

############################
####### Variables ##########
############################

load("./InputFiles/01_All_InteractiveNetwork.RData")
load("./InputFiles/02_Splicing_InteractiveNetwork.RData")
load("./InputFiles/03_Export_InteractiveNetwork.RData")
load("./InputFiles/04_Ribosome_InteractiveNetwork.RData")
load("./InputFiles/05_Synthesis_InteractiveNetwork.RData")
load("./InputFiles/06_Metabolism_InteractiveNetwork.RData")
load("./InputFiles/07_Degradation_InteractiveNetwork.RData")

############################
######### Script ###########
############################

server <- function(input, output, session) {
  
  #####################
  #### All section ####

  
  PPI_All_vis_nodes_dataset  <- function(){
    
    PPI_All_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_All_vis_nodes$id)
    
    PPI_All_vis_nodes$title <- PPI_All_vis_nodes$id
    
    if (input$All_PPI_NodeSize == T) {
      
      PPI_All_vis_nodes$size <- PPI_All_vis_nodes$n
      
      PPI_All_vis_nodes[PPI_All_vis_nodes$size == 1, ]$size <- input$All_PPI_NodeSliderSize
      
    }else{
      
      PPI_All_vis_nodes$size <- input$All_PPI_NodeSliderSize
      
    }
    
    PPI_All_vis_nodes$color.background <- "#b4b4b4"
    PPI_All_vis_nodes$color.border <- "black"
    
    if (length(input$All_PPI_NodeColour) > 0) {
      
      if (grepl(pattern = "Multiple", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Multiple")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Multiple", ]$color.background <- "#a157fa"
          
        }
        
      }
      
      if (grepl(pattern = "Ribosome", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Ribosome")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Ribosome", ]$color.background <- "#66C2A5"
          
        }
        
      }
      
      if (grepl(pattern = "Splicing", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Splicing")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Splicing", ]$color.background <- "#E78AC3"
          
        }
        
      }
      
      if (grepl(pattern = "Synthesis", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Synthesis")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Synthesis", ]$color.background <- "#FC8D62"
          
        }
        
      }
      
      if (grepl(pattern = "Degradation", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Degradation")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Degradation", ]$color.background <- "#A6D854"
          
        }
        
      }
      
      if (grepl(pattern = "Export", x = paste0(input$All_PPI_NodeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_nodes$Functionality == "Export")) {
          
          PPI_All_vis_nodes[PPI_All_vis_nodes$Functionality == "Export", ]$color.background <- "#666666"
          
        }
        
      }
      
    }

    return(PPI_All_vis_nodes)
    
  }
  
  PPI_All_vis_edges_dataset  <- function(){
    
    PPI_All_vis_edges$color <- "#b4b4b4"
    
    if (length(input$All_PPI_EdgeColour) > 0) {
      
      if (grepl(pattern = "Multiple", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Multiple")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Multiple", ]$color <- "#a157fa"
          
        }
        
      }
      
      if (grepl(pattern = "Ribosome", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Ribosome")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Ribosome", ]$color <- "#66C2A5"
          
        }
        
      }
      
      if (grepl(pattern = "Splicing", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Splicing")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Splicing", ]$color <- "#E78AC3"
          
        }
        
      }
      
      if (grepl(pattern = "Synthesis", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Synthesis")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Synthesis", ]$color <- "#FC8D62"
          
        }
        
      }
      
      if (grepl(pattern = "Degradation", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Degradation")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Degradation", ]$color <- "#A6D854"
          
        }
        
      }
      
      if (grepl(pattern = "Export", x = paste0(input$All_PPI_EdgeColour,collapse = "_"))) {
        
        if (any(PPI_All_vis_edges$Functionality == "Export")) {
          
          PPI_All_vis_edges[PPI_All_vis_edges$Functionality == "Export", ]$color <- "#666666"
          
        }
        
      }
      
    }
    
    PPI_All_vis_edges$width <- input$All_PPI_EdgeThickness
    
    PPI_All_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_All_vis_edges$to)
    
    return(PPI_All_vis_edges)
    
  }
  
  output$All_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_All_vis_nodes <- PPI_All_vis_nodes_dataset()
    
    PPI_All_vis_edges <- PPI_All_vis_edges_dataset()
    
    layout <- input$All_PPI_NetworkLayout
    
    visNetwork(PPI_All_vis_nodes, PPI_All_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_All_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "") 
  })
  
  output$All_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_All_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_All_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$All_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_All_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_All_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )

  RDI_All_vis_nodes_dataset  <- function(){
    
    RDI_All_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_All_vis_nodes$id)
    
    RDI_All_vis_nodes$title <- RDI_All_vis_nodes$id
    
    if (input$All_RDI_NodeSize == T) {
      
      RDI_All_vis_nodes$size <- RDI_All_vis_nodes$n
      
      RDI_All_vis_nodes[RDI_All_vis_nodes$size == 1, ]$size <- input$All_RDI_NodeSliderSize
      
    }else{
      
      RDI_All_vis_nodes$size <- input$All_RDI_NodeSliderSize
      
    }
    
    RDI_All_vis_nodes$color.background <- "#b4b4b4"
    RDI_All_vis_nodes$color.border <- "black"
    
    if (length(input$All_RDI_NodeColour) > 0) {
      
      if (grepl(pattern = "Multiple", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Multiple")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Multiple", ]$color.background <- "#a157fa"
          
        }
        
      }
      
      if (grepl(pattern = "Metabolism", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Metabolism")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Metabolism", ]$color.background <- "#E5C494"
          
        }
        
      }
      
      if (grepl(pattern = "Ribosome", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Ribosome")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Ribosome", ]$color.background <- "#66C2A5"
          
        }
        
      }
      
      if (grepl(pattern = "Splicing", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Splicing")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Splicing", ]$color.background <- "#E78AC3"
          
        }

      }
      
      if (grepl(pattern = "Synthesis", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Synthesis")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Synthesis", ]$color.background <- "#FC8D62"
          
        }
        
      }
      
      if (grepl(pattern = "Degradation", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Degradation")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Degradation", ]$color.background <- "#A6D854"
          
        }

      }
      
      if (grepl(pattern = "Export", x = paste0(input$All_RDI_NodeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_nodes$Functionality == "Export")) {
          
          RDI_All_vis_nodes[RDI_All_vis_nodes$Functionality == "Export", ]$color.background <- "#666666"
          
        }

      }
      
    }
    
    return(RDI_All_vis_nodes)
    
  }
  
  RDI_All_vis_edges_dataset  <- function(){
    
    RDI_All_vis_edges$color <- "#b4b4b4"
    
    if (length(input$All_RDI_EdgeColour) > 0) {
      
      if (grepl(pattern = "Multiple", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Multiple")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Multiple", ]$color <- "#a157fa"
          
        }

      }
      
      if (grepl(pattern = "Ribosome", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Ribosome")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Ribosome", ]$color <- "#66C2A5"
          
        }

      }
      
      if (grepl(pattern = "Metabolism", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Metabolism")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Metabolism", ]$color <- "#E5C494"
          
        }
        
      }
      
      if (grepl(pattern = "Splicing", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Splicing")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Splicing", ]$color <- "#E78AC3"
          
        }

      }
      
      if (grepl(pattern = "Synthesis", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Synthesis")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Synthesis", ]$color <- "#FC8D62"
          
        }
        
      }
      
      if (grepl(pattern = "Degradation", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Degradation")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Degradation", ]$color <- "#A6D854"
          
        }
        
      }
      
      if (grepl(pattern = "Export", x = paste0(input$All_RDI_EdgeColour,collapse = "_"))) {
        
        if (any(RDI_All_vis_edges$Functionality == "Export")) {
          
          RDI_All_vis_edges[RDI_All_vis_edges$Functionality == "Export", ]$color <- "#666666"
          
        }

      }
      
    }
    
    RDI_All_vis_edges$width <- input$All_RDI_EdgeThickness
    
    RDI_All_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_All_vis_edges$to)
    
    return(RDI_All_vis_edges)
    
  }
  
  output$All_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_All_vis_nodes <- RDI_All_vis_nodes_dataset()
    
    RDI_All_vis_edges <- RDI_All_vis_edges_dataset()
    
    layout <- input$All_RDI_NetworkLayout
    
    visNetwork(RDI_All_vis_nodes, RDI_All_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                             "_All_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "") 
 
  })

  output$All_RDI_NodeDownload <- downloadHandler(

    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                  "_All_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_All_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$All_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_All_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_All_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  #####################
  
  ##########################
  #### Splicing section ####
  
  PPI_Splicing_vis_nodes_dataset  <- function(){
    
    PPI_Splicing_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Splicing_vis_nodes$id)
    
    PPI_Splicing_vis_nodes$title <- PPI_Splicing_vis_nodes$id
    
    if (input$Splicing_PPI_NodeSize == T) {
      
      PPI_Splicing_vis_nodes$size <- PPI_Splicing_vis_nodes$n
      
      PPI_Splicing_vis_nodes[PPI_Splicing_vis_nodes$size == 1, ]$size <- input$Splicing_PPI_NodeSliderSize
      
    }else{
      
      PPI_Splicing_vis_nodes$size <- input$Splicing_PPI_NodeSliderSize
      
    }
    
    PPI_Splicing_vis_nodes$color.background <- "#A6CEE3"
    PPI_Splicing_vis_nodes$color.border <- "black"
    
    if (length(input$Splicing_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Splicing_PPI_NodeColour))) {
        
        if (any(PPI_Splicing_vis_nodes$type == "Bait")) {
          
          PPI_Splicing_vis_nodes[PPI_Splicing_vis_nodes$type == "Bait", ]$color.background <- "#E78AC3"
          
        }
 
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Splicing_PPI_NodeColour))) {
        
        if (any(PPI_Splicing_vis_nodes$type == "BioGRID")) {
          
          PPI_Splicing_vis_nodes[PPI_Splicing_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }

      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Splicing_PPI_NodeColour))) {
        
        if (any(PPI_Splicing_vis_nodes$type == "RBP census")) {
          
          PPI_Splicing_vis_nodes[PPI_Splicing_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
 
      }
      
      if (any(grepl(pattern = "\\&", x = input$Splicing_PPI_NodeColour))) {
        
        if (any(PPI_Splicing_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Splicing_vis_nodes[PPI_Splicing_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }

      }
      
    }
    
    return(PPI_Splicing_vis_nodes)
    
  }
  
  PPI_Splicing_vis_edges_dataset  <- function(){
    
    PPI_Splicing_vis_edges$color <- "#b4b4b4"
    
    PPI_Splicing_vis_edges$width <- input$Splicing_PPI_EdgeThickness
    
    PPI_Splicing_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Splicing_vis_edges$to)
    
    return(PPI_Splicing_vis_edges)
    
  }
  
  output$Splicing_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Splicing_vis_nodes <- PPI_Splicing_vis_nodes_dataset()
    
    PPI_Splicing_vis_edges <- PPI_Splicing_vis_edges_dataset()
    
    layout <- input$Splicing_PPI_NetworkLayout
    
    visNetwork(PPI_Splicing_vis_nodes, PPI_Splicing_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Splicing_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
      
    
    # net <- graph_from_data_frame(d = PPI_Splicing_vis_edges, vertices = PPI_Splicing_vis_nodes)
    # 
    # cytoscapePing()
    # 
    # createNetworkFromIgraph(net,"myIgraph")
    
    
  })
  
  output$Splicing_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Splicing_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Splicing_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Splicing_vis_nodes_dataset  <- function(){
    
    RDI_Splicing_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Splicing_vis_nodes$id)
    
    RDI_Splicing_vis_nodes$title <- RDI_Splicing_vis_nodes$id
    
    if (input$Splicing_RDI_NodeSize == T) {
      
      RDI_Splicing_vis_nodes$size <- RDI_Splicing_vis_nodes$n
      
      RDI_Splicing_vis_nodes[RDI_Splicing_vis_nodes$size == 1, ]$size <- input$Splicing_RDI_NodeSliderSize
      
    }else{
      
      RDI_Splicing_vis_nodes$size <- input$Splicing_RDI_NodeSliderSize
      
    }
    
    RDI_Splicing_vis_nodes$color.background <- "#A6CEE3"
    RDI_Splicing_vis_nodes$color.border <- "black"
    
    if (length(input$Splicing_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Splicing_RDI_NodeColour))) {
        
        if (any(RDI_Splicing_vis_nodes$type == "Bait")) {
          
          RDI_Splicing_vis_nodes[RDI_Splicing_vis_nodes$type == "Bait", ]$color.background <- "#E78AC3"
          
        }

      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Splicing_RDI_NodeColour))) {
        
        if (any(RDI_Splicing_vis_nodes$type == "BioGRID")) {
          
          RDI_Splicing_vis_nodes[RDI_Splicing_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }

      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Splicing_RDI_NodeColour))) {
        
        if (any(RDI_Splicing_vis_nodes$type == "RBP census")) {
          
          RDI_Splicing_vis_nodes[RDI_Splicing_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
 
      }
      
      if (any(grepl(pattern = "\\&", x = input$Splicing_RDI_NodeColour))) {
        
        if (any(RDI_Splicing_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Splicing_vis_nodes[RDI_Splicing_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }

      }
      
    }
    
    return(RDI_Splicing_vis_nodes)
    
  }
  
  RDI_Splicing_vis_edges_dataset  <- function(){
    
    RDI_Splicing_vis_edges$color <- "#b4b4b4"
    
    RDI_Splicing_vis_edges$width <- input$Splicing_RDI_EdgeThickness
    
    RDI_Splicing_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Splicing_vis_edges$to)
    
    return(RDI_Splicing_vis_edges)
    
  }
  
  output$Splicing_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Splicing_vis_nodes <- RDI_Splicing_vis_nodes_dataset()
    
    RDI_Splicing_vis_edges <- RDI_Splicing_vis_edges_dataset()
    
    layout <- input$Splicing_RDI_NetworkLayout
    
    visNetwork(RDI_Splicing_vis_nodes, RDI_Splicing_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Splicing_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")

    
  })
  
  output$Splicing_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Splicing_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Splicing_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Splicing_vis_nodes_dataset  <- function(){
    
    Both_Splicing_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Splicing_vis_nodes$id)
    
    Both_Splicing_vis_nodes$title <- Both_Splicing_vis_nodes$id
    
    if (input$Splicing_Both_NodeSize == T) {
      
      Both_Splicing_vis_nodes$size <- Both_Splicing_vis_nodes$n
      
      Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$size == 1, ]$size <- input$Splicing_Both_NodeSliderSize
      
    }else{
      
      Both_Splicing_vis_nodes$size <- input$Splicing_Both_NodeSliderSize
      
    }
    
    Both_Splicing_vis_nodes$color.background <- "#A6CEE3"
    Both_Splicing_vis_nodes$color.border <- "black"
    
    if (length(input$Splicing_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Splicing_Both_NodeColour))) {
        
        if (any(Both_Splicing_vis_nodes$type == "Bait")) {
          
          Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$type == "Bait", ]$color.background <- "#E78AC3"
          
        }

      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Splicing_Both_NodeColour))) {
        
        if (any(Both_Splicing_vis_nodes$type == "BioGRID")) {
          
          Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }

      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Splicing_Both_NodeColour))) {
        
        if (any(Both_Splicing_vis_nodes$type == "RBP census")) {
          
          Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }

      }
      
      if (any(grepl(pattern = "\\&", x = input$Splicing_Both_NodeColour))) {
        
        if (any(Both_Splicing_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }

      }
      
    }
    
    return(Both_Splicing_vis_nodes)
    
  }
  
  Both_Splicing_vis_edges_dataset  <- function(){
    
    Both_Splicing_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Splicing_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Splicing_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Splicing_vis_edges$type == "Both")) {
          
          Both_Splicing_vis_edges[Both_Splicing_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Splicing_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Splicing_vis_edges$type == "PPI")) {
          
          Both_Splicing_vis_edges[Both_Splicing_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }

      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Splicing_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Splicing_vis_edges$type == "RDI")) {
          
          Both_Splicing_vis_edges[Both_Splicing_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }

      }
      
    }
    
    Both_Splicing_vis_edges$width <- input$Splicing_Both_EdgeThickness
    
    Both_Splicing_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Splicing_vis_edges$to)
    
    return(Both_Splicing_vis_edges)
    
  }
  
  output$Splicing_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Splicing_vis_nodes <- Both_Splicing_vis_nodes_dataset()
    
    Both_Splicing_vis_edges <- Both_Splicing_vis_edges_dataset()
    
    layout <- input$Splicing_Both_NetworkLayout
    
    visNetwork(Both_Splicing_vis_nodes, Both_Splicing_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Splicing_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")

  })
  
  output$Splicing_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Splicing_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Splicing_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexSplicingRatios[ComplexSplicingRatios$ratio > input$Splicing_Complex_FilterSlider,]$Complex
    
    SplicingWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    SplicingWodakTable <- SplicingWodakTable[complete.cases(SplicingWodakTable),]
    
    SplicingWodakTable <- SplicingWodakTable[,3:8]
    
    ratio_table <- ComplexSplicingRatios[ComplexSplicingRatios$Complex %in% SplicingWodakTable$Complex,]
    
    SplicingWodakTable <- merge(SplicingWodakTable, ratio_table, by = "Complex")
    
    SplicingWodakTable <- SplicingWodakTable[,c(-7,-8)]
    
    SplicingWodakTable <- SplicingWodakTable %>% arrange(ratio)
    
    SplicingWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Splicing_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexSplicingRatios[ComplexSplicingRatios$ratio > input$Splicing_Complex_FilterSlider,]$Complex
    
    ComplexSplicingDotPlot <- ComplexSplicingRatios[ComplexSplicingRatios$Complex %in% WodakFilter,]
    
    ComplexSplicingDotPlot <- ComplexSplicingDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexSplicingDotPlot$Complex <- factor(x = ComplexSplicingDotPlot$Complex, levels = ComplexSplicingDotPlot$Complex)
    
    ComplexSplicingDotPlot$ratio <- round(ComplexSplicingDotPlot$ratio, digits = 2)
    
    ComplexSplicingDotPlot <- ggplot(ComplexSplicingDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexSplicingDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexSplicingDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexSplicingDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 30, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexSplicingDotPlot <- ggplotly(ComplexSplicingDotPlot, tooltip = "text")
    
    print(ComplexSplicingDotPlot)
    
  })
  
  output$Splicing_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexSplicingRatios[ComplexSplicingRatios$ratio > input$Splicing_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexSplicingRatios[ComplexSplicingRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Splicing_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Splicing_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Splicing_Complex_RadioButton_Selection) > 0) {
      
      # input$Splicing_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]$Name
      
      Complex_Splicing_vis_edges <- Both_Splicing_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$filter == T,]
      
      Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[,1:3]
      
      Complex_Splicing_vis_nodes <- Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$id %in% unique(c(Complex_Splicing_vis_edges$from, Complex_Splicing_vis_edges$to)), ]
      
      Complex_Splicing_vis_nodes$type <- "Prey"
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                                                                                complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                                                                                complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      SplicingTargets <- Complex_Splicing_vis_nodes[grep(pattern = "Bait", x = Complex_Splicing_vis_nodes$type),]$id
      
      ComplexSplicingBarPlot <- c()
      
      for (j in 1:length(SplicingTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$from == SplicingTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexSplicingRatios[ComplexSplicingRatios$Complex == input$Splicing_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = SplicingTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexSplicingBarPlot <- rbind(ComplexSplicingBarPlot, loop_df)
        
      }
      
      ComplexSplicingBarPlot <- ComplexSplicingBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexSplicingBarPlot <- ComplexSplicingBarPlot[ComplexSplicingBarPlot$ratio > 0, ]
      
      ComplexSplicingBarPlot$Target <- factor(x = ComplexSplicingBarPlot$Target, levels = unique(ComplexSplicingBarPlot$Target))
      
      ComplexSplicingBarPlot <- ggplot(ComplexSplicingBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      # ComplexSplicingBarPlot <- ggplotly(ComplexSplicingBarPlot)
      
      print(ComplexSplicingBarPlot)
      
    }
    
  })
  
  Complex_Splicing_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]$Name
    
    Complex_Splicing_vis_edges <- Both_Splicing_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$filter == T,]
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[,1:3]
    
    Complex_Splicing_vis_nodes <- Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$id %in% unique(c(Complex_Splicing_vis_edges$from, Complex_Splicing_vis_edges$to)), ]
    
    Complex_Splicing_vis_nodes$type <- "Prey"
    
    Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                                                                              complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                                                                              complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Splicing_Complex_NodeSize == T) {
      
      Complex_Splicing_vis_nodes$size <- Complex_Splicing_vis_nodes$n
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$size == 1, ]$size <- input$Splicing_Complex_NodeSliderSize
      
    }else{
      
      Complex_Splicing_vis_nodes$size <- input$Splicing_Complex_NodeSliderSize
      
    }
    
    Complex_Splicing_vis_nodes$color.background <- "#b4b4b4"
    Complex_Splicing_vis_nodes$color.border <- "black"
    
    if (length(input$Splicing_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Splicing_Complex_NodeColour))) {
        
        if (any(Complex_Splicing_vis_nodes$type == "Bait")) {
          
          Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Splicing_Complex_NodeColour))) {
        
        if (any(Complex_Splicing_vis_nodes$type == "Prey")) {
          
          Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Splicing_Complex_NodeColour))) {
        
        if (any(Complex_Splicing_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Splicing_Complex_NodeColour))) {
        
        if (any(Complex_Splicing_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Splicing_vis_nodes)
    
  }
  
  Complex_Splicing_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]$Name
    
    Complex_Splicing_vis_edges <- Both_Splicing_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$filter == T,]
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[,1:3]
    
    Complex_Splicing_vis_nodes <- Both_Splicing_vis_nodes[Both_Splicing_vis_nodes$id %in% unique(c(Complex_Splicing_vis_edges$from, Complex_Splicing_vis_edges$to)), ]
    
    Complex_Splicing_vis_nodes$type <- "Prey"
    
    Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey",]$id),
                                                                              complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% intersect(unique(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id),
                                                                              complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Splicing_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Splicing_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Splicing_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Splicing_vis_edges$type == "Both")) {
          
          Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
    
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Splicing_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Splicing_vis_edges$type == "PPI")) {
          
          Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Splicing_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Splicing_vis_edges$type == "RDI")) {
          
          Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
  
      }
      
    }
    
    Complex_Splicing_vis_edges$width <- input$Splicing_Complex_EdgeThickness
    
    Complex_Splicing_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Splicing_vis_edges$to)
    
    return(Complex_Splicing_vis_edges)
    
  }
  
  output$Splicing_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes_dataset_v01()
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges_dataset_v01()
    
    layout <- input$Splicing_Complex_NetworkLayout
    
    visNetwork(Complex_Splicing_vis_nodes, Complex_Splicing_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Splicing_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Splicing_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Splicing_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Splicing_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Splicing_vis_nodes_dataset_v02  <- function(){
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes_dataset_v01()
    
    Complex_Splicing_vis_nodes$size <- input$Splicing_Complex_NodeSliderSize
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges_dataset_v01()
    
    SplicingTargets <- Complex_Splicing_vis_nodes[grep(pattern = "Bait", x = Complex_Splicing_vis_nodes$type),]$id
    
    ComplexSplicingBarPlot <- c()
    
    for (j in 1:length(SplicingTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$from == SplicingTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexSplicingRatios[ComplexSplicingRatios$Complex == input$Splicing_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = SplicingTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexSplicingBarPlot <- rbind(ComplexSplicingBarPlot, loop_df)
      
    }
    
    ComplexSplicingBarPlot <- ComplexSplicingBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexSplicingBarPlot[ComplexSplicingBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id[Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$from %in% Complex_Splicing_vis_nodes$id,]
    
    return(Complex_Splicing_vis_nodes)
    
  }
  
  Complex_Splicing_vis_edges_dataset_v02  <- function(){
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes_dataset_v01()
    
    Complex_Splicing_vis_nodes$size <- input$Splicing_Complex_NodeSliderSize
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges_dataset_v01()
    
    SplicingTargets <- Complex_Splicing_vis_nodes[grep(pattern = "Bait", x = Complex_Splicing_vis_nodes$type),]$id
    
    ComplexSplicingBarPlot <- c()
    
    for (j in 1:length(SplicingTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Splicing_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$from == SplicingTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexSplicingRatios[ComplexSplicingRatios$Complex == input$Splicing_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = SplicingTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexSplicingBarPlot <- rbind(ComplexSplicingBarPlot, loop_df)
      
    }
    
    ComplexSplicingBarPlot <- ComplexSplicingBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexSplicingBarPlot[ComplexSplicingBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id[Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes[Complex_Splicing_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$from %in% Complex_Splicing_vis_nodes$id,]
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges[Complex_Splicing_vis_edges$to %in% Complex_Splicing_vis_nodes$id,]
    
    return(Complex_Splicing_vis_edges)
    
  }
  
  output$Splicing_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Splicing_vis_nodes <- Complex_Splicing_vis_nodes_dataset_v02()
    
    Complex_Splicing_vis_edges <- Complex_Splicing_vis_edges_dataset_v02()
    
    layout <- input$Splicing_Complex_NetworkLayout
    
    visNetwork(Complex_Splicing_vis_nodes, Complex_Splicing_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Splicing_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Splicing_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Splicing_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Splicing_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Splicing_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Splicing_Complex_BarplotTitle <- renderText({
    paste0(input$Splicing_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Splicing_Complex_Networkv01Title <- renderText({
    paste0(input$Splicing_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Splicing_Complex_Networkv02Title <- renderText({
    paste0(input$Splicing_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  
  ##########################
  
  ########################
  #### Export section ####
  
  PPI_Export_vis_nodes_dataset  <- function(){
    
    PPI_Export_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Export_vis_nodes$id)
    
    PPI_Export_vis_nodes$title <- PPI_Export_vis_nodes$id
    
    if (input$Export_PPI_NodeSize == T) {
      
      PPI_Export_vis_nodes$size <- PPI_Export_vis_nodes$n
      
      PPI_Export_vis_nodes[PPI_Export_vis_nodes$size == 1, ]$size <- input$Export_PPI_NodeSliderSize
      
    }else{
      
      PPI_Export_vis_nodes$size <- input$Export_PPI_NodeSliderSize
      
    }
    
    PPI_Export_vis_nodes$color.background <- "#A6CEE3"
    PPI_Export_vis_nodes$color.border <- "black"
    
    if (length(input$Export_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Export_PPI_NodeColour))) {
        
        if (any(PPI_Export_vis_nodes$type == "Bait")) {
          
          PPI_Export_vis_nodes[PPI_Export_vis_nodes$type == "Bait", ]$color.background <- "#666666"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Export_PPI_NodeColour))) {
        
        if (any(PPI_Export_vis_nodes$type == "BioGRID")) {
          
          PPI_Export_vis_nodes[PPI_Export_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Export_PPI_NodeColour))) {
        
        if (any(PPI_Export_vis_nodes$type == "RBP census")) {
          
          PPI_Export_vis_nodes[PPI_Export_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Export_PPI_NodeColour))) {
        
        if (any(PPI_Export_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Export_vis_nodes[PPI_Export_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(PPI_Export_vis_nodes)
    
  }
  
  PPI_Export_vis_edges_dataset  <- function(){
    
    PPI_Export_vis_edges$color <- "#b4b4b4"
    
    PPI_Export_vis_edges$width <- input$Export_PPI_EdgeThickness
    
    PPI_Export_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Export_vis_edges$to)
    
    return(PPI_Export_vis_edges)
    
  }
  
  output$Export_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Export_vis_nodes <- PPI_Export_vis_nodes_dataset()
    
    PPI_Export_vis_edges <- PPI_Export_vis_edges_dataset()
    
    layout <- input$Export_PPI_NetworkLayout
    
    visNetwork(PPI_Export_vis_nodes, PPI_Export_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Export_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Export_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Export_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Export_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Export_vis_nodes_dataset  <- function(){
    
    RDI_Export_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Export_vis_nodes$id)
    
    RDI_Export_vis_nodes$title <- RDI_Export_vis_nodes$id
    
    if (input$Export_RDI_NodeSize == T) {
      
      RDI_Export_vis_nodes$size <- RDI_Export_vis_nodes$n
      
      RDI_Export_vis_nodes[RDI_Export_vis_nodes$size == 1, ]$size <- input$Export_RDI_NodeSliderSize
      
    }else{
      
      RDI_Export_vis_nodes$size <- input$Export_RDI_NodeSliderSize
      
    }
    
    RDI_Export_vis_nodes$color.background <- "#A6CEE3"
    RDI_Export_vis_nodes$color.border <- "black"
    
    if (length(input$Export_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Export_RDI_NodeColour))) {
        
        if (any(RDI_Export_vis_nodes$type == "Bait")) {
          
          RDI_Export_vis_nodes[RDI_Export_vis_nodes$type == "Bait", ]$color.background <- "#666666"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Export_RDI_NodeColour))) {
        
        if (any(RDI_Export_vis_nodes$type == "BioGRID")) {
          
          RDI_Export_vis_nodes[RDI_Export_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Export_RDI_NodeColour))) {
        
        if (any(RDI_Export_vis_nodes$type == "RBP census")) {
          
          RDI_Export_vis_nodes[RDI_Export_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Export_RDI_NodeColour))) {
        
        if (any(RDI_Export_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Export_vis_nodes[RDI_Export_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(RDI_Export_vis_nodes)
    
  }
  
  RDI_Export_vis_edges_dataset  <- function(){
    
    RDI_Export_vis_edges$color <- "#b4b4b4"
    
    RDI_Export_vis_edges$width <- input$Export_RDI_EdgeThickness
    
    RDI_Export_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Export_vis_edges$to)
    
    return(RDI_Export_vis_edges)
    
  }
  
  output$Export_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Export_vis_nodes <- RDI_Export_vis_nodes_dataset()
    
    RDI_Export_vis_edges <- RDI_Export_vis_edges_dataset()
    
    layout <- input$Export_RDI_NetworkLayout
    
    visNetwork(RDI_Export_vis_nodes, RDI_Export_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Export_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
  })
  
  output$Export_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Export_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Export_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Export_vis_nodes_dataset  <- function(){
    
    Both_Export_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Export_vis_nodes$id)
    
    Both_Export_vis_nodes$title <- Both_Export_vis_nodes$id
    
    if (input$Export_Both_NodeSize == T) {
      
      Both_Export_vis_nodes$size <- Both_Export_vis_nodes$n
      
      Both_Export_vis_nodes[Both_Export_vis_nodes$size == 1, ]$size <- input$Export_Both_NodeSliderSize
      
    }else{
      
      Both_Export_vis_nodes$size <- input$Export_Both_NodeSliderSize
      
    }
    
    Both_Export_vis_nodes$color.background <- "#A6CEE3"
    Both_Export_vis_nodes$color.border <- "black"
    
    if (length(input$Export_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Export_Both_NodeColour))) {
        
        if (any(Both_Export_vis_nodes$type == "Bait")) {
          
          Both_Export_vis_nodes[Both_Export_vis_nodes$type == "Bait", ]$color.background <- "#666666"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Export_Both_NodeColour))) {
        
        if (any(Both_Export_vis_nodes$type == "BioGRID")) {
          
          Both_Export_vis_nodes[Both_Export_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Export_Both_NodeColour))) {
        
        if (any(Both_Export_vis_nodes$type == "RBP census")) {
          
          Both_Export_vis_nodes[Both_Export_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Export_Both_NodeColour))) {
        
        if (any(Both_Export_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Export_vis_nodes[Both_Export_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(Both_Export_vis_nodes)
    
  }
  
  Both_Export_vis_edges_dataset  <- function(){
    
    Both_Export_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Export_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Export_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Export_vis_edges$type == "Both")) {
          
          Both_Export_vis_edges[Both_Export_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Export_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Export_vis_edges$type == "PPI")) {
          
          Both_Export_vis_edges[Both_Export_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Export_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Export_vis_edges$type == "RDI")) {
          
          Both_Export_vis_edges[Both_Export_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Both_Export_vis_edges$width <- input$Export_Both_EdgeThickness
    
    Both_Export_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Export_vis_edges$to)
    
    return(Both_Export_vis_edges)
    
  }
  
  output$Export_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Export_vis_nodes <- Both_Export_vis_nodes_dataset()
    
    Both_Export_vis_edges <- Both_Export_vis_edges_dataset()
    
    layout <- input$Export_Both_NetworkLayout
    
    visNetwork(Both_Export_vis_nodes, Both_Export_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Export_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Export_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Export_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Export_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexExportRatios[ComplexExportRatios$ratio > input$Export_Complex_FilterSlider,]$Complex
    
    ExportWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    ExportWodakTable <- ExportWodakTable[complete.cases(ExportWodakTable),]
    
    ExportWodakTable <- ExportWodakTable[,3:8]
    
    ratio_table <- ComplexExportRatios[ComplexExportRatios$Complex %in% ExportWodakTable$Complex,]
    
    ExportWodakTable <- merge(ExportWodakTable, ratio_table, by = "Complex")
    
    ExportWodakTable <- ExportWodakTable[,c(-7,-8)]
    
    ExportWodakTable <- ExportWodakTable %>% arrange(ratio)
    
    ExportWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Export_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexExportRatios[ComplexExportRatios$ratio > input$Export_Complex_FilterSlider,]$Complex
    
    ComplexExportDotPlot <- ComplexExportRatios[ComplexExportRatios$Complex %in% WodakFilter,]
    
    ComplexExportDotPlot <- ComplexExportDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexExportDotPlot$Complex <- factor(x = ComplexExportDotPlot$Complex, levels = ComplexExportDotPlot$Complex)
    
    ComplexExportDotPlot$ratio <- round(ComplexExportDotPlot$ratio, digits = 2)
    
    ComplexExportDotPlot <- ggplot(ComplexExportDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexExportDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexExportDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexExportDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 30, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexExportDotPlot <- ggplotly(ComplexExportDotPlot, tooltip = "text")
    
    print(ComplexExportDotPlot)
    
  })
  
  output$Export_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexExportRatios[ComplexExportRatios$ratio > input$Export_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexExportRatios[ComplexExportRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Export_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Export_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Export_Complex_RadioButton_Selection) > 0) {
      
      # input$Export_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]$Name
      
      Complex_Export_vis_edges <- Both_Export_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$filter == T,]
      
      Complex_Export_vis_edges <- Complex_Export_vis_edges[,1:3]
      
      Complex_Export_vis_nodes <- Both_Export_vis_nodes[Both_Export_vis_nodes$id %in% unique(c(Complex_Export_vis_edges$from, Complex_Export_vis_edges$to)), ]
      
      Complex_Export_vis_nodes$type <- "Prey"
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                                                                                      complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                                                                                      complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      ExportTargets <- Complex_Export_vis_nodes[grep(pattern = "Bait", x = Complex_Export_vis_nodes$type),]$id
      
      ComplexExportBarPlot <- c()
      
      for (j in 1:length(ExportTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Export_vis_edges[Complex_Export_vis_edges$from == ExportTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexExportRatios[ComplexExportRatios$Complex == input$Export_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = ExportTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexExportBarPlot <- rbind(ComplexExportBarPlot, loop_df)
        
      }
      
      ComplexExportBarPlot <- ComplexExportBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexExportBarPlot <- ComplexExportBarPlot[ComplexExportBarPlot$ratio > 0, ]
      
      ComplexExportBarPlot$Target <- factor(x = ComplexExportBarPlot$Target, levels = unique(ComplexExportBarPlot$Target))
      
      ComplexExportBarPlot <- ggplot(ComplexExportBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      print(ComplexExportBarPlot)
      
    }
    
  })
  
  Complex_Export_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]$Name
    
    Complex_Export_vis_edges <- Both_Export_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$filter == T,]
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[,1:3]
    
    Complex_Export_vis_nodes <- Both_Export_vis_nodes[Both_Export_vis_nodes$id %in% unique(c(Complex_Export_vis_edges$from, Complex_Export_vis_edges$to)), ]
    
    Complex_Export_vis_nodes$type <- "Prey"
    
    Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                                                                                    complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                                                                                    complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Export_Complex_NodeSize == T) {
      
      Complex_Export_vis_nodes$size <- Complex_Export_vis_nodes$n
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$size == 1, ]$size <- input$Export_Complex_NodeSliderSize
      
    }else{
      
      Complex_Export_vis_nodes$size <- input$Export_Complex_NodeSliderSize
      
    }
    
    Complex_Export_vis_nodes$color.background <- "#b4b4b4"
    Complex_Export_vis_nodes$color.border <- "black"
    
    if (length(input$Export_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Export_Complex_NodeColour))) {
        
        if (any(Complex_Export_vis_nodes$type == "Bait")) {
          
          Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Export_Complex_NodeColour))) {
        
        if (any(Complex_Export_vis_nodes$type == "Prey")) {
          
          Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Export_Complex_NodeColour))) {
        
        if (any(Complex_Export_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Export_Complex_NodeColour))) {
        
        if (any(Complex_Export_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Export_vis_nodes)
    
  }
  
  Complex_Export_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]$Name
    
    Complex_Export_vis_edges <- Both_Export_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$filter == T,]
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[,1:3]
    
    Complex_Export_vis_nodes <- Both_Export_vis_nodes[Both_Export_vis_nodes$id %in% unique(c(Complex_Export_vis_edges$from, Complex_Export_vis_edges$to)), ]
    
    Complex_Export_vis_nodes$type <- "Prey"
    
    Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey",]$id),
                                                                                    complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% intersect(unique(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id),
                                                                                    complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Export_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Export_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Export_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Export_vis_edges$type == "Both")) {
          
          Complex_Export_vis_edges[Complex_Export_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Export_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Export_vis_edges$type == "PPI")) {
          
          Complex_Export_vis_edges[Complex_Export_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Export_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Export_vis_edges$type == "RDI")) {
          
          Complex_Export_vis_edges[Complex_Export_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Complex_Export_vis_edges$width <- input$Export_Complex_EdgeThickness
    
    Complex_Export_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Export_vis_edges$to)
    
    return(Complex_Export_vis_edges)
    
  }
  
  output$Export_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes_dataset_v01()
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges_dataset_v01()
    
    layout <- input$Export_Complex_NetworkLayout
    
    visNetwork(Complex_Export_vis_nodes, Complex_Export_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Export_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Export_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Export_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Export_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Export_vis_nodes_dataset_v02  <- function(){
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes_dataset_v01()
    
    Complex_Export_vis_nodes$size <- input$Export_Complex_NodeSliderSize
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges_dataset_v01()
    
    ExportTargets <- Complex_Export_vis_nodes[grep(pattern = "Bait", x = Complex_Export_vis_nodes$type),]$id
    
    ComplexExportBarPlot <- c()
    
    for (j in 1:length(ExportTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Export_vis_edges[Complex_Export_vis_edges$from == ExportTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexExportRatios[ComplexExportRatios$Complex == input$Export_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = ExportTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexExportBarPlot <- rbind(ComplexExportBarPlot, loop_df)
      
    }
    
    ComplexExportBarPlot <- ComplexExportBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexExportBarPlot[ComplexExportBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id[Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$from %in% Complex_Export_vis_nodes$id,]
    
    return(Complex_Export_vis_nodes)
    
  }
  
  Complex_Export_vis_edges_dataset_v02  <- function(){
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes_dataset_v01()
    
    Complex_Export_vis_nodes$size <- input$Export_Complex_NodeSliderSize
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges_dataset_v01()
    
    ExportTargets <- Complex_Export_vis_nodes[grep(pattern = "Bait", x = Complex_Export_vis_nodes$type),]$id
    
    ComplexExportBarPlot <- c()
    
    for (j in 1:length(ExportTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Export_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Export_vis_edges[Complex_Export_vis_edges$from == ExportTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexExportRatios[ComplexExportRatios$Complex == input$Export_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = ExportTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexExportBarPlot <- rbind(ComplexExportBarPlot, loop_df)
      
    }
    
    ComplexExportBarPlot <- ComplexExportBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexExportBarPlot[ComplexExportBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id[Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Export_vis_nodes[Complex_Export_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes[Complex_Export_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$from %in% Complex_Export_vis_nodes$id,]
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges[Complex_Export_vis_edges$to %in% Complex_Export_vis_nodes$id,]
    
    return(Complex_Export_vis_edges)
    
  }
  
  output$Export_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Export_vis_nodes <- Complex_Export_vis_nodes_dataset_v02()
    
    Complex_Export_vis_edges <- Complex_Export_vis_edges_dataset_v02()
    
    layout <- input$Export_Complex_NetworkLayout
    
    visNetwork(Complex_Export_vis_nodes, Complex_Export_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Export_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Export_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Export_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Export_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Export_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Export_Complex_BarplotTitle <- renderText({
    paste0(input$Export_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Export_Complex_Networkv01Title <- renderText({
    paste0(input$Export_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Export_Complex_Networkv02Title <- renderText({
    paste0(input$Export_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  
  ########################
  
  ##########################
  #### Ribosome section ####
  
  PPI_Ribosome_vis_nodes_dataset  <- function(){
    
    PPI_Ribosome_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Ribosome_vis_nodes$id)
    
    PPI_Ribosome_vis_nodes$title <- PPI_Ribosome_vis_nodes$id
    
    if (input$Ribosome_PPI_NodeSize == T) {
      
      PPI_Ribosome_vis_nodes$size <- PPI_Ribosome_vis_nodes$n
      
      PPI_Ribosome_vis_nodes[PPI_Ribosome_vis_nodes$size == 1, ]$size <- input$Ribosome_PPI_NodeSliderSize
      
    }else{
      
      PPI_Ribosome_vis_nodes$size <- input$Ribosome_PPI_NodeSliderSize
      
    }
    
    PPI_Ribosome_vis_nodes$color.background <- "#A6CEE3"
    PPI_Ribosome_vis_nodes$color.border <- "black"
    
    if (length(input$Ribosome_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Ribosome_PPI_NodeColour))) {
        
        if (any(PPI_Ribosome_vis_nodes$type == "Bait")) {
          
          PPI_Ribosome_vis_nodes[PPI_Ribosome_vis_nodes$type == "Bait", ]$color.background <- "#66C2A5"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Ribosome_PPI_NodeColour))) {
        
        if (any(PPI_Ribosome_vis_nodes$type == "BioGRID")) {
          
          PPI_Ribosome_vis_nodes[PPI_Ribosome_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Ribosome_PPI_NodeColour))) {
        
        if (any(PPI_Ribosome_vis_nodes$type == "RBP census")) {
          
          PPI_Ribosome_vis_nodes[PPI_Ribosome_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Ribosome_PPI_NodeColour))) {
        
        if (any(PPI_Ribosome_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Ribosome_vis_nodes[PPI_Ribosome_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(PPI_Ribosome_vis_nodes)
    
  }
  
  PPI_Ribosome_vis_edges_dataset  <- function(){
    
    PPI_Ribosome_vis_edges$color <- "#b4b4b4"
    
    PPI_Ribosome_vis_edges$width <- input$Ribosome_PPI_EdgeThickness
    
    PPI_Ribosome_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Ribosome_vis_edges$to)
    
    return(PPI_Ribosome_vis_edges)
    
  }
  
  output$Ribosome_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Ribosome_vis_nodes <- PPI_Ribosome_vis_nodes_dataset()
    
    PPI_Ribosome_vis_edges <- PPI_Ribosome_vis_edges_dataset()
    
    layout <- input$Ribosome_PPI_NetworkLayout
    
    visNetwork(PPI_Ribosome_vis_nodes, PPI_Ribosome_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Ribosome_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Ribosome_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Ribosome_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Ribosome_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Ribosome_vis_nodes_dataset  <- function(){
    
    RDI_Ribosome_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Ribosome_vis_nodes$id)
    
    RDI_Ribosome_vis_nodes$title <- RDI_Ribosome_vis_nodes$id
    
    if (input$Ribosome_RDI_NodeSize == T) {
      
      RDI_Ribosome_vis_nodes$size <- RDI_Ribosome_vis_nodes$n
      
      RDI_Ribosome_vis_nodes[RDI_Ribosome_vis_nodes$size == 1, ]$size <- input$Ribosome_RDI_NodeSliderSize
      
    }else{
      
      RDI_Ribosome_vis_nodes$size <- input$Ribosome_RDI_NodeSliderSize
      
    }
    
    RDI_Ribosome_vis_nodes$color.background <- "#A6CEE3"
    RDI_Ribosome_vis_nodes$color.border <- "black"
    
    if (length(input$Ribosome_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Ribosome_RDI_NodeColour))) {
        
        if (any(RDI_Ribosome_vis_nodes$type == "Bait")) {
          
          RDI_Ribosome_vis_nodes[RDI_Ribosome_vis_nodes$type == "Bait", ]$color.background <- "#66C2A5"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Ribosome_RDI_NodeColour))) {
        
        if (any(RDI_Ribosome_vis_nodes$type == "BioGRID")) {
          
          RDI_Ribosome_vis_nodes[RDI_Ribosome_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Ribosome_RDI_NodeColour))) {
        
        if (any(RDI_Ribosome_vis_nodes$type == "RBP census")) {
          
          RDI_Ribosome_vis_nodes[RDI_Ribosome_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Ribosome_RDI_NodeColour))) {
        
        if (any(RDI_Ribosome_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Ribosome_vis_nodes[RDI_Ribosome_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(RDI_Ribosome_vis_nodes)
    
  }
  
  RDI_Ribosome_vis_edges_dataset  <- function(){
    
    RDI_Ribosome_vis_edges$color <- "#b4b4b4"
    
    RDI_Ribosome_vis_edges$width <- input$Ribosome_RDI_EdgeThickness
    
    RDI_Ribosome_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Ribosome_vis_edges$to)
    
    return(RDI_Ribosome_vis_edges)
    
  }
  
  output$Ribosome_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Ribosome_vis_nodes <- RDI_Ribosome_vis_nodes_dataset()
    
    RDI_Ribosome_vis_edges <- RDI_Ribosome_vis_edges_dataset()
    
    layout <- input$Ribosome_RDI_NetworkLayout
    
    visNetwork(RDI_Ribosome_vis_nodes, RDI_Ribosome_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Ribosome_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
  })
  
  output$Ribosome_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Ribosome_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Ribosome_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Ribosome_vis_nodes_dataset  <- function(){
    
    Both_Ribosome_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Ribosome_vis_nodes$id)
    
    Both_Ribosome_vis_nodes$title <- Both_Ribosome_vis_nodes$id
    
    if (input$Ribosome_Both_NodeSize == T) {
      
      Both_Ribosome_vis_nodes$size <- Both_Ribosome_vis_nodes$n
      
      Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$size == 1, ]$size <- input$Ribosome_Both_NodeSliderSize
      
    }else{
      
      Both_Ribosome_vis_nodes$size <- input$Ribosome_Both_NodeSliderSize
      
    }
    
    Both_Ribosome_vis_nodes$color.background <- "#A6CEE3"
    Both_Ribosome_vis_nodes$color.border <- "black"
    
    if (length(input$Ribosome_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Ribosome_Both_NodeColour))) {
        
        if (any(Both_Ribosome_vis_nodes$type == "Bait")) {
          
          Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$type == "Bait", ]$color.background <- "#66C2A5"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Ribosome_Both_NodeColour))) {
        
        if (any(Both_Ribosome_vis_nodes$type == "BioGRID")) {
          
          Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Ribosome_Both_NodeColour))) {
        
        if (any(Both_Ribosome_vis_nodes$type == "RBP census")) {
          
          Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Ribosome_Both_NodeColour))) {
        
        if (any(Both_Ribosome_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(Both_Ribosome_vis_nodes)
    
  }
  
  Both_Ribosome_vis_edges_dataset  <- function(){
    
    Both_Ribosome_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Ribosome_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Ribosome_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Ribosome_vis_edges$type == "Both")) {
          
          Both_Ribosome_vis_edges[Both_Ribosome_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Ribosome_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Ribosome_vis_edges$type == "PPI")) {
          
          Both_Ribosome_vis_edges[Both_Ribosome_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Ribosome_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Ribosome_vis_edges$type == "RDI")) {
          
          Both_Ribosome_vis_edges[Both_Ribosome_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Both_Ribosome_vis_edges$width <- input$Ribosome_Both_EdgeThickness
    
    Both_Ribosome_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Ribosome_vis_edges$to)
    
    return(Both_Ribosome_vis_edges)
    
  }
  
  output$Ribosome_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Ribosome_vis_nodes <- Both_Ribosome_vis_nodes_dataset()
    
    Both_Ribosome_vis_edges <- Both_Ribosome_vis_edges_dataset()
    
    layout <- input$Ribosome_Both_NetworkLayout
    
    visNetwork(Both_Ribosome_vis_nodes, Both_Ribosome_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Ribosome_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Ribosome_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Ribosome_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Ribosome_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexRibosomeRatios[ComplexRibosomeRatios$ratio > input$Ribosome_Complex_FilterSlider,]$Complex
    
    RibosomeWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    RibosomeWodakTable <- RibosomeWodakTable[complete.cases(RibosomeWodakTable),]
    
    RibosomeWodakTable <- RibosomeWodakTable[,3:8]
    
    ratio_table <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex %in% RibosomeWodakTable$Complex,]
    
    RibosomeWodakTable <- merge(RibosomeWodakTable, ratio_table, by = "Complex")
    
    RibosomeWodakTable <- RibosomeWodakTable[,c(-7,-8)]
    
    RibosomeWodakTable <- RibosomeWodakTable %>% arrange(ratio)
    
    RibosomeWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Ribosome_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexRibosomeRatios[ComplexRibosomeRatios$ratio > input$Ribosome_Complex_FilterSlider,]$Complex
    
    ComplexRibosomeDotPlot <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex %in% WodakFilter,]
    
    ComplexRibosomeDotPlot <- ComplexRibosomeDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexRibosomeDotPlot$Complex <- factor(x = ComplexRibosomeDotPlot$Complex, levels = ComplexRibosomeDotPlot$Complex)
    
    ComplexRibosomeDotPlot$ratio <- round(ComplexRibosomeDotPlot$ratio, digits = 2)
    
    ComplexRibosomeDotPlot <- ggplot(ComplexRibosomeDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexRibosomeDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexRibosomeDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexRibosomeDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 85, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexRibosomeDotPlot <- ggplotly(ComplexRibosomeDotPlot, tooltip = "text")
    
    print(ComplexRibosomeDotPlot)
    
  })
  
  output$Ribosome_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexRibosomeRatios[ComplexRibosomeRatios$ratio > input$Ribosome_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Ribosome_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Ribosome_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Ribosome_Complex_RadioButton_Selection) > 0) {
      
      # input$Ribosome_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]$Name
      
      Complex_Ribosome_vis_edges <- Both_Ribosome_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$filter == T,]
      
      Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[,1:3]
      
      Complex_Ribosome_vis_nodes <- Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$id %in% unique(c(Complex_Ribosome_vis_edges$from, Complex_Ribosome_vis_edges$to)), ]
      
      Complex_Ribosome_vis_nodes$type <- "Prey"
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                                                                            complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                                                                            complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      RibosomeTargets <- Complex_Ribosome_vis_nodes[grep(pattern = "Bait", x = Complex_Ribosome_vis_nodes$type),]$id
      
      ComplexRibosomeBarPlot <- c()
      
      for (j in 1:length(RibosomeTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$from == RibosomeTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex == input$Ribosome_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = RibosomeTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexRibosomeBarPlot <- rbind(ComplexRibosomeBarPlot, loop_df)
        
      }
      
      ComplexRibosomeBarPlot <- ComplexRibosomeBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexRibosomeBarPlot <- ComplexRibosomeBarPlot[ComplexRibosomeBarPlot$ratio > 0, ]
      
      ComplexRibosomeBarPlot$Target <- factor(x = ComplexRibosomeBarPlot$Target, levels = unique(ComplexRibosomeBarPlot$Target))
      
      ComplexRibosomeBarPlot <- ggplot(ComplexRibosomeBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      print(ComplexRibosomeBarPlot)
      
    }
    
  })
  
  Complex_Ribosome_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]$Name
    
    Complex_Ribosome_vis_edges <- Both_Ribosome_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$filter == T,]
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[,1:3]
    
    Complex_Ribosome_vis_nodes <- Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$id %in% unique(c(Complex_Ribosome_vis_edges$from, Complex_Ribosome_vis_edges$to)), ]
    
    Complex_Ribosome_vis_nodes$type <- "Prey"
    
    Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                                                                          complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                                                                          complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Ribosome_Complex_NodeSize == T) {
      
      Complex_Ribosome_vis_nodes$size <- Complex_Ribosome_vis_nodes$n
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$size == 1, ]$size <- input$Ribosome_Complex_NodeSliderSize
      
    }else{
      
      Complex_Ribosome_vis_nodes$size <- input$Ribosome_Complex_NodeSliderSize
      
    }
    
    Complex_Ribosome_vis_nodes$color.background <- "#b4b4b4"
    Complex_Ribosome_vis_nodes$color.border <- "black"
    
    if (length(input$Ribosome_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Ribosome_Complex_NodeColour))) {
        
        if (any(Complex_Ribosome_vis_nodes$type == "Bait")) {
          
          Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Ribosome_Complex_NodeColour))) {
        
        if (any(Complex_Ribosome_vis_nodes$type == "Prey")) {
          
          Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Ribosome_Complex_NodeColour))) {
        
        if (any(Complex_Ribosome_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Ribosome_Complex_NodeColour))) {
        
        if (any(Complex_Ribosome_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Ribosome_vis_nodes)
    
  }
  
  Complex_Ribosome_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]$Name
    
    Complex_Ribosome_vis_edges <- Both_Ribosome_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$filter == T,]
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[,1:3]
    
    Complex_Ribosome_vis_nodes <- Both_Ribosome_vis_nodes[Both_Ribosome_vis_nodes$id %in% unique(c(Complex_Ribosome_vis_edges$from, Complex_Ribosome_vis_edges$to)), ]
    
    Complex_Ribosome_vis_nodes$type <- "Prey"
    
    Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey",]$id),
                                                                          complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% intersect(unique(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id),
                                                                          complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Ribosome_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Ribosome_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Ribosome_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Ribosome_vis_edges$type == "Both")) {
          
          Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Ribosome_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Ribosome_vis_edges$type == "PPI")) {
          
          Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Ribosome_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Ribosome_vis_edges$type == "RDI")) {
          
          Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Complex_Ribosome_vis_edges$width <- input$Ribosome_Complex_EdgeThickness
    
    Complex_Ribosome_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Ribosome_vis_edges$to)
    
    return(Complex_Ribosome_vis_edges)
    
  }
  
  output$Ribosome_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes_dataset_v01()
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges_dataset_v01()
    
    layout <- input$Ribosome_Complex_NetworkLayout
    
    visNetwork(Complex_Ribosome_vis_nodes, Complex_Ribosome_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Ribosome_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Ribosome_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Ribosome_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Ribosome_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Ribosome_vis_nodes_dataset_v02  <- function(){
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes_dataset_v01()
    
    Complex_Ribosome_vis_nodes$size <- input$Ribosome_Complex_NodeSliderSize
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges_dataset_v01()
    
    RibosomeTargets <- Complex_Ribosome_vis_nodes[grep(pattern = "Bait", x = Complex_Ribosome_vis_nodes$type),]$id
    
    ComplexRibosomeBarPlot <- c()
    
    for (j in 1:length(RibosomeTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$from == RibosomeTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex == input$Ribosome_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = RibosomeTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexRibosomeBarPlot <- rbind(ComplexRibosomeBarPlot, loop_df)
      
    }
    
    ComplexRibosomeBarPlot <- ComplexRibosomeBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexRibosomeBarPlot[ComplexRibosomeBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id[Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$from %in% Complex_Ribosome_vis_nodes$id,]
    
    return(Complex_Ribosome_vis_nodes)
    
  }
  
  Complex_Ribosome_vis_edges_dataset_v02  <- function(){
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes_dataset_v01()
    
    Complex_Ribosome_vis_nodes$size <- input$Ribosome_Complex_NodeSliderSize
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges_dataset_v01()
    
    RibosomeTargets <- Complex_Ribosome_vis_nodes[grep(pattern = "Bait", x = Complex_Ribosome_vis_nodes$type),]$id
    
    ComplexRibosomeBarPlot <- c()
    
    for (j in 1:length(RibosomeTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Ribosome_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$from == RibosomeTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexRibosomeRatios[ComplexRibosomeRatios$Complex == input$Ribosome_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = RibosomeTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexRibosomeBarPlot <- rbind(ComplexRibosomeBarPlot, loop_df)
      
    }
    
    ComplexRibosomeBarPlot <- ComplexRibosomeBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexRibosomeBarPlot[ComplexRibosomeBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id[Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes[Complex_Ribosome_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$from %in% Complex_Ribosome_vis_nodes$id,]
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges[Complex_Ribosome_vis_edges$to %in% Complex_Ribosome_vis_nodes$id,]
    
    return(Complex_Ribosome_vis_edges)
    
  }
  
  output$Ribosome_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Ribosome_vis_nodes <- Complex_Ribosome_vis_nodes_dataset_v02()
    
    Complex_Ribosome_vis_edges <- Complex_Ribosome_vis_edges_dataset_v02()
    
    layout <- input$Ribosome_Complex_NetworkLayout
    
    visNetwork(Complex_Ribosome_vis_nodes, Complex_Ribosome_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Ribosome_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Ribosome_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Ribosome_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Ribosome_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Ribosome_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Ribosome_Complex_BarplotTitle <- renderText({
    paste0(input$Ribosome_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Ribosome_Complex_Networkv01Title <- renderText({
    paste0(input$Ribosome_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Ribosome_Complex_Networkv02Title <- renderText({
    paste0(input$Ribosome_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  
  ##########################
  
  ###########################
  #### Synthesis section ####
  
  PPI_Synthesis_vis_nodes_dataset  <- function(){
    
    PPI_Synthesis_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Synthesis_vis_nodes$id)
    
    PPI_Synthesis_vis_nodes$title <- PPI_Synthesis_vis_nodes$id
    
    if (input$Synthesis_PPI_NodeSize == T) {
      
      PPI_Synthesis_vis_nodes$size <- PPI_Synthesis_vis_nodes$n
      
      PPI_Synthesis_vis_nodes[PPI_Synthesis_vis_nodes$size == 1, ]$size <- input$Synthesis_PPI_NodeSliderSize
      
    }else{
      
      PPI_Synthesis_vis_nodes$size <- input$Synthesis_PPI_NodeSliderSize
      
    }
    
    PPI_Synthesis_vis_nodes$color.background <- "#A6CEE3"
    PPI_Synthesis_vis_nodes$color.border <- "black"
    
    if (length(input$Synthesis_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Synthesis_PPI_NodeColour))) {
        
        if (any(PPI_Synthesis_vis_nodes$type == "Bait")) {
          
          PPI_Synthesis_vis_nodes[PPI_Synthesis_vis_nodes$type == "Bait", ]$color.background <- "#FC8D62"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Synthesis_PPI_NodeColour))) {
        
        if (any(PPI_Synthesis_vis_nodes$type == "BioGRID")) {
          
          PPI_Synthesis_vis_nodes[PPI_Synthesis_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Synthesis_PPI_NodeColour))) {
        
        if (any(PPI_Synthesis_vis_nodes$type == "RBP census")) {
          
          PPI_Synthesis_vis_nodes[PPI_Synthesis_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Synthesis_PPI_NodeColour))) {
        
        if (any(PPI_Synthesis_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Synthesis_vis_nodes[PPI_Synthesis_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(PPI_Synthesis_vis_nodes)
    
  }
  
  PPI_Synthesis_vis_edges_dataset  <- function(){
    
    PPI_Synthesis_vis_edges$color <- "#b4b4b4"
    
    PPI_Synthesis_vis_edges$width <- input$Synthesis_PPI_EdgeThickness
    
    PPI_Synthesis_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Synthesis_vis_edges$to)
    
    return(PPI_Synthesis_vis_edges)
    
  }
  
  output$Synthesis_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Synthesis_vis_nodes <- PPI_Synthesis_vis_nodes_dataset()
    
    PPI_Synthesis_vis_edges <- PPI_Synthesis_vis_edges_dataset()
    
    layout <- input$Synthesis_PPI_NetworkLayout
    
    visNetwork(PPI_Synthesis_vis_nodes, PPI_Synthesis_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Synthesis_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Synthesis_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Synthesis_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Synthesis_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Synthesis_vis_nodes_dataset  <- function(){
    
    RDI_Synthesis_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Synthesis_vis_nodes$id)
    
    RDI_Synthesis_vis_nodes$title <- RDI_Synthesis_vis_nodes$id
    
    if (input$Synthesis_RDI_NodeSize == T) {
      
      RDI_Synthesis_vis_nodes$size <- RDI_Synthesis_vis_nodes$n
      
      RDI_Synthesis_vis_nodes[RDI_Synthesis_vis_nodes$size == 1, ]$size <- input$Synthesis_RDI_NodeSliderSize
      
    }else{
      
      RDI_Synthesis_vis_nodes$size <- input$Synthesis_RDI_NodeSliderSize
      
    }
    
    RDI_Synthesis_vis_nodes$color.background <- "#A6CEE3"
    RDI_Synthesis_vis_nodes$color.border <- "black"
    
    if (length(input$Synthesis_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Synthesis_RDI_NodeColour))) {
        
        if (any(RDI_Synthesis_vis_nodes$type == "Bait")) {
          
          RDI_Synthesis_vis_nodes[RDI_Synthesis_vis_nodes$type == "Bait", ]$color.background <- "#FC8D62"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Synthesis_RDI_NodeColour))) {
        
        if (any(RDI_Synthesis_vis_nodes$type == "BioGRID")) {
          
          RDI_Synthesis_vis_nodes[RDI_Synthesis_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Synthesis_RDI_NodeColour))) {
        
        if (any(RDI_Synthesis_vis_nodes$type == "RBP census")) {
          
          RDI_Synthesis_vis_nodes[RDI_Synthesis_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Synthesis_RDI_NodeColour))) {
        
        if (any(RDI_Synthesis_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Synthesis_vis_nodes[RDI_Synthesis_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(RDI_Synthesis_vis_nodes)
    
  }
  
  RDI_Synthesis_vis_edges_dataset  <- function(){
    
    RDI_Synthesis_vis_edges$color <- "#b4b4b4"
    
    RDI_Synthesis_vis_edges$width <- input$Synthesis_RDI_EdgeThickness
    
    RDI_Synthesis_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Synthesis_vis_edges$to)
    
    return(RDI_Synthesis_vis_edges)
    
  }
  
  output$Synthesis_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Synthesis_vis_nodes <- RDI_Synthesis_vis_nodes_dataset()
    
    RDI_Synthesis_vis_edges <- RDI_Synthesis_vis_edges_dataset()
    
    layout <- input$Synthesis_RDI_NetworkLayout
    
    visNetwork(RDI_Synthesis_vis_nodes, RDI_Synthesis_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Synthesis_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
  })
  
  output$Synthesis_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Synthesis_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Synthesis_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Synthesis_vis_nodes_dataset  <- function(){
    
    Both_Synthesis_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Synthesis_vis_nodes$id)
    
    Both_Synthesis_vis_nodes$title <- Both_Synthesis_vis_nodes$id
    
    if (input$Synthesis_Both_NodeSize == T) {
      
      Both_Synthesis_vis_nodes$size <- Both_Synthesis_vis_nodes$n
      
      Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$size == 1, ]$size <- input$Synthesis_Both_NodeSliderSize
      
    }else{
      
      Both_Synthesis_vis_nodes$size <- input$Synthesis_Both_NodeSliderSize
      
    }
    
    Both_Synthesis_vis_nodes$color.background <- "#A6CEE3"
    Both_Synthesis_vis_nodes$color.border <- "black"
    
    if (length(input$Synthesis_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Synthesis_Both_NodeColour))) {
        
        if (any(Both_Synthesis_vis_nodes$type == "Bait")) {
          
          Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$type == "Bait", ]$color.background <- "#FC8D62"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Synthesis_Both_NodeColour))) {
        
        if (any(Both_Synthesis_vis_nodes$type == "BioGRID")) {
          
          Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Synthesis_Both_NodeColour))) {
        
        if (any(Both_Synthesis_vis_nodes$type == "RBP census")) {
          
          Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Synthesis_Both_NodeColour))) {
        
        if (any(Both_Synthesis_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(Both_Synthesis_vis_nodes)
    
  }
  
  Both_Synthesis_vis_edges_dataset  <- function(){
    
    Both_Synthesis_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Synthesis_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Synthesis_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Synthesis_vis_edges$type == "Both")) {
          
          Both_Synthesis_vis_edges[Both_Synthesis_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Synthesis_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Synthesis_vis_edges$type == "PPI")) {
          
          Both_Synthesis_vis_edges[Both_Synthesis_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Synthesis_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Synthesis_vis_edges$type == "RDI")) {
          
          Both_Synthesis_vis_edges[Both_Synthesis_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Both_Synthesis_vis_edges$width <- input$Synthesis_Both_EdgeThickness
    
    Both_Synthesis_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Synthesis_vis_edges$to)
    
    return(Both_Synthesis_vis_edges)
    
  }
  
  output$Synthesis_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Synthesis_vis_nodes <- Both_Synthesis_vis_nodes_dataset()
    
    Both_Synthesis_vis_edges <- Both_Synthesis_vis_edges_dataset()
    
    layout <- input$Synthesis_Both_NetworkLayout
    
    visNetwork(Both_Synthesis_vis_nodes, Both_Synthesis_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Synthesis_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Synthesis_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Synthesis_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Synthesis_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexSynthesisRatios[ComplexSynthesisRatios$ratio > input$Synthesis_Complex_FilterSlider,]$Complex
    
    SynthesisWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    SynthesisWodakTable <- SynthesisWodakTable[complete.cases(SynthesisWodakTable),]
    
    SynthesisWodakTable <- SynthesisWodakTable[,3:8]
    
    ratio_table <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex %in% SynthesisWodakTable$Complex,]
    
    SynthesisWodakTable <- merge(SynthesisWodakTable, ratio_table, by = "Complex")
    
    SynthesisWodakTable <- SynthesisWodakTable[,c(-7,-8)]
    
    SynthesisWodakTable <- SynthesisWodakTable %>% arrange(ratio)
    
    SynthesisWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Synthesis_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexSynthesisRatios[ComplexSynthesisRatios$ratio > input$Synthesis_Complex_FilterSlider,]$Complex
    
    ComplexSynthesisDotPlot <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex %in% WodakFilter,]
    
    ComplexSynthesisDotPlot <- ComplexSynthesisDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexSynthesisDotPlot$Complex <- factor(x = ComplexSynthesisDotPlot$Complex, levels = ComplexSynthesisDotPlot$Complex)
    
    ComplexSynthesisDotPlot$ratio <- round(ComplexSynthesisDotPlot$ratio, digits = 2)
    
    ComplexSynthesisDotPlot <- ggplot(ComplexSynthesisDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexSynthesisDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexSynthesisDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexSynthesisDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 85, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexSynthesisDotPlot <- ggplotly(ComplexSynthesisDotPlot, tooltip = "text")
    
    print(ComplexSynthesisDotPlot)
    
  })
  
  output$Synthesis_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexSynthesisRatios[ComplexSynthesisRatios$ratio > input$Synthesis_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Synthesis_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Synthesis_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Synthesis_Complex_RadioButton_Selection) > 0) {
      
      # input$Synthesis_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]$Name
      
      Complex_Synthesis_vis_edges <- Both_Synthesis_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$filter == T,]
      
      Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[,1:3]
      
      Complex_Synthesis_vis_nodes <- Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$id %in% unique(c(Complex_Synthesis_vis_edges$from, Complex_Synthesis_vis_edges$to)), ]
      
      Complex_Synthesis_vis_nodes$type <- "Prey"
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                                                                                complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                                                                                complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      SynthesisTargets <- Complex_Synthesis_vis_nodes[grep(pattern = "Bait", x = Complex_Synthesis_vis_nodes$type),]$id
      
      ComplexSynthesisBarPlot <- c()
      
      for (j in 1:length(SynthesisTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$from == SynthesisTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex == input$Synthesis_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = SynthesisTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexSynthesisBarPlot <- rbind(ComplexSynthesisBarPlot, loop_df)
        
      }
      
      ComplexSynthesisBarPlot <- ComplexSynthesisBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexSynthesisBarPlot <- ComplexSynthesisBarPlot[ComplexSynthesisBarPlot$ratio > 0, ]
      
      ComplexSynthesisBarPlot$Target <- factor(x = ComplexSynthesisBarPlot$Target, levels = unique(ComplexSynthesisBarPlot$Target))
      
      ComplexSynthesisBarPlot <- ggplot(ComplexSynthesisBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      print(ComplexSynthesisBarPlot)
      
    }
    
  })
  
  Complex_Synthesis_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]$Name
    
    Complex_Synthesis_vis_edges <- Both_Synthesis_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$filter == T,]
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[,1:3]
    
    Complex_Synthesis_vis_nodes <- Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$id %in% unique(c(Complex_Synthesis_vis_edges$from, Complex_Synthesis_vis_edges$to)), ]
    
    Complex_Synthesis_vis_nodes$type <- "Prey"
    
    Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                                                                              complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                                                                              complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Synthesis_Complex_NodeSize == T) {
      
      Complex_Synthesis_vis_nodes$size <- Complex_Synthesis_vis_nodes$n
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$size == 1, ]$size <- input$Synthesis_Complex_NodeSliderSize
      
    }else{
      
      Complex_Synthesis_vis_nodes$size <- input$Synthesis_Complex_NodeSliderSize
      
    }
    
    Complex_Synthesis_vis_nodes$color.background <- "#b4b4b4"
    Complex_Synthesis_vis_nodes$color.border <- "black"
    
    if (length(input$Synthesis_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Synthesis_Complex_NodeColour))) {
        
        if (any(Complex_Synthesis_vis_nodes$type == "Bait")) {
          
          Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Synthesis_Complex_NodeColour))) {
        
        if (any(Complex_Synthesis_vis_nodes$type == "Prey")) {
          
          Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Synthesis_Complex_NodeColour))) {
        
        if (any(Complex_Synthesis_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Synthesis_Complex_NodeColour))) {
        
        if (any(Complex_Synthesis_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Synthesis_vis_nodes)
    
  }
  
  Complex_Synthesis_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]$Name
    
    Complex_Synthesis_vis_edges <- Both_Synthesis_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$filter == T,]
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[,1:3]
    
    Complex_Synthesis_vis_nodes <- Both_Synthesis_vis_nodes[Both_Synthesis_vis_nodes$id %in% unique(c(Complex_Synthesis_vis_edges$from, Complex_Synthesis_vis_edges$to)), ]
    
    Complex_Synthesis_vis_nodes$type <- "Prey"
    
    Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey",]$id),
                                                                              complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% intersect(unique(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id),
                                                                              complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Synthesis_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Synthesis_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Synthesis_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Synthesis_vis_edges$type == "Both")) {
          
          Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Synthesis_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Synthesis_vis_edges$type == "PPI")) {
          
          Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Synthesis_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Synthesis_vis_edges$type == "RDI")) {
          
          Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Complex_Synthesis_vis_edges$width <- input$Synthesis_Complex_EdgeThickness
    
    Complex_Synthesis_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Synthesis_vis_edges$to)
    
    return(Complex_Synthesis_vis_edges)
    
  }
  
  output$Synthesis_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes_dataset_v01()
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges_dataset_v01()
    
    layout <- input$Synthesis_Complex_NetworkLayout
    
    visNetwork(Complex_Synthesis_vis_nodes, Complex_Synthesis_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Synthesis_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Synthesis_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Synthesis_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Synthesis_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Synthesis_vis_nodes_dataset_v02  <- function(){
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes_dataset_v01()
    
    Complex_Synthesis_vis_nodes$size <- input$Synthesis_Complex_NodeSliderSize
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges_dataset_v01()
    
    SynthesisTargets <- Complex_Synthesis_vis_nodes[grep(pattern = "Bait", x = Complex_Synthesis_vis_nodes$type),]$id
    
    ComplexSynthesisBarPlot <- c()
    
    for (j in 1:length(SynthesisTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$from == SynthesisTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex == input$Synthesis_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = SynthesisTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexSynthesisBarPlot <- rbind(ComplexSynthesisBarPlot, loop_df)
      
    }
    
    ComplexSynthesisBarPlot <- ComplexSynthesisBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexSynthesisBarPlot[ComplexSynthesisBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id[Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$from %in% Complex_Synthesis_vis_nodes$id,]
    
    return(Complex_Synthesis_vis_nodes)
    
  }
  
  Complex_Synthesis_vis_edges_dataset_v02  <- function(){
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes_dataset_v01()
    
    Complex_Synthesis_vis_nodes$size <- input$Synthesis_Complex_NodeSliderSize
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges_dataset_v01()
    
    SynthesisTargets <- Complex_Synthesis_vis_nodes[grep(pattern = "Bait", x = Complex_Synthesis_vis_nodes$type),]$id
    
    ComplexSynthesisBarPlot <- c()
    
    for (j in 1:length(SynthesisTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Synthesis_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$from == SynthesisTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexSynthesisRatios[ComplexSynthesisRatios$Complex == input$Synthesis_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = SynthesisTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexSynthesisBarPlot <- rbind(ComplexSynthesisBarPlot, loop_df)
      
    }
    
    ComplexSynthesisBarPlot <- ComplexSynthesisBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexSynthesisBarPlot[ComplexSynthesisBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id[Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes[Complex_Synthesis_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$from %in% Complex_Synthesis_vis_nodes$id,]
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges[Complex_Synthesis_vis_edges$to %in% Complex_Synthesis_vis_nodes$id,]
    
    return(Complex_Synthesis_vis_edges)
    
  }
  
  output$Synthesis_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Synthesis_vis_nodes <- Complex_Synthesis_vis_nodes_dataset_v02()
    
    Complex_Synthesis_vis_edges <- Complex_Synthesis_vis_edges_dataset_v02()
    
    layout <- input$Synthesis_Complex_NetworkLayout
    
    visNetwork(Complex_Synthesis_vis_nodes, Complex_Synthesis_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Synthesis_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Synthesis_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Synthesis_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Synthesis_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Synthesis_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Synthesis_Complex_BarplotTitle <- renderText({
    paste0(input$Synthesis_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Synthesis_Complex_Networkv01Title <- renderText({
    paste0(input$Synthesis_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Synthesis_Complex_Networkv02Title <- renderText({
    paste0(input$Synthesis_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  ###########################
  
  ############################
  #### Metabolism section ####
  
  PPI_Metabolism_vis_nodes_dataset  <- function(){
    
    PPI_Metabolism_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Metabolism_vis_nodes$id)
    
    PPI_Metabolism_vis_nodes$title <- PPI_Metabolism_vis_nodes$id
    
    if (input$Metabolism_PPI_NodeSize == T) {
      
      PPI_Metabolism_vis_nodes$size <- PPI_Metabolism_vis_nodes$n
      
      PPI_Metabolism_vis_nodes[PPI_Metabolism_vis_nodes$size == 1, ]$size <- input$Metabolism_PPI_NodeSliderSize
      
    }else{
      
      PPI_Metabolism_vis_nodes$size <- input$Metabolism_PPI_NodeSliderSize
      
    }
    
    PPI_Metabolism_vis_nodes$color.background <- "#A6CEE3"
    PPI_Metabolism_vis_nodes$color.border <- "black"
    
    if (length(input$Metabolism_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Metabolism_PPI_NodeColour))) {
        
        if (any(PPI_Metabolism_vis_nodes$type == "Bait")) {
          
          PPI_Metabolism_vis_nodes[PPI_Metabolism_vis_nodes$type == "Bait", ]$color.background <- "#E5C494"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Metabolism_PPI_NodeColour))) {
        
        if (any(PPI_Metabolism_vis_nodes$type == "BioGRID")) {
          
          PPI_Metabolism_vis_nodes[PPI_Metabolism_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Metabolism_PPI_NodeColour))) {
        
        if (any(PPI_Metabolism_vis_nodes$type == "RBP census")) {
          
          PPI_Metabolism_vis_nodes[PPI_Metabolism_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Metabolism_PPI_NodeColour))) {
        
        if (any(PPI_Metabolism_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Metabolism_vis_nodes[PPI_Metabolism_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(PPI_Metabolism_vis_nodes)
    
  }
  
  PPI_Metabolism_vis_edges_dataset  <- function(){
    
    PPI_Metabolism_vis_edges$color <- "#b4b4b4"
    
    PPI_Metabolism_vis_edges$width <- input$Metabolism_PPI_EdgeThickness
    
    PPI_Metabolism_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Metabolism_vis_edges$to)
    
    return(PPI_Metabolism_vis_edges)
    
  }
  
  output$Metabolism_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Metabolism_vis_nodes <- PPI_Metabolism_vis_nodes_dataset()
    
    PPI_Metabolism_vis_edges <- PPI_Metabolism_vis_edges_dataset()
    
    layout <- input$Metabolism_PPI_NetworkLayout
    
    visNetwork(PPI_Metabolism_vis_nodes, PPI_Metabolism_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Metabolism_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Metabolism_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Metabolism_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Metabolism_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Metabolism_vis_nodes_dataset  <- function(){
    
    RDI_Metabolism_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Metabolism_vis_nodes$id)
    
    RDI_Metabolism_vis_nodes$title <- RDI_Metabolism_vis_nodes$id
    
    if (input$Metabolism_RDI_NodeSize == T) {
      
      RDI_Metabolism_vis_nodes$size <- RDI_Metabolism_vis_nodes$n
      
      RDI_Metabolism_vis_nodes[RDI_Metabolism_vis_nodes$size == 1, ]$size <- input$Metabolism_RDI_NodeSliderSize
      
    }else{
      
      RDI_Metabolism_vis_nodes$size <- input$Metabolism_RDI_NodeSliderSize
      
    }
    
    RDI_Metabolism_vis_nodes$color.background <- "#A6CEE3"
    RDI_Metabolism_vis_nodes$color.border <- "black"
    
    if (length(input$Metabolism_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Metabolism_RDI_NodeColour))) {
        
        if (any(RDI_Metabolism_vis_nodes$type == "Bait")) {
          
          RDI_Metabolism_vis_nodes[RDI_Metabolism_vis_nodes$type == "Bait", ]$color.background <- "#E5C494"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Metabolism_RDI_NodeColour))) {
        
        if (any(RDI_Metabolism_vis_nodes$type == "BioGRID")) {
          
          RDI_Metabolism_vis_nodes[RDI_Metabolism_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Metabolism_RDI_NodeColour))) {
        
        if (any(RDI_Metabolism_vis_nodes$type == "RBP census")) {
          
          RDI_Metabolism_vis_nodes[RDI_Metabolism_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Metabolism_RDI_NodeColour))) {
        
        if (any(RDI_Metabolism_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Metabolism_vis_nodes[RDI_Metabolism_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(RDI_Metabolism_vis_nodes)
    
  }
  
  RDI_Metabolism_vis_edges_dataset  <- function(){
    
    RDI_Metabolism_vis_edges$color <- "#b4b4b4"
    
    RDI_Metabolism_vis_edges$width <- input$Metabolism_RDI_EdgeThickness
    
    RDI_Metabolism_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Metabolism_vis_edges$to)
    
    return(RDI_Metabolism_vis_edges)
    
  }
  
  output$Metabolism_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Metabolism_vis_nodes <- RDI_Metabolism_vis_nodes_dataset()
    
    RDI_Metabolism_vis_edges <- RDI_Metabolism_vis_edges_dataset()
    
    layout <- input$Metabolism_RDI_NetworkLayout
    
    visNetwork(RDI_Metabolism_vis_nodes, RDI_Metabolism_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Metabolism_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
  })
  
  output$Metabolism_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Metabolism_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Metabolism_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Metabolism_vis_nodes_dataset  <- function(){
    
    Both_Metabolism_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Metabolism_vis_nodes$id)
    
    Both_Metabolism_vis_nodes$title <- Both_Metabolism_vis_nodes$id
    
    if (input$Metabolism_Both_NodeSize == T) {
      
      Both_Metabolism_vis_nodes$size <- Both_Metabolism_vis_nodes$n
      
      Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$size == 1, ]$size <- input$Metabolism_Both_NodeSliderSize
      
    }else{
      
      Both_Metabolism_vis_nodes$size <- input$Metabolism_Both_NodeSliderSize
      
    }
    
    Both_Metabolism_vis_nodes$color.background <- "#A6CEE3"
    Both_Metabolism_vis_nodes$color.border <- "black"
    
    if (length(input$Metabolism_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Metabolism_Both_NodeColour))) {
        
        if (any(Both_Metabolism_vis_nodes$type == "Bait")) {
          
          Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$type == "Bait", ]$color.background <- "#E5C494"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Metabolism_Both_NodeColour))) {
        
        if (any(Both_Metabolism_vis_nodes$type == "BioGRID")) {
          
          Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Metabolism_Both_NodeColour))) {
        
        if (any(Both_Metabolism_vis_nodes$type == "RBP census")) {
          
          Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Metabolism_Both_NodeColour))) {
        
        if (any(Both_Metabolism_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(Both_Metabolism_vis_nodes)
    
  }
  
  Both_Metabolism_vis_edges_dataset  <- function(){
    
    Both_Metabolism_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Metabolism_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Metabolism_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Metabolism_vis_edges$type == "Both")) {
          
          Both_Metabolism_vis_edges[Both_Metabolism_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Metabolism_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Metabolism_vis_edges$type == "PPI")) {
          
          Both_Metabolism_vis_edges[Both_Metabolism_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Metabolism_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Metabolism_vis_edges$type == "RDI")) {
          
          Both_Metabolism_vis_edges[Both_Metabolism_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Both_Metabolism_vis_edges$width <- input$Metabolism_Both_EdgeThickness
    
    Both_Metabolism_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Metabolism_vis_edges$to)
    
    return(Both_Metabolism_vis_edges)
    
  }
  
  output$Metabolism_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Metabolism_vis_nodes <- Both_Metabolism_vis_nodes_dataset()
    
    Both_Metabolism_vis_edges <- Both_Metabolism_vis_edges_dataset()
    
    layout <- input$Metabolism_Both_NetworkLayout
    
    visNetwork(Both_Metabolism_vis_nodes, Both_Metabolism_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Metabolism_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Metabolism_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Metabolism_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Metabolism_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexMetabolismRatios[ComplexMetabolismRatios$ratio > input$Metabolism_Complex_FilterSlider,]$Complex
    
    MetabolismWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    MetabolismWodakTable <- MetabolismWodakTable[complete.cases(MetabolismWodakTable),]
    
    MetabolismWodakTable <- MetabolismWodakTable[,3:8]
    
    ratio_table <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex %in% MetabolismWodakTable$Complex,]
    
    MetabolismWodakTable <- merge(MetabolismWodakTable, ratio_table, by = "Complex")
    
    MetabolismWodakTable <- MetabolismWodakTable[,c(-7,-8)]
    
    MetabolismWodakTable <- MetabolismWodakTable %>% arrange(ratio)
    
    MetabolismWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Metabolism_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexMetabolismRatios[ComplexMetabolismRatios$ratio > input$Metabolism_Complex_FilterSlider,]$Complex
    
    ComplexMetabolismDotPlot <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex %in% WodakFilter,]
    
    ComplexMetabolismDotPlot <- ComplexMetabolismDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexMetabolismDotPlot$Complex <- factor(x = ComplexMetabolismDotPlot$Complex, levels = ComplexMetabolismDotPlot$Complex)
    
    ComplexMetabolismDotPlot$ratio <- round(ComplexMetabolismDotPlot$ratio, digits = 2)
    
    ComplexMetabolismDotPlot <- ggplot(ComplexMetabolismDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexMetabolismDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexMetabolismDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexMetabolismDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 85, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexMetabolismDotPlot <- ggplotly(ComplexMetabolismDotPlot, tooltip = "text")
    
    print(ComplexMetabolismDotPlot)
    
  })
  
  output$Metabolism_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexMetabolismRatios[ComplexMetabolismRatios$ratio > input$Metabolism_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Metabolism_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Metabolism_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Metabolism_Complex_RadioButton_Selection) > 0) {
      
      # input$Metabolism_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]$Name
      
      Complex_Metabolism_vis_edges <- Both_Metabolism_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$filter == T,]
      
      Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[,1:3]
      
      Complex_Metabolism_vis_nodes <- Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$id %in% unique(c(Complex_Metabolism_vis_edges$from, Complex_Metabolism_vis_edges$to)), ]
      
      Complex_Metabolism_vis_nodes$type <- "Prey"
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                                                                                  complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                                                                                  complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      MetabolismTargets <- Complex_Metabolism_vis_nodes[grep(pattern = "Bait", x = Complex_Metabolism_vis_nodes$type),]$id
      
      ComplexMetabolismBarPlot <- c()
      
      for (j in 1:length(MetabolismTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$from == MetabolismTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex == input$Metabolism_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = MetabolismTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexMetabolismBarPlot <- rbind(ComplexMetabolismBarPlot, loop_df)
        
      }
      
      ComplexMetabolismBarPlot <- ComplexMetabolismBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexMetabolismBarPlot <- ComplexMetabolismBarPlot[ComplexMetabolismBarPlot$ratio > 0, ]
      
      ComplexMetabolismBarPlot$Target <- factor(x = ComplexMetabolismBarPlot$Target, levels = unique(ComplexMetabolismBarPlot$Target))
      
      ComplexMetabolismBarPlot <- ggplot(ComplexMetabolismBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      print(ComplexMetabolismBarPlot)
      
    }
    
  })
  
  Complex_Metabolism_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]$Name
    
    Complex_Metabolism_vis_edges <- Both_Metabolism_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$filter == T,]
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[,1:3]
    
    Complex_Metabolism_vis_nodes <- Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$id %in% unique(c(Complex_Metabolism_vis_edges$from, Complex_Metabolism_vis_edges$to)), ]
    
    Complex_Metabolism_vis_nodes$type <- "Prey"
    
    Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                                                                                complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                                                                                complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Metabolism_Complex_NodeSize == T) {
      
      Complex_Metabolism_vis_nodes$size <- Complex_Metabolism_vis_nodes$n
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$size == 1, ]$size <- input$Metabolism_Complex_NodeSliderSize
      
    }else{
      
      Complex_Metabolism_vis_nodes$size <- input$Metabolism_Complex_NodeSliderSize
      
    }
    
    Complex_Metabolism_vis_nodes$color.background <- "#b4b4b4"
    Complex_Metabolism_vis_nodes$color.border <- "black"
    
    if (length(input$Metabolism_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Metabolism_Complex_NodeColour))) {
        
        if (any(Complex_Metabolism_vis_nodes$type == "Bait")) {
          
          Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Metabolism_Complex_NodeColour))) {
        
        if (any(Complex_Metabolism_vis_nodes$type == "Prey")) {
          
          Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Metabolism_Complex_NodeColour))) {
        
        if (any(Complex_Metabolism_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Metabolism_Complex_NodeColour))) {
        
        if (any(Complex_Metabolism_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Metabolism_vis_nodes)
    
  }
  
  Complex_Metabolism_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]$Name
    
    Complex_Metabolism_vis_edges <- Both_Metabolism_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$filter == T,]
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[,1:3]
    
    Complex_Metabolism_vis_nodes <- Both_Metabolism_vis_nodes[Both_Metabolism_vis_nodes$id %in% unique(c(Complex_Metabolism_vis_edges$from, Complex_Metabolism_vis_edges$to)), ]
    
    Complex_Metabolism_vis_nodes$type <- "Prey"
    
    Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey",]$id),
                                                                                complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% intersect(unique(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id),
                                                                                complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Metabolism_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Metabolism_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Metabolism_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Metabolism_vis_edges$type == "Both")) {
          
          Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Metabolism_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Metabolism_vis_edges$type == "PPI")) {
          
          Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Metabolism_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Metabolism_vis_edges$type == "RDI")) {
          
          Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Complex_Metabolism_vis_edges$width <- input$Metabolism_Complex_EdgeThickness
    
    Complex_Metabolism_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Metabolism_vis_edges$to)
    
    return(Complex_Metabolism_vis_edges)
    
  }
  
  output$Metabolism_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes_dataset_v01()
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges_dataset_v01()
    
    layout <- input$Metabolism_Complex_NetworkLayout
    
    visNetwork(Complex_Metabolism_vis_nodes, Complex_Metabolism_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Metabolism_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Metabolism_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Metabolism_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Metabolism_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Metabolism_vis_nodes_dataset_v02  <- function(){
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes_dataset_v01()
    
    Complex_Metabolism_vis_nodes$size <- input$Metabolism_Complex_NodeSliderSize
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges_dataset_v01()
    
    MetabolismTargets <- Complex_Metabolism_vis_nodes[grep(pattern = "Bait", x = Complex_Metabolism_vis_nodes$type),]$id
    
    ComplexMetabolismBarPlot <- c()
    
    for (j in 1:length(MetabolismTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$from == MetabolismTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex == input$Metabolism_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = MetabolismTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexMetabolismBarPlot <- rbind(ComplexMetabolismBarPlot, loop_df)
      
    }
    
    ComplexMetabolismBarPlot <- ComplexMetabolismBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexMetabolismBarPlot[ComplexMetabolismBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id[Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$from %in% Complex_Metabolism_vis_nodes$id,]
    
    return(Complex_Metabolism_vis_nodes)
    
  }
  
  Complex_Metabolism_vis_edges_dataset_v02  <- function(){
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes_dataset_v01()
    
    Complex_Metabolism_vis_nodes$size <- input$Metabolism_Complex_NodeSliderSize
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges_dataset_v01()
    
    MetabolismTargets <- Complex_Metabolism_vis_nodes[grep(pattern = "Bait", x = Complex_Metabolism_vis_nodes$type),]$id
    
    ComplexMetabolismBarPlot <- c()
    
    for (j in 1:length(MetabolismTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Metabolism_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$from == MetabolismTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexMetabolismRatios[ComplexMetabolismRatios$Complex == input$Metabolism_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = MetabolismTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexMetabolismBarPlot <- rbind(ComplexMetabolismBarPlot, loop_df)
      
    }
    
    ComplexMetabolismBarPlot <- ComplexMetabolismBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexMetabolismBarPlot[ComplexMetabolismBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id[Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes[Complex_Metabolism_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$from %in% Complex_Metabolism_vis_nodes$id,]
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges[Complex_Metabolism_vis_edges$to %in% Complex_Metabolism_vis_nodes$id,]
    
    return(Complex_Metabolism_vis_edges)
    
  }
  
  output$Metabolism_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Metabolism_vis_nodes <- Complex_Metabolism_vis_nodes_dataset_v02()
    
    Complex_Metabolism_vis_edges <- Complex_Metabolism_vis_edges_dataset_v02()
    
    layout <- input$Metabolism_Complex_NetworkLayout
    
    visNetwork(Complex_Metabolism_vis_nodes, Complex_Metabolism_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Metabolism_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Metabolism_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Metabolism_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Metabolism_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Metabolism_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Metabolism_Complex_BarplotTitle <- renderText({
    paste0(input$Metabolism_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Metabolism_Complex_Networkv01Title <- renderText({
    paste0(input$Metabolism_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Metabolism_Complex_Networkv02Title <- renderText({
    paste0(input$Metabolism_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  ############################
  
  #############################
  #### Degradation section ####
  
  PPI_Degradation_vis_nodes_dataset  <- function(){
    
    PPI_Degradation_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = PPI_Degradation_vis_nodes$id)
    
    PPI_Degradation_vis_nodes$title <- PPI_Degradation_vis_nodes$id
    
    if (input$Degradation_PPI_NodeSize == T) {
      
      PPI_Degradation_vis_nodes$size <- PPI_Degradation_vis_nodes$n
      
      PPI_Degradation_vis_nodes[PPI_Degradation_vis_nodes$size == 1, ]$size <- input$Degradation_PPI_NodeSliderSize
      
    }else{
      
      PPI_Degradation_vis_nodes$size <- input$Degradation_PPI_NodeSliderSize
      
    }
    
    PPI_Degradation_vis_nodes$color.background <- "#A6CEE3"
    PPI_Degradation_vis_nodes$color.border <- "black"
    
    if (length(input$Degradation_PPI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Degradation_PPI_NodeColour))) {
        
        if (any(PPI_Degradation_vis_nodes$type == "Bait")) {
          
          PPI_Degradation_vis_nodes[PPI_Degradation_vis_nodes$type == "Bait", ]$color.background <- "#A6D854"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Degradation_PPI_NodeColour))) {
        
        if (any(PPI_Degradation_vis_nodes$type == "BioGRID")) {
          
          PPI_Degradation_vis_nodes[PPI_Degradation_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Degradation_PPI_NodeColour))) {
        
        if (any(PPI_Degradation_vis_nodes$type == "RBP census")) {
          
          PPI_Degradation_vis_nodes[PPI_Degradation_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Degradation_PPI_NodeColour))) {
        
        if (any(PPI_Degradation_vis_nodes$type == "BioGRID & RBP census")) {
          
          PPI_Degradation_vis_nodes[PPI_Degradation_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(PPI_Degradation_vis_nodes)
    
  }
  
  PPI_Degradation_vis_edges_dataset  <- function(){
    
    PPI_Degradation_vis_edges$color <- "#b4b4b4"
    
    PPI_Degradation_vis_edges$width <- input$Degradation_PPI_EdgeThickness
    
    PPI_Degradation_vis_edges$to <- gsub(pattern = ",", replacement = "",x = PPI_Degradation_vis_edges$to)
    
    return(PPI_Degradation_vis_edges)
    
  }
  
  output$Degradation_PPI_Interactive_Network <- renderVisNetwork({
    
    PPI_Degradation_vis_nodes <- PPI_Degradation_vis_nodes_dataset()
    
    PPI_Degradation_vis_edges <- PPI_Degradation_vis_edges_dataset()
    
    layout <- input$Degradation_PPI_NetworkLayout
    
    visNetwork(PPI_Degradation_vis_nodes, PPI_Degradation_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Degradation_PPI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
    # net <- graph_from_data_frame(d = PPI_Degradation_vis_edges, vertices = PPI_Degradation_vis_nodes)
    # 
    # cytoscapePing()
    # 
    # createNetworkFromIgraph(net,"myIgraph")
    
    
  })
  
  output$Degradation_PPI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_PPI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(PPI_Degradation_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_PPI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_PPI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(PPI_Degradation_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  RDI_Degradation_vis_nodes_dataset  <- function(){
    
    RDI_Degradation_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = RDI_Degradation_vis_nodes$id)
    
    RDI_Degradation_vis_nodes$title <- RDI_Degradation_vis_nodes$id
    
    if (input$Degradation_RDI_NodeSize == T) {
      
      RDI_Degradation_vis_nodes$size <- RDI_Degradation_vis_nodes$n
      
      RDI_Degradation_vis_nodes[RDI_Degradation_vis_nodes$size == 1, ]$size <- input$Degradation_RDI_NodeSliderSize
      
    }else{
      
      RDI_Degradation_vis_nodes$size <- input$Degradation_RDI_NodeSliderSize
      
    }
    
    RDI_Degradation_vis_nodes$color.background <- "#A6CEE3"
    RDI_Degradation_vis_nodes$color.border <- "black"
    
    if (length(input$Degradation_RDI_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Degradation_RDI_NodeColour))) {
        
        if (any(RDI_Degradation_vis_nodes$type == "Bait")) {
          
          RDI_Degradation_vis_nodes[RDI_Degradation_vis_nodes$type == "Bait", ]$color.background <- "#A6D854"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Degradation_RDI_NodeColour))) {
        
        if (any(RDI_Degradation_vis_nodes$type == "BioGRID")) {
          
          RDI_Degradation_vis_nodes[RDI_Degradation_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Degradation_RDI_NodeColour))) {
        
        if (any(RDI_Degradation_vis_nodes$type == "RBP census")) {
          
          RDI_Degradation_vis_nodes[RDI_Degradation_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Degradation_RDI_NodeColour))) {
        
        if (any(RDI_Degradation_vis_nodes$type == "BioGRID & RBP census")) {
          
          RDI_Degradation_vis_nodes[RDI_Degradation_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(RDI_Degradation_vis_nodes)
    
  }
  
  RDI_Degradation_vis_edges_dataset  <- function(){
    
    RDI_Degradation_vis_edges$color <- "#b4b4b4"
    
    RDI_Degradation_vis_edges$width <- input$Degradation_RDI_EdgeThickness
    
    RDI_Degradation_vis_edges$to <- gsub(pattern = ",", replacement = "",x = RDI_Degradation_vis_edges$to)
    
    return(RDI_Degradation_vis_edges)
    
  }
  
  output$Degradation_RDI_Interactive_Network <- renderVisNetwork({
    
    RDI_Degradation_vis_nodes <- RDI_Degradation_vis_nodes_dataset()
    
    RDI_Degradation_vis_edges <- RDI_Degradation_vis_edges_dataset()
    
    layout <- input$Degradation_RDI_NetworkLayout
    
    visNetwork(RDI_Degradation_vis_nodes, RDI_Degradation_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Degradation_RDI_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
    
  })
  
  output$Degradation_RDI_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_RDI_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(RDI_Degradation_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_RDI_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_RDI_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(RDI_Degradation_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  Both_Degradation_vis_nodes_dataset  <- function(){
    
    Both_Degradation_vis_nodes$id <- gsub(pattern = ",", replacement = "",x = Both_Degradation_vis_nodes$id)
    
    Both_Degradation_vis_nodes$title <- Both_Degradation_vis_nodes$id
    
    if (input$Degradation_Both_NodeSize == T) {
      
      Both_Degradation_vis_nodes$size <- Both_Degradation_vis_nodes$n
      
      Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$size == 1, ]$size <- input$Degradation_Both_NodeSliderSize
      
    }else{
      
      Both_Degradation_vis_nodes$size <- input$Degradation_Both_NodeSliderSize
      
    }
    
    Both_Degradation_vis_nodes$color.background <- "#A6CEE3"
    Both_Degradation_vis_nodes$color.border <- "black"
    
    if (length(input$Degradation_Both_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Degradation_Both_NodeColour))) {
        
        if (any(Both_Degradation_vis_nodes$type == "Bait")) {
          
          Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$type == "Bait", ]$color.background <- "#A6D854"
          
        }
        
      }
      
      if (any(grepl(pattern = "^BioGRID$", x = input$Degradation_Both_NodeColour))) {
        
        if (any(Both_Degradation_vis_nodes$type == "BioGRID")) {
          
          Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$type == "BioGRID", ]$color.background <- "#969696"
          
        }
        
      }
      
      if (any(grepl(pattern = "^RBP census$", x = input$Degradation_Both_NodeColour))) {
        
        if (any(Both_Degradation_vis_nodes$type == "RBP census")) {
          
          Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$type == "RBP census", ]$color.background <- "#1F78B4"
          
        }
        
      }
      
      if (any(grepl(pattern = "\\&", x = input$Degradation_Both_NodeColour))) {
        
        if (any(Both_Degradation_vis_nodes$type == "BioGRID & RBP census")) {
          
          Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$type == "BioGRID & RBP census", ]$color.background <- "#525252"
          
        }
        
      }
      
    }
    
    return(Both_Degradation_vis_nodes)
    
  }
  
  Both_Degradation_vis_edges_dataset  <- function(){
    
    Both_Degradation_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Degradation_Both_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Degradation_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Degradation_vis_edges$type == "Both")) {
          
          Both_Degradation_vis_edges[Both_Degradation_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Degradation_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Degradation_vis_edges$type == "PPI")) {
          
          Both_Degradation_vis_edges[Both_Degradation_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Degradation_Both_EdgeColour,collapse = "_"))) {
        
        if (any(Both_Degradation_vis_edges$type == "RDI")) {
          
          Both_Degradation_vis_edges[Both_Degradation_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Both_Degradation_vis_edges$width <- input$Degradation_Both_EdgeThickness
    
    Both_Degradation_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Both_Degradation_vis_edges$to)
    
    return(Both_Degradation_vis_edges)
    
  }
  
  output$Degradation_Both_Interactive_Network <- renderVisNetwork({
    
    Both_Degradation_vis_nodes <- Both_Degradation_vis_nodes_dataset()
    
    Both_Degradation_vis_edges <- Both_Degradation_vis_edges_dataset()
    
    layout <- input$Degradation_Both_NetworkLayout
    
    visNetwork(Both_Degradation_vis_nodes, Both_Degradation_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Degradation_Both_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Degradation_Both_NodeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Both_NetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Both_Degradation_vis_nodes_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_Both_EdgeDownload <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Both_NetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Both_Degradation_vis_edges_dataset(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_Complex_WodakTable <- renderDataTable({
    
    WodakFilter <- ComplexDegradationRatios[ComplexDegradationRatios$ratio > input$Degradation_Complex_FilterSlider,]$Complex
    
    DegradationWodakTable <- ComplexWodak[ComplexWodak$Complex %in% WodakFilter,]
    
    DegradationWodakTable <- DegradationWodakTable[complete.cases(DegradationWodakTable),]
    
    DegradationWodakTable <- DegradationWodakTable[,3:8]
    
    ratio_table <- ComplexDegradationRatios[ComplexDegradationRatios$Complex %in% DegradationWodakTable$Complex,]
    
    DegradationWodakTable <- merge(DegradationWodakTable, ratio_table, by = "Complex")
    
    DegradationWodakTable <- DegradationWodakTable[,c(-7,-8)]
    
    DegradationWodakTable <- DegradationWodakTable %>% arrange(ratio)
    
    DegradationWodakTable %>%
      datatable(options = list(pageLength = 4, autoWidth = TRUE), 
                rownames = F) %>%
      formatRound(c("ratio","Jaccard_Index"), digits = 2)
    
  })
  
  output$Degradation_Complex_DotPlot <- renderPlotly({
    
    WodakFilter <- ComplexDegradationRatios[ComplexDegradationRatios$ratio > input$Degradation_Complex_FilterSlider,]$Complex
    
    ComplexDegradationDotPlot <- ComplexDegradationRatios[ComplexDegradationRatios$Complex %in% WodakFilter,]
    
    ComplexDegradationDotPlot <- ComplexDegradationDotPlot %>% arrange(desc(n_complex),ratio)
    
    ComplexDegradationDotPlot$Complex <- factor(x = ComplexDegradationDotPlot$Complex, levels = ComplexDegradationDotPlot$Complex)
    
    ComplexDegradationDotPlot$ratio <- round(ComplexDegradationDotPlot$ratio, digits = 2)
    
    ComplexDegradationDotPlot <- ggplot(ComplexDegradationDotPlot) +
      geom_segment(aes(x=Complex, xend=Complex, y=n_complex, yend=n_network), color="#666666") +
      geom_point(data = subset(ComplexDegradationDotPlot, ratio < 1), aes(x=Complex, y=n_complex, label = ratio), color="#FDBF6F", size=4,) +
      geom_point(data = subset(ComplexDegradationDotPlot, ratio < 1), aes(x=Complex, y=n_network, label = ratio), color="#CAB2D6", size=4) +
      geom_point(data = subset(ComplexDegradationDotPlot, ratio == 1), aes(x=Complex, y=n_complex, label = ratio), 
                 color="#CAB2D6", fill = "#FDBF6F", shape = 21, size=4, stroke =1) +
      coord_flip()+
      scale_y_continuous(breaks = seq(0, 30, by = 5))+
      theme_minimal() +
      theme(axis.text=element_text(size=10),
            axis.title=element_text(size=15,face="bold"))+
      theme(legend.text = element_text(size = 15),
            legend.title=element_text(size=10,face="bold"))+
      theme(legend.key.size = unit(1, 'cm'))+
      theme(legend.position = "top")+
      xlab("Protein complex") +
      ylab("Number of proteins")
    
    # ComplexDegradationDotPlot <- ggplotly(ComplexDegradationDotPlot, tooltip = "text")
    
    print(ComplexDegradationDotPlot)
    
  })
  
  output$Degradation_Complex_RadioButton <- renderUI({
    
    WodakFilter <- ComplexDegradationRatios[ComplexDegradationRatios$ratio > input$Degradation_Complex_FilterSlider,]$Complex
    
    FilteredComplexes <- ComplexDegradationRatios[ComplexDegradationRatios$Complex %in% WodakFilter,]$Complex
    
    radioButtons("Degradation_Complex_RadioButton_Selection","Choose a complex:", choices=FilteredComplexes)
  })
  
  output$Degradation_Complex_BarPlot <- renderPlotly({
    
    if (length(input$Degradation_Complex_RadioButton_Selection) > 0) {
      
      # input$Degradation_Complex_RadioButton_Selection
      
      complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]$Name
      
      Complex_Degradation_vis_edges <- Both_Degradation_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
      
      Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$filter == T,]
      
      Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[,1:3]
      
      Complex_Degradation_vis_nodes <- Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$id %in% unique(c(Complex_Degradation_vis_edges$from, Complex_Degradation_vis_edges$to)), ]
      
      Complex_Degradation_vis_nodes$type <- "Prey"
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
      
      if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                           complex_ids))) {
        
        Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                                                                                      complex_ids),]$type <- "Prey & Complex member"
        
      }
      
      if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                           complex_ids)) > 0 ) {
        
        Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                                                                                      complex_ids),]$type <- "Bait & Complex member"
        
      }
      
      
      
      DegradationTargets <- Complex_Degradation_vis_nodes[grep(pattern = "Bait", x = Complex_Degradation_vis_nodes$type),]$id
      
      ComplexDegradationBarPlot <- c()
      
      for (j in 1:length(DegradationTargets)) {
        
        complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]
        
        loop_targets <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$from == DegradationTargets[j],]
        
        loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
        
        loop_n <- ComplexDegradationRatios[ComplexDegradationRatios$Complex == input$Degradation_Complex_RadioButton_Selection,]$n_complex
        
        loop_ratio <- loop_overlap/loop_n
        
        loop_df <- data.frame(Target = DegradationTargets[j],
                              n_complex = loop_n,
                              n_network = loop_overlap,
                              ratio = loop_ratio)
        
        ComplexDegradationBarPlot <- rbind(ComplexDegradationBarPlot, loop_df)
        
      }
      
      ComplexDegradationBarPlot <- ComplexDegradationBarPlot  %>% arrange(desc(n_network),ratio)
      
      ComplexDegradationBarPlot <- ComplexDegradationBarPlot[ComplexDegradationBarPlot$ratio > 0, ]
      
      ComplexDegradationBarPlot$Target <- factor(x = ComplexDegradationBarPlot$Target, levels = unique(ComplexDegradationBarPlot$Target))
      
      ComplexDegradationBarPlot <- ggplot(ComplexDegradationBarPlot, aes(fill = ratio,y = n_network , x= Target)) +
        geom_bar(position=position_dodge(width = 1), stat="identity", size = 1.5)+
        scale_fill_viridis(breaks = c(0,0.2,0.4,0.6,0.8,1), option = "G",direction = -1)+
        theme_minimal() +
        theme(axis.text.x=element_text(size=12,angle = 45, hjust = 1),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16,face="bold"))+
        theme(legend.text = element_text(size = 16),
              legend.title=element_text(size=12,face="bold"))+
        theme(legend.key.size = unit(0.75, 'cm'))+
        xlab("Bait")+
        ylab("Preys in the complex")
      
      
      print(ComplexDegradationBarPlot)
      
    }
    
  })
  
  Complex_Degradation_vis_nodes_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]$Name
    
    Complex_Degradation_vis_edges <- Both_Degradation_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$filter == T,]
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[,1:3]
    
    Complex_Degradation_vis_nodes <- Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$id %in% unique(c(Complex_Degradation_vis_edges$from, Complex_Degradation_vis_edges$to)), ]
    
    Complex_Degradation_vis_nodes$type <- "Prey"
    
    Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                                                                                    complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                                                                                    complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    if (input$Degradation_Complex_NodeSize == T) {
      
      Complex_Degradation_vis_nodes$size <- Complex_Degradation_vis_nodes$n
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$size == 1, ]$size <- input$Degradation_Complex_NodeSliderSize
      
    }else{
      
      Complex_Degradation_vis_nodes$size <- input$Degradation_Complex_NodeSliderSize
      
    }
    
    Complex_Degradation_vis_nodes$color.background <- "#b4b4b4"
    Complex_Degradation_vis_nodes$color.border <- "black"
    
    if (length(input$Degradation_Complex_NodeColour) > 0) {
      
      if (any(grepl(pattern = "^Bait$", x = input$Degradation_Complex_NodeColour))) {
        
        if (any(Complex_Degradation_vis_nodes$type == "Bait")) {
          
          Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait", ]$color.background <- "#6A3D9A"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey$", x = input$Degradation_Complex_NodeColour))) {
        
        if (any(Complex_Degradation_vis_nodes$type == "Prey")) {
          
          Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey", ]$color.background <- "#CAB2D6"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Prey & Complex member$", x = input$Degradation_Complex_NodeColour))) {
        
        if (any(Complex_Degradation_vis_nodes$type == "Prey & Complex member")) {
          
          Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey & Complex member", ]$color.background <- "#FDBF6F"
          
        }
        
      }
      
      if (any(grepl(pattern = "^Bait & Complex member$", x = input$Degradation_Complex_NodeColour))) {
        
        if (any(Complex_Degradation_vis_nodes$type == "Bait & Complex member")) {
          
          Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait & Complex member", ]$color.background <- "#FF7F00"
          
        }
        
      }
      
    }
    
    return(Complex_Degradation_vis_nodes)
    
  }
  
  Complex_Degradation_vis_edges_dataset_v01  <- function(){
    
    complex_ids <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]$Name
    
    Complex_Degradation_vis_edges <- Both_Degradation_vis_edges %>% group_by(from) %>% mutate(filter = ifelse(any(to %in% complex_ids), T, F))
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$filter == T,]
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[,1:3]
    
    Complex_Degradation_vis_nodes <- Both_Degradation_vis_nodes[Both_Degradation_vis_nodes$id %in% unique(c(Complex_Degradation_vis_edges$from, Complex_Degradation_vis_edges$to)), ]
    
    Complex_Degradation_vis_nodes$type <- "Prey"
    
    Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% BaitIDs,]$type <- "Bait"
    
    if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                         complex_ids))) {
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey",]$id),
                                                                                    complex_ids),]$type <- "Prey & Complex member"
      
    }
    
    if (length(intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                         complex_ids)) > 0 ) {
      
      Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% intersect(unique(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id),
                                                                                    complex_ids),]$type <- "Bait & Complex member"
      
    }
    
    Complex_Degradation_vis_edges$color <- "#b4b4b4"
    
    if (length(input$Degradation_Complex_EdgeColour) > 0) {
      
      if (grepl(pattern = "Both", x = paste0(input$Degradation_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Degradation_vis_edges$type == "Both")) {
          
          Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$type == "Both", ]$color <- "#666666"
          
        }
        
      }
      
      if (grepl(pattern = "PPI", x = paste0(input$Degradation_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Degradation_vis_edges$type == "PPI")) {
          
          Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$type == "PPI", ]$color <- "#66A61E"
          
        }
        
      }
      
      if (grepl(pattern = "RDI", x = paste0(input$Degradation_Complex_EdgeColour,collapse = "_"))) {
        
        if (any(Complex_Degradation_vis_edges$type == "RDI")) {
          
          Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$type == "RDI", ]$color <- "#D95F02"
          
        }
        
      }
      
    }
    
    Complex_Degradation_vis_edges$width <- input$Degradation_Complex_EdgeThickness
    
    Complex_Degradation_vis_edges$to <- gsub(pattern = ",", replacement = "",x = Complex_Degradation_vis_edges$to)
    
    return(Complex_Degradation_vis_edges)
    
  }
  
  output$Degradation_Complex_Interactive_Network_v01 <- renderVisNetwork({
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes_dataset_v01()
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges_dataset_v01()
    
    layout <- input$Degradation_Complex_NetworkLayout
    
    visNetwork(Complex_Degradation_vis_nodes, Complex_Degradation_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Degradation_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Degradation_Complex_NodeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Complex_GlobalNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Degradation_vis_nodes_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_Complex_EdgeDownload_v01 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Complex_GlobalNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Degradation_vis_edges_dataset_v01(), file,quote = F,row.names = F)
    }
  )
  
  Complex_Degradation_vis_nodes_dataset_v02  <- function(){
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes_dataset_v01()
    
    Complex_Degradation_vis_nodes$size <- input$Degradation_Complex_NodeSliderSize
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges_dataset_v01()
    
    DegradationTargets <- Complex_Degradation_vis_nodes[grep(pattern = "Bait", x = Complex_Degradation_vis_nodes$type),]$id
    
    ComplexDegradationBarPlot <- c()
    
    for (j in 1:length(DegradationTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$from == DegradationTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexDegradationRatios[ComplexDegradationRatios$Complex == input$Degradation_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = DegradationTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexDegradationBarPlot <- rbind(ComplexDegradationBarPlot, loop_df)
      
    }
    
    ComplexDegradationBarPlot <- ComplexDegradationBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexDegradationBarPlot[ComplexDegradationBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id[Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$from %in% Complex_Degradation_vis_nodes$id,]
    
    return(Complex_Degradation_vis_nodes)
    
  }
  
  Complex_Degradation_vis_edges_dataset_v02  <- function(){
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes_dataset_v01()
    
    Complex_Degradation_vis_nodes$size <- input$Degradation_Complex_NodeSliderSize
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges_dataset_v01()
    
    DegradationTargets <- Complex_Degradation_vis_nodes[grep(pattern = "Bait", x = Complex_Degradation_vis_nodes$type),]$id
    
    ComplexDegradationBarPlot <- c()
    
    for (j in 1:length(DegradationTargets)) {
      
      complex_loop <- ComplexWodak[ComplexWodak$Complex == input$Degradation_Complex_RadioButton_Selection, ]
      
      loop_targets <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$from == DegradationTargets[j],]
      
      loop_overlap <- length(intersect(complex_loop$Name, loop_targets$to))
      
      loop_n <- ComplexDegradationRatios[ComplexDegradationRatios$Complex == input$Degradation_Complex_RadioButton_Selection,]$n_complex
      
      loop_ratio <- loop_overlap/loop_n
      
      loop_df <- data.frame(Target = DegradationTargets[j],
                            n_complex = loop_n,
                            n_network = loop_overlap,
                            ratio = loop_ratio)
      
      ComplexDegradationBarPlot <- rbind(ComplexDegradationBarPlot, loop_df)
      
    }
    
    ComplexDegradationBarPlot <- ComplexDegradationBarPlot  %>% arrange(desc(n_network),ratio)
    
    ComplexBaitFilter <- ComplexDegradationBarPlot[ComplexDegradationBarPlot$ratio > 0, ]$Target
    
    ComplexBaitFilter <- unique(c(Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id[Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait",]$id %in% ComplexBaitFilter],
                                  Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Bait & Complex member",]$id,
                                  Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$type == "Prey & Complex member",]$id))
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes[Complex_Degradation_vis_nodes$id %in% ComplexBaitFilter,]
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$from %in% Complex_Degradation_vis_nodes$id,]
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges[Complex_Degradation_vis_edges$to %in% Complex_Degradation_vis_nodes$id,]
    
    return(Complex_Degradation_vis_edges)
    
  }
  
  output$Degradation_Complex_Interactive_Network_v02 <- renderVisNetwork({
    
    Complex_Degradation_vis_nodes <- Complex_Degradation_vis_nodes_dataset_v02()
    
    Complex_Degradation_vis_edges <- Complex_Degradation_vis_edges_dataset_v02()
    
    layout <- input$Degradation_Complex_NetworkLayout
    
    visNetwork(Complex_Degradation_vis_nodes, Complex_Degradation_vis_edges) %>%
      visIgraphLayout(layout = layout, randomSeed = 666) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),nodesIdSelection = T) %>% 
      visExport(type = "png", name = paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
                                            "_Degradation_Complex_Network"), float = "right", label = "Save network image", background = "white", style = "")
    
  })
  
  output$Degradation_Complex_NodeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Complex_ComplexNetworkNodes.csv")
    },
    content = function(file) {
      write.csv(Complex_Degradation_vis_nodes_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_Complex_EdgeDownload_v02 <- downloadHandler(
    
    filename = function() {
      paste0(gsub(pattern = "-",replacement = "",x = Sys.Date()),
             "_Degradation_Complex_ComplexNetworkEdges.csv")
    },
    content = function(file) {
      write.csv(Complex_Degradation_vis_edges_dataset_v02(), file,quote = F,row.names = F)
    }
  )
  
  output$Degradation_Complex_BarplotTitle <- renderText({
    paste0(input$Degradation_Complex_RadioButton_Selection, " bait ratio bar plot")
  })
  
  output$Degradation_Complex_Networkv01Title <- renderText({
    paste0(input$Degradation_Complex_RadioButton_Selection, ": Global network")
  })
  
  output$Degradation_Complex_Networkv02Title <- renderText({
    paste0(input$Degradation_Complex_RadioButton_Selection, ": Complex interactor network")
  })
  
  #############################
  
}
