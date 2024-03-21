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
######### Script ###########
############################

source('myui.R', local = TRUE)
source('myserver.R')

shinyApp(
  ui = ui,
  server = server
)

