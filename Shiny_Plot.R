#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# install.packages("shinydashboard")
library(shinydashboard)
library(openxlsx)
library(igraph)
library(plotly)
library(visNetwork)
library(DT)
library(tidyr)
#setwd("/Users/woolg/OneDrive/Documents/CARNEGIE/Research")

data <- read.csv("https://raw.githubusercontent.com/ClySun/MBACourseDesign/main/MON_2020_EdgeLists_Advice_NEW.csv")
edge <- data.frame(data$V1,data$V2)
edge <- transform(edge, data.V1 = as.character(data.V1), data.V2 = as.character(data.V2))
edgeL <- as.matrix(edge)
net <- graph_from_edgelist(edgeL)
A.size <- vcount(net)
A.dens <- round(edge_density(net, loops = FALSE),2)
A.dia <- round(diameter(net, directed = FALSE, unconnected = FALSE, weights = NULL),2)
A.avgpath <-round(mean_distance(net, directed = FALSE, unconnected = FALSE),2)

stats <- data.frame(Attributes = c('Number of People', 'Graph Density', 'Diameter', 'Average Path', 'Number of Clusters'),
                    values = c(A.size, A.dens, A.dia, A.avgpath, "NULL"))

#individual report
d.in <- data.frame(V1 = degree(net, v = V(net), mode =  "in",
                     loops = FALSE, normalized = FALSE))

d.out<- data.frame(V1 = degree(net, v = V(net), mode =  "out",
                                     loops = FALSE, normalized = FALSE))
d.bet <- data.frame(V1 = betweenness(net, V(net), directed=FALSE))
d.close <- data.frame(V1 = closeness(net, V(net)))
d.egen <- data.frame (V1 = evcent (net, directed = FALSE, scale = TRUE, weights = NULL,
                  options = igraph.arpack.default)$vector)
report <- data.frame(id = V(net)$name, indegree = d.in$V1, outdegree = d.out$V1, betweeness = d.bet$V1, closeness = d.close$V1,
                     egen = d.egen$V1)
report <- report %>% mutate(across(is.numeric, ~ round(., 2)))
sum.in <-as.data.frame(as.matrix(summary(d.in, digits=3), ncol = 2))[,3]
sum.out <-as.data.frame(as.matrix(summary(d.out, digits=3), ncol = 2))[,3]
sum.bet <-as.data.frame(as.matrix(summary(d.bet, digits=3), ncol = 2))[,3]
sum.close <-as.data.frame(as.matrix(summary(d.close, digits=3), ncol = 2))[,3]
sum.egen <-as.data.frame(as.matrix(summary(d.egen, digits=3), ncol = 2))[,3]
descrip.data <- data.frame("Indegree" = sum.in, "Outdegree" = sum.out, "Betweeness" = sum.bet, "Close Neighbors" = sum.close, "Egen Vector" = sum.egen)
fig <- plot_ly(
  type = 'table',
  header = list(
    values = c('<b>Attributes</b>', '<b>Value</b>'),
    line = list(color = '#506784'),
    fill = list(color = 'grey'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = rbind(
      c('Number of People', 'Graph Density', 'Diameter', 'Average Path', 'Number of Clusters'),
      c(A.size, A.dens, A.dia, A.avgpath, "NULL")),
    line = list(color = '#506784'),
    fill = list(color = c('white')),
    align = c('left', 'center'),
    font = list(color = c('#506784'), size = 12)
  ))
# Define UI for application that draws a histogram

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "MBA Network"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Holistic Report", tabName = "1", icon = icon("dashboard")),
      menuItem("Individual Report", tabName = "2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "1",
              fluidPage(
                
                fluidPage(
                  fluidRow(
                    column(
                      width = 12,
                      HTML("<h2> Network Graph No.1")),
                    column(
                      width = 12,
                      visNetworkOutput("plot1", height = "400px")),
                    column(
                      width = 12,
                      HTML("<h3> Overall Attributes")),
                    column(
                      width = 10,
                      dataTableOutput( "all_table")),
                    column(
                      width = 12, 
                      HTML("<h3> Individual Attributes")),
                    column(
                      width = 12, 
                      dataTableOutput( "all_indiv"))
                    )
                    )
                    )
                    
                  ),
      tabItem(tabName = "2",
              fluidPage(
                fluidRow(
                  column(
                    width = 12,
                    HTML("<h2> Network Graph No.1")),
                  column(
                    width = 12,
                    visNetworkOutput("plot2", height = "400px")),
                  column(
                    width = 12,
                    textInput("idNum", "Input your individual ID", value = "10100", width = NULL, placeholder = "Input your ID")
                  ),column(
                    width = 12, 
                    HTML("<h3> Individual Attributes")),
                  column(
                    width = 12, 
                    dataTableOutput( "dt_table")),
                  column(
                    width = 12,
                    HTML("<h3> See your stats in comparison to your peers")),
                  column(
                    width = 12,
                    dataTableOutput("descrip_table")
                  )
                )
              ))
                )
                  
                )
              )      
     
                                       

              

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot1 <- renderVisNetwork({
      visIgraph(net)%>%
        visOptions(highlightNearest = TRUE,
                 nodesIdSelection = list(enabled = TRUE, selected = "10100")) %>%
        visNodes(color = list(highlight = '#a0522d'))
    })
    output$plot2 <- renderVisNetwork({
      visIgraph(net)%>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = list(enabled = TRUE, selected = input$idNum))%>%
        visNodes(color = list(highlight = '#a0522d'))
    })
    df_data <- reactive({
      
      # 1. Read UI element
      id_selected <- input$idNum
      
      # 2. Filter data
      df <- report %>%
        filter(id == id_selected)
      
      # 3. Return result
      df
    })
    
    # 3. Datatable
    output$all_table <- renderDataTable({
      datatable(stats, options = list(dom = 't'))
    })
    output$all_indiv <- renderDataTable({
      datatable(report)
    })
    output$dt_table <- renderDataTable({
      datatable(df_data(), options = list(dom = 't'))
    })
    output$descrip_table <- renderDataTable({
      datatable(descrip.data, options = list(dom = 't'))
    })
   
}


# Run the application 
shinyApp(ui = ui, server = server)

