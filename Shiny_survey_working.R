library(shiny)
library(DT)
library(purrr)
library(htmltools)
library(tidyverse)
library(shinydashboard)
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

namelist <- c("Clytze Sun", "Puxin Sun", "Brandy Aven", "Andrew Carnegie", "Raymond Reddington")
namedf <- data.frame(User = rep(namelist, each = 5), Connection = rep(namelist, times = 5), Rating = NA)
linebreaks <- function(n){HTML(strrep(br(), n))}
opening <- "<h4> Dear MON students, <br/> <br/> In this survey, you will be asked to report your feelings toward and relationships with your classmates. <br/> This information is only for classroom purposes and will be kept ANONYMOUS.<br/>
<br/>This survey will take less than 20 minutes to complete. It is best viewed on PC. <br/><br/>All data collected in this survey will be aggregated, so your individual responses will be anonymous. <br/> We will discuss the results during class.<br/><br/>Thank you,<br/>
Professor Aven"
ui <- fluidPage(
        tags$head(
          # Note the wrapping of the string in HTML()
          tags$style(HTML("
            #inTabset { display:none; }
            body {
              font-family: 'Times';
            }"))
        ),
        tabsetPanel(id = "inTabset",
              tabPanel("Read Me",
                       column(
                         width = 6, align = "left",
                         linebreaks(8),
                         HTML(opening)),
                       column(
                         width = 12, align = "center",
                         linebreaks(2),
                         actionButton(inputId = "start", label = "Next Page", style='font-family:Times')
                       )),
                         
                        
              tabPanel("Basic Information",
                              column(
                                width = 12, 
                                HTML("<h3> Select Your Name")),
                              column(
                                width = 12,
                                selectInput("user",NULL, choices = namelist)),
                              column(
                                width = 12,
                                br(),
                                br(),
                                HTML("<br> For whatever reason, if you don't want to put names in any of the boxes below, <br/>use the 'choose not to disclose' option instead of leaving it blank."),
                                HTML("<h3> Select People That You Can Trust In Class")),
                              column(
                                width = 12,
                                selectInput("friends",NULL, choices = c("Choose not to disclose", namelist), multiple = T)),
                              column(
                                width = 12,
                                br(),
                                br(),
                                HTML("<h3> Select People That You Would Go To for Advice")),
                              column(
                                width = 12,
                                selectInput("adv",NULL, choices = c("Choose not to disclose", namelist), multiple = T)),
                              column(
                                width = 12,
                                br(),
                                br(),
                                HTML("<h3> Select People That You Communicate With")),
                              column(
                                width = 12,
                                selectInput("comm",NULL, choices = c("Choose not to disclose",namelist), multiple = T)),
                              column(
                                width = 12, 
                                br(),
                                br(),
                                actionButton(inputId = "submitInfo", label = "Next Page", style='font-family:Times'),
                                HTML("<br> You cannot come back to this page once you submit your answer")
                              )),
              tabPanel("Question #1",
                          column(
                            width = 12,
                            HTML("<h4> On a scale of 1 - 5, 5 being the most trustworthy, how much do you trust the following people.")
                          ),
                          column(
                            width = 12, 
                            dataTableOutput( "table1")),
                          column(
                            width = 4,
                            actionButton("saveBtn", "Confirm Answers")
                          ),
                          column(
                            width = 12, 
                            HTML("<br> If you chose not to disclose whom you can trust in class, click 'confirm answers' and skip this page."),
                            uiOutput("continueBtn")
                          )
                  
              
                ),
                tabPanel("Question #2",
                           column(
                            width = 12,
                            HTML("<h4> On a scale of 1 - 5, 5 being the most frequent communication, how often do you communicate with the following people.")
                          ),
                          column(
                            width = 12, 
                            dataTableOutput( "table2")),
                          column(
                            width = 4,
                            actionButton("saveBtn2", "Confirm Answers")
                          ),
                          column(
                            width = 12, 
                            HTML("<br> If you chose not to disclose whom you communicate with in class, click 'confirm answers' and skip this page."),
                            uiOutput("continueBtn2")
                          )
                        ),
                tabPanel("Submit",

                            column(
                              width = 12, align = "center",
                              HTML("<h4>Thank you for your participation!")
                  
))))
server <- function(input, output, session){
  observeEvent(input$start, {
    updateTabsetPanel(session = session, inputId = "inTabset", selected = "Basic Information")
  })
  observeEvent(input$submitInfo, {
    updateTabsetPanel(session = session, inputId = "inTabset", selected = "Question #1")
  })
  observeEvent(input$continue, {
    updateTabsetPanel(session = session, inputId = "inTabset", selected = "Question #2")
  })
  observeEvent(input$continue2, {
    updateTabsetPanel(session = session, inputId = "inTabset", selected = "Submit")
  })

  vals <- reactiveValues(data = namedf, edited = namedf) #trust
  vals2 <-reactiveValues(data = namedf, edited = namedf) #communication
  
  output$table1 <- renderDataTable(DT::datatable(filter(vals$edited, User == input$user, Connection%in% input$friends), options = list(dom = 't'), editable =
                                                   list(target = "cell", disable = list(column = c("User","Connection")))
    ))
  #else{
   # noRes <- data.frame(message = "Skip to the next page.")
    #renderDataTable((datatable(noRes)))
  #}
  output$table2 <- renderDataTable(DT::datatable(filter(vals2$edited, User == input$user, Connection%in% input$comm), options = list(dom = 't'), editable =
                                                   list(target = "cell", disable = list(column = c("User","Connection")))
  ))
  ## Editing trust table                                               
  observeEvent(
    input$table1_cell_edit,{
      cat(input$table1_cell_edit$row)
      vals$edited[intersect(which(vals$edited$User == input$user),which(vals$edited$Connection == input$friends[as.numeric(input$table1_cell_edit$row)])),input$table1_cell_edit$col] <<-
        input$table1_cell_edit$value})
  ## Editing communication table
  observeEvent(
    input$table2_cell_edit,{
      cat(input$table2_cell_edit$row)
      vals2$edited[intersect(which(vals$edited$User == input$user),which(vals$edited$Connection == input$comm[as.numeric(input$table2_cell_edit$row)])),input$table2_cell_edit$col] <<-
        input$table2_cell_edit$value})
  
  observeEvent(input$submitInfo,{
    outputdf <- data.frame(User = input$user, Connection = input$adv)
    filename <- paste("advice_", as.character(input$user),".csv", sep = "")
    write.csv(outputdf, filename)
    
  })
  observeEvent(input$saveBtn,{
    outputdf <- vals$edited[which(!is.na(vals$edited[3])),]
    filename <- paste("trust_", as.character(input$user),".csv", sep = "")
    write.csv(outputdf, filename)
  
  })
  observeEvent(input$saveBtn2,{
    outputdf <- vals2$edited[which(!is.na(vals2$edited[3])),]
    filename <- paste("comm_", as.character(input$user),".csv", sep = "")
    write.csv(outputdf, filename)
    
  })
  
  observeEvent(input$saveBtn, {
    output$continueBtn <- renderUI({
      actionButton("continue", label = "Next Page")
    })
  })
  observeEvent(input$saveBtn2, {
    output$continueBtn2 <- renderUI({
      actionButton("continue2", label = "Next Page")
    })
  })
  
}

shinyApp(ui = ui, server = server)
