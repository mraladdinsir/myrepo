library(shiny)
library(DT)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(tidyr)
library(reprex)

ui <- fluidPage(
  
  # App title ----
  headerPanel("Barcode Scanning Dashboard"),
  
  fluidRow(
    tabsetPanel(  
      
      tabPanel("Tab 1",
               fluidRow(
                 column(2, align="center", uiOutput("select_fil1")),
                 column(1, align="center",uiOutput("select_fil2")),
                 column(2, align="center",uiOutput("select_fil3"))),
               fluidRow(
                 column(1, numericInput("num", label = h5("Day:"), value = 1)),
                 column(1, align="center",textInput("text", label = h5("CMMS:"), value = "g")),
                 column(1, align="center",textInput("text2", label = h5("Comments:"), value = "g")),
                 column(1, align="center",textInput("text3", label = h5("Initials"), value = "")),
                 column(3, align="center",actionButton("goButton", "Submit!")))
               
      )))
)


server <- function(input, output,session) {
  
  theoptions <- reactive({
    theoptions <- testtable()
    theoptions %>% filter(fruit==req(input$var1)) %>% filter(InOut==req(input$var2)) %>% filter(Destination==req(input$var3))
    
  })
  
  testtable<- function(){
    
    theoptions <- data.frame(
      fruit=c("kiwi", "kiwi","kiwi","kiwi","kiwi", 
                "apricol", "apricol",
                "apple","apple","apple","apple","apple","apple","apple","apple","apple","apple","apple",
                "banana","banana","banana","banana","banana","banana","banana","banana","banana","banana","banana","banana","banana",
                "strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry","strawberry"),
      InOut=c("In", "Out","Out","Out","Out",
              "In", "Out",
              "Out", "Out", "In","In","In","In","In","In","In","In","In",
              "Out", "Out", "In","In","In","In","In","In","In","In","In","In","In",
              "Out", "Out", "In","In","In","In","In","In","In","In","In","In","In"),
      Destination=c("EMPTY", "apple", "banana", "strawberry", "apricol",
                    "EMPTY", "EMPTY",
                    "Yellow Tag", "Red Tag", "Cabinet", "Position 2", "Position 3", "Position 4", "Position 5","Position 8","Position 9","Position 10","Position 11",
                    "Yellow Tag", "Red Tag", "Cabinet", "Position 1", "Position 2", "Position 3", "Position 4", "Position 5","Position 6","Position 7","Position 8","Position 9","Position 10",
                    "Yellow Tag", "Red Tag", "Cabinet", "Position 1", "Position 2", "Position 3", "Position 4", "Position 5","Position 6","Position 7","Position 8","Position 9","Position 10"))
    
  }
  
  output$select_fil1 <- renderUI({
    tbl2<-testtable()
    selectizeInput('var1', 'Select Fruit:', choices = c("select" = "", levels(tbl2$fruit)))
  })
  
  output$select_fil2 <- renderUI({
    tbl2<-testtable()
    choice_var2<- reactive({
      tbl2 %>% filter(fruit==req(input$var1)) %>% pull(InOut) %>% as.character()
    })
    selectizeInput('var2', 'In/Out', choices = c("select" = "", choice_var2())) 
  })
  
  output$select_fil3 <- renderUI({
    tbl2<-testtable()
    choice_var3<- reactive({
      tbl2 %>% filter(fruit==req(input$var1)) %>% filter(InOut==req(input$var2)) %>% pull(Destination) %>% as.character()
      
    })
    selectizeInput('var3', 'Select destination:', choices = c("select" = "", choice_var3()))
  })
  
  
  
  
}

shinyApp(ui, server)