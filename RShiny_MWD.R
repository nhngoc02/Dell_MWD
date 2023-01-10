
library(shiny)
library(shinythemes)
library(data.table)
library(dplyr)

# Read in the RF model
model <- readRDS("shiny_model.rds")


# User interface                  
ui <- pageWithSidebar(
  
  # Page header
  headerPanel('MWD Predictor'),
  
  # Input values
  sidebarPanel(
    tags$label(h3('Input parameters')),
    selectInput("Carrier", 
                label = "Carrier", 
                choices = list("Carrier 1" = "Carrier 1", "Carrier 2" = "Carrier 2", 
                               "Carrier 3" = "Carrier 3", "Carrier 4" = "Carrier 4", 
                               "Carrier 5" = "Carrier 5", "Carrier 6" = "Carrier 6", 
                               "Carrier 7" = "Carrier 7", "Carrier 8" = "Carrier 8",
                               "Carrier 9" = "Carrier 9", "Carrier 10" = "Carrier 10",
                               "Carrier 11" = "Carrier 11", "Carrier 12" = "Carrier 12",
                               "Carrier 13" = "Carrier 13", "Carrier 14" = "Carrier 14",
                               "Carrier 15" = "Carrier 15", "Carrier 16" = "Carrier 16",
                               "Carrier 17" = "Carrier 17", "Carrier 18" = "Carrier 18",
                               "Carrier 19" = "Carrier 19", "Carrier 20" = "Carrier 20",
                               "Carrier 21" = "Carrier 21", "Carrier 22" = "Carrier 22",
                               "Carrier 23" = "Carrier 23")),
    selectInput("ServiceLevel",
                label = "Service Level",
                choices = list("LTL", "LTL Priority", "Parcel 1 day", "Parcel 3 Day", "Parcel Ground", "Parcel 2 day", "Parcel Home", 
                               "Parcel SurePost", "Parcel WWS")),
    numericInput("TotalOrders", 
                 label = "Total Orders", 
                 value = 10),
    numericInput("TotalOrderQuantity", 
                 label = "Total Order Quantity", 
                 value = 20),
    numericInput("WeekInt", 
                 label = "Week number in quarter", 
                 value = 1),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)


# Server                           
server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Carrier",
               # "SpamFacilityCode",
               "ServiceLevel",
               "TotalOrders",
               "TotalOrderQuantity",
               "WeekInt"),
      Value = as.character(c(input$Carrier,
                             # input$SpamFacilityCode,
                             input$ServiceLevel,
                             input$TotalOrders,
                             input$TotalOrderQuantity,
                             input$WeekInt)),
      stringsAsFactors = FALSE)
    
    # x <- 0
    # df <- rbind(df)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), "MWD Percentage" = paste(round(predict(model,test)*100/test$TotalOrderQuantity, 2), " %"))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Create the shiny app            
shinyApp(ui = ui, server = server)