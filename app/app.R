#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
source("../core.r")
# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("ERICA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        shinyFilesButton("data_file",title="Choose xlsx File",multiple=FALSE,label="File:"),
        uiOutput("date_range"),
        uiOutput("products")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("query1_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # Process file
  shinyFileChoose(input,"data_file",roots=c(wd=".."))
  dt <- reactive({
    if(is.null(input$data_file))
      NULL
    else {
      file_path <- parseFilePaths(c(wd=".."),input$data_file)
      suppressWarnings(as.character(file_path$datapath) %>% load_sheets)
      }
    })
  tidy_dt <- reactive({dt() %>% tidy_sheets})
  date_range <- reactive({tidy_dt() %>% min_max_date})
  pds <- reactive({tidy_dt() %>% products})
  
  output$date_range = renderUI({
    if(is.null(input$data_file))
      dateRangeInput("some_random_stuff","Choose your date here",min="1995-9-17",max="2009-9-18")
    else
      dateRangeInput("date_range","Choose the dates:",min=date_range()$min,max=date_range()$max)
  })
  
  output$products = renderUI({
    if(is.null(input$data_file)) 
      checkboxGroupInput("some_other_random_stuff","Choose your product here",choices=c())
    else
      checkboxGroupInput("products","Choose your product here",choices=pds())
  })
  
   output$query1_plot <- renderPlot({
      if(is.null(input$data_file))
        ggplot()+geom_line(mapping=aes(x=c(1:10),y=c(1:10)*c(1:10)+4))
      else
        dt() %>% query1_then_plot(input$products,input$date_range[1],input$date_range[2])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

