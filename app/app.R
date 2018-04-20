
## This is a Shiny web application. You can run the application by clicking
## the 'Run App' button above.

## Find out more about building applications with Shiny here:

##    http://shiny.rstudio.com/


library(shiny)
library(shinyFiles)
## A more portable way to do this?
Sys.setlocale("LC_CTYPE","chinese")
source("../core.R",encoding="utf8")
## Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("ERICA"),

    
    tabsetPanel(type="tabs",
                tabPanel("Query 1",
                         ## Sidebar with a slider input for number of bins 
                         sidebarLayout(
                             sidebarPanel(shinyFilesButton("data_file",
                                                           title="Choose xlsx File",
                                                           multiple=FALSE,
                                                           label="File:"),
                                          uiOutput("date_range1"),
                                          uiOutput("products1")),
                             ## Show a plot of the generated distribution
                             mainPanel(
                                 plotOutput("query1_plot")
                             )
                         )),
                tabPanel("Query 2",
                         sidebarLayout(
                             sidebarPanel(uiOutput("date_range2"),
                                          radioButtons("unit",
                                                       "Unit",
                                                       choices=c('day',
                                                                 'week',
                                                                 'month',
                                                                 'bimonth',
                                                                 'quarter',
                                                                 'season',
                                                                 'halfyear',
                                                                 'year')),
                                          uiOutput("error_codes"),
                                          actionLink("selectall","Select All"),
                                          uiOutput("products2")),
                             mainPanel(
                                 plotOutput("query2_plot")
                             ))),
                tabPanel("File Conversion",
                         shinySaveButton("save",
                                             "Convert to xlsx",
                                             "Convert file to ...",
                                             filetype=list(xlsx="xlsx",csv="csv")))
                )
)

## Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    ## Process file
    
    shinyFileChoose(input,"data_file",roots=c(wd=".."))

    shinyFileSave(input,"save",roots=c(wd=".."))
    
    
    data <- reactive({
        if(is.null(input$data_file))
            NULL
        else {
            file_path <- parseFilePaths(c(wd=".."),input$data_file)
            
            suppressWarnings(as.character(file_path$datapath) %>% load_sheets)
        }
    })
    date_range <- reactive({data()[[1]] %>% min_max_date})
    pds <- reactive({data()[[1]] %>% products})
    
    output$date_range1 = renderUI({
        if(is.null(input$data_file))
            dateRangeInput("some_random_stuff",
                           "Choose your date here:",min="1995-9-17",
                           max="2009-9-18")
        else
            dateRangeInput("date_range1","Choose the dates:",
                           min=date_range()$min,max=date_range()$max)
    })

    output$date_range2 = renderUI({
        if(is.null(input$data_file))
            dateRangeInput("some_random_stuff",
                           "Choose your date here:",min="1995-9-17",
                           max="2009-9-18")
        else
            dateRangeInput("date_range2","Choose the dates:",
                           min=date_range()$min,max=date_range()$max)
    })
    
    output$products1 = renderUI({
        if(is.null(data())) 
            checkboxGroupInput("some_random_stuff",
                               "Products:",choices=c())
        else
            checkboxGroupInput("products1","Products:",choices=pds())
    })

    output$products2 = renderUI({
        if(is.null(data())) 
            checkboxGroupInput("some_random_stuff",
                               "Products:",choices=c())
        else
            checkboxGroupInput("products2","Products:",choices=pds())
    })

    output$error_codes = renderUI({
        if(is.null(data()))
            checkboxGroupInput("some_random_stuff",
                               "Error codes:",choices=c())
        else
            checkboxGroupInput("error_codes",
                               "Error codes:",choices=data()[[2]] %>% error_codes,
                               inline=TRUE)
    })
    
    output$query1_plot <- renderPlot({
        if(is.null(date()) || is.null(input$date_range1))
            ggplot()+geom_line(mapping=aes(x=c(1:10),y=c(1:10)*c(1:10)+4))
        else
        {
            data() %>% query1(input$products1,input$date_range1[1],input$date_range1[2])
        }
    })

    

    output$query2_plot <- renderPlot({
        if(is.null(data())||is.null(input$date_range2))
            ggplot()+geom_line(mapping=aes(x=c(1:10),y=-c(1:10)*c(1:10)+4))
        else
        {
            data() %>% query2_natural(input$unit,
                                      input$products2,
                                      input$date_range2[1],
                                      input$date_range2[2],
                                      input$error_codes)
        }
    })

    observe({
        if(!is.null(data())&&!is.null(input$save) ){
            fileinfo <- parseSavePath(roots=(c("wd"="..")),input$save)
            if(fileinfo$type=="xlsx")
                left_join(data()[[1]],data()[[2]]) %>% export_sheets(as.character(fileinfo$datapath))
        }
    })

    observe({
        if(!is.null(data())){
            if(input$selectall %% 2 == 1){
                updateCheckboxGroupInput(session,
                                         "error_codes",
                                         selected=data()[[2]] %>% error_codes)
            }
            else {
                updateCheckboxGroupInput(session,
                                         "error_codes",
                                         choices=data()[[2]] %>% error_codes,
                                         selected=NULL,
                                         inline = TRUE)
            }
        }
    })
}

## Run the application 
shinyApp(ui = ui, server = server)

