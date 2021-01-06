#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(DT)
library(plotly)

economics <- read.csv("economics.csv")

themes <- list("black & white" = theme_bw(),
               "classic" = theme_classic(),
               "dark" = theme_dark(),
               "minimal" = theme_minimal(),
               "light" = theme_light(),
               "line" = theme_linedraw())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    

    # Sidebar with a slider input for number of bins 
    navbarPage("Economia", 
               
               tabPanel("Graph",
                        
                        sidebarPanel(
                            dateRangeInput("daterange", "Elige fecha", start = as.Date("1967-07-01", "%Y-%m-%d"), end = as.Date("2015-04-01", "%Y-%m-%d"), 
                                           startview = "decade"),
                            
                            selectInput("yvariable", "Escoge variable Y", choices = c("pce", "pop", "psavert",
                                                                                      "uempmed", "unemploy")),
                            
                            radioButtons("definitions", "Elige variable", choices = c("pce", "pop", "psavert",
                                                                                      "uempmed", "unemploy")),
                            
                            selectInput("theme", "Select Theme", choices = names(themes), selected = "minimal"),
                            
                            textOutput("variable") %>% 
                                tagAppendAttributes(style = "color:black; font-weight:bold")
                            
                            
                            
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                            plotOutput("linegraph")
                            
                        )
                        
                        
               ),  #tabpanel - graph
               
               tabPanel("Table", 
                        
                        sidebarPanel(
                            dateRangeInput("tabledaterange", "Elige fecha", start = as.Date("1967-07-01", "%Y-%m-%d"), end = as.Date("2015-04-01", "%Y-%m-%d"), 
                                           startview = "decade"), 
                            
                            checkboxGroupInput("tablevariables", "Choose Variables", choices = c("date", "pce", "pop", "psavert",
                                                                                      "uempmed", "unemploy"))
                            
                            
                        ), 
                        
                        mainPanel(
                            dataTableOutput("table") 

                        )
                        
                        
                        ),  #tabpanel - table 
               
               
               tabPanel("Interactive Graphs", 
                        
                        sidebarPanel(
                          dateRangeInput("plotlydaterange", "Elige fecha", start = as.Date("1967-07-01", "%Y-%m-%d"), end = as.Date("2015-04-01", "%Y-%m-%d"), 
                                         startview = "decade"), 
                          
                          selectInput("plotlyvariables", "Escoge variable Y", choices = c("pce", "pop", "psavert",
                                                                                    "uempmed", "unemploy")),
                          
                          
                        ), 
                        
                        mainPanel(
                          plotlyOutput("plotlygraph")
                          
                        )
                        
                        
               ) #tabpanel - Interactive Graphs
                        
                        
                        
                        
                        
                        
                        
                        ) # navbarpage
        
        
        
        
        
        
        
        
        
    ) # fluid page 
    
    
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  datafilter1 <- reactive({
      
      economics %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
          filter(between(date,input$daterange[1], input$daterange[2])) 
        
  })
  
  vardef <- read.csv("vardef.csv")
  
  variableinfo <- reactive({
      
      vardef %>% mutate(definitions = as.character(definitions)) %>%
          filter(variables == input$definitions) %>% select(definitions)
  })
  
    plot_theme <- reactive({
        themes[[input$theme]]
    })

    output$linegraph <- renderPlot({
        
        ggplot(data = datafilter1()) + geom_point(aes_string(x = "date", y = input$yvariable), size = 0.5) + 
            ggtitle(paste(input$yvariable, "v.s. date between", input$daterange[1], "and", input$daterange[2])) + 
            plot_theme() + 
            theme(axis.text.x=element_text(angle=60, hjust=1)) + 
            theme(title = element_text(colour = "Black", size = 15, face = "bold"),
                  axis.text = element_text(size = 10)) 
    })
    
    output$variable <- renderText({
        paste(input$definitions, ":", variableinfo())
    })
    
    ###Table tab ----------------------------------
    
    datafilter2 <- reactive({
        
        economics %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
            filter(between(date,input$tabledaterange[1], input$tabledaterange[2])) %>% select(input$tablevariables) 
    })
    
    output$table <- renderDataTable({datafilter2() 
        
        
        
    })
    
    
    ### Plotly tab 
    
    datafilter3 <- reactive({
      
      economics %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
        filter(between(date,input$plotlydaterange[1], input$plotlydaterange[2])) 
      
    })
    
    output$plotlygraph <- renderPlotly({
    df <- datafilter3()
    df$yy <- df[[input$plotlyvariables]]
  
    plot<-plot_ly(df, x = ~date, y = ~yy, mode = "markers", type = "scatter") %>% 
      layout( xaxis = list( title= "Date"), 
              yaxis = list( title=input$plotlyvariables ) )
    plot
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
