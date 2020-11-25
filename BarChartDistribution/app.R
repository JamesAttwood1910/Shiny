# Packages

library(shiny)
library(dplyr)
library(ggplot2)
library(colourpicker)
library(readr)

ui <- basicPage(
    
    h2("Nacionalidad df inmigrantes por comuna"),
    br(),
    HTML("<p>El grafico a la continuacion muestra la distribucion por las 32 comunas de inmigrantes que llegaron a la provincia de Santiago de Chile en 2019. Puedes elegir los cinco nacionalidades mas prominentes. Se pueden descargar los datos
       <a href='https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx'>aquí.</a></p>"),
    br(),
    selectInput(inputId = "pais",
                label = "Elige Nacionalidad",
                list("Bolivia","Colombia","Haití","Perú", "Venezuela")),
    br(),
    colourInput("col", "Elige Color", "purple"),
    plotOutput("plot")
)

server <- function(input, output, session) {
    
    visadata <- read_csv("data/visadata.csv")
    
    #Summarize Data and then Plot
    data <- reactive({
        req(input$pais)
        df <- visadata %>% filter(PROVINCIA == "Santiago") %>% group_by(PAÍS, COMUNA) %>% count() %>% arrange(COMUNA) %>%
            filter(PAÍS %in% input$pais) 
    })
    
    #Plot 
    output$plot <- renderPlot({
        g <- ggplot(data(), aes( y = n, x = COMUNA))
        g + geom_bar(stat = "sum", 
                     fill = input$col) +
            theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ylab("Población") + xlab("Comuna") +
            ggtitle(paste("Distribución de inmigrantes de", input$pais, "que llegaron en 2019")) + 
            theme(plot.title = element_text(size = rel(1.5), face = "bold"), axis.title = element_text(size = rel(1.2)),
                  axis.text = element_text(size = rel(1.1)))
    })
}

shinyApp(ui = ui, server = server)