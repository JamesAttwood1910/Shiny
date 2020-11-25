
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Data 

visadata <- read_csv("data/visadata.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Edad de inmigrantes"),
  br(),
  HTML("<p>El gráfico a la continuación muestra las edades de diferentes grupos de inmigrantes que llegaron a Santiago de Chile en 2019.
  Puedes elegir los cinco nacionalidades mas prominentes y cambiar el número de contenedores de histograma.
  Además se exponen la media, mediana, desviación estándar, edad mínima y edad máxima para cada grupo de inmigrantes.
  Se pueden descargar los datos
       <a href='https://www.extranjeria.gob.cl/media/2020/03/visas_otorgadas_2019.xlsx'>aquí.</a></p>"),
  br(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Selección"),
      selectInput("Nationality", "1. Escoje País de Nacimiento", choices = c("Venezuela" = "Venezuela", "Haití" = "Haití", "Colombia" ="Colombia", "Perú" = "Perú", "Bolivia" = "Bolivia"), selected = "Venezuela"),
      br(),
      sliderInput("bin", "2. Seleccione el número de contenedores de histograma", min=1, max=25, value= c(10)),
      br()
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("myhist"),
      tableOutput("mytable"),
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$myhist <- renderPlot(ggplot(visadata %>% filter(PROVINCIA == "Santiago"), aes(x = Age)) + 
                                geom_histogram(bins = input$bin, group=input$Nationality, 
                                               data=visadata[visadata$PAÍS == input$Nationality,],
                                               fill = "#F7B539", color = "black"))
  output$mytable <- renderTable(visadata %>%
                                  filter(PAÍS == input$Nationality) %>%
                                  summarise("Mean" = mean(Age), 
                                            "Median" = median(Age),
                                            "STDEV" = sd(Age), 
                                            "Min" = min(Age),
                                            "Max" = max(Age)))
  
}


# Run the application 
shinyApp(ui = ui, server = server)
