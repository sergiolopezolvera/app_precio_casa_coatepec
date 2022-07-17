library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("¿En cuánto puedo vender mi casa en Coatepec, Veracruz?"),

    # Sidebar with a  input for number of bins 
    fluidRow(
      column(6,
                 selectInput("ubicacion", "¿Dónde se encuentra tu casa?", c("Campo Viejo",
                                                                     "Centro",
                                                                     "La Mata",
                                                                     "Nueva Nestle",
                                                                     "Rafael Hernandez Ochoa" ))
                 ),
      column(6,
             sliderInput("recamaras", "¿Cuántas recámaras tiene?", min = 1, max = 6, value = 3)
      )
          ),
    fluidRow(
      column(6,
             numericInput("superficie", "¿Cuántos metros cuadrados tiene de superficie?", value = 0)
      ),
      column(6,
          sliderInput("banos", "¿Cuántos baños tiene?", min = 1, max = 6, value = 3, step = 0.5)
          )
    ),
    fluidRow(
      actionButton("calcula", "¡Calcula el precio de venta!", class = "btn-block")
    ),
    
    fluidRow(
          textOutput("texto")
    ),
    
    fluidRow(
      verbatimTextOutput("precio")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$calcula,{
  output$texto <- renderText("¡Felicidades! Puedes anunciar tu casa con un precio de venta de:")
  output$precio <- renderPrint(1000000 * (-0.903748 + (input$recamaras * 0.612049) + (input$banos * 0.261302) + (input$superficie * 0.002974) + (ubi() * 0.0704)))
  })
  
  ubi <- reactive(if (input$ubicacion == "Campo Viejo") {
      0} else if (input$ubicacion == "Centro") {
          1} else if (input$ubicacion == "La Mata") {
              2} else if (input$ubicacion == "Nueva Nestle") {
                  3} else {4}
  )
  
  ubi <- reactive({
    switch(input$ubicacion,
           "Campo Viejo" = 0,
           "Centro" =  1,
           "La Mata" = 2,
           "Nueva Nestle" = 3,
           "Rafael Hernandez Ochoa" = 4)
    })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
