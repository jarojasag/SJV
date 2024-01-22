library(shiny)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  h1("Title Here"),
  h2("Explanation/Introduction Here"),
  sidebarLayout(
    mainPanel(
      plotOutput("CommPlot")
    ),
    sidebarPanel(
      sliderInput("solar", "Solar:", min = 0, max = 20000, value = 10000),
      sliderInput("wind", "Wind:", min = 0, max = 10000, value = 5000),
      sliderInput("bio", "Bioenergy:", min = 0, max = 3000, value = 1500),
      downloadButton("downloadData", "Download Portfolio .csv"),
      actionButton("resetValues", "Reset to Default")
    )
  )
)

server <- function(input, output, session) {
  
  solar_end <- reactive({
    rep(input$solar / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  wind_end <- reactive({
    rep(input$wind / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  bio_end <- reactive({
    rep(input$bio / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame <- reactive({
    data.frame(
      Installation = c(solar_end(), wind_end(), bio_end()),
      Commodity = rep(c("Solar", "Wind", "Bioenergy"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 3)
    )
  })
  
  output$CommPlot <- renderPlot({
    ggplot(commodity_frame(), aes(x = Year, y = Installation, fill = Commodity)) + 
      geom_area() +
      labs(y = "Installed Capacity (MW)")
  })
  
  output$downloadData <- downloadHandler(
    filename = "User Portfolio.csv",
    content = function(file) {
      write_csv(commodity_frame() %>% 
                  pivot_wider(names_from = Year, 
                              values_from = Installation),
                file)
    }
  )
  
  observeEvent(input$resetValues, {
    updateSliderInput(session, "solar", value = 10000)
    updateSliderInput(session, "wind", value = 5000)
    updateSliderInput(session, "bio", value = 1500)
  })
}

shinyApp(ui, server)