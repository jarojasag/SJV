library(shiny)
library(tidyverse)

ui <- fluidPage(
  h1("Title Here"),
  h2("Explanation/Introduction Here"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Electricity",
                 sliderInput("solar_electricity", "Solar:", min = 0, max = 20000, value = 10000),
                 sliderInput("wind_electricity", "Wind:", min = 0, max = 10000, value = 5000),
                 sliderInput("bio_electricity", "Bioenergy:", min = 0, max = 3000, value = 1500),
                 actionButton("resetValues_electricity", "Reset to Default (Electricity)")
        ),
        tabPanel("Hydrogen",
                 sliderInput("solar_hydrogen", "Solar:", min = 0, max = 20000, value = 10000),
                 sliderInput("natgas_hydrogen", "Natural Gas:", min = 0, max = 10000, value = 5000),
                 actionButton("resetValues_hydrogen", "Reset to Default (Hydrogen)")
        ),
        tabPanel("Biogas",
                 sliderInput("manure_biogas", "Manure:", min = 0, max = 5000, value = 2500),
                 sliderInput("organic_waste_biogas", "Organic Waste:", min = 0, max = 5000, value = 2500),
                 actionButton("resetValues_biogas", "Reset to Default (Biogas)")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Electricity",
                 plotOutput("CommPlot_electricity")
        ),
        tabPanel("Hydrogen",
                 plotOutput("CommPlot_hydrogen")
        ),
        tabPanel("Biogas",
                 plotOutput("CommPlot_biogas")
        )
      ),
      tabsetPanel(
        tabPanel("Download",
                 downloadButton("downloadData", "Download All Data")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Electricity
  solar_end_electricity <- reactive({
    rep(input$solar_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  wind_end_electricity <- reactive({
    rep(input$wind_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  bio_end_electricity <- reactive({
    rep(input$bio_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_electricity <- reactive({
    data.frame(
      Installation = c(solar_end_electricity(), wind_end_electricity(), bio_end_electricity()),
      Commodity = rep(c("Solar", "Wind", "Bioenergy"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 3)
    )
  })
  
  output$CommPlot_electricity <- renderPlot({
    ggplot(commodity_frame_electricity(), aes(x = Year, y = Installation, fill = Commodity)) + 
      geom_area() +
      labs(y = "Installed Capacity (MW)")
  })
  
  # Hydrogen
  solar_end_hydrogen <- reactive({
    rep(input$solar_hydrogen / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  natgas_end_hydrogen <- reactive({
    rep(input$natgas_hydrogen / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_hydrogen <- reactive({
    data.frame(
      Installation = c(solar_end_hydrogen(), natgas_end_hydrogen()),
      Commodity = rep(c("Solar", "Natural Gas"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 2)
    )
  })
  
  output$CommPlot_hydrogen <- renderPlot({
    ggplot(commodity_frame_hydrogen(), aes(x = Year, y = Installation, fill = Commodity)) + 
      geom_area() +
      labs(y = "Installed Capacity (MW)")
  })
  
  # Biogas
  manure_end_biogas <- reactive({
    rep(input$manure_biogas / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  organic_waste_end_biogas <- reactive({
    rep(input$organic_waste_biogas / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_biogas <- reactive({
    data.frame(
      Installation = c(manure_end_biogas(), organic_waste_end_biogas()),
      Commodity = rep(c("Manure", "Organic Waste"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 2)
    )
  })
  
  output$CommPlot_biogas <- renderPlot({
    ggplot(commodity_frame_biogas(), aes(x = Year, y = Installation, fill = Commodity)) + 
      geom_area() +
      labs(y = "Installed Capacity (MW)")
  })
  
  # Download all data
  output$downloadData <- downloadHandler(
    filename = "User Portfolio (All Data).csv",
    content = function(file) {
      write_csv(commodity_frame_electricity() %>% 
                  pivot_wider(names_from = Year, 
                              values_from = Installation),
                file)
      write_csv(commodity_frame_hydrogen() %>% 
                  pivot_wider(names_from = Year, 
                              values_from = Installation),
                file, append = TRUE)
      write_csv(commodity_frame_biogas() %>% 
                  pivot_wider(names_from = Year, 
                              values_from = Installation),
                file, append = TRUE)
    }
  )
  
  observeEvent(c(input$resetValues_electricity, input$resetValues_hydrogen, input$resetValues_biogas), {
    updateSliderInput(session, "solar_electricity", value = 10000)
    updateSliderInput(session, "wind_electricity", value = 5000)
    updateSliderInput(session, "bio_electricity", value = 1500)
    
    updateSliderInput(session, "solar_hydrogen", value = 10000)
    updateSliderInput(session, "natgas_hydrogen", value = 5000)
    
    updateSliderInput(session, "manure_biogas", value = 2500)
    updateSliderInput(session, "organic_waste_biogas", value = 2500)
  })
}

shinyApp(ui, server)