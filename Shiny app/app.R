library(shiny)
library(shinyjs)
library(ggplot2)
library(tidyverse)
library(scales)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h4, h5, h6 {
        font-size: 12pt;
      }
      axis.title, axis.text, legend.title, legend.text {
        font-size: 12pt !important;
      }
    "))
  ),
  shinyjs::useShinyjs(),
  
  h1("SJV Portfolio Design Tool (v 0.0)"),
  h2("About"),
  h5("This Portfolio Design Tool is part of the San Joaquin Valley Energy Visioning Toolkit supported by the Clean Air Task Force. You can use it to design a portfolio of additional energy buildouts in the San Joaquin Valley and download and email that portfolio to nidhi@rand.org to run through the SJV Energy Visioning Model. It will be evaluated in terms of energy produced, jobs created, land requirements, water requirements, and GHG emissions."),
  h2("Instructions"),
  h5("This tool presents a template portfolio of additional electricity, hydrogen, biomethane, and jet fuel energy buildouts in the SJV between 2025 and 2045. You can use the slider bars on the left to increase or decrease the buildout, switching between tabs to specify production of each energy type."),
  
  fluidRow(
    column(1, offset = 0, downloadButton("downloadData", "Download Portfolio", style = "color: #fff; background-color: #FB8072; border-color: #FB8072;"))
  ), 
  h5(" "),
  navbarPage(
    "",
    id = "tabs",
    tabPanel("Electricity",
             shinyjs::useShinyjs(),
             sidebarLayout(
               sidebarPanel(
                 helpText("Please specify the amount of electric resources in GW by 2045 from each of the following sources:"),
                 fluidRow(
                   column(10,
                          sliderInput("solar_electricity", "Solar:", min = 0, max = 100, value = 26.6, step = 1, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_solar_electricity", "", style = "font-size:12px; padding: 0px 6px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("wind_electricity", "Wind:", min = 0, max = 5, value = 0.55, step = 0.5, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_wind_electricity", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("battery_electricity", "Battery:", min = 0, max = 20, value = 14.8, step = 0.5, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_battery_electricity", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("long_duration_storage_electricity", "Long-Duration Storage:", min = 0, max = 2, value = 1, step = 0.5, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_long_duration_storage_electricity", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("biomass_electricity", "Biomass:", min = 0, max = 1, value = 0.04, step = 0.01, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_biomass_electricity", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),                 
                 actionButton("resetValues_electricity", "Reset to Default (Electricity)")
               ),
               mainPanel(
                 fluidRow(
                   column(8, plotOutput("CommPlot_electricity")),
                   column(4, 
                          h3("Portfolio Summary"),
                          h5("Your portfolio procedures are:"),
                          h5(htmlOutput("portfolioSummary_electricity"))
                   )
                 )
               )
             )
    ),
    tabPanel("Hydrogen",
             sidebarLayout(
               sidebarPanel(
                 helpText("Please specify the annual production of hydrogen in thousands of tons in 2045 from each of the following sources:"),
                 fluidRow(
                   column(10,
                          sliderInput("solar_hydrogen", "Solar:", min = 0, max = 500, value = 310, step = 50, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_solar_hydrogen", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("biomass_hydrogen", "Biomass:", min = 0, max = 500, value = 140, step = 50, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_biomass_hydrogen", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),                 
                 fluidRow(
                   column(10,
                          sliderInput("natural_gas_hydrogen", "Natural Gas:", min = 0, max = 500, value = 11, step = 10, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_natural_gas_hydrogen", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 actionButton("resetValues_hydrogen", "Reset to Default (Hydrogen)")
               ),
               mainPanel(
                 fluidRow(
                   column(8, plotOutput("CommPlot_hydrogen")),
                   column(4, 
                          h3("Portfolio Summary"),
                          h5("Your portfolio procedures are:"),
                          h5(htmlOutput("portfolioSummary_hydrogen"))
                   )
                 )
               )
             )
    ),
    tabPanel("Biomethane",
             sidebarLayout(
               sidebarPanel(
                 helpText("Please specify the annual production of biomethane in billions of cubic feet by 2045 from each of the following sources:"),
                 fluidRow(
                   column(10,
                          sliderInput("biomass_biomethane", "Biomass:", min = 0, max = 30, value = 6, step = 1, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_biomass_biomethane", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),                
                 actionButton("resetValues_biomethane", "Reset to Default (Biomethane)")
               ),
               mainPanel(
                 fluidRow(
                   column(8, plotOutput("CommPlot_biomethane")),
                   column(4, 
                          h3("Portfolio Summary"),
                          h5("Your portfolio procedures are:"),
                          h5(htmlOutput("portfolioSummary_biomethane"))
                   )
                 )
               )
             )
    ),
    tabPanel("Sustainable Aviation Fuel",
             sidebarLayout(
               sidebarPanel(
                 helpText("Please specify the annual production of sustainable aviation fuel in millions of gallons by 2045 from each of the following sources:"),
                 fluidRow(
                   column(10,
                          sliderInput("fats_oils_greases_saf", "Fats, Oils, and Greases:", min = 0, max = 10, value = 5, step = 1, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_fats_oils_greases_saf", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),
                 fluidRow(
                   column(10,
                          sliderInput("agriculture_forest_residue_saf", "Agriculture and Forest Residue:", min = 0, max = 100, value = 50, step = 5, ticks = FALSE)
                   ),
                   column(1,
                          div(style = "position: relative; top: 40px;",
                              actionButton("info_agriculture_forest_residue_saf", "", style = "font-size:12px; padding: 0px 5px;", icon = icon("question-circle"))
                          )
                   )
                 ),                
                 actionButton("resetValues_saf", "Reset to Default (SAF)")
               ),
               mainPanel(
                 fluidRow(
                   column(8, plotOutput("CommPlot_saf")),
                   column(4, 
                          h3("Portfolio Summary"),
                          h5("Your portfolio procedures are:"),
                          h5(htmlOutput("portfolioSummary_saf"))
                   )
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$tabs, {
    updateNavbarPage(session, "tabs", selected = input$tabs)
  })
  
  # Electricity
  solar_end_electricity <- reactive({
    rep(input$solar_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  wind_end_electricity <- reactive({
    rep(input$wind_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  battery_end_electricity <- reactive({
    rep(input$battery_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  long_duration_storage_end_electricity <- reactive({
    rep(input$long_duration_storage_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  biomass_end_electricity <- reactive({
    rep(input$biomass_electricity / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_electricity <- reactive({
    data.frame(
      Installation = c(solar_end_electricity(), wind_end_electricity(), battery_end_electricity(), long_duration_storage_end_electricity(), biomass_end_electricity()),
      Feedstock = rep(c("Solar", "Wind", "Battery", "Long-Duration Storage", "Biomass"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 5)
    )
  })
  
  output$CommPlot_electricity <- renderPlot({
    ggplot(commodity_frame_electricity(), aes(x = Year, y = Installation, fill = Feedstock)) +
      geom_area(color = "black", lwd = 0.2) +
      labs(y = "Installed Capacity (GW)", fill = " ") +
      ylim(0, 128) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "top") +
      scale_fill_manual(values = c("Biomass" = "#FB8072", "Solar" = "#FFFFB3", "Wind" = "#80B1D3", "Battery" = "#8DD3C7", "Long-Duration Storage" = "#BEBADA"))
    })
  
  # Hydrogen
  solar_end_hydrogen <- reactive({
    rep(input$solar_hydrogen / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  biomass_end_hydrogen <- reactive({
    rep(input$biomass_hydrogen / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  natural_gas_end_hydrogen <- reactive({
    rep(input$natural_gas_hydrogen / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_hydrogen <- reactive({
    data.frame(
      Installation = c(solar_end_hydrogen(), biomass_end_hydrogen(), natural_gas_end_hydrogen()),
      Feedstock = rep(c("Solar", "Biomass","Natural Gas"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 3)
    )
  })
  
  output$CommPlot_hydrogen <- renderPlot({
    ggplot(commodity_frame_hydrogen(), aes(x = Year, y = Installation, fill = Feedstock)) +
      geom_area(color = "black", lwd = 0.2) +
      labs(y = "1000 x Metric Tons (MT)", fill = " ") +
      ylim(0, 1500) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "top") +
      scale_fill_manual(values = c("Biomass" = "#8DD3C7", "Solar" = "#FFFFB3", "Natural Gas" = "#BEBADA"))
  })
  
  # Biomethane
  biomass_end_biomethane <- reactive({
    rep(input$biomass_biomethane / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_biomethane <- reactive({
    data.frame(
      Installation = c(biomass_end_biomethane()),
      Feedstock = rep(c("Biomass"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 1) 
    )
  })
  
  output$CommPlot_biomethane <- renderPlot({
    ggplot(commodity_frame_biomethane(), aes(x = Year, y = Installation, fill = Feedstock)) +
      geom_area(color = "black", lwd = 0.2) +
      labs(y = "Billion Cubic Feet (MCF)", fill = " ")  +
      ylim(0, 30) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "top") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Sustainable Aviation Fuel
  fats_oils_greases_end_saf <- reactive({
    rep(input$fats_oils_greases_saf / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  agriculture_forest_residue_end_saf <- reactive({
    rep(input$agriculture_forest_residue_saf / length(seq(2025, 2045)), length(seq(2025, 2045))) %>% cumsum()
  })
  
  commodity_frame_saf <- reactive({
    data.frame(
      Installation = c(fats_oils_greases_end_saf(), agriculture_forest_residue_end_saf()),
      Feedstock = rep(c("Fats, Oils, and Greases", "Agriculture and Forest Residue"), each = length(seq(2025, 2045))),
      Year = rep(seq(2025, 2045), 2)
    )
  })
  
  output$CommPlot_saf <- renderPlot({
    ggplot(commodity_frame_saf(), aes(x = Year, y = Installation, fill = Feedstock)) +
      geom_area(color = "black", lwd = 0.2) +
      labs(y = "Million Gallons", fill = " ")  +
      scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, by = 20)) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "top") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Download all data
  output$downloadData <- downloadHandler(
    filename = "User Portfolio.csv",
    content = function(file) {
      bind_rows(commodity_frame_electricity() %>% mutate(Commodity = "Electricity"), 
                commodity_frame_hydrogen() %>% mutate(Commodity = "Hydrogen"), 
                commodity_frame_biomethane() %>% mutate(Commodity = "Biomethane") , 
                commodity_frame_saf() %>% mutate(Commodity = "Sustainable Aviation Fuel")) %>% 
        pivot_wider(names_from = Year,
                    values_from = Installation) %>% 
        write_csv(file) 
      })
                
  
  # Reset Values 
  
  observeEvent(input$resetValues_electricity, {
    updateSliderInput(session, "solar_electricity", value = 26.6)
    updateSliderInput(session, "wind_electricity", value = 0.55)
    updateSliderInput(session, "battery_electricity", value = 14.8)
    updateSliderInput(session, "long_duration_storage_electricity", value = 1)
    updateSliderInput(session, "biomass_electricity", value = 0.04)
  })
  
  observeEvent(input$resetValues_hydrogen, {
    updateSliderInput(session, "solar_hydrogen", value = 310)
    updateSliderInput(session, "biomass_hydrogen", value = 140)
    updateSliderInput(session, "natural_gas_hydrogen", value = 11)
  })
  

observeEvent(input$resetValues_biomethane, {
  updateSliderInput(session, "biomass_biomethane", value = 6)
})

observeEvent(input$resetValues_saf, {
  updateSliderInput(session, "fats_oils_greases_saf", value = 5)
  updateSliderInput(session, "agriculture_forest_residue_saf", value = 50)
})

  # Reactive text for Biomethane
  output$portfolioSummary_biomethane <- renderUI({
    total_biomethane <- sum(input$biomass_biomethane)
    
    biomethane_summary <- paste0("\u2022", " ",
                                 total_biomethane, " Million Cubic Feet of Biomethane (",
                                 scales::percent(input$biomass_biomethane / total_biomethane), " from biomass)"
    )
    
    return(HTML(biomethane_summary))
  })
  
  # Reactive text for Sustainable Aviation Fuel
  output$portfolioSummary_saf <- renderUI({
    total_saf <- sum(input$fats_oils_greases_saf, input$agriculture_forest_residue_saf)
    
    saf_summary <- paste0("\u2022", " ",
                          total_saf, " Million Gallons of Sustainable Aviation Fuel (",
                          scales::percent(input$fats_oils_greases_saf / total_saf), " from Fats, Oils, and Greases, ",
                          scales::percent(input$agriculture_forest_residue_saf / total_saf), " from Agriculture and Forest Residue)"
    )
    
    return(HTML(saf_summary))
  })
  
  # Reactive text for Electricity Composition
  output$electricityComposition <- renderUI({
    total_electricity <- sum(input$solar_electricity, input$wind_electricity, input$battery_electricity, input$long_duration_storage_electricity, input$biomass_electricity)
    
    electricity_summary <- paste0("\u2022", " ",
                                  total_electricity, " GW of Electricity (",
                                  scales::percent(input$solar_electricity / total_electricity), " from Solar, ",
                                  scales::percent(input$wind_electricity / total_electricity), " from Wind, ",
                                  scales::percent(input$battery_electricity / total_electricity), " from Battery, ",
                                  scales::percent(input$long_duration_storage_electricity / total_electricity), " from Long-Duration Storage, ",
                                  scales::percent(input$biomass_electricity / total_electricity), " from Biomass)"
    )
    
    return(HTML(electricity_summary))
  })
  
  # Reactive text for Hydrogen Composition
  output$hydrogenComposition <- renderUI({
    total_hydrogen <- sum(input$solar_hydrogen, input$biomass_hydrogen, input$natural_gas_hydrogen)
    
    hydrogen_summary <- paste0("\u2022", " ",
                               total_hydrogen, " thousand metric tons of Hydrogen (",
                               scales::percent(input$solar_hydrogen / total_hydrogen), " from Solar, ",
                               scales::percent(input$biomass_hydrogen / total_hydrogen), " from Biomass, ",
                               scales::percent(input$natural_gas_hydrogen / total_hydrogen), " from Natural Gas + CCS)"
    )
    
    return(HTML(hydrogen_summary))
  })
  
  # Overall Composition 
  overall_composition_summary <- reactive({
    total_electricity <- sum(input$solar_electricity, input$wind_electricity, input$battery_electricity, input$long_duration_storage_electricity, input$biomass_electricity)
    total_hydrogen <- sum(input$solar_hydrogen, input$biomass_hydrogen, input$natural_gas_hydrogen)
    total_biomethane <- sum(input$biomass_biomethane)
    total_saf <- sum(input$fats_oils_greases_saf, input$agriculture_forest_residue_saf)
    
    overall_summary <- paste(
      "\u2022", " Building ", total_electricity, " GW of electric resources by 2045 (",
      scales::percent(input$solar_electricity / total_electricity), " from Solar, ",
      scales::percent(input$wind_electricity / total_electricity), " from Wind, ",
      scales::percent(input$battery_electricity / total_electricity), " from Battery, ",
      scales::percent(input$long_duration_storage_electricity / total_electricity), " from Long-Duration Storage, ",
      scales::percent(input$biomass_electricity / total_electricity), " from Biomass)", sep = ""
    )
    
    paragraph_break <- "<br><br>"
    
    overall_summary <- paste(
      overall_summary,
      paragraph_break,
      "\u2022", " Producing ", total_hydrogen, " thousand metric tons of Hydrogen annually in 2045 (",
      scales::percent(input$solar_hydrogen / total_hydrogen), " from Solar, ",
      scales::percent(input$biomass_hydrogen / total_hydrogen), " from Biomass, ",
      scales::percent(input$natural_gas_hydrogen / total_hydrogen), " from Natural Gas + CCS)", sep = ""
    )
    
    overall_summary <- paste(
      overall_summary,
      paragraph_break,
      "\u2022", " Producing ", total_biomethane, " Billion Cubic Feet of Biomethane annually in 2045 (",
      scales::percent(input$biomass_biomethane / total_biomethane), " from Biomass)", sep = ""
    )
    
    overall_summary <- paste(
      overall_summary,
      paragraph_break,
      "\u2022", " Producing ", total_saf, " Million Gallons of Sustainable Aviation Fuel annually in 2045 (",
      scales::percent(input$fats_oils_greases_saf / total_saf), " from Fats, Oils, and Greases, ",
      scales::percent(input$agriculture_forest_residue_saf / total_saf), " from Agriculture and Forest Residue)", sep = ""
    )
    
    return(HTML(overall_summary))
  })
  
  output$portfolioSummary_electricity <- renderUI({
    overall_composition_summary()
  })
  
  output$portfolioSummary_hydrogen <- renderUI({
    overall_composition_summary()
  })
  
  output$portfolioSummary_biomethane <- renderUI({
    overall_composition_summary()
  })
  
  output$portfolioSummary_saf <- renderUI({
    overall_composition_summary()
  })
  
  
  ### Modal Dialogues
  
  # Solar
  observeEvent(input$info_solar_electricity, {
    showModal(modalDialog(
      title = "Solar Electricity Information",
      "According to the Power of Place West, the SJV could support up to 117 GW of new solar production used for grid electricity, hydrogen production, or other uses. The 2023-2024 Transmission Planning Process in support of the IRP anticipates 13.3 GW of new utility solar in the SJV by 2035, which projects to 26.6 GW by 2045. According to CARB's Scoping Plan, approximately 72 GW of new utility solar is needed in total by 2045 (not only from the SJV) to meet the state's electricity demands modeled in SB100."
    ))
  })
  
  observeEvent(input$info_wind_electricity, {
    showModal(modalDialog(
      title = "Wind Electricity Information",
      "According to the Power of Place West Study, the SJV land could support up to 5 GW of new wind. The 2023-2024 Transmission Planning Process in support of the IRP anticipates 270 MW (0.27 GW) of new wind in the SJV by 2035, which projects to 550 MW (0.55 GW) by 2045. According to CARB's Scoping Plan, approximately 3.5 GW of new on-shore wind is needed in total by 2045 (not only from the SJV) to meet the state's electricity demands modeled in SB100."
    ))
  })
  
  observeEvent(input$info_battery_electricity, {
    showModal(modalDialog(
      title = "Battery Electricity Information",
      "The 2023-2024 Transmission Planning Process in support of the IRP anticipates 7.4 GW of new battery storage in the SJV by 2035, which projects to 14.8 GW by 2045. According to CARB's Scoping Plan, approximately 37 GW of battery storage are needed in total (not only from the SJV) to meet statewide electricity demands modeled in SB100."
    ))
  })
  
  observeEvent(input$info_long_duration_storage_electricity, {
    showModal(modalDialog(
      title = "Long Duration Storage Electricity Information",
      "The California IRP anticipates 1 GW of new Long-Duration Energy Storage (i.e., pumped hydropower) in the SJV by 2035. According to CARB's Scoping Plan, approximately 3 GW of LDES are needed in total by 2045 (not only from the SJV) to meet statewide electricity demands modeled in SB100."
    ))
  })
  
  observeEvent(input$info_biomass_electricity, {
    showModal(modalDialog(
      title = "Biomass Electricity Information",
      "Biomass includes animal manure and agricultural, forest, and diverted organic waste. The California IRP anticipates approximately 20 MW (0.02 GW) of electricity generated from biomass, which projects to 40 MW (0.040 GW by 2045). According to CARB's Scoping Plan, approximately 134 MW (0.134 GW) of electricity from new biomass is needed in total by 2045 (not only from the SJV) to meet statewide electricity demands modeled in SB100."
    ))
  })
  
  # Hydrogen 
  observeEvent(input$info_solar_hydrogen, {
    showModal(modalDialog(
      title = "Solar Hydrogen Information",
      "The Scoping Plan estimates demand for approximately 1.2 million metric tons of H2 in 2045 from solar. According to NREL (2020), the SJV has approximately 40% of California's solar H2 production potential, though this resource must be shared between both electricity and hydrogen. If the region were to produce 25% of the Scoping Plan's demands, this would amount to 310,000 metric tons of H2."
    ))
  })
  
  observeEvent(input$info_biomass_hydrogen, {
    showModal(modalDialog(
      title = "Biomass Hydrogen Information",
      "The Scoping Plan estimates demand for approximately 0.7 million metric tons of H2 in 2045 from biomass. According to NREL (2020), the SJV has approximately 20% of California's solid and gaseous biomass resources. If the region were to produce 20% of the Scoping Plan's demands for H2 from biomass, this would amount to 140,000 metric tons of H2 from biomass. The total potential based on total biomass resource is approximately 700,000 tons of H2."
    ))
  })
  
  observeEvent(input$info_natural_gas_hydrogen, {
    showModal(modalDialog(
      title = "Natural Gas Hydrogen Information",
      "The Lone Cypress Blue Hydrogen facility in Kern County is scheduled to come online in 2026 and produce 11,000 tons of H2 per year (30 tons/day). However, such blue hydrogen is not part of the Scoping Plan as is not expected to receive new investments from the State."
    ))
  })
  
# Biomethane
  
  observeEvent(input$info_biomass_biomethane, {
    showModal(modalDialog(
      title = "Biomass Biomethane Information",
      "The Scoping Plan anticipates an additinoal statewide demand of 21 billion cubic feet of biomethane by 2045 compared to today (based on a demand of 0.04 EJ of biogas). According to NREL (2020), the SJV has approximately 28% of California's gaseous waste from manure and diverted organic waste, some of which may go to producing hydrogen or electricity. If the region were to produce 28% of the Scoping Plan's demands for biomethane (separate from biomethane's use in hydrogen or electricity production), this would amount to 6 billion cubic feet of biomethane."
    ))
  })

# SAF
  
  observeEvent(input$info_fats_oils_greases_saf, {
    showModal(modalDialog(
      title = "Fats, Oils, and Greases SAF Information",
      "The Scoping Plan anticipates an additional 400 million gallons of sustainable aviation fuel (SAF) by 2045. Fats, oils, and greases can be refined alongside fossil fuel feedstocks to produce SAF. The valley has approximately 2.5% of the state's refining capacity. If half of that refining capacity were used as the basis of the valley's production of SAF from fats, oils, and greases, it would amount to approximately 5 million gallons of jet fuel."
    ))
  })
  
  observeEvent(input$info_agriculture_forest_residue_saf, {
    showModal(modalDialog(
      title = "Agriculture and Forest Residue SAF Information",
      "SAF can also be produced from forest and agricultural biomass. According to LLNL, the SJV has 24% of the state's forest and agricultural biomass resources. If the region were to produce 24% of the additional 400 million gallons of SAF demanded in the scoping plan, it would be approximately 100 million gallons. Given other uses of this resource, the template portfolio describes a future in which the valley produces half of that."
    ))
  })
    
}

shinyApp(ui, server)
