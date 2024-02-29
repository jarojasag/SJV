#' Pivot the Data to Long Format by Year
#' 
#' This function takes a dataframe in wide format with columns representing years and
#' pivots it to long format with a 'year' column.
#' 
#' @param data The dataframe in wide format.
#' @param val_names Name of the new column.
#' @return The dataframe in long format.
#' 
#' @examples
#' \dontrun{
#' pivoted_data <- pivot_year(data_frame, "Value_Column_Name")
#' }
#'
pivot_year <- function(data, val_names){
  # data: df wide format for years
  # val_names: Name of new column
  
  data <- data %>% 
    pivot_longer(`2025`:`2045`, 
                 names_to = "year", 
                 values_to = val_names) %>%
    mutate(year = as.integer(year))
  
  return(data)
}

#' Calculate Feedstock Use Based on User Input
#' 
#' This function filters the buildout data based on user input for commodities and feedstocks,
#' pivots the buildout data to long format by year, and performs a left join with feedstock-to-commodity data.
#' 
#' @param buildout User input for buildout data.
#' @param commodities User input for commodities.
#' @param feedstock User input for feedstocks.
#' @param f2c_data Coefficients from Excel Sheet for specific commodity.
#' @return A tibble representing feedstock use.
#' 
#' @examples
#' \dontrun{
#' feedstock_data <- feedstock_use(buildout_data, c("Commodity1", "Commodity2"), c("Feedstock1", "Feedstock2"), f2c_data)
#' }
#'
feedstock_use <- function(buildout, commodities, feedstock, f2c_data){
  # buildout: User Input
  # commodities: User Input
  # feedstock: User Input
  # f2c_data: Coefficients from Excel Sheet for specific commodity
  
  feedstock_use <- buildout %>% 
    filter(Commodity %in% commodities,
           Feedstock %in% feedstock) %>% 
    pivot_year("Buildout") %>% 
    left_join(f2c_data, 
              by = c("Feedstock", "Commodity"),
              relationship = "many-to-many")
  
  return(feedstock_use)
}

#' Create a Line Plot for Feedstock Use
#' 
#' This function takes an instance of feedstock use data and creates a line plot
#' showing the conversion values over the years.
#' 
#' @param feedstock_use_instance Output from feedstock_use function.
#' @return A line plot showing feedstock use.
#' 
#' @examples
#' \dontrun{
#' plot_instance <- feedstock_plot(feedstock_use_instance)
#' }
feedstock_plot <- function(feedstock_use_instance){
  # feedstock_use_instance: Output from feedstock_use
  
  units <- feedstock_use_instance %>% 
    select(`Variable Unit`) %>% distinct
  
  feedstock_use_instance %>% 
    filter(!is.na(Installation)) %>% 
    ggplot(aes(x = year, y = `Conversion Value`)) +
    geom_line() +
    ylab(str_to_title(units$`Variable Unit`)) +
    theme_bw()  
}

#' Calculate Avoided Emissions
#' 
#' This function calculates avoided emissions by left joining F2C emissions data
#' with C2U emission coefficients and applying necessary calculations.
#' 
#' @param emission_data F2C emissions generated.
#' @param use_intensities C2U emission coefficients.
#' @return A tibble with calculated avoided emissions.
#' 
#' @examples
#' \dontrun{
#' avoided_emissions_data <- avoided_emissions(emission_data, use_intensities)
#' }
avoided_emissions <- function(emission_data, use_intensities){
  # emission_data (tibble): F2C emissions generated
  # use_intensities (tibble): C2U emission coefficients
  
  emission_data %>% 
    left_join(use_intensities, by = c("Commodity", "year"), 
              relationship = "many-to-many") %>% 
    mutate(`Adjusted Energy Used in MJ` = `Energy Produced` * Weight * `Adjustment Factor`,
           `Emissions Avoided in C2U` = `Adjusted Energy Used in MJ` * `Carbon Intensity of Use`,
           `Net Avoided Emissions` = (`Emissions Avoided in C2U` - `Generated Emissions in F2C` * Weight)/1e+12)
}

#' Calculate Total Avoided Emissions for a Portfolio
#' 
#' This function takes input from commodity and feedstock use data, and calculates
#' total avoided emissions considering F2C (Feedstock to Commodity) and C2U (Commodity to Use) emissions.
#' 
#' @param feedstock_commodity A tibble containing feedstock to commodity conversion data.
#' @param commodity_use A tibble containing commodity use data.
#' @param portfolio_name The name of the portfolio.
#' @param ref_commodities A vector of reference commodities for F2C emissions.
#' @param conversion_units A tibble containing conversion units for energy values.
#' @param carbon_intensity_f2c A tibble containing carbon intensity for F2C emissions.
#' @param effective_energy A tibble containing effective energy factors for electricity.
#' @return A tibble with the calculated total avoided emissions.
#' 
#' @examples
#' \dontrun{
#' total_avoided_emissions <- calculate_total_avoided_emissions(
#'   commodity_use, ref_commodities, conversion_units, 
#'   carbon_intensity_f2c, conversion_units, effective_energy,
#'   feedstock_commodity, "PortfolioName", c("Year", "Avoided Emissions", "Other Columns"))
#' }
get_avoided_emissions <- function(
    feedstock_commodity, commodity_use, portfolio_name, ref_commodities, 
    conversion_units, carbon_intensity_f2c, effective_energy) {
  
  order_vector <- c("Portfolio", "Feedstock", "Commodity", "Use", "year", "Weight",
                    "Buildout", "Commodity Unit", "Unit Code",  "Energy Produced", 
                    "Energy Unit", "Adjusted Energy Used in MJ", 
                    "Uncertainty Range Category F2C", "Uncertainty Range Category C2U", 
                    "Net Avoided Emissions")
  
  # Avoided Emissions -------------------------------------------------------
  
  #### F2C Emissions 
  
  non_electric <- feedstock_commodity %>% 
    filter(Commodity %in% ref_commodities, Commodity != "Electricity") %>% 
    pivot_year("Buildout") %>% 
    left_join(conversion_units, by = "Commodity") %>% 
    left_join(carbon_intensity_f2c, by = c("Feedstock", "Commodity"), relationship = "many-to-many") %>% 
    mutate(`Energy Produced` = Buildout * `Conversion Factor MJ`,
           `Energy Unit` = "MJ",
           `Generated Emissions in F2C` = `Energy Produced` * `Carbon Intensity`) 
  
  electric <- feedstock_commodity %>% 
    filter(Commodity == "Electricity") %>% 
    pivot_year("Buildout") %>% 
    left_join(conversion_units, by = "Commodity") %>% 
    left_join(effective_energy, by = c("Feedstock", "Commodity", "year")) %>% 
    mutate(`Energy Produced` = Buildout * `Effective Energy Factor` * `Conversion Factor MJ`,
           `Energy Unit` = "MJ",
           `Generated Emissions in F2C` = 0,
           `Uncertainty Range Category F2C` = NA)
  
  #### C2U Emissions
  
  use_intensities <- commodity_use %>% 
    pivot_year("Weight") %>% 
    left_join(adjustment_factors, by = c("Commodity", "Use")) %>% 
    left_join(carbon_intensity_c2u, by = c("Commodity", "Use", "year")) 
  
  avoided_non_electric <- non_electric %>% 
    avoided_emissions(use_intensities) 
  
  avoided_electric <- electric %>% 
    avoided_emissions(use_intensities)
  
  ## Formatting Output
  
  avoided_non_electric <- avoided_non_electric %>% 
    mutate(Portfolio = portfolio_name) %>% 
    select(all_of(order_vector)) %>% 
    filter(`Uncertainty Range Category F2C` == `Uncertainty Range Category C2U`)
  # Making Uncertainty Range Column
  
  avoided_electric <- avoided_electric %>% 
    mutate(Portfolio = portfolio_name) %>%  
    select(all_of(order_vector)) %>% 
    mutate(`Uncertainty Range Category F2C` = `Uncertainty Range Category C2U`)
  # Making Uncertainty Range Column
  
  total_avoided_emissions <- avoided_non_electric %>% 
    bind_rows(avoided_electric)
  
  total_avoided_emissions <- total_avoided_emissions %>% 
    select(-`Uncertainty Range Category F2C`) %>% 
    pivot_wider(names_from = `Uncertainty Range Category C2U`,
                values_from = `Net Avoided Emissions`) %>% 
    rename(Year = year, 
           `Net Avoided Emissions (MTCO2e) (min)` = Low,
           `Net Avoided Emissions (MTCO2e) (max)` = High, 
           `Net Avoided Emissions (MTCO2e) (nominal)` = Nominal) %>% 
    mutate(Buildout = Buildout * Weight, 
           `Energy Produced` = `Energy Produced` * Weight) 
  
  total_avoided_emissions <- total_avoided_emissions %>% 
    select(all_of(c("Portfolio", "Feedstock", "Commodity", "Use", "Year", 
                    "Buildout", "Commodity Unit", "Unit Code", "Energy Produced",
                    "Energy Unit", "Adjusted Energy Used in MJ", 
                    "Net Avoided Emissions (MTCO2e) (min)", 
                    "Net Avoided Emissions (MTCO2e) (max)",
                    "Net Avoided Emissions (MTCO2e) (nominal)",
                    "Weight")))
  
  return(total_avoided_emissions)
}

#' Find Uses of Feedstock and Calculate Environmental Impact
#' 
#' This function calculates the uses of specified feedstocks, including water and land use,
#' based on the provided feedstock-to-commodity data and relevant coefficients. It then 
#' joins the calculated uses with F2C conversion data to estimate the environmental impact.
#' 
#' @param feedstock_commodity A tibble containing feedstock-to-commodity conversion data.
#' @param portfolio_name The name of the portfolio.
#' @param ref_commodities A vector of reference commodities for feedstock use.
#' @param water_use_coef Water use coefficients for specific feedstocks.
#' @param land_use_coef Land use coefficients for specific feedstocks.
#' @param jobs_use_coef Jobs use coefficients for specific feedstocks.
#' @param f2c_conversion F2C conversion data.
#' 
#' @return A tibble with calculated environmental impact, including water and land use.
#' 
#' @examples
#' \dontrun{
#' uses_data <- find_uses(feedstock_commodity_data, "PortfolioName", 
#'                        c("Electricity", "Hydrogen", "Biomethane", "Sustainable Aviation Fuel"), 
#'                        water_use_coefficients, land_use_coefficients, jobs_use_coef,
#'                        f2c_conversion_data)
#' }
#'
find_uses <- function(feedstock_commodity, portfolio_name, ref_commodities, 
                      water_use_coef, land_use_coef, jobs_use_coef, f2c_conversion){
  
  # Selecting columns for final output
  selector <- c("Feedstock", "Commodity", "year", "Buildout",
                "Commodity Unit", "Uncertainty Range Category",
                "Variable Subcategory", "Conversion Value")
  
  # Water use calculation
  water_use <- feedstock_use(feedstock_commodity, ref_commodities, 
                             c("Solar", "Natural Gas + CCS", 
                               "Agricultural waste", "Forest waste",
                               "Animal Manure", "Diverted Organic Waste"), 
                             water_use_coef) %>% 
    arrange(`Variable Subcategory`, Feedstock, Commodity, year) %>% 
    mutate(`Conversion Value` = Buildout * Value,
           `Uncertainty Range Category` = "Nominal") %>%  # Should I change this?
    select(all_of(selector)) 
  
  # Land use calculation
  land_use <- feedstock_use(feedstock_commodity, ref_commodities, 
                            c("Solar", "Wind"), land_use_coef) %>% 
    arrange(`Uncertainty Range Category`, `Variable Subcategory`, Feedstock, Commodity, year) %>% 
    group_by(`Uncertainty Range Category`, `Variable Subcategory`, Feedstock, Commodity) %>% 
    mutate(diff = Buildout - dplyr::lag(Buildout),
           `Conversion Value` = case_when(diff > 0 ~ Buildout * Value,
                                          is.na(diff) ~ Buildout * Value,  
                                          .default = lag(Buildout) * lag(Value))) %>% 
    select(-diff) %>% 
    ungroup() %>% 
    select(all_of(selector))
  
  # Jobs Use Calculation
  
  jobs_use <- feedstock_use(feedstock_commodity,  ref_commodities , 
                            c("Solar", "Wind", "Li Battery", "LDES",  "Forest waste", "Agricultural waste",
                              "Animal Manure", "Diverted Organic Waste", "Natural Gas + CCS", "Animal Fat"), 
                            jobs_use_coef) %>% 
    arrange(Feedstock, Commodity, `Variable Subcategory`, year) %>% 
    group_by(Feedstock, Commodity, `Variable Subcategory`) %>% 
    mutate(diff = Buildout - dplyr::lag(Buildout),
           `Conversion Value` = case_when(diff > 0 ~ diff * Value *  (1 - `Productivity`) ** (year - 2025),
                                          year == 2025 ~ Buildout *  Value,  
                                          is.na(Buildout * Value) ~ NA,
                                          .default = 0)) %>% 
    select(-diff) %>% 
    ungroup() %>% 
    mutate(`Uncertainty Range Category` = "Nominal") %>% 
    select(all_of(selector)) 
  
  # Pivot data for wide representation
  total_uses <- bind_rows(water_use, land_use, jobs_use) %>% 
    left_join(f2c_conversion) %>% 
    mutate(`Feedstock Quantity` = Buildout / `F2C Conversion Factor`,
           `Feedstock Energy MJ` = `Feedstock Quantity` * `Feedstock Energy Conversion (based on LHV)`) %>% 
    filter(!is.na(`Variable Subcategory`)) # Remove What Cannot be Identified
    
  # Pivot data for wider representation
  total_uses <- total_uses %>% 
    pivot_wider(values_from = `Conversion Value`,
                names_from = c(`Variable Subcategory`, `Uncertainty Range Category`)) %>% 
    mutate(Portfolio = portfolio_name) %>% 
    select(all_of(c("Portfolio", "Feedstock", "Commodity", "year", "Buildout",  
                    "Commodity Unit", "Feedstock Quantity", "Feedstock Unit",
                    "Water Withdrawal_Nominal", "Water Consumption_Nominal",
                    "Land Consumed_Low", "Land Impacted_Low",
                    "Land Consumed_High", "Land Impacted_High",
                    "Land Consumed_Nominal", "Land Impacted_Nominal",
                    "Feedstock Energy MJ", "Direct_Nominal", "Indirect_Nominal", 
                    "Induced_Nominal"))) %>% 
    rename(`Water Withdrawn (acre-feet) (nominal)` = `Water Withdrawal_Nominal`,
           `Water Consumed (acre-feet) (nominal)` = `Water Consumption_Nominal`,
           `Land Consumed (acres) (nominal)` = `Land Consumed_Nominal`, 
           `Land Impacted (acres) (nominal)` = `Land Impacted_Nominal`,
           `Land Consumed (acres) (min)` = `Land Consumed_Low`,
           `Land Impacted (acres) (min)` = `Land Impacted_Low`,
           `Land Consumed (acres) (max)` = `Land Consumed_High`,
           `Land Impacted (acres) (max)` = `Land Impacted_High`,
           `Direct Employment (# New Jobs) (nominal)` = `Direct_Nominal`, 
           `Indirect Employment (# New Jobs) (nominal)` = `Indirect_Nominal`,    
           `Induced Employment (# New Jobs) (nominal)` = `Induced_Nominal`,  
           Year = year) 
  
  return(total_uses)
}
