library(tidyverse)
library(readxl)

#Functions ---------------------------------------------------------------

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

feedstock_use <- function(buildout, commodities, feedstock, f2c_data){
  # buildout: User Input
  # commodities: User Input
  # feedstock: User Input
  # f2c_data: Coefficients from Excel Sheet for specific commodity
  
  feedstock_use <- buildout %>% 
    filter(Commodity %in% commodities,
           Feedstock %in% feedstock) %>% 
    pivot_year("Installation") %>% 
    left_join(f2c_data, 
              by = c("Feedstock", "Commodity"),
              relationship = "many-to-many")
  
  return(feedstock_use)
  
}

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

avoided_emissions <- function(emission_data, use_intensities){
  # emission_data (tibble): F2C emissions generated
  # use_intensities (tibble): C2U emission coefficients
  
  emission_data %>% 
    left_join(use_intensities, by = c("Commodity", "year"), 
            relationship = "many-to-many") %>% 
    mutate(`Adjusted Energy Used in MJ` = `Energy Produced` * Weight * `Adjustment Factor`,
           `Emissions Avoided in C2U` = `Adjusted Energy Used in MJ` * `Carbon Intensity of Use`,
           `Net Avoided Emissions` = (`Emissions Avoided in C2U` - `Generated Emissions in F2C` * Weight)/1e+12,
           `Net Avoided Emissions Unit` = "million MT CO2e")
}

# Base Buildouts ----------------------------------------------------------

commodity_use <- read_csv("data/commodity_to_use.csv")
feedstock_commodity <- read_csv("data/feedstock_to_commodity.csv")

# Feedstock / Commodity / Use Items ---------------------------------------

ref_commodities <- c("Electricity", "Hydrogen", "Biomethane")

hydrogen_feedstock <-  c("Solar", "Natural Gas + CCS")
hydrogen_use <- c("Surface Transport Fuel", "Ammonia Production",
                  "Green Steel", "Industrial Heat")

elec_feedstock <- c("Solar", "Wind", "Biomass", "Li Battery", "LDES")
elec_use <- c("Power Grid")

biomethane_feedstock <- c("Animal Manure", "Diverted Organic Waste")
biomethane_use <- c("Gas Grid", "Electricity", "Hydrogen")

# Coefficients -----------------------------------------------------------

architecture_sheets <- readxl::excel_sheets("SJV Variable Architecture.xlsx")
f2c_sheets <- grepl("F2C", architecture_sheets)
c2u_sheets <- grepl("C2U", architecture_sheets)
conversion_sheets <- grepl("Conversion", architecture_sheets)

coef_sheets <- architecture_sheets[f2c_sheets]
use_sheets <- architecture_sheets[c2u_sheets]
conversion_sheets <- architecture_sheets[conversion_sheets]

f2c_data <- lapply(coef_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(f2c_data) <- coef_sheets

c2u_data <- lapply(use_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(c2u_data) <- use_sheets

conversion_data <- lapply(conversion_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(conversion_data) <- conversion_sheets

water_use_coef <- f2c_data$`F2C Water` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% "Hydrogen")

land_use_coef <- f2c_data$`F2C Land` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% land_ref_commodities,
         Feedstock %in% land_ref_feedstock)

jobs_use_coef <- f2c_data$`F2C Jobs` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% jobs_ref_commodities,
         Feedstock %in% jobs_ref_feedstock)

# Avoided Emissions -------------------------------------------------------

conversion_units <- conversion_data$`Unit Conversion` %>% 
  filter(Commodity %in% ref_commodities) %>% 
  select(Commodity, `Conversion Factor MJ`, `Unit MJ`) %>% 
  distinct() 

effective_energy <- conversion_data$Conversion %>% 
  filter(Commodity == "Electricity") %>% 
  mutate(`Effective Unit` = "Effective MWh per MW") %>% 
  select(Feedstock, Commodity, `Effective Unit`, `2025`:`2045`) %>%
  mutate(`2025` = as.numeric(`2025`)) %>% 
  pivot_year("Effective Energy Factor")

carbon_intensity <- f2c_data$`F2C CI` %>% 
  filter(Commodity %in% ref_commodities) %>% 
  select(Feedstock, Commodity, `Uncertainty Range Category`, 
         `Energy Unit`, `Energy Value`) %>% 
  distinct() %>% 
  rename(`Carbon Intensity Unit` = `Energy Unit`,
         `Carbon Intensity` = `Energy Value`)

adjustment_factors <- c2u_data$`C2U UO Adjustment` %>% 
  select(Commodity:`Adjustment Factor`)

carbon_intensity_use <- c2u_data$`C2U CI` %>% 
  pivot_year("Carbon Intensity") %>% 
  select(Commodity, Use, year, `Carbon Intensity`) %>% 
  rename(`Carbon Intensity of Use` = `Carbon Intensity`)

# F2C Emissions 

non_electric <- feedstock_commodity %>% 
  filter(Commodity %in% ref_commodities, Commodity != "Electricity") %>% 
  pivot_year("Buildout") %>% 
  left_join(conversion_units, by = "Commodity") %>% 
  left_join(carbon_intensity, by = c("Feedstock", "Commodity"), relationship = "many-to-many") %>% 
  mutate(`Energy Produced` = Buildout * `Conversion Factor MJ`,
         `Energy Unit` = "MJ",
         `Generated Emissions in F2C` = `Energy Produced` * `Carbon Intensity`) %>% 
  filter(!is.na(`Energy Produced`))
  
electric <- feedstock_commodity %>% 
  filter(Commodity == "Electricity") %>% 
  pivot_year("Buildout") %>% 
  left_join(conversion_units, by = "Commodity") %>% 
  left_join(effective_energy, by = c("Feedstock", "Commodity", "year")) %>% 
  mutate(`Energy Produced` = Buildout * `Effective Energy Factor` * `Conversion Factor MJ`,
         `Energy Unit` = "MJ",
         `Generated Emissions in F2C` = 0) %>% 
  filter(!is.na(`Energy Produced`))


# Avoided Emissions DataFrames

use_intensities <- commodity_use %>% 
  pivot_year("Weight") %>% 
  left_join(adjustment_factors, by = c("Commodity", "Use")) %>% 
  left_join(carbon_intensity_use, by = c("Commodity", "Use", "year")) 
  
avoided_non_electric <- non_electric %>% 
  avoided_emissions(use_intensities)
  
avoided_electric <- electric %>% 
  avoided_emissions(use_intensities)

avoided_non_electric %>% write_csv("output/non_electric_avoided_emissions.csv")
avoided_electric %>% write_csv("output/electric_avoided_emissions.csv")

# F2C Outputs -------------------------------------------------------------

# Water
# Just Hydrogen for now
# Metrics: consumption + withdrawal
# Unit: Acre-ft/year per feedstock & commodity

water_use <- feedstock_use(feedstock_commodity, "Hydrogen", hydrogen_feedstock, 
                           water_use_coef) %>% 
  arrange(`Variable Subcategory`, Feedstock, Commodity, year) %>% 
  mutate(`Conversion Value` = Installation * Value)

  # Plot
  
  feedstock_plot(water_use) +
    facet_wrap( ~ Feedstock + `Variable Subcategory`, scales = "free")
  
  ggsave("plots/water.png")
  
  # Test Output 

  water_use %>% write_csv("output/water_commodity_base.csv")

# Land
# Metrics: Land Impacted + Land Consumed
# Unit: Acres/year per feedstock & commodity

land_use <- feedstock_use(feedstock_commodity, c("Electricity", "Hydrogen"), 
                          c("Solar", "Wind"), land_use_coef) %>% 
  arrange(`Uncertainty Range Category`, `Variable Subcategory`, Feedstock, Commodity, year) %>% 
  group_by(`Uncertainty Range Category`, `Variable Subcategory`, Feedstock, Commodity) %>% 
  mutate(diff = Installation - dplyr::lag(Installation),
         `Conversion Value` = case_when(diff > 0 ~ Installation * Value,
                                        is.na(diff) ~ Installation * Value,  
                                        .default = lag(Installation) * lag(Value))) %>% 
  select(-diff) %>% 
  ungroup()

  # Plot
  
  land_units <- land_use %>% select(`Variable Unit`) %>% distinct
  land_use %>% 
    ggplot(aes(x = year, y = `Conversion Value`, color = `Uncertainty Range Category`)) +
    facet_wrap( ~ Feedstock + Commodity +`Variable Subcategory`, ncol = 2, scales = "free") +
    geom_line() +
    ylab(str_to_title(land_units$`Variable Unit`)) +
    theme_bw()
  
  ggsave("plots/land.png")
  
  # Test Output 
  
  land_use %>% write_csv("output/land_commodity_base.csv")

# Jobs
# Metrics:  Six metrics (4 by education and skill level, 1 for total, 2 for upper bound)
# Unit: Jobs by year per feedstock & commodity 

jobs_use <- feedstock_use(feedstock_commodity,  c("Electricity", "Hydrogen"), 
                          c("Solar", "Wind"), jobs_use_coef) %>% 
  arrange(Feedstock, Commodity, `Variable Subcategory`, year) %>% 
  group_by(Feedstock, Commodity, `Variable Subcategory`) %>% 
  mutate(diff = Installation - dplyr::lag(Installation),
         `Conversion Value` = case_when(diff > 0 ~ diff *  Value,
                                        is.na(diff) ~ Installation * Value,  
                                        .default = 0))  %>% 
  select(-diff) %>% 
  ungroup()

  # Plot
  
  feedstock_plot(jobs_use) +
    facet_wrap( ~ Feedstock + Commodity +`Variable Subcategory`, ncol = 4, scales = "free")
  
  ggsave("plots/jobs.png")
  
  # Test Output 
  
  jobs_use %>% write_csv("output/jobs_commodity_base.csv")



