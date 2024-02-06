library(tidyverse)
library(readxl)
library(writexl)
source("SJV Functions.R")

# Feedstock / Commodity / Use Items ---------------------------------------

ref_commodities <- c("Electricity", "Hydrogen", "Biomethane", 
                     "Sustainable Aviation Fuel")

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
  filter(Commodity %in% c("Electricity", "Hydrogen", "Biomethane"))

land_use_coef <- f2c_data$`F2C Land` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% c("Electricity", "Hydrogen"))

jobs_use_coef <- f2c_data$`F2C Jobs` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% c("Electricity", "Hydrogen"))

conversion_units <- conversion_data$`Unit Conversion` %>% 
  filter(Commodity %in% ref_commodities) %>% 
  select(Commodity, `Conversion Factor MJ`, `Unit MJ`) %>% 
  distinct()

effective_energy <- conversion_data$Conversion %>% 
  filter(Commodity == "Electricity") %>% 
  mutate(`Effective Unit` = "Effective MWh per MW") %>% 
  select(Feedstock, Commodity, `Effective Unit`, `2025`:`2045`) %>% 
  pivot_year("Effective Energy Factor")

carbon_intensity_f2c <- f2c_data$`F2C CI` %>% 
  filter(Commodity %in% ref_commodities) %>% 
  select(Feedstock, Commodity, `Uncertainty Range Category`, 
         `Energy Unit`, `Energy Value`) %>% 
  distinct() %>% 
  rename(`Carbon Intensity Unit` = `Energy Unit`,
         `Carbon Intensity` = `Energy Value`,
         `Uncertainty Range Category F2C` = `Uncertainty Range Category`)

adjustment_factors <- c2u_data$`C2U UO Adjustment` %>% 
  select(Commodity:`Adjustment Factor`)

carbon_intensity_c2u <- c2u_data$`C2U CI` %>% 
  pivot_year("Carbon Intensity") %>%  
  select(Commodity, Use, year, `Uncertainty Range Category`, `Carbon Intensity`) %>% 
  rename(`Carbon Intensity of Use` = `Carbon Intensity`,
         `Uncertainty Range Category C2U` = `Uncertainty Range Category`)

f2c_conversion <- f2c_data$`F2C Conversion` %>% 
  select(Feedstock:`Commodity Unit`, `F2C Conversion Factor`, `Feedstock Energy Conversion (based on LHV)`)

# Base Buildouts ----------------------------------------------------------

portfolios <- list.files("data", pattern="*.xlsx", full.names = TRUE)
f2c_porfolios <- lapply(portfolios, 
                        function(x) read_excel(x, sheet = "feedstock_to_commodity"))
c2u_portfolios <- lapply(portfolios, 
                         function(x) read_excel(x, sheet = "commodity_to_use"))

portfolio_names <- portfolios %>% 
  str_extract("SJV Portfolio.*xlsx")

portfolio_names <- gsub("^.+ - |.xlsx$", "", portfolio_names)

names(f2c_porfolios) <- portfolio_names
names(c2u_portfolios) <- portfolio_names

f2c_porfolios <- lapply(f2c_porfolios, function(x) x %>% 
                          select(Feedstock:`2045`)) # Removing unnecessary column

# Avoided Emissions -------------------------------------------------------

total_avoided_emissions <- lapply(seq_along(f2c_porfolios), function(x) 
  get_avoided_emissions(f2c_porfolios[[x]], c2u_portfolios[[x]],
                        portfolio_names[x], ref_commodities, 
                        conversion_units, carbon_intensity_f2c,
                        effective_energy))

total_avoided_emissions <- total_avoided_emissions %>% 
  bind_rows() 

# Water / Land / Jobs -----------------------------------------------------

total_uses <- lapply(seq_along(f2c_porfolios), function(x)
  find_uses(f2c_porfolios[[x]], portfolio_names[x],ref_commodities, 
            water_use_coef, land_use_coef, f2c_conversion))

total_uses <- total_uses %>% 
  bind_rows() 

# Joining Data Frames -----------------------------------------------------

final_output <- total_avoided_emissions %>% 
  left_join(total_uses %>% select(-Buildout)) %>% 
  mutate(`Feedstock Quantity` = `Feedstock Quantity` * Weight,
         across(`Water Withdrawn (acre-feet) (nominal)`:`Land Impacted (acres) (nominal)`,  ~ . * Weight)) %>% 
  select(-Weight)

# Saving Output -----------------------------------------------------------

write_xlsx(list("Avoided Emissions" = final_output),
           path = paste0("output/Full Portfolio Analysis ", today(), ".xlsx"))
