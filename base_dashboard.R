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

# Base Buildouts ----------------------------------------------------------

commodity_use <- read_csv("data/commodity_to_use.csv")
feedstock_commodity <- read_csv("data/feedstock_to_commodity.csv")

# Reference Commodities/Feedstocks ----------------------------------------

water_ref_commodities <- c("Hydrogen")
water_ref_feedstock <- c("Natural Gas", "Solar")

land_ref_commodities <- c("Electricity", "Hydrogen")
land_ref_feedstock <- c("Solar", "Wind")

jobs_ref_commodities <- c("Electricity", "Hydrogen")
jobs_ref_feedstock <- c("Solar", "Wind")

intensity_ref_commodities <- c("Hydrogen")
intensity_ref_feedstock <- c("Natural Gas", "Solar")

intensity_elec_commodities <- c("Electricity")
intensity_elec_feedstock <- c("Solar", "Wind")

# Coefficients -----------------------------------------------------------

architecture_sheets <- readxl::excel_sheets("SJV Variable Architecture.xlsx")
f2c_sheets <- grepl("F2C", architecture_sheets)
c2u_sheets <- grepl("C2U", architecture_sheets)

coef_sheets <- architecture_sheets[f2c_sheets]
use_sheets <- architecture_sheets[c2u_sheets]

f2c_data <- lapply(coef_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(f2c_data) <- coef_sheets

c2u_data <- lapply(use_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(c2u_data) <- use_sheets

water_use_coef <- f2c_data$`F2C Water` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% water_ref_commodities)

land_use_coef <- f2c_data$`F2C Land` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% land_ref_commodities,
         Feedstock %in% land_ref_feedstock)

jobs_use_coef <- f2c_data$`F2C Jobs` %>% 
  select(Feedstock:Value) %>% 
  filter(Commodity %in% jobs_ref_commodities,
         Feedstock %in% jobs_ref_feedstock)

intensity_use_coef <- f2c_data$`F2C CI` %>% 
  select(Feedstock:`Energy Value`) %>% 
  filter(Commodity %in% intensity_ref_commodities,
         Feedstock %in% intensity_ref_feedstock)

# Unit Conversion ---------------------------------------------------------

conversion_sheets <- grepl("Conversion", architecture_sheets)
conversion_sheets <- architecture_sheets[conversion_sheets]

conversion_data <- lapply(conversion_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x))
names(conversion_data) <- conversion_sheets

# Hydrogen Use ------------------------------------------------------------

emissions_hydrogen <- feedstock_commodity %>% 
  filter(Commodity %in% intensity_ref_commodities,
         Feedstock %in% intensity_ref_feedstock) %>% 
  pivot_year("Installation") %>% 
  left_join(conversion_data$`Unit Conversion` %>% filter(Commodity %in% intensity_ref_commodities), 
            by = c("Feedstock", "Commodity", "Commodity Unit", "Unit Code"),
            relationship = "many-to-many") %>% 
  mutate(`Energy in MJ` = Installation * `Conversion Factor MJ`) %>% 
  select(-`Conversion Factor EJ`:-Unit) %>% # First Variable Drop
  left_join(intensity_use_coef %>% filter(`Uncertainty Range Category` == "Nominal") %>% distinct, # Should I filter more? 
            by = c("Feedstock", "Commodity"),
            relationship = "many-to-many") %>% 
  mutate(`GHG Emissions generated during F2C` = `Energy in MJ` * `Energy Value`)

emissions_hydrogen_agg <- emissions_hydrogen %>% 
  group_by(Commodity, year) %>% 
  summarize(`Energy in MJ` = sum(`Energy in MJ`), 
            `GHG Emissions generated during F2C` = sum(`GHG Emissions generated during F2C`)) %>% 
  ungroup()

# Uses
# Changing order of Joins (Works Better)

use_coefs <- commodity_use %>% 
  rename(Use = `End Use`) %>% 
  filter(Commodity %in% intensity_ref_commodities) %>% 
  pivot_year("Proportion") %>% 
  left_join(c2u_data$`C2U UO Adjustment` %>% # Renamed to Make joinable with commodity_use (should I replace Grid with Electric Generation?)
              select(Commodity:`Adjustment Factor`) %>% 
              filter(Commodity %in% intensity_ref_commodities), 
            by = c("Commodity", "Use"),
            relationship = "many-to-many") %>% 
  left_join(c2u_data$`C2U CI` %>% # Renamed to Make joinable with commodity_use
              filter(Commodity %in% intensity_ref_commodities) %>%
              filter(Use == "Surface Transportation Fuel") %>% 
              pivot_year("Carbon Intensity Coef"),
            by = c("Commodity", "Use", "year"),
            relationship = "many-to-many") %>% # Compared to and fuel displaced?
  filter(`Uncertainty Range Category` == "Nominal") # Huge Filter 


emissions_hydrogen_agg <- emissions_hydrogen_agg %>% 
  left_join(use_coefs,
            by = c("Commodity", "year")) %>% # If not aggregate, proportionality assumption
  mutate(`Energy in MJ C2U` = `Energy in MJ` * Proportion,
         `Adjusted Energy in MJ C2U` = `Energy in MJ` * `Adjustment Factor`,
         `GHG Avoided During Use` = `Adjusted Energy in MJ C2U` * `Carbon Intensity Coef`,
         `Net Avoidad GHG` = (`GHG Avoided During Use` - `GHG Emissions generated during F2C` * Proportion) / 1e12,
         `Net Avoidad GHG Unit` = "Million MT CO2e")

emissions_hydrogen_agg %>% write_csv("output/hydrogen_use.csv")

# Electricity Use ---------------------------------------------------------

emissions_electricity <- feedstock_commodity %>% 
  filter(Commodity %in% intensity_elec_commodities,
         Feedstock %in% intensity_elec_feedstock) %>% 
  pivot_year("Installation") %>% 
  left_join(conversion_data$Conversion %>% 
              filter(Commodity %in% intensity_elec_commodities,
                     Feedstock %in% intensity_elec_feedstock) %>% 
              pivot_year("Effective Energy Factor Mwh/MW") %>% 
              select(-`Time Dependent Variable Names`),
          by = c("Feedstock", "Commodity", "year")) %>% 
  mutate(`Energy in MWh` = Installation * `Effective Energy Factor Mwh/MW`) %>% 
  left_join(conversion_data$`Unit Conversion` %>% filter(Commodity %in% intensity_elec_commodities), 
            by = c("Feedstock", "Commodity"),
            relationship = "many-to-many") %>% 
  mutate(`Energy in MJ` = `Energy in MWh` * `Conversion Factor MJ`)

emissions_electricity_agg <- emissions_electricity %>% 
  group_by(Commodity, year) %>% 
  summarize(`Energy in MJ` = sum(`Energy in MJ`)) %>% 
  ungroup()
  
  
# Uses
  
use_coefs_elec <- commodity_use %>% 
  filter(Commodity %in% intensity_elec_commodities) %>% # Grid for Electric Generation
  pivot_year("Proportion") %>% 
    left_join(c2u_data$`C2U UO Adjustment` %>% # Renamed to Make joinable with commodity_use (should I replace Grid with Electric Generation?)
              select(Commodity:`Adjustment Factor`) %>% 
              filter(Commodity %in% intensity_elec_commodities), 
            by = c("Commodity", "Use"),
            relationship = "many-to-many") %>% 
  left_join(c2u_data$`C2U CI` %>% # Renamed to Make joinable with commodity_use
              filter(Commodity %in% intensity_elec_commodities) %>%
              filter(Use %in% c("Surface Transportation Fuel", "Electric Generation")) %>% 
              pivot_year("Carbon Intensity Coef"),
            by = c("Commodity", "Use", "year"),
            relationship = "many-to-many") %>% # Compared to and fuel displaced?
  filter(`Uncertainty Range Category` == "Nominal") # Huge Filter 


emissions_electricity_agg <- emissions_electricity_agg %>% 
  left_join(use_coefs_elec,
            by = c("Commodity", "year")) %>% # If not aggregate, proportionality assumption
  mutate(`Energy in MJ C2U` = `Energy in MJ` * Proportion,
         `Adjusted Energy in MJ C2U` = `Energy in MJ` * `Adjustment Factor`,
         `GHG Avoided During Use` = `Adjusted Energy in MJ C2U` * `Carbon Intensity Coef`) # Here Instructions Stop

emissions_hydrogen_agg %>% write_csv("output/hydrogen_use.csv")

# Water Commodity ---------------------------------------------------------------
# Just Hydrogen for now
# Metrics: consumption + withdrawal
# Unit: Acre-ft/year per feedstock & commodity

water_use <- feedstock_use(feedstock_commodity, water_ref_commodities, 
                           water_ref_feedstock, water_use_coef) %>% 
  arrange(`Variable Subcategory`, Feedstock, Commodity, year) %>% 
  mutate(`Conversion Value` = Installation * Value) 

# Plot

feedstock_plot(water_use) +
  facet_wrap( ~ Feedstock + `Variable Subcategory`, scales = "free")

ggsave("plots/water.png")

# Test Output 

water_use %>% write_csv("output/water_commodity_base.csv")

# Land Commodity ----------------------------------------------------------------
# Metrics: Land Impacted + Land Consumed
# Unit: Acres/year per feedstock & commodity

land_use <- feedstock_use(feedstock_commodity, land_ref_commodities, 
                          land_ref_feedstock, land_use_coef) %>% 
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

# Job Commodity ---------------------------------------------------------
# Metrics:  Six metrics (4 by education and skill level, 1 for total, 2 for upper bound)
# Unit: Jobs by year per feedstock & commodity 

jobs_use <- feedstock_use(feedstock_commodity, jobs_ref_commodities, 
                          jobs_ref_feedstock, jobs_use_coef) %>% 
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



