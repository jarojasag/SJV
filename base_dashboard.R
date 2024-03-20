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
  select(Feedstock:Value2) %>% 
  select(-Value) %>% 
  rename(Value = Value2) %>% 
  filter(Commodity %in% ref_commodities) %>% 
  mutate(Productivity = case_when(is.na(Productivity) ~ 0, 
                                  .default = Productivity))

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
         `Energy Value`) %>% 
  distinct() %>% 
  mutate(`Carbon Intensity` = `Energy Value`,
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
            water_use_coef, land_use_coef, jobs_use_coef, f2c_conversion))

total_uses <- total_uses %>% 
  bind_rows() 

# Joining Data Frames -----------------------------------------------------

final_output <- total_avoided_emissions %>% 
  left_join(total_uses %>% select(-Buildout)) %>% 
  mutate(`Feedstock Quantity` = `Feedstock Quantity` * Weight,
         across(`Water Withdrawn (acre-feet) (nominal)`:`Induced Employment (# New Jobs) (nominal)`,  ~ . * Weight)) %>% 
  select(-Weight)


# Buildout differentials --------------------------------------------------

robust_effects <- final_output %>% 
  select(Portfolio, Commodity, Year, Buildout, `Energy Produced`)

robust_effects <- robust_effects %>% group_by(Portfolio, Year, Commodity) %>% 
  summarise(total_energy = sum(`Energy Produced`),
            total_buildout = sum(Buildout)) %>% 
  group_by(Commodity) %>% 
  group_split()

robust_effects <- lapply(robust_effects, function(.x) .x %>% 
                           filter(Year == 2045))

robust_names <- lapply(robust_effects, function(.x) .x %>% 
         select(Commodity) %>% distinct)

robust_names <- do.call(rbind, robust_names)

portfolio_names <- robust_effects[[1]][, 1]
  
robust_diffs <- lapply(robust_effects, function(.x) {
  outer(.x$total_energy, .x$total_energy, FUN = function(x, y) (x - y) / y)
  #outer(.x$total_buildout, .x$total_buildout, FUN = function(x, y) (x - y) / y)
})

robust_diffs <- lapply(seq_along(robust_diffs), function(.x) 
  robust_diffs[[.x]] %>% t() %>% as_tibble %>% 
    mutate(Commodity = robust_names$Commodity[.x]))

robust_diffs <- bind_rows(robust_diffs) %>%
  setNames(c(portfolio_names$Portfolio, "Commodity")) %>% 
  mutate(Portfolio = portfolio_names$Portfolio %>% rep(4)) %>% 
  select(Commodity, Portfolio, everything()) %>% 
  arrange(Portfolio, Commodity) %>%  
  select(Commodity, everything()) %>% 
  pivot_longer(!c(Commodity, Portfolio), 
               names_to = "Comparison Portfolio", 
               values_to = "Relative Difference") %>% 
  select(Portfolio,`Comparison Portfolio`, everything()) %>% 
  arrange(Portfolio,`Comparison Portfolio`) %>% 
  mutate(`Relative Difference` = case_when(Portfolio == `Comparison Portfolio` ~ 0, 
                                           .default = `Relative Difference`)) %>% 
  group_by(Portfolio) %>% 
  group_split() 

robust_diffs <- lapply(robust_diffs, function(x) 
  x %>% 
  filter(Commodity %in% c("Biomethane", "Hydrogen")) %>% 
  bind_cols(
  x %>% filter(!(Commodity %in% c("Biomethane", "Hydrogen"))) %>% 
    select(-matches("Portfolio"))) %>% 
  select(-matches("Commodity")) %>% 
  rename(col_1 = `Relative Difference...4`, 
         col_2 = `Relative Difference...6`) %>% 
  mutate(row = rep(c(1, 2), length(robust_diffs))) %>% 
  pivot_wider(names_from = `Comparison Portfolio`, 
                values_from = c(col_1, col_2))
  )
  
robust_diffs <- robust_diffs %>% bind_rows() 
names(robust_diffs) <- gsub(x = names(robust_diffs), pattern = "col_\\d_", replacement = "")
names(robust_diffs)[3:14] <- paste0(names(robust_diffs)[3:14], "_1")
names(robust_diffs)[15:26] <- paste0(names(robust_diffs)[15:26], "_2")
robust_diffs <- robust_diffs %>% select(order(colnames(robust_diffs))) %>% 
  select(Portfolio, everything(), - row)
names(robust_diffs) <- gsub(x = names(robust_diffs), pattern = "_\\d", replacement = "")

# Adding Updated Info and New Requests ------------------------------------

# Jobs by Industry  

industry_weights <- read_excel("SJV Variable Architecture.xlsx", sheet = "Job_IndustryWeights") %>%
  select(-Technology, - `Type of Energy`) %>% 
  mutate(Temporary = Construction + Manufacturing,
         Permanent = 1 - Temporary)

jobs_names <- paste(colnames(industry_weights)[-c(1:2)], 
                    rep(c("Direct Employment (# New Jobs) (nominal)", 
                          "Direct Employment (# New Jobs) (nominal) (cumulative)"), each = dim(industry_weights)[2] - 2), 
                    sep = " - ")


final_output <- final_output %>% 
  arrange(Portfolio, Feedstock, Commodity, Use, Year) %>% 
  group_by(Portfolio, Feedstock, Commodity, Use) %>% 
  mutate(across(`Direct Employment (# New Jobs) (nominal)`:`Induced Employment (# New Jobs) (nominal)`,
                ~ cumsum(.x),
                .names = "{.col} (cumulative)")) %>%
  left_join(industry_weights) %>% 
  mutate(across(Agriculture:Permanent, ~ `Direct Employment (# New Jobs) (nominal)` * .x,
                .names = "{.col}_direct")) %>% 
  mutate(across(Agriculture_direct:Permanent_direct, ~ cumsum(.x),
                .names = "{.col}_cumulative")) %>% 
  ungroup() %>% 
  select(-Agriculture:-Permanent)

colnames(final_output)[32:49] <- jobs_names


# Avoided Pollutants
avoided_pollutants <- read_excel("SJV Variable Architecture.xlsx", sheet = "Emissions") %>% 
  select(Commodity:`Energy Value`, -`Energy Unit`) %>% 
  rename(`Variable Category Pollutants` = `Variable Category`, 
         `Energy Value Pollutants` = `Energy Value`)

final_output <- final_output %>%
  left_join(avoided_pollutants, relationship = "many-to-many") %>% 
  pivot_wider(names_from = `Variable Category Pollutants`,
              values_from = `Energy Value Pollutants`) %>% 
  mutate(`Avoided NOx Emisison (ton NOx) (nominal)` = `Adjusted Energy Used in MJ` * `NOx Emission` /  1e+06,
         `Avoided PM2.5 Emisison (ton PM2.5) (nominal)` =  `Adjusted Energy Used in MJ` * `PM2.5 Emission` /  1e+06) %>%
  select(-`PM2.5 Emission`:-`NA`)

# Revenue

commodity_prices <- read_excel("SJV Variable Architecture.xlsx", sheet = "Commodity Prices") %>% 
  select(Feedstock:Value) %>% 
  select(-Unit, - `Variable Category`) %>% 
  rename(`Uncertainty Range Category Revenue` = `Uncertainty Range Category`,
         `Value Revenue` = Value)
  

final_output <- final_output %>% 
  left_join(commodity_prices %>% filter(Commodity == "Electricity"), 
            by = c("Feedstock", "Commodity"),
            relationship = "many-to-many") %>% 
  left_join(commodity_prices %>% filter(Commodity != "Electricity") %>% 
              select(-Feedstock), 
            by = "Commodity",
            relationship = "many-to-many") %>% 
  mutate(`Uncertainty Range Category Revenue` = case_when(!is.na(`Uncertainty Range Category Revenue.x`) & is.na(`Uncertainty Range Category Revenue.y`)  ~ `Uncertainty Range Category Revenue.x`,
                                                 is.na(`Uncertainty Range Category Revenue.x`) & !is.na(`Uncertainty Range Category Revenue.y`)  ~ `Uncertainty Range Category Revenue.y`,
                                                 .default = NA),
         `Value Revenue` = case_when(!is.na(`Value Revenue.x`) & is.na(`Value Revenue.y`) ~ `Value Revenue.x`, 
                                     is.na(`Value Revenue.x`) & !is.na(`Value Revenue.y`) ~ `Value Revenue.y`, 
                                     .default = NA)) %>%
  select(-`Uncertainty Range Category Revenue.x`:-`Value Revenue.y`) %>% 
  mutate(`Gross Revenue (Million Dollars)` = Buildout * `Value Revenue` / 1e+06) %>% 
  pivot_wider(names_from = `Uncertainty Range Category Revenue`,
              values_from = c(`Gross Revenue (Million Dollars)`, `Value Revenue`)) %>% 
  rename(`Gross Revenue (Million Dollars) (nominal)` = `Gross Revenue (Million Dollars)_Nominal`,
         `Gross Revenue (Million Dollars) (low)` = `Gross Revenue (Million Dollars)_Low`) %>% 
  select(-`Value Revenue_Nominal`:-`Value Revenue_Low`)

# Living Wages

wage_distribution <- read_excel("SJV Variable Architecture.xlsx", sheet = "Wage Distribution") %>% 
  pivot_wider(names_from =`Variable Subcategory`, values_from = Value) %>% 
  select(-`Type of Energy`:-`Variable Category`)
         
final_output <- final_output %>% left_join(wage_distribution) %>% 
  mutate(`Direct Employment (# New Jobs Under Living Wage) (nominal)` = 
           `Direct Employment (# New Jobs) (nominal)` * `Share of Postings Under Living Wage`, 
         `Direct Employment (# New Jobs Over Living Wage) (nominal)` = 
           `Direct Employment (# New Jobs) (nominal)` * `Share of Postings Over Living Wage`) %>%
  select(-`Share of Postings Under Living Wage`, -`Share of Postings Over Living Wage`) 

# Saving Output -----------------------------------------------------------

write_xlsx(list("Avoided Emissions" = final_output),
           path = paste0("output/Full Portfolio Analysis ", today(), ".xlsx"))

write_xlsx(list("Robustness base" = robust_diffs),
           path = paste0("output/Portfolio Robustness ", today(), ".xlsx"))
