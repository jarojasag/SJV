library(tidyverse)
library(readxl)

#Functions ---------------------------------------------------------------

feedstock_use <- function(buildout, commodities, feedstock, f2c_data){
  # buildout: User Input
  # commodities: User Input
  # feedstock: User Input
  # f2c_data: Coefficients from Excel Sheet for specific commodity
  
  feedstock_use <- buildout %>% 
    filter(Commodity %in% commodities,
           Feedstock %in% feedstock) %>% 
    pivot_longer(`2025`:`2045`, 
                 names_to = "year", 
                 values_to = "Installation") %>%
    mutate(year = as.integer(year)) %>% 
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


# Use Coefficients --------------------------------------------------------

coef_sheets <- readxl::excel_sheets("SJV Variable Architecture.xlsx")
f2c_sheets  <- grepl("F2C", coef_sheets)
coef_sheets <- coef_sheets[f2c_sheets]

f2c_data <- lapply(coef_sheets, function(x) 
  read_excel("SJV Variable Architecture.xlsx", sheet = x) %>% 
    select(Feedstock:Value))

names(f2c_data) <- coef_sheets

water_use_coef <- f2c_data$`F2C Water` %>% 
  filter(Commodity %in% water_ref_commodities)

land_use_coef <- f2c_data$`F2C Land` %>% 
  filter(Commodity %in% land_ref_commodities,
         Feedstock %in% land_ref_feedstock)

jobs_use_coef <- f2c_data$`F2C Jobs` %>% 
  filter(Commodity %in% jobs_ref_commodities,
         Feedstock %in% jobs_ref_feedstock)

# Water Use ---------------------------------------------------------------
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

water_use %>% write_csv("output/water_use_base.csv")

# Land Use ----------------------------------------------------------------
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

land_use %>% write_csv("output/land_use_base.csv")

# Job Calculation ---------------------------------------------------------
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

jobs_use %>% write_csv("output/jobs_use_base.csv")

# Avoided Emissions -------------------------------------------------------
# Figuring out the procedure with Hye-Min


