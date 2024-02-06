
hydrogen_feedstock <-  c("Solar", "Natural Gas + CCS", "Agricultural waste",
                         "Forest waste")
hydrogen_use <- c("Surface Transport Fuel", "Ammonia Production",
                  "Green Steel", "Industrial Heat")

elec_feedstock <- c("Solar", "Wind", "Biomass", "Li Battery", "LDES")
elec_use <- c("Power Grid")

biomethane_feedstock <- c("Animal Manure", "Diverted Organic Waste")
biomethane_use <- c("Gas Grid", "Electricity", "Hydrogen")

jobs_use <- feedstock_use(f2c_porfolios[[1]],  "Electricity", 
                          c("Solar", "Wind", "Li Battery", "LDES",  "Forest waste", "Agricultural waste"), jobs_use_coef) %>% 
  arrange(Feedstock, Commodity, `Variable Subcategory`, year) %>% 
  group_by(Feedstock, Commodity, `Variable Subcategory`) %>% 
  mutate(diff = Buildout - dplyr::lag(Buildout),
         test = (1 - `Annual Productivity`) ** (year - 2025),
         `Conversion Value` = case_when(diff > 0 ~ diff *  Value * (1 - `Annual Productivity`) ** (year - 2025),
                                        is.na(diff) ~ Buildout * Value,  
                                        .default = 0))  %>% 
  select(-diff) %>% 
  ungroup()
jobs_use %>% View
