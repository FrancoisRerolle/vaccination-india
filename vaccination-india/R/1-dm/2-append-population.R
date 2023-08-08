#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file adds district-level population data

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_sf <- readRDS(file = here("data/final",
                                        "data_district_sf"))

# World pop
worldpop2020 <- raster(here("data/untouched/worldpop/ind_ppp_2020_1km_Aggregated_UNadj.tif"))

#-------------------------------------------------------------------------------

## Process data
number_of_birth_cohort <- 2
crude_birth_rate <- (17/1000) # World bank
data_district_sf$Population <- (number_of_birth_cohort*crude_birth_rate)*raster::extract(x = worldpop2020, y = data_district_sf, fun = sum, na.rm = T)

data_district_sf <- (data_district_sf
                     %>% mutate(Undervaccinated_FIC = Population * (1-FIC),
                                Vaccinated_FIC = Population * (FIC))
                     )

#-------------------------------------------------------------------------------

# Save dataset
data_district_pop_sf <- data_district_sf
saveRDS(data_district_pop_sf, file = here("data/final", "data_district_pop_sf"))

#-------------------------------------------------------------------------------