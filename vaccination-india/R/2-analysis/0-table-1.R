#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file produces a table 1

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))
source(here("vaccination-india/R", "0-modified-table1-package.R"))

#-------------------------------------------------------------------------------

## Load data
data_child <- readRDS(file = here("data/final",
                                  "data_child"))

#-------------------------------------------------------------------------------

## Prepare inouts for tables
# label variables' factors
data <- (data_child %>%
                 mutate(Women_Sampling_Weight = wt,
                        wealth = factor(wealth,
                                        levels = c("1", "2", "3", "4", "5"),
                                        labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
                        )
               )



# label variable
labels <- list(
  variables=list(Hib = "Hib",
                 Hep = "Hep",
                 DTP = "DTP",
                 MCV = "MCV",
                 Polio = "Polio",
                 BCG = "BCG",
                 Rotavirus = "Rotavirus",
                 Deworming = "Deworming",
                 VitaminA = "Vitamin A")
)

# Specify tables column's stratification
strata.1 <- c(split(data, ~ wealth),
              list(Overall = data))

# Specify render functions
my.render.cat <- function(x, weights, variable) {
  c("", sapply(stats.default(x, weights), function(y) with(y, sprintf("%d (%0.0f %%)",
                                                                      FREQ, PCT))))
}

# Weighted Table 1 (stratified by wealth index)
table1(x = strata.1,
       labels,
       render.categorical = my.render.cat
)

