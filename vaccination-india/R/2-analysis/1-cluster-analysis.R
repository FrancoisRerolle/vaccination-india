#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file identifies clusters of undervaccination
# This code is inspired from Gabriel Carrasco Escobar's code 
# https://github.com/benmarhnia-lab/vaccines_ineq/blob/main/5_Cluster%20analysis_v3.Rmd

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_pop_sf <- readRDS(file = here("data/final",
                                        "data_district_pop_sf"))

#-------------------------------------------------------------------------------

## Process data
# Function to remove regions with no neighbors (Islands of India basically)
clean_sp <- function(object) {
  
  #share_sp_0 <- as(object, "Spatial")
  object_1 <- st_make_valid(object)
  share_nb_0 <- poly2nb(object_1)  #queen contiguity
  #summary(share_nb_0)
  
  list_r <- card(share_nb_0) %>%
    as_data_frame() %>%
    rownames_to_column() %>%
    filter(value<1) %>%
    pull(rowname)
  
  share <- object_1 %>%
    rownames_to_column(var = "id") %>%
    filter(!(id %in% list_r)) 
  
  return(share)
}

# Cluster analysis
breaks <- c(-Inf, -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, Inf)
labels <- c("Low (99% confidence)", "Low (95% confidence)", "Low (90% confidence)", "NS","High (90% confidence)", "High (95% confidence)", "High (99% confidence)")

share_getis <- data_district_pop_sf %>%
  nest() %>%
  #crossing(var1 = c("FIC","PIC")) %>%
  mutate(var1 = "FIC") %>%
  mutate(data_clean = map(.x = data, .f = ~clean_sp(.)),
         data_sub = map2(.x = data_clean, .y = var1, 
                         .f = ~dplyr::select(.x, .y, region, district) %>% 
                           rename(output = 1)),
         dat_tbl = map(.x = data_sub, .f = ~st_set_geometry(.x, NULL)),
         units = map_dbl(.x = dat_tbl, .f = ~nrow(.x))) %>%
  filter(units>0) %>%
  mutate(#share_sp = map(.x = data_sub, .f = ~as(.x, "Spatial")),
    share_nb = map(.x = data_sub, .f = ~poly2nb(.x)),
    share_w = map(.x = share_nb, .f = ~nb2listw(.x)),
    LISA = map2(.x = data_sub, .y = share_w, .f = ~localG(.x$output, .y)),
    clust_LISA = map(.x = LISA, .f = ~cut(.x, include.lowest = TRUE, 
                                          breaks = breaks, labels = labels,
                                          ordered_result = T))) %>%
  dplyr::select(dat_tbl, var1, LISA, clust_LISA) %>%
  unnest() %>%
  dplyr::select(-output, -var1) %>%
  rename(FIC_LISA = LISA,
         FIC_clust_LISA = clust_LISA)

# Append to original dataset
data_district_lisa_sf <- data_district_pop_sf %>% inner_join(share_getis)

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(data_district_lisa_sf, file = here("data/final", "data_district_lisa_sf"))

#-------------------------------------------------------------------------------