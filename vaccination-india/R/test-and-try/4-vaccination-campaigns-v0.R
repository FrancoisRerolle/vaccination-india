#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code simulates vaccination campaigns

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))
source(here("vaccination-india/R", "0-functions.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_lisa_sf <- readRDS(file = here("data/final",
                                        "data_district_lisa_sf"))

#-------------------------------------------------------------------------------
total_population <- sum(data_district_lisa_sf$Population)
total_population_fic <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$FIC)
total_population_dtp <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$DTP)
total_population_bcg <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$BCG)
total_population_mcv <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$MCV)
total_population_pol <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$Polio)
total_population_rot <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$Rotavirus)
total_population_vita <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$VitaminA)
total_population_dewo <- sum(data_district_lisa_sf$Population * data_district_lisa_sf$Deworming)

vaccines <- c("BCG", "Polio", "DTP", "MCV")
number_of_vaccines <- length(vaccines)
number_of_districts <- nrow(data_district_lisa_sf)

## Vaccination campaign 1: identify district where FIC immunization campaign yields best increase in FIC
vaccination_campaign_1 <- (data_district_lisa_sf
                           %>% arrange(FIC)
                           %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                      Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                      Percent_DTP = (total_population_dtp + cumsum(Population*(1-DTP)))/total_population,
                                      Percent_BCG = (total_population_bcg + cumsum(Population*(1-BCG)))/total_population,
                                      Percent_MCV = (total_population_mcv + cumsum(Population*(1-MCV)))/total_population,
                                      Percent_Polio = (total_population_pol + cumsum(Population*(1-Polio)))/total_population,
                                      Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                      Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                      Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                           )

vaccination_campaign_1_rot <- (data_district_lisa_sf
                           %>% arrange(Rotavirus)
                           %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                      Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                      Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                      Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                      Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
)

vaccination_campaign_1_vita <- (data_district_lisa_sf
                               %>% arrange(VitaminA)
                               %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                          Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                          Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                          Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                          Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
)

vaccination_campaign_1_dewo <- (data_district_lisa_sf
                               %>% arrange(Deworming)
                               %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                          Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                          Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                          Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                          Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
)

# Adjusted for distance between 2 consecutive districts of the vaccination campaign
vaccination_campaign_1_km <- (data_district_lisa_sf
                              %>% arrange(FIC)
                              %>% mutate(geometry_previous = lag(geometry,1))
                              %>% rowwise()
                              %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                              %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                              %>% ungroup()
                              %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                         Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                         Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                         Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                         Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                              )

## Vaccination campaign 1cs: identify district where FIC immunization campaign yields best increase in FIC, restricted to undervaccination cluster districts
vaccination_campaign_1cs <- (data_district_lisa_sf
                             %>% filter(FIC_LISA <= -1.65)
                             %>% arrange(FIC)
                             %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                        Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                        Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                        Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                        Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                             )

# Adjusted for distance between 2 consecutive districts of the vaccination campaign
vaccination_campaign_1cs_km <- (data_district_lisa_sf
                                %>% filter(FIC_LISA <= -1.65)
                                %>% arrange(FIC)
                                %>% mutate(geometry_previous = lag(geometry,1))
                                %>% rowwise()
                                %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                                %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                                %>% ungroup()
                                %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                           Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                           Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                           Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                           Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                )

## Vaccination campaign 1cs: identify district where FIC immunization campaign yields best increase in FIC, restricted to cluster
vaccination_campaign_1cs_threshold <- (data_district_lisa_sf
                                       %>% filter(FIC <= 0.9)
                                       %>% arrange(FIC)
                                       %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                                  Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                  Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                  Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                  Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                       )

# Adjusted for distance between 2 consecutive districts of the vaccination campaign
vaccination_campaign_1cs_threshold_km <- (data_district_lisa_sf
                                          %>% filter(FIC <= 0.9)
                                          %>% arrange(FIC)
                                          %>% mutate(geometry_previous = lag(geometry,1))
                                          %>% rowwise()
                                          %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                                          %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                                          %>% ungroup()
                                          %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                                     Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                     Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                     Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                     Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                          )

## Vaccination campaign 1 stratified by state: identify district where FIC immunization campaign yields best increase in FIC
vaccination_campaign_1_state <- (data_district_lisa_sf
                                 %>% arrange(FIC)
                                 %>% group_by(State)
                                 %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                            Population_State = sum(Population),
                                            Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                 %>% ungroup()
                                 %>% arrange(-Percent_Undervaccinated_FIC_State)
                                 %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                            Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                            Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                            Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                            Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                 )

vaccination_campaign_1_state_km <- (data_district_lisa_sf
                                    %>% arrange(FIC)
                                    %>% group_by(State)
                                    %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                               Population_State = sum(Population),
                                               Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                    %>% ungroup()
                                    %>% arrange(-Percent_Undervaccinated_FIC_State)
                                    %>% mutate(geometry_previous = lag(geometry,1))
                                    %>% rowwise()
                                    %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                                    %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                                    %>% ungroup()
                                    %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                               Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                               Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                               Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                               Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                    )

## Vaccination campaign 1cs stratified by state: identify district where FIC immunization campaign yields best increase in FIC, restricted to cluster
vaccination_campaign_1cs_state <- (data_district_lisa_sf
                                   %>% filter(FIC_LISA <= -1.65)
                                   %>% arrange(FIC)
                                   %>% group_by(State)
                                   %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                              Population_State = sum(Population),
                                              Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                   %>% ungroup()
                                   %>% arrange(-Percent_Undervaccinated_FIC_State)
                                   %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                              Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                              Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                              Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                              Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                   )

vaccination_campaign_1cs_state_bis <- (data_district_lisa_sf
                                   %>% filter(FIC_LISA <= -1.285)
                                   %>% arrange(FIC)
                                   %>% group_by(State)
                                   %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                              Population_State = sum(Population),
                                              Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                   %>% ungroup()
                                   %>% arrange(-Percent_Undervaccinated_FIC_State)
                                   %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                              Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                              Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                              Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                              Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
)

vaccination_campaign_1cs_state_km <- (data_district_lisa_sf
                                      %>% filter(FIC_LISA <= -1.65)
                                      %>% arrange(FIC)
                                      %>% group_by(State)
                                      %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                                 Population_State = sum(Population),
                                                 Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                      %>% ungroup()
                                      %>% arrange(-Percent_Undervaccinated_FIC_State)
                                      %>% mutate(geometry_previous = lag(geometry,1))
                                      %>% rowwise()
                                      %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                                      %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                                      %>% ungroup()
                                      %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                                 Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                 Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                 Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                 Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                      )

## Vaccination campaign 1cs stratified by state: identify district where FIC immunization campaign yields best increase in FIC, restricted to cluster
vaccination_campaign_1cs_threshold_state <- (data_district_lisa_sf
                                             %>% filter(FIC <= 0.9)
                                             %>% arrange(FIC)
                                             %>% group_by(State)
                                             %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                                        Population_State = sum(Population),
                                                        Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                             %>% ungroup()
                                             %>% arrange(-Percent_Undervaccinated_FIC_State)
                                             %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                                        Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                        Percent_DTP = (total_population_dtp + cumsum(Population*(1-DTP)))/total_population,
                                                        Percent_BCG = (total_population_bcg + cumsum(Population*(1-BCG)))/total_population,
                                                        Percent_MCV = (total_population_mcv + cumsum(Population*(1-MCV)))/total_population,
                                                        Percent_Polio = (total_population_pol + cumsum(Population*(1-Polio)))/total_population,
                                                        Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                        Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                        Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                             )


vaccination_campaign_1cs_threshold_state_bis <- (data_district_lisa_sf
                                             %>% filter(FIC <= 0.8)
                                             %>% arrange(FIC)
                                             %>% group_by(State)
                                             %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                                        Population_State = sum(Population),
                                                        Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                             %>% ungroup()
                                             %>% arrange(-Percent_Undervaccinated_FIC_State)
                                             %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines),
                                                        Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                        Percent_DTP = (total_population_dtp + cumsum(Population*(1-DTP)))/total_population,
                                                        Percent_BCG = (total_population_bcg + cumsum(Population*(1-BCG)))/total_population,
                                                        Percent_MCV = (total_population_mcv + cumsum(Population*(1-MCV)))/total_population,
                                                        Percent_Polio = (total_population_pol + cumsum(Population*(1-Polio)))/total_population,
                                                        Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                        Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                        Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
)

vaccination_campaign_1cs_threshold_state_km <- (data_district_lisa_sf
                                                %>% filter(FIC <= 0.9)
                                                %>% arrange(FIC)
                                                %>% group_by(State)
                                                %>% mutate(Undervaccinated_FIC_State = sum(Undervaccinated_FIC),
                                                           Population_State = sum(Population),
                                                           Percent_Undervaccinated_FIC_State = Undervaccinated_FIC_State/Population_State)
                                                %>% ungroup()
                                                %>% arrange(-Percent_Undervaccinated_FIC_State)
                                                %>% mutate(geometry_previous = lag(geometry,1))
                                                %>% rowwise()
                                                %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
                                                %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
                                                %>% ungroup()
                                                %>% mutate(Cummulative_Immunization_Doses = cumsum(Population*number_of_vaccines*Distance),
                                                           Percent_FIC = (total_population_fic + cumsum(Undervaccinated_FIC))/total_population,
                                                           Percent_Rotavirus = (total_population_rot + cumsum(Population*(1-Rotavirus)))/total_population,
                                                           Percent_Vitamin_A = (total_population_vita + cumsum(Population*(1-VitaminA)))/total_population,
                                                           Percent_Deworming = (total_population_dewo + cumsum(Population*(1-Deworming)))/total_population)
                                                )

## Inequalities
vaccination_campaign_1$Concentration_Index <- concentration_index_function(vaccination_campaign_1)
vaccination_campaign_1_state$Concentration_Index <- concentration_index_function(vaccination_campaign_1_state)
vaccination_campaign_1cs_state$Concentration_Index <- concentration_index_function(vaccination_campaign_1cs_state)
vaccination_campaign_1cs_threshold_state$Concentration_Index <- concentration_index_function(vaccination_campaign_1cs_threshold_state)

vaccination_campaign_1_km$Concentration_Index <- concentration_index_function(vaccination_campaign_1_km)
vaccination_campaign_1cs_state_km$Concentration_Index <- concentration_index_function(vaccination_campaign_1cs_state_km)
vaccination_campaign_1cs_threshold_state_km$Concentration_Index <- concentration_index_function(vaccination_campaign_1cs_threshold_state_km)

vaccination_campaign_1$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1)
vaccination_campaign_1_state$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1_state)
vaccination_campaign_1cs_state$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1cs_state)
vaccination_campaign_1cs_threshold_state$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1cs_threshold_state)

vaccination_campaign_1_km$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1_km)
vaccination_campaign_1cs_state_km$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1cs_state_km)
vaccination_campaign_1cs_threshold_state_km$Concentration_Index_Rot <- rot_concentration_index_function(vaccination_campaign_1cs_threshold_state_km)


## Vaccination campaign 2: identify district/immunization combination where specific immunization yields best increase in FIC
vaccination_campaign_2_loop <- data_district_lisa_sf %>% st_set_geometry(NULL) %>% mutate(BCG_Polio_DTP_MCV = FIC)
Immunization_Doses <- NULL
FIC_Gain <- NULL
State <- NULL
District <- NULL
Vaccine <- NULL
for(i in 1:(number_of_districts*number_of_vaccines)){
  ## Identify district/immunization yielding best FIC increase
  data <- (vaccination_campaign_2_loop
           %>% dplyr::select(No_BCG_Only, No_Polio_Only, No_DTP_Only, No_MCV_Only))
  
  max_gain <- max(data)
  index_district_vaccine <- which(data == max_gain, arr.ind = T)[1,]
  index_district <- data_district_lisa_sf$district[index_district_vaccine[1]]
  index_State <- data_district_lisa_sf$State[index_district_vaccine[1]]
  index_vaccine <- colnames(data)[index_district_vaccine[2]]
  vaccine <- gsub(pattern = "No_|_Only", replacement = "", x = index_vaccine)
  
  ## Perform vaccination in identified district
  vaccination_campaign_2_loop[index_district_vaccine[1], vaccine] <- 1
  
  ## Derive all other vaccination metrics
  # Identify variables to be updated
  variables_containing_identified_vaccine_0 <- grep(pattern = vaccine, x = colnames(vaccination_campaign_2_loop), value = T)
  variables_containing_identified_vaccine_1 <- grep(pattern = "_", x = variables_containing_identified_vaccine_0, value = T) # Combination of vaccines only
  variables_containing_identified_vaccine <- grep(pattern = "Only", x = variables_containing_identified_vaccine_1, value = T, invert = T) # Combination of vaccines only
  
  # Identify variables to update from
  variables_containing_identified_vaccine_updated_0 <- gsub(pattern = vaccine, replacement = "", x = variables_containing_identified_vaccine)
  variables_containing_identified_vaccine_updated_1 <- gsub(pattern = "__", replacement = "_", x = variables_containing_identified_vaccine_updated_0)
  variables_containing_identified_vaccine_updated <- gsub(pattern = "^_|_$", replacement = "", x = variables_containing_identified_vaccine_updated_1)
  
  # Update
  vaccination_campaign_2_loop[index_district_vaccine[1], variables_containing_identified_vaccine] <- vaccination_campaign_2_loop[index_district_vaccine[1], variables_containing_identified_vaccine_updated]
  
  # Update No_Vaccination_Only variables
  vaccination_campaign_2_loop[index_district_vaccine[1], "No_BCG_Only"] <- (vaccination_campaign_2_loop[index_district_vaccine[1], "Polio_DTP_MCV"]
                                                                     - vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"])
  vaccination_campaign_2_loop[index_district_vaccine[1], "No_Polio_Only"] <- (vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_DTP_MCV"]
                                                                       - vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"])
  vaccination_campaign_2_loop[index_district_vaccine[1], "No_DTP_Only"] <- (vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_MCV"]
                                                                       - vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"])
  vaccination_campaign_2_loop[index_district_vaccine[1], "No_MCV_Only"] <- (vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_DTP"]
                                                                       - vaccination_campaign_2_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"])
  
  # Store results
  Immunization_Doses <- c(Immunization_Doses, vaccination_campaign_2_loop[index_district_vaccine[1], "Population"])
  FIC_Gain <- c(FIC_Gain, max_gain*vaccination_campaign_2_loop[index_district_vaccine[1], "Population"])
  State <- c(State, index_State)
  District <- c(District, index_district)
  Vaccine <- c(Vaccine, vaccine)
}

vaccination_campaign_2 <- (tibble(Immunization_Doses, FIC_Gain, State, District, Vaccine)
                           %>% mutate(Cummulative_Immunization_Doses = cumsum(Immunization_Doses),
                                      Percent_FIC = (total_population_fic + cumsum(FIC_Gain))/total_population)
                           )


# Vaccination campaign 3: identify district/immunization combination where specific immunization yields best increase in specific vaccine coverage
vaccination_campaign_3_loop <- data_district_lisa_sf %>% st_set_geometry(NULL) %>% mutate(BCG_Polio_DTP_MCV = FIC)
Immunization_Doses <- NULL
FIC_Gain <- NULL
State <- NULL
District <- NULL
Vaccine <- NULL
for(i in 1:(number_of_districts*number_of_vaccines)){
  ## Identify district/immunization yielding best FIC increase
  data <- (vaccination_campaign_3_loop
           %>% dplyr::select(BCG, Polio, DTP, MCV))
  
  max_gain <- min(data)
  index_district_vaccine <- which(data == max_gain, arr.ind = T)[1,]
  index_district <- data_district_lisa_sf$district[index_district_vaccine[1]]
  index_State <- data_district_lisa_sf$State[index_district_vaccine[1]]
  vaccine <- colnames(data)[index_district_vaccine[2]]
  
  ## Perform vaccination in identified district
  vaccination_campaign_3_loop[index_district_vaccine[1], vaccine] <- 1
  
  ## Derive all other vaccination metrics
  # Identify variables to be updated
  variables_containing_identified_vaccine_0 <- grep(pattern = vaccine, x = colnames(vaccination_campaign_3_loop), value = T)
  variables_containing_identified_vaccine_1 <- grep(pattern = "_", x = variables_containing_identified_vaccine_0, value = T) # Combination of vaccines only
  variables_containing_identified_vaccine <- grep(pattern = "Only", x = variables_containing_identified_vaccine_1, value = T, invert = T) # Combination of vaccines only
  
  # Identify variables to update from
  variables_containing_identified_vaccine_updated_0 <- gsub(pattern = vaccine, replacement = "", x = variables_containing_identified_vaccine)
  variables_containing_identified_vaccine_updated_1 <- gsub(pattern = "__", replacement = "_", x = variables_containing_identified_vaccine_updated_0)
  variables_containing_identified_vaccine_updated <- gsub(pattern = "^_|_$", replacement = "", x = variables_containing_identified_vaccine_updated_1)
  
  # Update
  fic_before_vaccination <- vaccination_campaign_3_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"]
  vaccination_campaign_3_loop[index_district_vaccine[1], variables_containing_identified_vaccine] <- vaccination_campaign_3_loop[index_district_vaccine[1], variables_containing_identified_vaccine_updated]
  fic_after_vaccination <- vaccination_campaign_3_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"]
  fic_gain <- fic_after_vaccination - fic_before_vaccination
  
  # Store results
  Immunization_Doses <- c(Immunization_Doses, vaccination_campaign_3_loop[index_district_vaccine[1], "Population"])
  FIC_Gain <- c(FIC_Gain, fic_gain*vaccination_campaign_3_loop[index_district_vaccine[1], "Population"])
  State <- c(State, index_State)
  District <- c(District, index_district)
  Vaccine <- c(Vaccine, vaccine)
}

vaccination_campaign_3 <- (tibble(Immunization_Doses, FIC_Gain, State, District, Vaccine)
                           %>% mutate(Cummulative_Immunization_Doses = cumsum(Immunization_Doses),
                                      Percent_FIC = (total_population_fic + cumsum(FIC_Gain))/total_population)
)

# vaccination_campaign_3_km <- (tibble(Immunization_Doses, FIC_Gain, State, District, Vaccine)
#                               %>% right_join(data_district_lisa_sf %>% dplyr::select(`State`, `District`))
#                               %>% mutate(geometry_previous = lag(geometry,1))
#                               %>% rowwise()
#                               %>% mutate(Distance = st_distance(x = geometry, y = geometry_previous))
#                               %>% mutate(Distance = replace(Distance, is.na(Distance), 0))
#                               %>% ungroup()
#                               %>% mutate(Cummulative_Immunization_Doses = cumsum(Immunization_Doses*Distance),
#                                          Percent_FIC = (total_population_fic + cumsum(FIC_Gain))/total_population)
#                               )

# Vaccination campaigns: identify district where specific immunization yields best increase in specific vaccine coverage
# 24 different combination possible
vaccination_campaign_4 <- vaccination_campaign_successive(vaccine_order = c("Polio", "DTP", "MCV", "BCG"))
vaccination_campaign_5 <- vaccination_campaign_successive(vaccine_order = c("Polio", "DTP", "BCG", "MCV"))
vaccination_campaign_6 <- vaccination_campaign_successive(vaccine_order = c("Polio", "MCV", "DTP", "BCG"))
vaccination_campaign_7 <- vaccination_campaign_successive(vaccine_order = c("Polio", "MCV", "BCG", "DTP"))
vaccination_campaign_8 <- vaccination_campaign_successive(vaccine_order = c("Polio", "BCG", "DTP", "MCV"))
vaccination_campaign_9 <- vaccination_campaign_successive(vaccine_order = c("Polio", "BCG", "MCV", "DTP"))
vaccination_campaign_10 <- vaccination_campaign_successive(vaccine_order = c("DTP", "Polio", "MCV", "BCG"))
vaccination_campaign_11 <- vaccination_campaign_successive(vaccine_order = c("DTP", "Polio", "BCG", "MCV"))
vaccination_campaign_12 <- vaccination_campaign_successive(vaccine_order = c("MCV", "Polio", "DTP", "BCG"))
vaccination_campaign_13 <- vaccination_campaign_successive(vaccine_order = c("MCV", "Polio", "BCG", "DTP"))
vaccination_campaign_14 <- vaccination_campaign_successive(vaccine_order = c("BCG", "Polio", "DTP", "MCV"))
vaccination_campaign_15 <- vaccination_campaign_successive(vaccine_order = c("BCG", "Polio", "MCV", "DTP"))
vaccination_campaign_16 <- vaccination_campaign_successive(vaccine_order = c("DTP", "MCV", "Polio", "BCG"))
vaccination_campaign_17 <- vaccination_campaign_successive(vaccine_order = c("DTP", "BCG", "Polio", "MCV"))
vaccination_campaign_18 <- vaccination_campaign_successive(vaccine_order = c("MCV", "DTP", "Polio", "BCG"))
vaccination_campaign_19 <- vaccination_campaign_successive(vaccine_order = c("MCV", "BCG", "Polio", "DTP"))
vaccination_campaign_20 <- vaccination_campaign_successive(vaccine_order = c("BCG", "DTP", "Polio", "MCV"))
vaccination_campaign_21 <- vaccination_campaign_successive(vaccine_order = c("BCG", "MCV", "Polio", "DTP"))
vaccination_campaign_22 <- vaccination_campaign_successive(vaccine_order = c("DTP", "MCV", "BCG", "Polio"))
vaccination_campaign_23 <- vaccination_campaign_successive(vaccine_order = c("DTP", "BCG", "MCV", "Polio"))
vaccination_campaign_24 <- vaccination_campaign_successive(vaccine_order = c("MCV", "DTP", "BCG", "Polio"))
vaccination_campaign_25 <- vaccination_campaign_successive(vaccine_order = c("MCV", "BCG", "DTP", "Polio"))
vaccination_campaign_26 <- vaccination_campaign_successive(vaccine_order = c("BCG", "DTP", "MCV", "Polio"))
vaccination_campaign_27 <- vaccination_campaign_successive(vaccine_order = c("BCG", "MCV", "DTP", "Polio"))

#-------------------------------------------------------------------------------

# Save dataset
saveRDS(vaccination_campaign_1, file = here("data/final", "vaccination_campaign_1"))
saveRDS(vaccination_campaign_1cs, file = here("data/final", "vaccination_campaign_1cs"))
saveRDS(vaccination_campaign_1cs_threshold, file = here("data/final", "vaccination_campaign_1cs_threshold"))
saveRDS(vaccination_campaign_1_km, file = here("data/final", "vaccination_campaign_1_km"))
saveRDS(vaccination_campaign_1cs_km, file = here("data/final", "vaccination_campaign_1cs_km"))
saveRDS(vaccination_campaign_1cs_threshold_km, file = here("data/final", "vaccination_campaign_1cs_threshold_km"))
saveRDS(vaccination_campaign_1_state, file = here("data/final", "vaccination_campaign_1_state"))
saveRDS(vaccination_campaign_1cs_state, file = here("data/final", "vaccination_campaign_1cs_state"))
saveRDS(vaccination_campaign_1cs_state_bis, file = here("data/final", "vaccination_campaign_1cs_state_bis"))
saveRDS(vaccination_campaign_1cs_threshold_state, file = here("data/final", "vaccination_campaign_1cs_threshold_state"))
saveRDS(vaccination_campaign_1cs_threshold_state_bis, file = here("data/final", "vaccination_campaign_1cs_threshold_state_bis"))
saveRDS(vaccination_campaign_1_state_km, file = here("data/final", "vaccination_campaign_1_state_km"))
saveRDS(vaccination_campaign_1cs_state_km, file = here("data/final", "vaccination_campaign_1cs_state_km"))
saveRDS(vaccination_campaign_1cs_threshold_state_km, file = here("data/final", "vaccination_campaign_1cs_threshold_state_km"))

saveRDS(vaccination_campaign_2, file = here("data/final", "vaccination_campaign_2"))
saveRDS(vaccination_campaign_3, file = here("data/final", "vaccination_campaign_3"))
saveRDS(vaccination_campaign_4, file = here("data/final", "vaccination_campaign_4"))
saveRDS(vaccination_campaign_5, file = here("data/final", "vaccination_campaign_5"))
saveRDS(vaccination_campaign_6, file = here("data/final", "vaccination_campaign_6"))
saveRDS(vaccination_campaign_7, file = here("data/final", "vaccination_campaign_7"))
saveRDS(vaccination_campaign_8, file = here("data/final", "vaccination_campaign_8"))
saveRDS(vaccination_campaign_9, file = here("data/final", "vaccination_campaign_9"))
saveRDS(vaccination_campaign_10, file = here("data/final", "vaccination_campaign_10"))
saveRDS(vaccination_campaign_11, file = here("data/final", "vaccination_campaign_11"))
saveRDS(vaccination_campaign_12, file = here("data/final", "vaccination_campaign_12"))
saveRDS(vaccination_campaign_13, file = here("data/final", "vaccination_campaign_13"))
saveRDS(vaccination_campaign_14, file = here("data/final", "vaccination_campaign_14"))
saveRDS(vaccination_campaign_15, file = here("data/final", "vaccination_campaign_15"))
saveRDS(vaccination_campaign_16, file = here("data/final", "vaccination_campaign_16"))
saveRDS(vaccination_campaign_17, file = here("data/final", "vaccination_campaign_17"))
saveRDS(vaccination_campaign_18, file = here("data/final", "vaccination_campaign_18"))
saveRDS(vaccination_campaign_19, file = here("data/final", "vaccination_campaign_19"))
saveRDS(vaccination_campaign_20, file = here("data/final", "vaccination_campaign_20"))
saveRDS(vaccination_campaign_21, file = here("data/final", "vaccination_campaign_21"))
saveRDS(vaccination_campaign_22, file = here("data/final", "vaccination_campaign_22"))
saveRDS(vaccination_campaign_23, file = here("data/final", "vaccination_campaign_23"))
saveRDS(vaccination_campaign_24, file = here("data/final", "vaccination_campaign_24"))
saveRDS(vaccination_campaign_25, file = here("data/final", "vaccination_campaign_25"))
saveRDS(vaccination_campaign_26, file = here("data/final", "vaccination_campaign_26"))
saveRDS(vaccination_campaign_27, file = here("data/final", "vaccination_campaign_27"))

#-------------------------------------------------------------------------------