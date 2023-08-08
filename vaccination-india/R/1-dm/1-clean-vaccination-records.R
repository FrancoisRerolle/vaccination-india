#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file cleans the birth record history out of the DHS data
#                and immunization status
# This code is inspired from Anna Dimitrova's code 
# https://github.com/benmarhnia-lab/vaccines_ineq/blob/main/1_Retrieve%20DHS7%20data.R

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
# DHS birth records
India_2019_DHS <- read_dta(here("data/untouched/dhs",
                                "IAKR7EDT",
                                "IAKR7EFL.DTA"))

# Geographical info
India_2019_GPS <- st_read(here("data/untouched/dhs",
                              "IAGE7AFL",
                              "IAGE7AFL.shp"))

# Merge
India_2019 <- (India_2019_DHS
               %>% mutate(DHSCLUST = v001)
               %>% left_join(India_2019_GPS))

#-------------------------------------------------------------------------------

## Process data
# Select and rename variables of interest
data_0 <- (India_2019
           %>% rename(age.months = b19, #age of the child measured in months 
                      region = ADM1NAME,
                      district = DHSREGNA,
                      wealth = v190) 
           %>% mutate(
             resident = as.character(to_factor(v135)),
             # Immunization records
             rcvd.BCG = as.character(to_factor(h2)),
             rcvd.OPV1 = as.character(to_factor(h4)),
             rcvd.OPV2 = as.character(to_factor(h6)),
             rcvd.OPV3 = as.character(to_factor(h8)),
             rcvd.DTP1 = as.character(to_factor(h3)),
             rcvd.DTP2 = as.character(to_factor(h5)),
             rcvd.DTP3 = as.character(to_factor(h7)),
             rcvd.MCV = as.character(to_factor(h9)),
             rcvd.VitaminA = as.character(to_factor(h33)),
             rcvd.Penta = as.character(to_factor(h51)),
             rcvd.Rota = as.character(to_factor(h57)),
             rcvd.Hepa = as.character(to_factor(h61)),
             rcvd.Deworming = as.character(to_factor(h43)),
             wt=v005/1000000) #generate weights for the mother
           %>% dplyr::select(age.months, region, district, wealth, resident, wt,
                             paste0("rcvd.", c("BCG", "OPV1", "OPV2", "OPV3", "DTP1", "DTP2", "DTP3", "MCV", "VitaminA", "Penta", "Rota", "Hepa", "Deworming")))
           )

# Filter data to resident and children aged between 12 and 35 months
data_1 <- (data_0
           %>% filter(age.months >= 12 & age.months <= 35) #restrict the sample to children between 12 and 35 months of age (no info for older children for the latest survey wave since 2017)
           %>% filter(resident == "usual resident")
           %>% dplyr::select(-age.months, -resident)
           )

# Process vaccination variables
data_2 <- (data_1
           %>% mutate(across(grep(pattern = "rcvd", # Clean vaccination variables
                                  x = colnames(data_1),
                                  value = T),
                             function(x){ifelse(x %in% c("no", "don't know"),
                                                FALSE, # Per DHS guideline, missing vaccination status should be treated as no vaccination
                                                TRUE)})))

data_3 <- (data_2
           %>% mutate(Polio = rcvd.OPV1 & rcvd.OPV2 & rcvd.OPV3,
                      DTP = rcvd.DTP1 & rcvd.DTP2 & rcvd.DTP3,
                      BCG = rcvd.BCG,
                      MCV = rcvd.MCV,
                      Hib = rcvd.Penta,
                      Hep = rcvd.Hepa,
                      Rotavirus = rcvd.Rota,
                      VitaminA = rcvd.VitaminA,
                      Deworming = rcvd.Deworming,
                      BCG_Polio = BCG & Polio,
                      BCG_DTP = BCG & DTP,
                      BCG_MCV = BCG & MCV,
                      Polio_DTP = Polio & DTP,
                      Polio_MCV = Polio & MCV,
                      DTP_MCV = DTP & MCV,
                      BCG_Polio_DTP = BCG & Polio & DTP,
                      BCG_Polio_MCV = BCG & Polio & MCV,
                      BCG_DTP_MCV = BCG & DTP & MCV,
                      Polio_DTP_MCV = Polio & DTP & MCV,
                      FIC = BCG & Polio & DTP & MCV,
                      Zero_Child = !(BCG | Polio | DTP | MCV),
                      No_BCG_Only = !BCG & Polio & DTP & MCV,
                      No_Polio_Only = BCG & !Polio & DTP & MCV,
                      No_DTP_Only = BCG & Polio & !DTP & MCV,
                      No_MCV_Only = BCG & Polio & DTP & !MCV)
           %>% dplyr::select(-grep(pattern = "rcvd", x = colnames(data_2), value = T)))

# Aggregate at district level
data_district <- (data_3 %>% 
                    group_by(region, district) %>% 
                    summarise(Wealth_1_FIC = sum(FIC*(wealth == 1), na.rm = T),
                              Wealth_1_Rot = sum(Rotavirus*(wealth == 1), na.rm = T),
                              Wealth_1 = sum((wealth == 1), na.rm = T),
                              Wealth_2_FIC = sum(FIC*(wealth == 2), na.rm = T),
                              Wealth_2_Rot = sum(Rotavirus*(wealth == 2), na.rm = T),
                              Wealth_2 = sum((wealth == 2), na.rm = T),
                              Wealth_3_FIC = sum(FIC*(wealth == 3), na.rm = T),
                              Wealth_3_Rot = sum(Rotavirus*(wealth == 3), na.rm = T),
                              Wealth_3 = sum((wealth == 3), na.rm = T),
                              Wealth_4_FIC = sum(FIC*(wealth == 4), na.rm = T),
                              Wealth_4_Rot = sum(Rotavirus*(wealth == 4), na.rm = T),
                              Wealth_4 = sum((wealth == 4), na.rm = T),
                              Wealth_5_FIC = sum(FIC*(wealth == 5), na.rm = T),
                              Wealth_5_Rot = sum(Rotavirus*(wealth == 5), na.rm = T),
                              Wealth_5 = sum((wealth == 5), na.rm = T),
                              across(grep(pattern = "BCG|Polio|DTP|MCV|VitaminA|Deworming|Rotavirus|Hep|Hib|FIC|Zero",
                                          x = colnames(data_3),
                                          value = T),
                                     function(x){weighted.mean(x, wt, na.rm=T)}),
                              N = n())
                  %>% ungroup())

# Append spatial boundaries
# Preliminary cleaning
data_district$district[data_district$district == "Buxar"] = "Buxer"  
data_district$district[data_district$district == "Janjgir - Champa"] = "Janjgir-Champa"  
data_district$district[data_district$district == "Mahrajganj"] = "Maharajganj"  
data_district$district[data_district$district == "Sant Ravidas Nagar (Bhadohi)"] = "Sant Ravidas Nagar" 
data_district$region[data_district$region == "DADRA & NAGAR HAVELI AND DAMAN & DIU"] = "DADRA & NAGAR HAVELI & DAMAN & DIU" 

borders <- download_boundaries(surveyId = "IA2020DHS", method = "sf")
borders.sf.0 <- do.call(rbind.data.frame, borders) #convert to special feature dataframe

borders.sf <- (borders.sf.0 
            %>% filter(LEVELCO == "Admin2") 
            %>% dplyr::select(DHSREGEN, OTHREGNA) 
            %>% rename(district = DHSREGEN,
                       region = OTHREGNA)
            %>% mutate(region= toupper(region)))

data_district_sf <- (borders.sf 
                     %>% left_join(data_district)
                     )

# Final cleaning
data_district_sf$State <- data_district_sf$region
data_district_sf$District <- data_district_sf$district
India_Regions <- tibble(Region = c(rep("North", 10),
                                   rep("Central", 6),
                                   rep("South", 6),
                                   rep("East", 5),
                                   rep("North East", 7)),
                        State = c("LADAKH", "JAMMU & KASHMIR", "HIMACHAL PRADESH", "CHANDIGARH", "PUNJAB", "UTTARAKHAND", "HARYANA", "RAJASTHAN", "UTTAR PRADESH", "NCT OF DELHI",
                                  "GUJARAT", "MAHARASHTRA", "DADRA & NAGAR HAVELI & DAMAN & DIU", "GOA" ,"MADHYA PRADESH", "CHHATTISGARH",
                                  "TELANGANA", "KARNATAKA" ,"ANDHRA PRADESH", "KERALA" ,"TAMIL NADU", "PUDUCHERRY",
                                  "BIHAR", "JHARKHAND", "WEST BENGAL", "ODISHA", "SIKKIM",
                                  "ARUNACHAL PRADESH", "ASSAM", "MEGHALAYA", "NAGALAND", "MANIPUR", "TRIPURA", "MIZORAM"))

data_district_sf <- (data_district_sf
                     %>% left_join(India_Regions))


#-------------------------------------------------------------------------------

# Save dataset
saveRDS(data_district_sf,
        file = here("data/final", "data_district_sf"))
saveRDS(data_3,
        file = here("data/final", "data_child"))

#-------------------------------------------------------------------------------
