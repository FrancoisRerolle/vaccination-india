#-------------------------------------------------------------------------------
# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This file lists functions to be used in other codes
#-------------------------------------------------------------------------------

# this function simulates 4 successive vaccination campaigns depending on the order of the vaccines to be administered
vaccination_campaign_successive <-function(vaccine_order){
  vaccine_order <- vaccine_order
  vaccination_campaign_loop <- data_district_lisa_sf %>% st_set_geometry(NULL) %>% mutate(BCG_Polio_DTP_MCV = FIC)
  Immunization_Doses <- NULL
  FIC_Gain <- NULL
  Region <- NULL
  District <- NULL
  Vaccine <- NULL
  for(j in 1:length(vaccine_order)){
    vaccine <- vaccine_order[j]
    for(i in 1:699){
      ## Identify district yielding best vaccine coverage specific increase
      data <- (vaccination_campaign_loop
               %>% dplyr::select(vaccine))
      
      max_gain <- min(data)
      if(max_gain < 1){ # Stop vaccination campaign if no more vaccination is needed
        index_district_vaccine <- which(data == max_gain, arr.ind = T)[1,] # In case of equality, pick first
        index_district <- data_district_lisa_sf$district[index_district_vaccine[1]]
        index_region <- data_district_lisa_sf$region[index_district_vaccine[1]]
        
        ## Perform vaccination in identified district
        vaccination_campaign_loop[index_district_vaccine[1], vaccine] <- 1
        
        ## Derive all other vaccination metrics
        # Identify variables to be updated
        variables_containing_identified_vaccine_0 <- grep(pattern = vaccine, x = colnames(vaccination_campaign_loop), value = T)
        variables_containing_identified_vaccine_1 <- grep(pattern = "_", x = variables_containing_identified_vaccine_0, value = T) # Combination of vaccines only
        variables_containing_identified_vaccine <- grep(pattern = "Only", x = variables_containing_identified_vaccine_1, value = T, invert = T) # Combination of vaccines only
        
        # Identify variables to update from
        variables_containing_identified_vaccine_updated_0 <- gsub(pattern = vaccine, replacement = "", x = variables_containing_identified_vaccine)
        variables_containing_identified_vaccine_updated_1 <- gsub(pattern = "__", replacement = "_", x = variables_containing_identified_vaccine_updated_0)
        variables_containing_identified_vaccine_updated <- gsub(pattern = "^_|_$", replacement = "", x = variables_containing_identified_vaccine_updated_1)
        
        # Update
        fic_before_vaccination <- vaccination_campaign_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"]
        vaccination_campaign_loop[index_district_vaccine[1], variables_containing_identified_vaccine] <- vaccination_campaign_loop[index_district_vaccine[1], variables_containing_identified_vaccine_updated]
        fic_after_vaccination <- vaccination_campaign_loop[index_district_vaccine[1], "BCG_Polio_DTP_MCV"]
        fic_gain <- fic_after_vaccination - fic_before_vaccination
        
        # Store results
        Immunization_Doses <- c(Immunization_Doses, vaccination_campaign_loop[index_district_vaccine[1], "Population"])
        FIC_Gain <- c(FIC_Gain, fic_gain*vaccination_campaign_loop[index_district_vaccine[1], "Population"])
        Region <- c(Region, index_region)
        District <- c(District, index_district)
        Vaccine <- c(Vaccine, vaccine)
      }
     
    }
  }
  vaccination_campaign <- (tibble(Immunization_Doses, FIC_Gain, Region, District, Vaccine)
                           %>% mutate(Cummulative_Immunization_Doses = cumsum(Immunization_Doses),
                                      Percent_FIC = (total_population_fic + cumsum(FIC_Gain))/total_population))
  return(vaccination_campaign)
}


## This function computes the concentration index as a vaccination campaigns unfolds
concentration_index_function <- function(vaccination_campaign){
  concentration_index <- NULL
  data <- (data_district_lisa_sf 
           %>% mutate(N_FIC = (Wealth_1_FIC + Wealth_2_FIC + Wealth_3_FIC + Wealth_4_FIC + Wealth_5_FIC))
           )
  for(i in 1:nrow(vaccination_campaign)){
    # Compute concentration index
    attach(data)
    cummulative_proportion_wealth_1 <- sum(Wealth_1*Population/N)/sum(Population)
    cummulative_proportion_wealth_2 <- sum((Wealth_1 + Wealth_2)*Population/N)/sum(Population)
    cummulative_proportion_wealth_3 <- sum((Wealth_1 + Wealth_2 + Wealth_3)*Population/N)/sum(Population)
    cummulative_proportion_wealth_4 <- sum((Wealth_1 + Wealth_2 + Wealth_3 + Wealth_4)*Population/N)/sum(Population)
    cummulative_proportion_wealth_5 <- sum((Wealth_1 + Wealth_2 + Wealth_3 + Wealth_4 + Wealth_5)*Population/N)/sum(Population)
    cummulative_proportion_wealth_1_fic <- sum(Wealth_1_FIC*Vaccinated_FIC/N_FIC)/sum(Vaccinated_FIC)
    cummulative_proportion_wealth_2_fic <- sum((Wealth_1_FIC + Wealth_2_FIC)*Vaccinated_FIC/N_FIC)/sum(Vaccinated_FIC)
    cummulative_proportion_wealth_3_fic <- sum((Wealth_1_FIC + Wealth_2_FIC + Wealth_3_FIC)*Vaccinated_FIC/N_FIC)/sum(Vaccinated_FIC)
    cummulative_proportion_wealth_4_fic <- sum((Wealth_1_FIC + Wealth_2_FIC + Wealth_3_FIC + Wealth_4_FIC)*Vaccinated_FIC/N_FIC)/sum(Vaccinated_FIC)
    cummulative_proportion_wealth_5_fic <- sum((Wealth_1_FIC + Wealth_2_FIC + Wealth_3_FIC + Wealth_4_FIC + Wealth_5_FIC)*Vaccinated_FIC/N_FIC)/sum(Vaccinated_FIC)
    concentration_index[i] <- ((cummulative_proportion_wealth_1*cummulative_proportion_wealth_2_fic - cummulative_proportion_wealth_2*cummulative_proportion_wealth_1_fic)
                               + (cummulative_proportion_wealth_2*cummulative_proportion_wealth_3_fic - cummulative_proportion_wealth_3*cummulative_proportion_wealth_2_fic)
                               + (cummulative_proportion_wealth_3*cummulative_proportion_wealth_4_fic - cummulative_proportion_wealth_4*cummulative_proportion_wealth_3_fic)
                               + (cummulative_proportion_wealth_4*cummulative_proportion_wealth_5_fic - cummulative_proportion_wealth_5*cummulative_proportion_wealth_4_fic))
    detach(data)
    
    # Perform step in proposed vaccination campaign
    data$Wealth_1_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_1[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_2_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_2[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_3_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_3[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_4_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_4[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_5_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_5[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$N_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$N[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Vaccinated_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Population[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
  }
  
  # Return concentration index vector
  return(concentration_index)
}


rot_concentration_index_function <- function(vaccination_campaign){
  concentration_index <- NULL
  data <- (data_district_lisa_sf 
           %>% mutate(N_Rot = (Wealth_1_Rot + Wealth_2_Rot + Wealth_3_Rot + Wealth_4_Rot + Wealth_5_Rot),
                      Vaccinated_Rot = Population*Rotavirus)
  )
  for(i in 1:nrow(vaccination_campaign)){
    # Compute concentration index
    attach(data)
    cummulative_proportion_wealth_1 <- sum(Wealth_1*Population/N)/sum(Population)
    cummulative_proportion_wealth_2 <- sum((Wealth_1 + Wealth_2)*Population/N)/sum(Population)
    cummulative_proportion_wealth_3 <- sum((Wealth_1 + Wealth_2 + Wealth_3)*Population/N)/sum(Population)
    cummulative_proportion_wealth_4 <- sum((Wealth_1 + Wealth_2 + Wealth_3 + Wealth_4)*Population/N)/sum(Population)
    cummulative_proportion_wealth_5 <- sum((Wealth_1 + Wealth_2 + Wealth_3 + Wealth_4 + Wealth_5)*Population/N)/sum(Population)
    cummulative_proportion_wealth_1_rot <- sum(Wealth_1_Rot*Vaccinated_Rot/N_Rot)/sum(Vaccinated_Rot)
    cummulative_proportion_wealth_2_rot <- sum((Wealth_1_Rot + Wealth_2_Rot)*Vaccinated_Rot/N_Rot)/sum(Vaccinated_Rot)
    cummulative_proportion_wealth_3_rot <- sum((Wealth_1_Rot + Wealth_2_Rot + Wealth_3_Rot)*Vaccinated_Rot/N_Rot)/sum(Vaccinated_Rot)
    cummulative_proportion_wealth_4_rot <- sum((Wealth_1_Rot + Wealth_2_Rot + Wealth_3_Rot + Wealth_4_Rot)*Vaccinated_Rot/N_Rot)/sum(Vaccinated_Rot)
    cummulative_proportion_wealth_5_rot <- sum((Wealth_1_Rot + Wealth_2_Rot + Wealth_3_Rot + Wealth_4_Rot + Wealth_5_Rot)*Vaccinated_Rot/N_Rot)/sum(Vaccinated_Rot)
    concentration_index[i] <- ((cummulative_proportion_wealth_1*cummulative_proportion_wealth_2_rot - cummulative_proportion_wealth_2*cummulative_proportion_wealth_1_rot)
                               + (cummulative_proportion_wealth_2*cummulative_proportion_wealth_3_rot - cummulative_proportion_wealth_3*cummulative_proportion_wealth_2_rot)
                               + (cummulative_proportion_wealth_3*cummulative_proportion_wealth_4_rot - cummulative_proportion_wealth_4*cummulative_proportion_wealth_3_rot)
                               + (cummulative_proportion_wealth_4*cummulative_proportion_wealth_5_rot - cummulative_proportion_wealth_5*cummulative_proportion_wealth_4_rot))
    detach(data)
    
    # Perform step in proposed vaccination campaign
    data$Wealth_1_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_1[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_2_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_2[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_3_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_3[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_4_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_4[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Wealth_5_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Wealth_5[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$N_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$N[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$Vaccinated_Rot[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Population[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
  }
  
  # Return concentration index vector
  return(concentration_index)
}


## This function computes the theil index as a vaccination campaigns unfolds

# Theil index component for each state
theil_index_state_function <- function(data, state){
  data_state <- data %>% filter(State == state)
  proportion_population_state <- sum(data_state$Population)/sum(data$Population)
  proportion_fic <- sum(data$Vaccinated_FIC)/sum(data$Population)
  proportion_fic_state <- sum(data_state$Vaccinated_FIC)/sum(data_state$Population)
  return((proportion_population_state*(proportion_fic_state/proportion_fic)*log(proportion_fic_state/proportion_fic)))
}

theil_index_function <- function(vaccination_campaign){
  theil_index <- NULL
  data <- data_district_lisa_sf
  for(i in 1:nrow(vaccination_campaign)){
    # Sum over theil index for all districts
    # theil_index[i] <- sum((data$Population/sum(data$Population))*(data$FIC/(sum(data$Vaccinated_FIC)/sum(data$Population)))*log((data$FIC/(sum(data$Vaccinated_FIC)/sum(data$Population)))))
    
    # Sum over theil index for all states
    theil_index[i] <- do.call(sum,
                              lapply(X = unique(data$State),
                                     FUN = function(x){theil_index_state_function(data = data,
                                                                                  state = x)}))
    
    # Perform step in proposed vaccination campaign
    data$Vaccinated_FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- data$Population[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])]
    data$FIC[which(data$District == vaccination_campaign$District[i] & data$State == vaccination_campaign$State[i])] <- 1
  }
  
  # Return concentration index vector
  return(theil_index)
}


#### These functions produce nice graphs for pair plot (credit to Dr Benjamin Arnold)
## lower panel function
myellipse<-function(x,y,...){
  maxx <- max(x,na.rm=TRUE)
  minx <- min(x,na.rm=TRUE)
  maxy <- max(y,na.rm=TRUE)
  miny <- min(y,na.rm=TRUE)
  midx <- (maxx+minx)/2
  midy <- (maxy+miny)/2
  corxy <- cor(x,y,method="pearson")
  xyc <-sprintf("%1.2f",corxy)
  xyc[grep("NA",xyc)]<-""
  exy <- ellipse(corxy,centre=c(midx,midy),scale=c((maxx-minx)/6,(maxy-miny)/6))
  polygon(exy,col=alpha(cols[abs(corr-corxy)<0.0000001][1],alpha=1))
  lines(exy)
  if(!is.na(corxy)) {
    if(corxy<0.69) {
      text(midx,midy,xyc,cex=1.1, col = "white")
    } else{
      text(maxx,midy-((maxy-miny)/3),xyc,cex=1.1,adj=1)
    }
  }
  
}

## upper panel function
scatterlowess<-function(x,y,...){
  points(x,y,pch=19,cex=0.5,col=alpha('black',alpha=0.3))
  # lines(lowess(y~x),col=brewcols[7])
  # drop the first and last obs to reduce edge effects
  # x and y objects from lowess are sorted by x
  lfit <- lowess(y~x)
  lines(lfit$x[-c(1,length(lfit$x))],lfit$y[-c(1,length(lfit$x))],col=brewcols[7],lwd=1.5)
  }
  
