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
  
