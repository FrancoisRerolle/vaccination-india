#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code maps undervaccination coverage

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_lisa_sf <- readRDS(file = here("data/final",
                                             "data_district_lisa_sf"))

## Plot data
size_district_line <- 0

map_bcg <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=BCG), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
            )

map_dtp <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=DTP), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),  
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
            )

map_pol <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=Polio), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),  
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
            )

map_mcv <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=MCV), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),  
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
            )

map_vit <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=VitaminA), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),  
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
            )

map_rot <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=Rotavirus), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                   labels = scales::percent_format(accuracy = 1),  
                                   name = "Coverage ",
                                   na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = c(1.5,0.5))
            )

map_dewo <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=Deworming), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                    labels = scales::percent_format(accuracy = 1),  
                                    name = "Coverage ",
                                    na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = c(1.5,0.5))
)

map_hib <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=Hib), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                    labels = scales::percent_format(accuracy = 1),  
                                    name = "Coverage ",
                                    na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
)

map_hep <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=Hep), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                    labels = scales::percent_format(accuracy = 1),  
                                    name = "Coverage ",
                                    na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "none")
)


### Save
pdf(here("vaccination-india/output/figures", "figure-2-vaccine-coverage-maps.pdf"))
ggarrange(map_hib, map_hep, map_dtp,
          map_mcv, map_pol, map_bcg,
          map_vit, map_dewo, map_rot,
          ncol = 3, nrow = 3,
          common.legend = T,
          legend = "top",
          align = "hv",
          labels = c("Hib", "Hep", "DTP", "MCV", "Polio", "BCG", "Vitamin A", "Deworming", "Rotavirus"),
          font.label = list(size = 12, face = "bold"),
          label.x = c(0.45,0.45,0.45,0.45,0.45,0.45,0.3,0.3,0.3),
          vjust = 2.5
)
dev.off()




