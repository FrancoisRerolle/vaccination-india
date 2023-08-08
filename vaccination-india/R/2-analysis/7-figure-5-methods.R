#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots figure 5 illustrating threshold vs cluster method

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_lisa_sf <- readRDS(file = here("data/final",
                                             "data_district_lisa_sf"))

## Plot
map_fic <- (ggplot(data_district_lisa_sf)
            + geom_sf(aes(fill=FIC), col="transparent")
            + scale_fill_continuous(type="viridis", direction=1, 
                                    labels = scales::percent_format(accuracy = 1),  
                                    name = "Full immunization\ncoverage",
                                    na.value = "darkgray", limits=c(0,1))
            + theme_void()
            + theme(legend.position = "right")
            )

map_fic_undervaccination_districts <- (ggplot(data_district_lisa_sf 
                                              %>% mutate(`Undervaccinated\ndistrict` = FIC_LISA <= -1.65))
                                       + geom_sf(aes(fill=`Undervaccinated\ndistrict`),
                                                 col="transparent")
                                       + scale_fill_viridis(discrete = T, direction = -1,
                                                            breaks = c("FALSE", "TRUE"),
                                                            labels = c("No", "Yes"))
                                       + theme_void()
                                       )

map_fic_undervaccination_districts_threshold <- (ggplot(data_district_lisa_sf 
                                              %>% mutate(`Undervaccinated\ndistrict` = FIC <= 0.9))
                                       + geom_sf(aes(fill=`Undervaccinated\ndistrict`),
                                                 col="transparent")
                                       + scale_fill_viridis(discrete = T, direction = -1,
                                                            breaks = c("FALSE", "TRUE"),
                                                            labels = c("No", "Yes"))
                                       + theme_void()
)


distribution_fic_undervaccination_districts <- (ggplot(data = data_district_lisa_sf 
                                                       %>% mutate(`Undervaccinated\ndistrict` = FIC_LISA <= -1.65))
                                                + geom_density(aes(x = 100*FIC, fill = `Undervaccinated\ndistrict`, y = ..count..),
                                                               alpha = 0.7,
                                                               col = "transparent")
                                                # + geom_vline(aes(xintercept = 80))
                                                + xlim(c(40,100))
                                                # + ylim(c(0,0.045))
                                                + scale_fill_viridis(discrete = T, direction = -1)
                                                + xlab("Full immunization coverage (%)")
                                                + ylab("Density")
                                                + theme_classic()
                                                + theme(legend.position = "none",
                                                        axis.text.y = element_blank(),
                                                        axis.ticks.y = element_blank())
                                                # + annotate(geom = "text", label = "80 %", x = 81, y = 0.041, angle = 45, size = 2.5)
)


x <- 100*data_district_lisa_sf$FIC
x.dens <- density(x)
df.dens <- data.frame(x = x.dens$x, y = x.dens$y)
distribution_fic_undervaccination_districts_threshold <- (ggplot(data = data_district_lisa_sf)
                                                + geom_density(aes(x = 100*FIC),
                                                               col = "transparent")
                                                + geom_area(data = subset(df.dens, x <= 90), 
                                                            aes(x=x,y=y), fill = viridis(4)[1], alpha = 0.7)
                                                + geom_area(data = subset(df.dens, x > 90), 
                                                            aes(x=x,y=y), fill = viridis(4)[4], alpha = 0.7)
                                                + geom_vline(aes(xintercept = 90))
                                                + xlim(c(40,100))
                                                + ylim(c(0,0.045))
                                                + xlab("Full immunization coverage (%)")
                                                + ylab("Density")
                                                + theme_classic()
                                                + theme(legend.position = "none",
                                                        axis.text.y = element_blank(),
                                                        axis.ticks.y = element_blank())
                                                + annotate(geom = "text", label = "90 %", x = 93.5, y = 0.041, angle = 45, size = 2.5)
)




figure <- ggarrange(ggarrange(NULL, map_fic, NULL,
                              nrow = 1, ncol = 3,
                              widths = c(0.5, 1, 0.3)),
                    ggarrange(NULL, map_fic_undervaccination_districts, map_fic_undervaccination_districts_threshold, NULL,
                              ncol = 4, nrow = 1,
                              widths = c(0.5, 1, 1, 0.5),
                              common.legend = T,
                              legend = "top"),
                    ggarrange(NULL, distribution_fic_undervaccination_districts, distribution_fic_undervaccination_districts_threshold, NULL,
                              ncol = 4, nrow = 1,
                              widths = c(0.5, 1, 1, 0.5),
                              common.legend = T,
                              legend = "none"),
                    nrow = 3, ncol = 1,
                    heights = c(1,1,0.5)
                    
                    )

figure

### Save
pdf(here("vaccination-india/output/figures", "figure-5-spatial-targeting-methods.pdf"))
figure
dev.off()

