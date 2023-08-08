#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code plots figure showing integrated vs optimal vs sequential vaccination campaigns

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_lisa_sf <- readRDS(file = here("data/final",
                                             "data_district_lisa_sf"))

# Vaccination campaigns
vaccination_campaign_1 <- readRDS(file = here("data/final","vaccination_campaign_1"))
vaccination_campaign_3 <- readRDS(file = here("data/final","vaccination_campaign_3"))
vaccination_campaign_4 <- readRDS(file = here("data/final","vaccination_campaign_4"))
vaccination_campaign_5 <- readRDS(file = here("data/final","vaccination_campaign_5"))
vaccination_campaign_6 <- readRDS(file = here("data/final","vaccination_campaign_6"))
vaccination_campaign_7 <- readRDS(file = here("data/final","vaccination_campaign_7"))
vaccination_campaign_8 <- readRDS(file = here("data/final","vaccination_campaign_8"))
vaccination_campaign_9 <- readRDS(file = here("data/final","vaccination_campaign_9"))
vaccination_campaign_10 <- readRDS(file = here("data/final","vaccination_campaign_10"))
vaccination_campaign_11 <- readRDS(file = here("data/final","vaccination_campaign_11"))
vaccination_campaign_12 <- readRDS(file = here("data/final","vaccination_campaign_12"))
vaccination_campaign_13 <- readRDS(file = here("data/final","vaccination_campaign_13"))
vaccination_campaign_14 <- readRDS(file = here("data/final","vaccination_campaign_14"))
vaccination_campaign_15 <- readRDS(file = here("data/final","vaccination_campaign_15"))
vaccination_campaign_16 <- readRDS(file = here("data/final","vaccination_campaign_16"))
vaccination_campaign_17 <- readRDS(file = here("data/final","vaccination_campaign_17"))
vaccination_campaign_18 <- readRDS(file = here("data/final","vaccination_campaign_18"))
vaccination_campaign_19 <- readRDS(file = here("data/final","vaccination_campaign_19"))
vaccination_campaign_20 <- readRDS(file = here("data/final","vaccination_campaign_20"))
vaccination_campaign_21 <- readRDS(file = here("data/final","vaccination_campaign_21"))
vaccination_campaign_22 <- readRDS(file = here("data/final","vaccination_campaign_22"))
vaccination_campaign_23 <- readRDS(file = here("data/final","vaccination_campaign_23"))
vaccination_campaign_24 <- readRDS(file = here("data/final","vaccination_campaign_24"))
vaccination_campaign_25 <- readRDS(file = here("data/final","vaccination_campaign_25"))
vaccination_campaign_26 <- readRDS(file = here("data/final","vaccination_campaign_26"))
vaccination_campaign_27 <- readRDS(file = here("data/final","vaccination_campaign_27"))
#-------------------------------------------------------------------------------



# Plot results
cex_a = 0.3
alpha_a = 5
panel_a <- (ggplot() 
  + geom_point(data = vaccination_campaign_3,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a)
  + geom_point(data = vaccination_campaign_1 %>% mutate(Vaccine = "All 4"),
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = 0.1)
  + geom_point(data = vaccination_campaign_4,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_5,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_6,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_7,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_8,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_9,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_10,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_11,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_12,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_13,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_14,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_15,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_16,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_17,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_18,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_19,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_20,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_21,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_22,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_23,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_24,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_25,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_26,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  + geom_point(data = vaccination_campaign_27,
               aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex_a, alpha = alpha_a)
  # + scale_color_viridis(discrete = T)
  + scale_color_manual(breaks = c("BCG", "DTP", "MCV", "Polio", "All 4"),
                       values = c("#E69F00", viridis(4)[2:4], "black"))
  + guides(color = guide_legend(override.aes = list(size = 4)))
  + ylab("Full immunization coverage (%)")
  + xlab("Number of child*immunization (million)")
  + theme_classic()
  + theme(legend.title = element_blank(),
          legend.position = c(0.9, 0.3),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10))
  + annotate("rect", xmin = -0.1*34, xmax = 1.35*34, ymin = 78, ymax = 87,
             alpha = 0, col = "darkgrey", fill = "transparent", linetype = "dotted", linewidth = 1)
  + annotate(geom = "text", x = 0, y = 86.2, label = "b", fontface = "bold", col = "black")
  + annotate("rect", xmin = 1.15*34, xmax = 4.4*34, ymin = 85, ymax = 97,
             alpha = 0, col = "darkgrey", fill = "transparent", linetype = "dotted", linewidth = 1)
  + annotate(geom = "text", x = 1.25*34, y = 96.2, label = "c", fontface = "bold", col = "black")
  + annotate(geom = "text", x = 0, y = 100, label = "a", fontface = "bold", col = "black")
  
)

cex = 0.75
alpha = 1
cex_light = 0.2
alpha_light = 0.2
panel_b <- (ggplot() 
            + geom_point(data = vaccination_campaign_3,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_1 %>% mutate(Vaccine = "All 4"),
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_4,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_5,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_6,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_7,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_8,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_9,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_10,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_11,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_12,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_13,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_14,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_15,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_16,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_17,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_18,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_19,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_20,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_21,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_22,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_23,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_24,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_25,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_26,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            + geom_point(data = vaccination_campaign_27,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = cex, alpha = alpha)
            # + scale_color_viridis(discrete = T)
            + scale_color_manual(breaks = c("BCG", "DTP", "MCV", "Polio", "All 4"),
                                 values = c("#E69F00", viridis(4)[2:4], "black"))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Full immunization coverage (%)")
            + xlab("Number of child*immunization (million)")
            + theme_classic()
            + theme(legend.position = "none",
                    axis.text = element_text(size = 8),
                    axis.title = element_text(size = 10))
            + xlim(c(0, 1.35*34))
            + ylim(c(78, 86))
            + annotate(geom = "text", x = 0.05*34, y = 85.8, label = "b", fontface = "bold", col = "black")
            
)

panel_c <- (ggplot() 
            + geom_point(data = vaccination_campaign_3,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_1 %>% mutate(Vaccine = "All 4"),
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_5,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_7,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_8,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_9,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_10,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_11,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_12,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_13,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_14,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_15,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_16,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_17,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_18,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_19,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_20,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_21,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_22,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_23,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_24,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_25,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_26,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_27,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC), col = "lightgrey", cex = cex_light, alpha = alpha_light)
            + geom_point(data = vaccination_campaign_4,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = 1, alpha = alpha)
            + geom_point(data = vaccination_campaign_6,
                         aes(x = Cummulative_Immunization_Doses/1000000, y = 100*Percent_FIC, col = Vaccine), cex = 1, alpha = alpha)
            # + scale_color_viridis(discrete = T)
            + scale_color_manual(breaks = c("BCG", "DTP", "MCV", "Polio", "All 4"),
                                 values = c("#E69F00", viridis(4)[2:4], "black"))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Full immunization coverage (%)")
            + xlab("Number of child*immunization (million)")
            + theme_classic()
            + theme(legend.position = "none",
                    axis.text = element_text(size = 8),
                    axis.title = element_text(size = 10))
            + xlim(c(1.15*34, 4.4*34))
            + ylim(c(85, 97.3))
            + annotate(geom = "text", x = 1.25*34, y = 97.1, label = "c", fontface = "bold", col = "black")
            
)
panel_c

figure <- ggarrange(panel_a,
                    ggarrange(panel_b, panel_c, ncol = 2, nrow = 1),
                    ncol = 1, nrow = 2,
                    heights = c(2,1))



### Save
pdf(here("vaccination-india/output/figures", "figure-3-integrated-campaigns.pdf"))
figure
dev.off()

