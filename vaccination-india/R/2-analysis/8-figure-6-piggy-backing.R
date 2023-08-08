#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code produces figure 6 illustrating benefits of additionnaly
# integrating vitamin A, deworming or rotavirus to fic-targeted strategies

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
# Vaccination campaigns
vaccination_campaign_1cs_threshold_state <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state"))

#-------------------------------------------------------------------------------

panel_a <- (ggplot() 
                + geom_point(data = vaccination_campaign_1cs_threshold_state,
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Vitamin_A,
                                 col = "Vitamin A"), cex = 0.5)
                + geom_point(data = vaccination_campaign_1cs_threshold_state,
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Rotavirus,
                                 col = "Rotavirus"), cex = 0.5)
                + geom_point(data = vaccination_campaign_1cs_threshold_state,
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Deworming,
                                 col = "Deworming"), cex = 0.5)
                + geom_point(data = vaccination_campaign_1cs_threshold_state,
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_FIC,
                                 col = "FIC"), cex = 0.5)
                + scale_color_manual(breaks = c("FIC","Vitamin A", "Deworming", "Rotavirus"),
                                     values = c("black", viridis(4)[4:2]))
                + guides(color = guide_legend(override.aes = list(size = 5)))
                + ylab("Coverage(%)")
                + xlab("Population targeted (million)")
                + theme_classic()
                + theme(legend.title = element_blank(),
                        axis.text = element_text(size = 10),
                        axis.title = element_text(size = 12))
                
)
panel_a

### Save
pdf(here("vaccination-india/output/figures", "figure-6-piggy-backing.pdf"))
ggarrange(panel_a, NULL,
          NULL, NULL,
          nrow = 2, ncol = 2,
          common.legend = F,
          legend = "top")
dev.off()
