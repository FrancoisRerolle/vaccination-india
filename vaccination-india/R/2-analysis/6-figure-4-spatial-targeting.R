#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code produces plot for figure 4 illustrating spatial targeting

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))

#-------------------------------------------------------------------------------

## Load data
# Vaccination campaigns
vaccination_campaign_1_state <- readRDS(file = here("data/final","vaccination_campaign_1_state"))
vaccination_campaign_1cs_state <- readRDS(file = here("data/final","vaccination_campaign_1cs_state"))
vaccination_campaign_1cs_threshold_state <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state"))
vaccination_campaign_1cs_threshold_state_bis <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state_bis"))

#-------------------------------------------------------------------------------

panel_a <- (ggplot() 
            + geom_point(data = (vaccination_campaign_1_state 
                                 %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)")),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state_bis %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method 80%)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method 90%)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", "#009E73", "#0072B2", "#E69F00", "#CC79A7"))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Full immunization coverage (%)")
            + xlab("Population targeted (million)")
            + theme_classic()
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))

)
panel_a

# Arrange to make room for adjacent table
figure <- ggarrange(panel_a, NULL,
                    NULL, NULL,
                    nrow = 2, ncol = 2,
                    common.legend = T,
                    legend = "top")


### Save
pdf(here("vaccination-india/output/figures", "figure-4-spatial-targeting.pdf"))
figure
dev.off()

