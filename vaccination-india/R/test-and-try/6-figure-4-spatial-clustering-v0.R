#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code produces figure 5

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
vaccination_campaign_1_state <- readRDS(file = here("data/final","vaccination_campaign_1_state"))
vaccination_campaign_1cs_state <- readRDS(file = here("data/final","vaccination_campaign_1cs_state"))
vaccination_campaign_1cs_threshold_state <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state"))
vaccination_campaign_1cs_threshold_state_bis <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state_bis"))

vaccination_campaign_1_km <- readRDS(file = here("data/final","vaccination_campaign_1_km"))
vaccination_campaign_1cs_state_km <- readRDS(file = here("data/final","vaccination_campaign_1cs_state_km"))
vaccination_campaign_1cs_threshold_state_km <- readRDS(file = here("data/final","vaccination_campaign_1cs_threshold_state_km"))

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
            # + geom_point(data = vaccination_campaign_1cs_state_bis %>% mutate(Strategy = "Under-vaccinated district\nwithin state targeting\n(Clustering method 80%)"),
            #              aes(x = Cummulative_Immunization_Doses/4/1000000,
            #                  y = 100*Percent_FIC,
            #                  col = Strategy), cex = 0.5)
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

# Derive differential dataset for differences in fic gains obtained from vaccination campaigns
fic_target <- seq(from = 80, to = 95, by = 5)
fic_all <- NULL
fic_clust <- NULL
fic_threshold <- NULL
for(i in 1:length(fic_target)){
  fic_all[i] = vaccination_campaign_1_state$Cummulative_Immunization_Doses[which.min(abs(100*vaccination_campaign_1_state$Percent_FIC - fic_target[i]))]
  fic_clust[i] = vaccination_campaign_1cs_state$Cummulative_Immunization_Doses[which.min(abs(100*vaccination_campaign_1cs_state$Percent_FIC - fic_target[i]))]
  fic_threshold[i] = vaccination_campaign_1cs_threshold_state$Cummulative_Immunization_Doses[which.min(abs(100*vaccination_campaign_1cs_threshold_state$Percent_FIC - fic_target[i]))]
}

panel_a_diff <- (ggplot()
                 + geom_point(data = (tibble(fic_target,
                                           Diff = fic_clust - fic_all) 
                                      %>% filter(fic_target <= 100*max(vaccination_campaign_1cs_state$Percent_FIC) )
                                      %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Cluster method)")),
                              aes(x = fic_target,
                                  y = Diff/4/1000000,
                                  col = Strategy))
                 + geom_point(data = (tibble(fic_target,
                                             Diff = fic_threshold - fic_all) 
                                      %>% filter(fic_target <= 100*max(vaccination_campaign_1cs_threshold_state$Percent_FIC) )
                                      %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)")),
                              aes(x = fic_target,
                                  y = Diff/4/1000000,
                                  col = Strategy))
                 + scale_color_manual(values = viridis(4)[3:4])
                 + guides(color = guide_legend(override.aes = list(size = 5)))
                 + ylab("Difference in population targeted (million)")
                 + xlab("Full immunization coverage (%)")
                 + theme_classic()
                 + theme(legend.title = element_blank(),
                         axis.text = element_text(size = 10),
                         axis.title = element_text(size = 12))
)
panel_a_diff


population_targeted_million <- seq(from = 0, to = 30, by = 0.5)
fic_all <- NULL
fic_clust <- NULL
fic_threshold <- NULL
for(i in 1:length(population_targeted_million)){
  fic_all[i] = vaccination_campaign_1_state$Percent_FIC[which.min(abs(vaccination_campaign_1_state$Cummulative_Immunization_Doses/4/1000000 - population_targeted_million[i]))]
  fic_clust[i] = vaccination_campaign_1cs_state$Percent_FIC[which.min(abs(vaccination_campaign_1cs_state$Cummulative_Immunization_Doses/4/1000000 - population_targeted_million[i]))]
  fic_threshold[i] = vaccination_campaign_1cs_threshold_state$Percent_FIC[which.min(abs(vaccination_campaign_1cs_threshold_state$Cummulative_Immunization_Doses/4/1000000 - population_targeted_million[i]))]
}

panel_a_diff <- (ggplot()
                 + geom_point(data = (tibble(population_targeted_million,
                                             Diff = fic_clust - fic_all) 
                                      %>% filter(population_targeted_million <= max(vaccination_campaign_1cs_state$Cummulative_Immunization_Doses/4/1000000) )
                                      %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Cluster method)")),
                              aes(x = population_targeted_million,
                                  y = Diff,
                                  col = Strategy))
                 + geom_point(data = (tibble(population_targeted_million,
                                             Diff = fic_threshold - fic_all) 
                                      %>% filter(population_targeted_million <= max(vaccination_campaign_1cs_threshold_state$Cummulative_Immunization_Doses/4/1000000) )
                                      %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)")),
                              aes(x = population_targeted_million,
                                  y = Diff,
                                  col = Strategy))
)
panel_a_diff


panel_a_rot <- (ggplot() 
            + geom_point(data = (vaccination_campaign_1_state 
                                 %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)")),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_Rotavirus,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_Rotavirus,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = 100*Percent_Rotavirus,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", viridis(4)[3:4]))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Rotavirus coverage(%)")
            + xlab("Population targeted (million)")
            + theme_classic()
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))
            
)
panel_a_rot

panel_a_vita <- (ggplot() 
                + geom_point(data = (vaccination_campaign_1_state 
                                     %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)")),
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Vitamin_A,
                                 col = Strategy), cex = 0.5)
                + geom_point(data = vaccination_campaign_1cs_threshold_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Vitamin_A,
                                 col = Strategy), cex = 0.5)
                + geom_point(data = vaccination_campaign_1cs_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                             aes(x = Cummulative_Immunization_Doses/4/1000000,
                                 y = 100*Percent_Vitamin_A,
                                 col = Strategy), cex = 0.5)
                + scale_color_manual(values = c("black", viridis(4)[3:4]))
                + guides(color = guide_legend(override.aes = list(size = 5)))
                + ylab("Rotavirus coverage(%)")
                + xlab("Population targeted (million)")
                + theme_classic()
                + theme(legend.title = element_blank(),
                        axis.text = element_text(size = 10),
                        axis.title = element_text(size = 12))
                
)
panel_a_vita

panel_a_ter <- (ggplot() 
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
                # + geom_point(data = vaccination_campaign_1cs_threshold_state,
                #              aes(x = Cummulative_Immunization_Doses/4/1000000,
                #                  y = 100*Percent_DTP,
                #                  col = "DTP"), cex = 0.5)
                # + geom_point(data = vaccination_campaign_1cs_threshold_state,
                #              aes(x = Cummulative_Immunization_Doses/4/1000000,
                #                  y = 100*Percent_BCG,
                #                  col = "BCG"), cex = 0.5)
                # + geom_point(data = vaccination_campaign_1cs_threshold_state,
                #              aes(x = Cummulative_Immunization_Doses/4/1000000,
                #                  y = 100*Percent_MCV,
                #                  col = "MCV"), cex = 0.5)
                # + geom_point(data = vaccination_campaign_1cs_threshold_state,
                #              aes(x = Cummulative_Immunization_Doses/4/1000000,
                #                  y = 100*Percent_Polio,
                #                  col = "Polio"), cex = 0.5)
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
panel_a_ter


panel_b <- (ggplot() 
            + geom_point(data = vaccination_campaign_1_km %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state_km %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state_km %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = 100*Percent_FIC,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", viridis(4)[3:4]))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Full immunization coverage (%)")
            + xlab("Number of child*immunization*distance (million*km)")
            + theme_classic()
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))
            
)
panel_b

panel_c <- (ggplot() 
            + geom_point(data = vaccination_campaign_1_state %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", viridis(4)[3:4]))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Concentration index\nFull immunization coverage")
            + xlab("Population targeted (million)")
            + theme_classic()
            + geom_hline(yintercept = 0, linetype = "dotted", col = "darkgrey")
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))
            
)
panel_c

panel_c_rot <- (ggplot() 
            + geom_point(data = vaccination_campaign_1_state %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index_Rot,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index_Rot,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                         aes(x = Cummulative_Immunization_Doses/4/1000000,
                             y = Concentration_Index_Rot,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", viridis(4)[3:4]))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Concentration index\nRotavirus")
            + xlab("Population targeted (million)")
            + theme_classic()
            + geom_hline(yintercept = 0, linetype = "dotted", col = "darkgrey")
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))
            
)
panel_c_rot

panel_d <- (ggplot() 
            + geom_point(data = vaccination_campaign_1_km %>% mutate(Strategy = "All district\nwithin state targeting\n(districts ranked by fic)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_state_km %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Clustering method)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + geom_point(data = vaccination_campaign_1cs_threshold_state_km %>% mutate(Strategy = "Undervaccinated district\nwithin state targeting\n(Threshold method)"),
                         aes(x = as.numeric(Cummulative_Immunization_Doses/4/(1000000*1000)),
                             y = Concentration_Index,
                             col = Strategy), cex = 0.5)
            + scale_color_manual(values = c("black", viridis(4)[3:4]))
            + guides(color = guide_legend(override.aes = list(size = 5)))
            + ylab("Concentration index")
            + xlab("Number of child*immunization*distance (million*km)")
            + theme_classic()
            + geom_hline(yintercept = 0, linetype = "dotted", col = "darkgrey")
            + theme(legend.title = element_blank(),
                    axis.text = element_text(size = 10),
                    axis.title = element_text(size = 12))
            
)
panel_d

figure <- ggarrange(panel_a, panel_b,
                    panel_c, panel_d,
                    nrow = 2, ncol = 2,
                    common.legend = T,
                    legend = "top")

figure <- ggarrange(panel_a, NULL,
                    NULL, NULL,
                    nrow = 2, ncol = 2,
                    common.legend = T,
                    legend = "top")

figure <- ggarrange(panel_a, panel_c,
                    panel_a_rot, panel_c_rot,
                    nrow = 2, ncol = 2,
                    common.legend = T,
                    legend = "top")

figure

### Save
pdf(here("vaccination-india/output/figures", "figure-4.pdf"))
figure
dev.off()

pdf(here("vaccination-india/output/figures", "figure-6.pdf"))
ggarrange(panel_a_ter, NULL,
          NULL, NULL,
          nrow = 2, ncol = 2,
          common.legend = F,
          legend = "top")
dev.off()
