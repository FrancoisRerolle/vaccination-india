#-------------------------------------------------------------------------------

# @Organization - UCSF
# @Project - Integrated vaccination campaigns
# @Author - Francois Rerolle, rerollefrancois@gmail.com
# @Description - This code produce the pair plot

#-------------------------------------------------------------------------------

# Source configuration code
library(here)
source(here("vaccination-india/R", "0-config.R"))
source(here("vaccination-india/R", "0-functions.R"))

#-------------------------------------------------------------------------------

## Load data
data_district_lisa_sf <- readRDS(file = here("data/final",
                                             "data_district_lisa_sf"))

data <- data_district_lisa_sf %>% st_set_geometry(NULL)


## Process data
myvars <- c("Hib", "Hep", "DTP", "MCV", "Polio", "BCG", "VitaminA", "Deworming", "Rotavirus")
mylabs <- c("Hib", "Hep", "DTP", "MCV", "Polio", "BCG", "Vitamin A", "Deworming", "Rotavirus")

data <- (data_district_lisa_sf
         %>% st_set_geometry(NULL)
         %>% dplyr::select(myvars)
         )

# get correlations to make color categories
# (could be a better way to do this)
corrmatrix <-cor(100*data,method="pearson")
corr <-corrmatrix[upper.tri(corrmatrix)]
colgroup <-cut(corr,9,labels=F)
brewcols <- brewer.pal(9,"BuGn")
brewcols <- viridis(9)
cols<-brewcols[colgroup]

### Save
pdf(here("vaccination-india/output/figures", "figure-1-correlation-plot.pdf"))
pairs(100*data,
      labels = mylabs,
      cex = 0.5,
      las = 1,
      upper.panel = scatterlowess,
      lower.panel = myellipse
)
mtext("Coverage (%)",side=1,line=4)
dev.off()

