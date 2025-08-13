# Create project structure
dir.create("spatial-capabilities-tutorial")
setwd("spatial-capabilities-tutorial")

# Create subdirectories
dirs <- c("R", "css", "data", "figures", "results")
sapply(dirs, dir.create)


# Install required packages
install.packages(c(
  "tidyverse", "sf", "spdep", "INLA", "BMA", "mice", "VIM",
  "leaflet", "plotly", "DT", "kableExtra", "corrplot", 
  "viridis", "patchwork", "quarto"
))

# Install INLA (requires special repository)
install.packages("INLA", repos=c(getOption("repos"), 
                                 INLA="https://inla.r-inla-download.org/R/stable"), 
                 dep=TRUE)