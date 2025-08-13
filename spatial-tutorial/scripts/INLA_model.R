####################################################################
## INLA SPATIAL MODELING SCRIPT                                  ##
## For Spatial Capabilities Analysis                             ##
## Based on INLA_HPI_2.R and model preparation scripts          ##
####################################################################

# Load required libraries
library(INLA)
library(tidyverse)
library(sf)
library(spdep)
library(leaflet)
library(viridis)

cat("=== INLA-BYM2 SPATIAL MODELING FOR CAPABILITIES ANALYSIS ===\n")

#==================================================================================
# STEP 1: DATA LOADING AND INITIAL PREPARATION
#==================================================================================

cat("Loading processed spatial data...\n")

# Load your main dataset (after all geocoding and merging)
model_data <- st_read("data/udc_census_full_v2.shp") %>%
  st_as_sf() %>%
  st_make_valid()

cat("Initial dataset: ", nrow(model_data), " census tracts\n")

# Load BMA results to get selected variables
if(file.exists("results/bma_selected_variables.csv")) {
  bma_results <- read_csv("results/bma_selected_variables.csv")
  selected_vars <- bma_results$Variable[bma_results$PIP > 0.50]  # Use moderate+ evidence
  cat("Using BMA-selected variables (PIP > 0.50): ", length(selected_vars), "\n")
  print(selected_vars)
} else {
  # Fallback to your known important variables
  selected_vars <- c("dieslpm", "cnssrsp", "autombl", "insured", "employd", "housrpr")
  cat("Using predefined key variables: ", length(selected_vars), "\n")
}

#==================================================================================
# STEP 2: EXPECTED COUNTS CALCULATION
#==================================================================================

cat("Calculating expected counts and SIR...\n")

# Remove missing correctional population data
model_data <- model_data %>%
  filter(!is.na(corr_pp), tpop > 0)

cat("After filtering: ", nrow(model_data), " census tracts\n")

# Calculate crude rate and expected counts (following your approach)
crude_rate <- sum(model_data$corr_pp * model_data$tpop, na.rm=TRUE) / 
  sum(model_data$tpop, na.rm=TRUE)

model_data <- model_data %>%
  mutate(
    # Expected counts (E) - population offset
    expected = crude_rate * tpop,
    
    # Standardized Incidence Ratio (SIR)
    SIR = corr_pp / expected,
    
    # Confidence intervals for SIR
    SIR_lower = qchisq(0.025, 2 * corr_pp) / (2 * expected),
    SIR_upper = qchisq(0.975, 2 * (corr_pp + 1)) / (2 * expected),
    
    # Sequential ID for INLA
    ID = row_number()
  )

cat("Crude rate: ", round(crude_rate, 6), "\n")
cat("Expected counts calculated\n")

#==================================================================================
# STEP 3: SPATIAL WEIGHTS MATRIX CREATION
#==================================================================================

cat("Creating spatial weights matrix...\n")

# Create neighborhood structure (following your approach)
nb <- poly2nb(model_data, queen = TRUE)

# Summary of neighborhood structure
nb_summary <- summary(nb)
cat("Neighborhood structure summary:\n")
print(nb_summary)

# Check for disconnected areas (islands)
disconnected <- which(card(nb) == 0)
if(length(disconnected) > 0) {
  cat("Warning: Found", length(disconnected), "disconnected areas\n")
  cat("Disconnected tract GEOIDs:", 
      paste(model_data$GEOID[disconnected], collapse = ", "), "\n")
  
  # Handle disconnected areas - connect to nearest neighbor
  coords <- st_coordinates(st_centroid(model_data))
  k_nb <- knn2nb(knearneigh(coords, k = 1))
  
  # Add connections for disconnected areas
  for(i in disconnected) {
    nb[[i]] <- k_nb[[i]]
  }
  
  cat("Connected disconnected areas to nearest neighbors\n")
}

# Convert to INLA format
nb2INLA("model_spatial.adj", nb)
g <- inla.read.graph(filename = "model_spatial.adj")

cat("Spatial structure created and saved\n")

#==================================================================================
# STEP 4: VARIABLE PREPARATION FOR INLA
#==================================================================================

cat("Preparing variables for INLA modeling...\n")

# Create standardized versions of selected variables
model_data_final <- model_data %>%
  mutate(
    # Spatial random effect indices  
    re_u = ID,
    re_v = ID
  )

# Standardize selected variables
for(var in selected_vars) {
  if(var %in% names(model_data_final) && is.numeric(model_data_final[[var]])) {
    std_name <- paste0("z_", var)
    model_data_final[[std_name]] <- as.numeric(scale(model_data_final[[var]]))
    cat("Standardized:", var, "->", std_name, "\n")
  } else {
    cat("Warning: Variable", var, "not found or not numeric\n")
  }
}

# Get list of standardized variables that exist
std_vars <- paste0("z_", selected_vars)
existing_std_vars <- std_vars[std_vars %in% names(model_data_final)]

cat("Final standardized variables for model: ", length(existing_std_vars), "\n")
print(existing_std_vars)

#==================================================================================
# STEP 5: MODEL FORMULA CONSTRUCTION
#==================================================================================

cat("Constructing INLA model formula...\n")

# Build formula string
fixed_effects <- paste(existing_std_vars, collapse = " + ")
spatial_effects <- "f(re_u, model = 'bym2', graph = g, scale.model = TRUE, constr = TRUE)"
unstructured_effects <- "f(re_v, model = 'iid')"

formula_str <- paste("corr_pp ~", fixed_effects, "+", spatial_effects, "+", unstructured_effects)

# Convert to formula object
model_formula <- as.formula(formula_str)

cat("Model formula:\n")
cat(formula_str, "\n")

#==================================================================================
# STEP 6: INLA MODEL FITTING  
#==================================================================================

cat("Fitting INLA-BYM2 model...\n")
cat("This may take several minutes...\n")

# Fit the model
inla_model <- inla(
  formula = model_formula,
  family = "poisson",
  data = model_data_final,
  E = expected,  # Population offset
  control.predictor = list(compute = TRUE),
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE,
    return.marginals.predictor = TRUE
  ),
  control.inla = list(strategy = "adaptive", int.strategy = "eb"),
  verbose = TRUE
)

cat("INLA model fitting completed!\n")

#==================================================================================
# STEP 7: EXTRACT AND PROCESS RESULTS
#==================================================================================

cat("Processing model results...\n")

# Extract fixed effects
fixed_effects <- inla_model$summary.fixed %>%
  rownames_to_column("Variable") %>%
  mutate(
    RR = exp(mean),
    RR_lower = exp(`0.025quant`),
    RR_upper = exp(`0.975quant`),
    Significant = ifelse(`0.025quant` * `0.975quant` > 0, "Yes", "No"),
    Interpretation = case_when(
      RR > 1 ~ paste0("+", round((RR - 1) * 100, 1), "% increase"),
      RR < 1 ~ paste0("-", round((1 - RR) * 100, 1), "% decrease"), 
      TRUE ~ "No effect"
    )
  )

cat("Fixed effects summary:\n")
print(fixed_effects %>% select(Variable, RR, RR_lower, RR_upper, Significant, Interpretation))

# Extract spatial components
if("Phi for re_u" %in% rownames(inla_model$summary.hyperpar)) {
  phi <- inla_model$summary.hyperpar["Phi for re_u", "mean"]
  tau <- inla_model$summary.hyperpar["Precision for re_u", "mean"]
  
  cat("\nSpatial components:\n")
  cat("Phi (mixing parameter):", round(phi, 3), "\n")
  cat("Tau (precision):", round(tau, 3), "\n")
  cat("Spatial variance:", round(1/tau, 3), "\n")
  cat("Percentage structured spatial variation:", round(phi * 100, 1), "%\n")
}

# Extract fitted values and relative risk
fitted_values <- inla_model$summary.fitted.values

model_data_final <- model_data_final %>%
  mutate(
    fitted_mean = fitted_values$mean,
    fitted_lower = fitted_values$`0.025quant`,
    fitted_upper = fitted_values$`0.975quant`,
    RR = fitted_mean,
    RR_lower = fitted_lower,
    RR_upper = fitted_upper
  )

# Extract spatial random effects
if("re_u" %in% names(inla_model$summary.random)) {
  spatial_effects <- inla_model$summary.random$re_u
  model_data_final$spatial_effect <- spatial_effects$mean
  model_data_final$spatial_sd <- spatial_effects$sd
}

#==================================================================================
# STEP 8: MODEL DIAGNOSTICS
#==================================================================================

cat("Running model diagnostics...\n")

# Model fit statistics
cat("\nModel fit statistics:\n")
cat("DIC:", round(inla_model$dic$dic, 2), "\n")
cat("WAIC:", round(inla_model$waic$waic, 2), "\n") 
cat("Marginal log-likelihood:", round(inla_model$mlik[1], 2), "\n")

# Check for outliers using CPO
cpo_values <- inla_model$cpo$cpo
outlier_threshold <- 0.001
outliers <- which(cpo_values < outlier_threshold)

if(length(outliers) > 0) {
  cat("Potential outliers (CPO <", outlier_threshold, "):\n")
  cat("Tract GEOIDs:", paste(model_data_final$GEOID[outliers], collapse = ", "), "\n")
} else {
  cat("No obvious outliers detected\n")
}

# Residual spatial autocorrelation test
observed <- model_data_final$corr_pp
fitted <- model_data_final$fitted_mean
pearson_resid <- (observed - fitted) / sqrt(fitted)

# Test for remaining spatial autocorrelation
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
moran_test <- moran.test(pearson_resid, listw, zero.policy = TRUE)

cat("\nResidual spatial autocorrelation test:\n")
cat("Moran's I:", round(moran_test$statistic, 4), "\n")
cat("P-value:", round(moran_test$p.value, 4), "\n")

if(moran_test$p.value > 0.05) {
  cat("✓ No significant residual spatial autocorrelation\n")
} else {
  cat("⚠ Some residual spatial autocorrelation remains\n")
}

#==================================================================================
# STEP 9: SAVE RESULTS
#==================================================================================

cat("Saving results...\n")

# Save the final dataset with results
st_write(model_data_final, "results/model_results_final.shp", delete_dsn = TRUE)
write_csv(st_drop_geometry(model_data_final), "results/model_results_final.csv")

# Save model object
saveRDS(inla_model, "results/inla_model_final.rds")

# Save results summary
results_summary <- fixed_effects %>%
  select(Variable, Coefficient = mean, SE = sd, RR, RR_lower, RR_upper, 
         Significant, Interpretation)

write_csv(results_summary, "results/model_results_summary.csv")

# Save spatial components summary
spatial_summary <- data.frame(
  Component = c("Structured_Proportion", "Precision", "Spatial_Variance", "DIC", "WAIC"),
  Value = c(
    ifelse(exists("phi"), phi, NA),
    ifelse(exists("tau"), tau, NA), 
    ifelse(exists("tau"), 1/tau, NA),
    inla_model$dic$dic,
    inla_model$waic$waic
  )
)

write_csv(spatial_summary, "results/spatial_components_summary.csv")

# Save diagnostics
diagnostics <- data.frame(
  Metric = c("Moran_I_residuals", "Moran_I_pvalue", "N_outliers", "N_observations"),
  Value = c(
    moran_test$statistic,
    moran_test$p.value, 
    length(outliers),
    nrow(model_data_final)
  )
)

write_csv(diagnostics, "results/model_diagnostics.csv")

cat("\nResults saved to results/ directory:\n")
cat("- model_results_final.shp/.csv (spatial data with results)\n")
cat("- inla_model_final.rds (model object)\n")
cat("- model_results_summary.csv (coefficient table)\n")
cat("- spatial_components_summary.csv (spatial parameters)\n")
cat("- model_diagnostics.csv (diagnostic tests)\n")

#==================================================================================
# STEP 10: CREATE VISUALIZATION
#==================================================================================

cat("Creating result visualizations...\n")

# 1. Relative Risk Map
if(require(leaflet, quietly = TRUE)) {
  pal <- colorQuantile("YlOrRd", model_data_final$RR, n = 7)
  
  rr_map <- leaflet(model_data_final) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(RR),
      fillOpacity = 0.7,
      color = "white",
      weight = 1,
      popup = ~paste(
        "<strong>Tract:</strong>", NAM, "<br>",
        "<strong>Relative Risk:</strong>", round(RR, 2), "<br>",
        "<strong>95% CI:</strong> (", round(RR_lower, 2), ", ", round(RR_upper, 2), ")<br>",
        "<strong>Observed:</strong>", corr_pp, "<br>",
        "<strong>Expected:</strong>", round(expected, 1)
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~RR,
      title = "Relative Risk",
      position = "bottomright"
    )
  
  # Save map as HTML
  htmlwidgets::saveWidget(rr_map, "results/relative_risk_map.html")
  cat("Interactive map saved to results/relative_risk_map.html\n")
}

# 2. Coefficient plot
if(require(ggplot2, quietly = TRUE)) {
  coef_plot <- ggplot(fixed_effects %>% filter(Variable != "(Intercept)"), 
                      aes(x = reorder(Variable, RR), y = RR)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = RR_lower, ymax = RR_upper), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    coord_flip() +
    scale_y_log10() +
    labs(x = "Variables", y = "Relative Risk (log scale)",
         title = "INLA Model Results: Fixed Effects",
         subtitle = "Error bars show 95% credible intervals") +
    theme_minimal()
  
  ggsave("figures/coefficient_plot.png", coef_plot, width = 10, height = 6, dpi = 300)
  cat("Coefficient plot saved to figures/coefficient_plot.png\n")
}

# Clean up temporary files
unlink("model_spatial.adj")

cat("\n=== INLA SPATIAL MODELING COMPLETE ===\n")
cat("Key findings:\n")
if(exists("phi")) {
  cat("- Spatial structure accounts for", round(phi * 100, 1), "% of spatial variation\n")
}
cat("- Model includes", length(existing_std_vars), "conversion factors\n")
cat("- Results available in results/ directory\n")
cat("- Interactive map and plots created\n")