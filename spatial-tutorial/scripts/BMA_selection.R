####################################################################
## BMA VARIABLE SELECTION SCRIPT                                 ##
## For Spatial Capabilities Analysis                             ##
## Based on BMA_2025_v3.R approach                              ##
####################################################################

# Load required libraries
library(BMA)
library(tidyverse)
library(sf)
library(fastDummies)

cat("=== BMA VARIABLE SELECTION FOR SPATIAL CAPABILITIES ===\n")

#==================================================================================
# STEP 1: DATA LOADING AND PREPARATION
#==================================================================================

cat("Loading spatial data...\n")
udc_census_full2 <- st_read("data/udc_census_full_v2.shp", optional = TRUE)

# Create vector of predictor variables (based on your comprehensive list)
predictors_3 <- c(
  # GeoDa Center variables
  "DisbP", "NnRlFhP", "BedCnt", "PntInTm", "YrlyBdC", 
  "GiniCff", "EduP", "HghRskP", "HltCrP", 
  "RetailP", "EssnWrP", "MobileP", "LngTrmP", "Ndvi", "NoIntP", "VacantP", 
  "FqhcTmD", "FqhcCnD", "HspTmDr", "HspCntD", "RxTmDr", 
  "RxCntDr", "MhTmDr", "MhCntDr", "SutTmDr", "StCntDr", 
  "OtpTmDr", "OtpCntD", "TotPcp", "PcpP100", "LmMbInd", 
  "MicaInd", "NghbTyp", 
  
  # Census variables
  "punemp", "ppov", "ppa", "pfemhh", "pchldrn", "MedinAg", 
  
  # Urban typology
  "UrbnTyp", 
  
  # HPI variables
  "LEB", "autombl", "bchlrsd", "bikccss", 
  "cnssrsp", "dieslpm", "employd", "hmwnrsh", "housrpr", "inhghsc", "inprsch", 
  "insured", "ownsevr", "ozone", "prkccs_", "prcptnc", "pm25", "rentsvr", 
  "traffic", "trecnpy", "uncrwdd", "voting", "phisp"
)

cat("Initial predictor variables: ", length(predictors_3), "\n")

#==================================================================================
# STEP 2: CATEGORICAL VARIABLE HANDLING
#==================================================================================

cat("Processing categorical variables...\n")

# Handle categorical variables with dummy creation
bma_model_data_3 <- udc_census_full2 %>%
  # Convert to factors first
  mutate(
    Ruralty = factor(Ruralty),
    NghbTyp = factor(NghbTyp)
  ) %>%
  # Create dummy variables for Ruralty
  dummy_cols(select_columns = "Ruralty", 
             remove_selected_columns = TRUE,
             remove_first_dummy = TRUE) %>%
  # Create dummy variables for NghbTyp  
  dummy_cols(select_columns = "NghbTyp", 
             remove_selected_columns = TRUE,
             remove_first_dummy = TRUE)

# Update predictor list to include dummy variables
rurality_dummies <- names(bma_model_data_3)[grep("Ruralty_", names(bma_model_data_3))]
neighb_dummies <- names(bma_model_data_3)[grep("NghbTyp_", names(bma_model_data_3))]

cat("Rurality dummies created: ", length(rurality_dummies), "\n")
cat("Neighborhood type dummies created: ", length(neighb_dummies), "\n")

# Final predictor list
predictors_updated <- c(
  predictors_3[!(predictors_3 %in% c("Ruralty", "NghbTyp"))],
  rurality_dummies,
  neighb_dummies
)

cat("Total predictors after dummy creation: ", length(predictors_updated), "\n")

#==================================================================================
# STEP 3: FINAL DATASET PREPARATION
#==================================================================================

cat("Preparing final BMA dataset...\n")

# Select variables and remove missing cases
bma_model_data_3 <- bma_model_data_3 %>%
  select(all_of(predictors_updated), corr_pp) %>%
  na.omit()

cat("Dataset after removing incomplete cases: ", nrow(bma_model_data_3), " observations\n")
cat("Final variable count: ", ncol(bma_model_data_3)-1, " predictors\n")

# Convert to numeric (required for BMA)
bma_model_data_3 <- map_df(bma_model_data_3, as.numeric)

# Check for any remaining issues
if(any(is.na(bma_model_data_3))) {
  cat("Warning: Still have missing values after conversion\n")
}

if(any(!is.finite(as.matrix(bma_model_data_3)))) {
  cat("Warning: Non-finite values detected\n")
}

#==================================================================================
# STEP 4: PREPARE MATRICES FOR BMA
#==================================================================================

cat("Preparing matrices for BMA analysis...\n")

# Separate predictors and response
X <- bma_model_data_3 %>% select(-corr_pp) %>% as.matrix()
y <- bma_model_data_3$corr_pp

cat("X matrix dimensions: ", dim(X)[1], " x ", dim(X)[2], "\n")
cat("Y vector length: ", length(y), "\n")

# Identify dummy variables for appropriate standardization
dummy_cols <- grep("(Ruralty_|UrbnTyp)", colnames(X))
neighb_cols <- grep("NghbTyp_", colnames(X))
no_scale_cols <- c(dummy_cols, neighb_cols)
continuous_cols <- setdiff(1:ncol(X), no_scale_cols)

cat("Dummy/categorical columns: ", length(no_scale_cols), "\n")
cat("Continuous columns for standardization: ", length(continuous_cols), "\n")

# Standardize continuous variables only
X_scaled_3 <- X
X_scaled_3[,continuous_cols] <- scale(X[,continuous_cols])

cat("Standardization completed\n")

#==================================================================================
# STEP 5: RUN BMA ANALYSIS
#==================================================================================

cat("Running BMA analysis...\n")
cat("This may take several minutes with ", ncol(X_scaled_3), " variables...\n")

# Run BMA with parameters suitable for large number of predictors
bma_result <- bicreg(
  x = X_scaled_3,
  y = y,
  strict = FALSE,     # Less strict model selection
  OR = 50,            # Increased Occam's window  
  maxCol = 40         # Maximum predictors in any model
)

cat("BMA analysis completed successfully\n")

#==================================================================================
# STEP 6: PROCESS AND SAVE RESULTS
#==================================================================================

cat("Processing BMA results...\n")

# Get coefficient summary
coef_summary <- summary(bma_result)
cat("Coefficient summary structure:\n")
str(coef_summary)

# Create results dataframe
coef_matrix <- as.data.frame(coef_summary)

posterior_df <- data.frame(
  Variable = rownames(coef_matrix),
  PIP = as.numeric(gsub(" ", "", coef_matrix[, 1]))/100,  # p!=0 column (convert percentage)
  Mean = as.numeric(gsub(" ", "", coef_matrix[, 2])),     # EV column  
  SD = as.numeric(gsub(" ", "", coef_matrix[, 3]))        # SD column
) %>%
  mutate(
    CI_lower = Mean - 1.96*SD,
    CI_upper = Mean + 1.96*SD,
    Effect_Size = abs(Mean),
    Signal_to_Noise = abs(Mean)/SD,
    Evidence_Tier = case_when(
      PIP >= 0.75 ~ "Strong Evidence",
      PIP >= 0.50 ~ "Moderate Evidence",
      PIP >= 0.25 ~ "Suggestive Evidence", 
      TRUE ~ "Weak Evidence"
    )
  ) %>%
  arrange(desc(PIP))

# Print summary by evidence tier
cat("\nEvidence Tier Summary:\n")
tier_summary <- posterior_df %>%
  count(Evidence_Tier) %>%
  arrange(match(Evidence_Tier, c("Strong Evidence", "Moderate Evidence", 
                                 "Suggestive Evidence", "Weak Evidence")))
print(tier_summary)

# Print top variables
cat("\nTop 10 Variables by Posterior Inclusion Probability:\n")
print(head(posterior_df %>% select(Variable, PIP, Evidence_Tier, Effect_Size), 10))

#==================================================================================
# STEP 7: SAVE RESULTS
#==================================================================================

cat("Saving results...\n")

# Save detailed results
write_csv(posterior_df, "results/bma_posterior_results.csv")

# Save important variables by tier
important_vars <- posterior_df %>%
  filter(PIP > 0.25) %>%
  arrange(desc(PIP))

write_csv(important_vars, "results/bma_selected_variables.csv")

# Save model object
saveRDS(bma_result, "results/bma_model_object.rds")

# Save summary for quick reference
bma_summary <- list(
  total_variables = ncol(X_scaled_3),
  observations = nrow(bma_model_data_3),
  strong_evidence = sum(posterior_df$Evidence_Tier == "Strong Evidence"),
  moderate_evidence = sum(posterior_df$Evidence_Tier == "Moderate Evidence"),
  suggestive_evidence = sum(posterior_df$Evidence_Tier == "Suggestive Evidence"),
  top_variables = head(important_vars$Variable, 10)
)

saveRDS(bma_summary, "results/bma_summary.rds")

cat("\nBMA Variable Selection Completed Successfully!\n")
cat("Results saved to results/ directory\n")
cat("Key output files:\n")
cat("- bma_posterior_results.csv (complete results)\n") 
cat("- bma_selected_variables.csv (PIP > 0.25)\n")
cat("- bma_model_object.rds (BMA model)\n")
cat("- bma_summary.rds (summary statistics)\n")

#==================================================================================
# STEP 8: CREATE VISUALIZATION
#==================================================================================

cat("Creating BMA results visualization...\n")

# Create and save visualization
library(ggplot2)
library(viridis)

p_bma <- ggplot(important_vars, aes(x = PIP, y = reorder(Variable, PIP), 
                                    color = Evidence_Tier, size = Effect_Size)) +
  geom_point(alpha = 0.8) +
  geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", alpha = 0.6) +
  scale_color_manual(values = c("Strong Evidence" = "#d73027", 
                                "Moderate Evidence" = "#f46d43",
                                "Suggestive Evidence" = "#fee08b")) +
  scale_size_continuous(range = c(3, 8)) +
  labs(x = "Posterior Inclusion Probability", 
       y = "Variables", 
       title = "BMA Results: Variable Selection Evidence",
       subtitle = paste("Variables with PIP > 0.25 (n =", nrow(important_vars), ")"),
       color = "Evidence Level", 
       size = "Effect Size") +
  theme_minimal() +
  theme(text = element_text(size = 12))

ggsave("figures/bma_results.png", p_bma, width = 12, height = 8, dpi = 300)

cat("BMA visualization saved to figures/bma_results.png\n")
cat("\n=== BMA ANALYSIS COMPLETE ===\n")