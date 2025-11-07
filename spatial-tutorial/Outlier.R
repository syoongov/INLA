model_data_hpi <- st_read("data/model_data_hpi.shp", quiet = TRUE)


# Enhanced concentration analysis with tract identification
utah_concentration_analysis <- model_data_hpi %>%
  # Sort by correctional rate (highest impact first)
  arrange(desc(corr_rt)) %>%
  mutate(
    # Calculate cumulative population and corrections
    cumulative_population = cumsum(tpop),
    cumulative_corrections = cumsum(corr_pp),
    
    # Convert to percentiles for interpretation
    pop_percentile = cumulative_population / sum(tpop, na.rm = TRUE),
    corrections_percentile = cumulative_corrections / sum(corr_pp, na.rm = TRUE),
    
    # Calculate impact ratio (how much each tract contributes relative to its population)
    impact_ratio = (corr_pp / sum(corr_pp, na.rm = TRUE)) / 
      (tpop / sum(tpop, na.rm = TRUE)),
    
    # Rank tracts by impact
    concentration_rank = row_number()
  )

# Find the concentration threshold (following Simes' methodology)
concentration_threshold <- utah_concentration_analysis %>%
  filter(pop_percentile <= 0.15) %>%  # Top 15% of population
  summarise(
    corrections_share = max(corrections_percentile),
    n_tracts = n(),
    total_pop_in_threshold = max(cumulative_population),
    total_corrections_in_threshold = max(cumulative_corrections)
  )

# Identify the specific high-impact census tracts
high_impact_tracts <- utah_concentration_analysis %>%
  filter(pop_percentile <= 0.15) %>%
  select(
    tract_id = GEOID,           # or whatever your tract ID variable is
    tract_name = NAM,           # or whatever your tract name variable is
    county,       # if you have this
    corr_rt,
    tpop,
    corr_pp,
    impact_ratio,
    concentration_rank,
    pop_percentile,
    corrections_percentile,
    # Add your environmental variables for context
    diesel_pm = dieslpm,        # or whatever your diesel PM variable is
    UrbnTyp
  ) %>%
  arrange(concentration_rank)

# Create summary table showing the concentration pattern
concentration_summary <- utah_concentration_analysis %>%
  mutate(
    pop_decile = ntile(pop_percentile * 10, 10)
  ) %>%
  group_by(pop_decile) %>%
  summarise(
    n_tracts = n(),
    avg_corrections_rate = mean(corr_rt, na.rm = TRUE),
    total_corrections = sum(corr_pp, na.rm = TRUE),
    corrections_share = sum(corr_pp, na.rm = TRUE) / sum(utah_concentration_analysis$corr_pp, na.rm = TRUE),
    avg_diesel_pm = mean(dieslpm, na.rm = TRUE),
    .groups = 'drop'
  )

# Print results
print("=== UTAH CORRECTIONAL CONCENTRATION ANALYSIS ===")
print(paste("Top 15% of population accounts for", 
            round(concentration_threshold$corrections_share * 100, 1), 
            "% of all correctional supervision"))
#[1] "Top 15% of population accounts for 47.1 % of all correctional supervision"

print(paste("This represents", concentration_threshold$n_tracts, "census tracts"))
#[1] "This represents 121 census tracts"

print("\n=== TOP 10 HIGHEST IMPACT CENSUS TRACTS ===")
print(head(high_impact_tracts, 10))


#Simes version: census tracts covering 15% of the state's population account for half of all prison admissions.



#====== simpler version

# Identify Utah's high-impact census tracts
utah_concentration_analysis <- model_data_hpi %>%
  arrange(desc(corr_rt)) %>%
  mutate(
    cumulative_population = cumsum(tpop),
    cumulative_corrections = cumsum(corr_pp),
    pop_percentile = cumulative_population / sum(tpop),
    corrections_percentile = cumulative_corrections / sum(corr_pp)
  )

# Find the concentration threshold (following Simes' 15% finding)
concentration_threshold <- utah_concentration_analysis %>%
  filter(pop_percentile <= 0.20) %>%
  summarise(corrections_share = max(corrections_percentile))

# Census tracts covering 20% of the state's population account for half of all prison admissions