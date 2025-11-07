## 3.1 Data Sources and Integration Framework

### 3.1.1 Primary Data Sources

This research integrates multiple administrative and survey data sources to construct a comprehensive spatial dataset for analyzing correctional population patterns across Utah census tracts:
  
  **Administrative Data:**
  - **Utah Department of Corrections (UDC)**: Individual-level correctional supervision records (2016-2023) containing residential addresses, supervision types, risk assessments, and demographic information
- **Geographic Coverage**: All 29 Utah counties with complete address histories for supervised individuals

**Demographic and Socioeconomic Data:**
  - **American Community Survey (ACS)**: 5-year estimates (2017-2022) providing tract-level demographic, economic, and housing characteristics
- **Coverage**: All 716 Utah census tracts with standardized geographic identifiers

**Environmental and Health Data:**
  - **Utah Healthy Places Index (HPI)**: Comprehensive neighborhood-level health and environmental indicators
- **GeoDa Center Health Database**: Additional healthcare access and facility proximity measures
- **Environmental Variables**: Air quality measures, green space access, transportation infrastructure

### 3.1.2 Data Architecture and Integration Strategy

The data integration follows a hierarchical spatial framework connecting individual-level records to neighborhood-level characteristics through geographic assignment.

```{r data-architecture, eval=FALSE}
# Data integration workflow demonstration
# Note: Actual file paths will vary based on your directory structure

# Step 1: Load individual-level correctional data
correctional_raw <- read_csv("data/raw_corrections_data.csv")
cat("Original correctional records:", nrow(correctional_raw), "\n")

# Step 2: Load spatial boundaries  
utah_tracts %
st_transform(4326)
cat("Utah census tracts:", nrow(utah_tracts), "\n")

# Step 3: Load neighborhood-level covariates
acs_data <- get_acs(
  geography = "tract",
  state = "UT", 
  variables = c("B01003_001", "B19013_001"),
  year = 2021,
  output = "wide",
  geometry = FALSE
)

hpi_data <- read_csv("data/utah_hpi_data.csv")
geodacenter_data <- read_csv("data/T_Latest.csv") %>%
  filter(STATEFP == 49)  # Utah state code

cat("Data sources successfully loaded\n")
```

## 3.2 Data Cleaning and Preparation Workflow

### 3.2.1 Address Validation and Standardization

Following established protocols for administrative address data, the address cleaning process involved multiple validation steps:
  
  ```{r address-cleaning, eval=FALSE}
# Load and assess raw correctional data
clean_addresses <- read_csv("data/clean_addresses.csv")
total_records %
filter(
  !str_detect(clean_address, "P\\.?O\\.? BOX|PO BOX"),
  !str_detect(clean_address, "PRISON|JAIL|DETENTION"),
  !is.na(clean_address),
  str_length(clean_address) > 10
)

validated_records <- nrow(residential_addresses)
retention_rate <- round((validated_records / total_records) * 100, 1)

cat("Validated residential addresses:", validated_records, "\n")
cat("Retention rate:", retention_rate, "%\n")

# Remove duplicates for efficient geocoding
addresses_unique %
distinct(clean_address, .keep_all = TRUE)

cat("Unique addresses for geocoding:", nrow(addresses_unique), "\n")
```

### 3.2.2 Geocoding Methodology

The geocoding process employed a batch processing approach to handle the large address dataset efficiently while maintaining spatial accuracy standards.

```{r geocoding-demo, eval=FALSE}
# Geocoding configuration and batch processing
library(tidygeocoder)

# Set batch parameters for large datasets
batch_size <- 1000
total_rows <- nrow(addresses_unique)
num_batches <- ceiling(total_rows / batch_size)

cat("Processing", total_rows, "addresses in", num_batches, "batches\n")

# Initialize storage for results
results_list %
geocode(
  address = clean_address,
  method = 'osm',
  lat = latitude,
  long = longitude,
  limit = 1,
  verbose = FALSE
)

results_list[[i]]  0) {
  geocoded_sample <- bind_rows(results_list)
  success_rate <- round(sum(!is.na(geocoded_sample$latitude)) / 
                          nrow(geocoded_sample) * 100, 1)
  cat("Sample geocoding success rate:", success_rate, "%\n")
}
```

### 3.2.3 Spatial Assignment and Validation

```{r spatial-assignment, eval=FALSE}
# Load final geocoded results (placeholder - use your actual file)
# final_geocoded <- readRDS("data/final_geocoded_addresses.rds")

# For demonstration, create sample geocoded data structure
set.seed(42)
final_geocoded <- data.frame(
  id = 1:1000,
  clean_address = paste("Sample Address", 1:1000),
  latitude = runif(1000, 37, 42),
  longitude = runif(1000, -114, -109),
  supervision_type = sample(c("Probation", "Parole"), 1000, replace = TRUE)
)

# Convert to spatial features
addresses_sf %
filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

cat("Spatial points created:", nrow(addresses_sf), "\n")

# Load Utah census tracts
utah_tracts %
st_transform(4326) %>%
  select(GEOID, NAME)

# Spatial join to assign census tracts
addresses_with_tracts <- st_join(
  addresses_sf,
  utah_tracts,
  join = st_intersects
)

# Validation
successful_assignments <- sum(!is.na(addresses_with_tracts$GEOID))
assignment_rate <- round(successful_assignments / nrow(addresses_sf) * 100, 1)

cat("Successful tract assignments:", successful_assignments, "\n")
cat("Assignment rate:", assignment_rate, "%\n")

# Check for addresses outside Utah
utah_bounds %
filter(
  longitude < utah_bounds[1] | longitude > utah_bounds[3] |
    latitude < utah_bounds[2] | latitude > utah_bounds[4]
)

cat("Addresses outside Utah boundaries:", nrow(outside_utah), "\n")
```