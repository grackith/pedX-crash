library(sf)
library(dplyr)
library(skimr)

# Read the sf object
sf_object <- st_read("~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/v2-misclassified_data.shp")

# Convert to dataframe, remove specified columns, and filter top 25% by prb_dff
df <- sf_object %>%
  st_drop_geometry() %>%
  select(-c(C_DISTR, crsh_dd, crsh_vn)) %>%
  filter(prb_dff >= quantile(prb_dff, 0.75, na.rm = TRUE))

# Generate summary statistics grouped by vldtn_t
summary_stats <- df %>% 
  group_by(vldtn_t, errr_ty) %>% 
  skim()

# Print the summary statistics
print(summary_stats)

# Display the first few rows of the processed dataframe
head(df)

# Optional: Print the number of rows in the filtered dataframe
cat("\nNumber of rows in the filtered dataframe:", nrow(df), "\n")

# Optional: Print the quantile used for filtering
cat("75th percentile of prb_dff used for filtering:", 
    quantile(sf_object$prb_dff, 0.75, na.rm = TRUE), "\n")