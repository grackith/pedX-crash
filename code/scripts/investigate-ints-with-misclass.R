library(tidyverse)
library(sf)
library(readxl)
library(ggplot2)
library(sf)
library(osmdata)
library(dplyr)


# Set the path to your result directory
result_dir <- "~/gad9515@nyu.edu - Google Drive/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/code"

# Set the path to your original data files
original_data_path <- "~/gad9515@nyu.edu - Google Drive/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/dt_after_enet.xlsx"
original_sf_path <- "~/gad9515@nyu.edu - Google Drive/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/data for ml mods/data/ml-dt-final.shp"

# Function to process misclassifications
process_misclassifications <- function(results, validation_type) {
  misclassifications <- results$xgbTree_rose$misclassifications
  misclassifications$validation_type <- validation_type
  return(misclassifications)
}

# Read the results
spatial_results <- readRDS(file.path(result_dir, "spatial_validation_results.rds"))
temporal_results <- readRDS(file.path(result_dir, "temporal_validation_results.rds"))

# Process misclassifications
spatial_misclassifications <- process_misclassifications(spatial_results, "spatial")
temporal_misclassifications <- process_misclassifications(temporal_results, "temporal")

# Combine misclassifications
all_misclassifications <- bind_rows(spatial_misclassifications, temporal_misclassifications)

# First get the misclassified locations with avg_prob_diff
# Read original data files first
original_data <- read_excel(original_data_path)
original_sf <- st_read(original_sf_path)
original_sf$idx = as.factor(original_sf$idx)  # Ensure idx is character type
all_misclassifications$idx = as.factor(all_misclassifications$idx) 
all_misclassifications$validation_type = as.factor(all_misclassifications$validation_type) 
all_misclassifications$error_type = as.factor(all_misclassifications$error_type) 

# Then proceed with filtering
misclassified_both <- all_misclassifications %>%
  group_by(idx) %>%
  filter(n() > 1 & 
           all(misclassified) &
           n_distinct(validation_type) > 1) %>%
  mutate(avg_prob_diff = mean(prob_diff)) %>%
  mutate(percentile_85 = quantile(avg_prob_diff, 0.85, na.rm = TRUE)) %>%
  filter(avg_prob_diff >= percentile_85) %>%
  arrange(desc(avg_prob_diff))

# Keep original columns
new.df <- misclassified_both[,c(1:8)]

# Join with original spatial data
new.sf <- original_sf %>% 
  filter(idx %in% new.df$idx)

# Extract coordinates
new.sf_with_coords <- new.sf %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

# Create final export dataframe
new.df_export <- new.df %>%
  left_join(st_drop_geometry(new.sf_with_coords), by = "idx") %>%
  dplyr::select(-DISPLAY_NA)

# Convert back to sf object
new_sf <- st_as_sf(new.df_export, coords = c("lon", "lat"), crs = 4326)


# Export if needed
setwd("~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/misclass.shps/2024.11.13")
st_write(new_sf, "high_prob_diff_misclassified.shp")
write.csv(new.df_export, "high_prob_diff_misclassified.csv", row.names = FALSE)

# Alternatively, export as Shapefile
#st_write(new.df_export, "~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/v2-misclassified_data.shp")

# Define the bounding box for Seattle
seattle_bbox <- getbb("Seattle")

# Get street data for Seattle
seattle_streets <- opq(seattle_bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Ensure the CRS is correct
new_sf <- st_transform(new_sf, 4326)

# Create the plot
ggplot() +
  # Add the streets
  geom_sf(data = seattle_streets$osm_lines, color = "gray90", size = 0.1) +
  # Add the points
  geom_sf(data = new_sf, aes(color = validation_type, shape = error_type), size = 4, alpha = 0.7) +
  # Set the extent to Seattle
  coord_sf(xlim = seattle_bbox[1,], ylim = seattle_bbox[2,], expand = FALSE) +
  # Customize the theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  # Add labels
  labs(
    title = "Misclassifications in Seattle, WA",
    subtitle = "By Validation Type and Error Type",
    color = "Validation Type",
    shape = "Error Type"
  ) +
  # Adjust the color scale
  scale_color_brewer(palette = "Set1")



#write.csv(new.df_export, "~/gad9515@nyu.edu - Google Drive/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/code/new_sf_with_coords.csv", row.names = FALSE)
