# Script 1: Predictions Processing

# Load required libraries
library(tidyverse)
library(pROC)
library(caret)

# Set the directory path
dir_path <- "~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/ml results/1 NHTSA Ped Crash.local-results/upload"

# List all files in the directory
all_files <- list.files(dir_path, pattern = "\\.rds$", full.names = TRUE)

# Function to read RDS files
read_rds_files <- function(pattern, exclude_pattern = NULL) {
  files <- all_files[grep(pattern, all_files)]
  if (!is.null(exclude_pattern)) {
    files <- files[!grepl(exclude_pattern, files)]
  }
  if(length(files) > 0) {
    result <- lapply(files, readRDS)
    names(result) <- basename(files)
    return(result)
  } else {
    return(list())
  }
}

# Read and organize results
results <- list(
  spatial = list(
    predictions = read_rds_files("v10\\.sp.*predictions")
  ),
  temporal = list(
    predictions = read_rds_files("v10\\.tm.*predictions")
  )
)

# Function to process predictions
process_predictions <- function(predictions_list, validation_type) {
  processed_dfs <- lapply(names(predictions_list), function(file_name) {
    print(paste("Processing file:", file_name))
    model_data <- predictions_list[[file_name]]
    
    if(is.null(model_data) || length(model_data) == 0) {
      print(paste("Warning: Empty or NULL data for file:", file_name))
      return(NULL)
    }
    
    model <- tools::file_path_sans_ext(tail(strsplit(file_name, "_")[[1]], 1))
    
    print(paste("Model:", model))
    print("Structure of model_data:")
    print(str(model_data))
    
    subsampling_dfs <- lapply(names(model_data), function(subsampling) {
      print(paste("Processing subsampling:", subsampling))
      df <- model_data[[subsampling]]
      
      print("Structure of df:")
      print(str(df))
      
      if(is.null(df) || !is.data.frame(df)) {
        if(is.list(df) && "predictions" %in% names(df)) {
          df <- df$predictions
          print("Using 'predictions' element from list")
        } else {
          print(paste("Warning: Not a data frame for subsampling:", subsampling))
          return(NULL)
        }
      }
      
      if(!is.data.frame(df)) {
        print(paste("Warning: Still not a data frame for subsampling:", subsampling))
        return(NULL)
      }
      
      # Rename columns if they don't match expected names
      if("temp$predictions" %in% names(df)) {
        names(df)[names(df) == "temp$predictions"] <- "class"
      }
      if(all(c("X0", "X1") %in% names(df))) {
        names(df)[names(df) == "X0"] <- "p0"
        names(df)[names(df) == "X1"] <- "p1"
      }
      if("actual" %in% names(df)) {
        names(df)[names(df) == "actual"] <- "obs"
      }
      
      predictor_set <- paste0("M", substr(subsampling, 1, 1))
      
      df$validation <- validation_type
      df$model <- model
      df$subsampling <- substr(subsampling, 3, nchar(subsampling))
      df$predictor_set <- predictor_set
      
      return(df)
    })
    
    subsampling_dfs <- subsampling_dfs[!sapply(subsampling_dfs, is.null)]
    if(length(subsampling_dfs) > 0) {
      return(do.call(rbind, subsampling_dfs))
    } else {
      print(paste("Warning: No valid data frames for file:", file_name))
      return(NULL)
    }
  })
  
  processed_dfs <- processed_dfs[!sapply(processed_dfs, is.null)]
  if(length(processed_dfs) > 0) {
    return(do.call(rbind, processed_dfs))
  } else {
    print("Warning: No valid data frames to combine")
    return(NULL)
  }
}

# The rest of the script remains the same...

# Process spatial predictions
spatial_predictions <- tryCatch({
  process_predictions(results$spatial$predictions, "spatial")
}, error = function(e) {
  print(paste("Error processing spatial predictions:", e$message))
  return(NULL)
})

# Process temporal predictions
temporal_predictions <- tryCatch({
  process_predictions(results$temporal$predictions, "temporal")
}, error = function(e) {
  print(paste("Error processing temporal predictions:", e$message))
  return(NULL)
})

# Check and print dimensions of predictions
print_dimensions <- function(predictions, name) {
  if(!is.null(predictions)) {
    print(paste(name, "predictions dimensions:", paste(dim(predictions), collapse = " x ")))
  } else {
    print(paste("No valid", name, "predictions"))
  }
}

print_dimensions(spatial_predictions, "Spatial")
print_dimensions(temporal_predictions, "Temporal")

# Combine spatial and temporal predictions
all_predictions <- NULL
if(!is.null(spatial_predictions) && !is.null(temporal_predictions)) {
  all_predictions <- rbind(spatial_predictions, temporal_predictions)
  print(paste("All predictions dimensions:", paste(dim(all_predictions), collapse = " x ")))
} else if(!is.null(spatial_predictions)) {
  all_predictions <- spatial_predictions
  print("Only spatial predictions available")
} else if(!is.null(temporal_predictions)) {
  all_predictions <- temporal_predictions
  print("Only temporal predictions available")
} else {
  print("No valid predictions available")
}

if(!is.null(all_predictions)) {
  # Rename columns
  names(all_predictions) <- c("class", "p0", "p1", "obs", "validation", "model", "subsampling", "set")
  
  print("Column names:")
  print(colnames(all_predictions))
  
  # Write the combined results to a CSV file
  write.csv(all_predictions, "~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/ml results/1 NHTSA Ped Crash.local-results/all_ml_predictions.csv", row.names = FALSE)
  print("Results written to all_ml_predictions.csv")
} else {
  print("No data to write to CSV")
}