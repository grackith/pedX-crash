# Load necessary libraries
libraries <- c("pROC", "tidyverse", "caret", "ROSE", "e1071", "xgboost", "gbm", "sf", "readxl", "progress", "futile.logger")
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

library(Matrix)  # For matrix.data.frame


# Global variables
set.seed(1998)

result_dir <- "~/gad9515@nyu.edu - Google Drive/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/code"

# Set up logging
flog.appender(appender.file("ml_script.log"))
flog.info("Script started")

debug_print <- function(message) {
  flog.info(paste("DEBUG:", message))
  cat(paste("DEBUG:", message, "\n"))
}

# Define models, subsampling methods, and predictor set
model_name.list <- c("xgbTree")
method_subsample.list <- c("rose")
vars.list <- list(
  c("log_exp", "DOWNTOWN_Y", "Presence.of.stop.sign", "Presence.of.curve.sign",
    "Presence.of.vehicle.sign", "Roadway.segment.count", "TradeLandUse",
    "ServiceLandUse", "RecreationalLandUse", "UnderdevelopedLandUse",
    "Total.sidewalk.length..ft.", "TransportationLandUse", "y")
)

prepare_data <- function(file_path) {
  tryCatch({
    cat("DEBUG: Starting data preparation\n")
    
    # Read data
    cat("Reading data...\n")
    data <- read.csv(file_path)
    
    # Print initial data structure
    cat("\nInitial data structure:\n")
    str(data)
    
    # Remove NODE_ID and MONTH if they exist
    if("NODE_ID" %in% names(data)) data <- data %>% select(-NODE_ID)
    if("MONTH" %in% names(data)) data <- data %>% select(-MONTH)
    
    # Identify variables that should be factors
    factor_vars <- c("RW_SEGM_C", "PARK_Y", "PNR_Y", "DOWNTOWN_Y",
                     "BIKEPED__1", "STOP_SIG_1", "ONEWAY_S_1",
                     "TURN_SIG_1", "CW_SIGN_Y", "CURVE_SI_1", "VEH_NEG_SI")
    
    # Identify continuous numeric variables to standardize
    numeric_to_standardize <- c(
      "N_EDGES", "log_exp", "TOTAL_WB", "AVG_RW_WID", "MAX_SPEEDL", 
      "MAX_SLOPE_", "TOTAL_BIKE", "TOTAL_SIDE", "TOTAL_CROS",
      "TRAFF_SIGN", "BUS_RIDERS", "RES_UNIT_C", "JOBS_C_ACR",
      "RES_CENSUS", "POP_CENSUS", "WHITE_CENS", "MED_MEDHHI",
      "TOTAL_TRAI", "RES_LU_PER", "MANUFAC_LU", "TRANSPORT_",
      "TRADE_LU_P", "SERVICE_LU", "RECRE_LU_P", "PRODUCT_LU",
      "UNDERDEV_L", "BIKEPED_SI", "STOP_SIGN_", "ONEWAY_SIG",
      "NOTURN_SIG", "CALMING_SI"
    )
    
    # Variables to keep as is
    keep_as_is <- c("int_idx", "C_DISTRICT", "YEAR", "crash", "lat_y", "lon_x")
    
    # Convert factors
    data <- data %>%
      mutate(across(all_of(factor_vars), ~as.factor(as.character(.)))) # Ensure clean factor conversion
    
    # Handle missing values in numeric variables
    data <- data %>%
      mutate(across(all_of(numeric_to_standardize), ~as.numeric(as.character(.)))) # Ensure numeric
    
    # Impute missing values with median
    data <- data %>%
      mutate(across(all_of(numeric_to_standardize),
                    ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    # Standardize numeric variables
    data <- data %>%
      mutate(across(all_of(numeric_to_standardize),
                    ~scale(.) %>% as.vector()))
    
    # Create binary crash outcome
    data <- data %>%
      mutate(
        crash = as.numeric(crash > 0),
        y = factor(crash, levels = c(0, 1), labels = c("X0", "X1"))
      )
    
    # Final checks
    cat("\nFinal Data Summary:\n")
    cat("Rows:", nrow(data), "\n")
    cat("Columns:", ncol(data), "\n")
    cat("Class distribution:", table(data$y), "\n")
    
    return(data)
    
  }, error = function(e) {
    cat("ERROR in data preparation:", e$message, "\n")
    print(traceback())
    stop(e$message)
  })
}

perform_feature_selection <- function(data, n_folds = 5) {
  # Get potential features
  exclude_vars <- c("int_idx", "C_DISTRICT", "YEAR", "crash", "lat_y", "lon_x", "y", "NODE_ID", "MONTH")
  features <- setdiff(names(data), exclude_vars)
  
  cat("Features being evaluated:", paste(features, collapse=", "), "\n")
  
  # Store importance scores
  importance_scores <- data.frame(
    Feature = features,
    Importance = 0,
    stringsAsFactors = FALSE
  )
  rownames(importance_scores) <- features
  
  # Set up cross-validation
  set.seed(1998)
  folds <- createFolds(data$crash, k = n_folds)
  
  for(i in seq_along(folds)) {
    cat(sprintf("Processing fold %d of %d\n", i, n_folds))
    
    train_data <- data[-folds[[i]], ] %>%
      mutate(y = factor(crash > 0, levels = c(FALSE, TRUE), labels = c("X0", "X1")))
    
    # Create formula for model matrix
    formula_str <- paste("~", paste(features, collapse = " + "), "- 1")
    formula_obj <- as.formula(formula_str)
    
    # Create model matrix
    train_matrix <- try({
      model.matrix(formula_obj, data = train_data)
    })
    
    if(inherits(train_matrix, "try-error")) {
      cat("Error in creating train matrix. Data structure:\n")
      print(str(train_data[, features]))
      stop("Failed to create training matrix")
    }
    
    # Train model
    model <- try({
      train(
        x = train_matrix,
        y = train_data$y,
        method = "xgbTree",
        trControl = trainControl(
          method = "cv",
          number = 3,
          verboseIter = TRUE
        )
      )
    })
    
    if(inherits(model, "try-error")) {
      cat("Error in training model. Matrix structure:\n")
      print(str(train_matrix))
      stop("Failed to train model")
    }
    
    # Get feature importance using varImp
    imp <- varImp(model)$importance
    imp$Overall <- imp$Overall / sum(imp$Overall) # Normalize
    
    # Update importance scores
    for(feat in rownames(imp)) {
      if(feat %in% rownames(importance_scores)) {
        importance_scores[feat, "Importance"] <- 
          importance_scores[feat, "Importance"] + imp[feat, "Overall"]
      }
    }
  }
  
  # Average importance across folds
  importance_scores$Importance <- importance_scores$Importance / n_folds
  
  # Sort by importance
  importance_scores <- importance_scores[order(importance_scores$Importance, decreasing = TRUE), ]
  
  # Print importance scores
  cat("\nFeature Importance Scores:\n")
  print(importance_scores)
  
  # Select features above mean importance
  mean_importance <- mean(importance_scores$Importance)
  selected_features <- rownames(importance_scores)[importance_scores$Importance > mean_importance]
  
  cat("\nSelected features:", paste(selected_features, collapse=", "), "\n")
  
  return(selected_features)
}

train_model <- function(train, test, selected_features, model_name, method_subsample = "none") {
  # Convert any character columns to factors
  train <- train %>% mutate(across(where(is.character), as.factor))
  test <- test %>% mutate(across(where(is.character), as.factor))
  
  # Create formula for model matrix
  formula_str <- paste0("~", paste(selected_features, collapse = " + "), "- 1")
  formula_obj <- as.formula(formula_str)
  
  # Create model matrices
  train_matrix <- model.matrix(formula_obj, data = train)
  test_matrix <- model.matrix(formula_obj, data = test)
  
  # Train model
  model <- train(
    x = train_matrix,
    y = train$y,
    method = model_name,
    trControl = trainControl(
      method = "cv",
      number = 5,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    ),
    tuneGrid = expand.grid(
      nrounds = 100,
      max_depth = 3,
      eta = 0.3,
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.75
    ),
    verbose = TRUE
  )
  
  # Make predictions
  predictions <- predict(model, newdata = test_matrix, type = "prob")
  
  return(list(
    model = model,
    predictions = predictions,
    test_data = test_data
  ))
}


# Prepare data with binary crash state for all intersections/years
prepare_temporal_data <- function(data) {
  # Get unique intersection IDs and years 
  intersections <- unique(data$int_idx)
  years <- sort(unique(data$YEAR[!is.na(data$YEAR)]))
  
  # Create complete intersection-year combinations
  temporal_data <- expand.grid(
    int_idx = intersections,
    YEAR = years
  ) %>%
    # Join with original data to get all features
    left_join(
      data %>% 
        select(-YEAR, -crash) %>% 
        distinct(int_idx, .keep_all = TRUE),
      by = "int_idx"
    ) %>%
    # Join with crash state (binary)
    left_join(
      data %>%
        group_by(int_idx, YEAR) %>%
        summarize(crash = max(crash, na.rm = TRUE), # If any crash occurred that year
                  .groups = "drop"),
      by = c("int_idx", "YEAR")
    ) %>%
    # Fill NAs in crash with 0
    mutate(crash = replace_na(crash, 0),
           y = factor(crash, levels = c(0, 1), labels = c("X0", "X1")))
  
  return(temporal_data)
}

perform_temporal_validation <- function(data, selected_features) {
  # First ensure selected features exist in data
  missing_features <- setdiff(selected_features, names(data))
  if(length(missing_features) > 0) {
    stop("Missing features in data: ", paste(missing_features, collapse=", "))
  }
  
  # Prepare data with binary crash state for all intersections/years
  temporal_data <- prepare_temporal_data(data)
  
  # Get unique years
  years <- sort(unique(temporal_data$YEAR))
  
  results <- list()
  
  # Rolling validation: train on 3 years, test on 4th
  for(i in 4:length(years)) {
    # Training data: 3 years
    train_years <- years[(i-3):(i-1)]
    test_year <- years[i]
    
    cat(sprintf("Training on years %s, testing on year %d\n",
                paste(train_years, collapse=", "),
                test_year))
    
    # Get ALL intersections for both periods
    train_data <- temporal_data %>%
      filter(YEAR %in% train_years) %>%
      # Ensure features are properly typed
      mutate(
        crash = as.numeric(crash > 0),
        y = factor(crash, levels = c(0, 1), labels = c("X0", "X1"))
      )
    
    test_data <- temporal_data %>%
      filter(YEAR == test_year) %>%
      mutate(
        crash = as.numeric(crash > 0),
        y = factor(crash, levels = c(0, 1), labels = c("X0", "X1"))
      )
    
    # Create formula for model matrix
    formula_str <- paste("~", paste(selected_features, collapse = " + "), "- 1")
    formula_obj <- as.formula(formula_str)
    
    # Create model matrices
    train_matrix <- try({
      model.matrix(formula_obj, data = train_data)
    })
    
    if(inherits(train_matrix, "try-error")) {
      cat("Error in creating train matrix. Data structure:\n")
      print(str(train_data[, selected_features]))
      stop("Failed to create training matrix")
    }
    
    test_matrix <- try({
      model.matrix(formula_obj, data = test_data)
    })
    
    if(inherits(test_matrix, "try-error")) {
      cat("Error in creating test matrix. Data structure:\n")
      print(str(test_data[, selected_features]))
      stop("Failed to create test matrix")
    }
    
    # Train model
    model <- try({
      train(
        x = train_matrix,
        y = train_data$y,
        method = "xgbTree",
        trControl = trainControl(
          method = "cv",
          number = 5,
          classProbs = TRUE,
          summaryFunction = twoClassSummary
        ),
        tuneGrid = expand.grid(
          nrounds = 100,
          max_depth = 3,
          eta = 0.3,
          gamma = 0,
          colsample_bytree = 0.8,
          min_child_weight = 1,
          subsample = 0.75
        ),
        verbose = TRUE
      )
    })
    
    if(inherits(model, "try-error")) {
      cat("Error in training model. Matrix structure:\n")
      print(str(train_matrix))
      stop("Failed to train model")
    }
    
    # Make predictions
    predictions <- predict(model, newdata = test_matrix, type = "prob")
    
    results[[as.character(test_year)]] <- list(
      model = model,
      predictions = predictions,
      test_data = test_data
    )
  }
  
  return(results)
}

perform_spatial_validation <- function(data, selected_features) {
  # Get unique districts
  districts <- sort(unique(data$C_DISTRICT))
  
  results <- list()
  
  # Use a rolling window of districts
  for(i in 4:length(districts)) {
    # Training on 3 districts
    train_districts <- districts[(i-3):(i-1)]
    test_district <- districts[i]
    
    train_data <- data %>% filter(C_DISTRICT %in% train_districts)
    test_data <- data %>% filter(C_DISTRICT == test_district)
    
    model_results <- train_model(
      train_data,
      test_data,
      selected_features,
      "xgbTree",
      "rose"
    )
    
    results[[as.character(test_district)]] <- model_results
  }
  
  return(results)
}

main <- function() {
  # Load and prepare data
  # Load and prepare data
  data <- prepare_data(file_path = "/Users/gracedouglas/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/crash-data-for-validation.2024-12-30.csv")
  
  # Check years available
  years <- sort(unique(data$YEAR[!is.na(data$YEAR)]))
  cat("Available years:", paste(years, collapse=", "), "\n")
  
  # Check districts available
  districts <- sort(unique(data$C_DISTRICT))
  cat("Available districts:", paste(districts, collapse=", "), "\n")
  
  # Step 1: Feature selection
  selected_features <- perform_feature_selection(data)
  
  # Step 2: Temporal validation
  temporal_results <- perform_temporal_validation(data, selected_features)
  
  # Step 3: Spatial validation
  spatial_results <- perform_spatial_validation(data, selected_features)
  
  # Save results with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  saveRDS(list(
    data_summary = list(
      n_rows = nrow(data),
      n_cols = ncol(data),
      years = years,
      districts = districts,
      class_distribution = table(data$y)
    ),
    selected_features = selected_features,
    temporal_results = temporal_results,
    spatial_results = spatial_results
  ), file.path(result_dir, paste0("validation_results_", timestamp, ".rds")))
}

main()
