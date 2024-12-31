# Load required libraries
library(tidyverse)
library(pROC)
library(caret)
library(MLmetrics)

# Function to compute Accuracy
compute_accuracy <- function(df) {
  mean(df$class == df$obs)
}

# Function to compute Balanced Accuracy
compute_balanced_accuracy <- function(df) {
  confusion_matrix <- table(df$class, df$obs)
  sensitivity <- confusion_matrix["X1", "X1"] / sum(confusion_matrix[, "X1"])
  specificity <- confusion_matrix["X0", "X0"] / sum(confusion_matrix[, "X0"])
  balanced_accuracy <- (sensitivity + specificity) / 2
  return(balanced_accuracy)
}

# Function to compute Precision
compute_precision <- function(df) {
  Precision(y_pred = df$class, y_true = df$obs, positive = "X1")
}

# Function to compute Recall
compute_recall <- function(df) {
  Recall(y_pred = df$class, y_true = df$obs, positive = "X1")
}

# Function to compute F1 Score
compute_f1 <- function(df) {
  F1_Score(y_pred = df$class, y_true = df$obs, positive = "X1")
}

# Function to compute Matthews Correlation Coefficient (MCC)
# Updated Function to compute Matthews Correlation Coefficient (MCC)
compute_mcc <- function(df) {
  confusion_matrix <- table(df$class, df$obs)
  
  # Print confusion matrix for debugging
  print(confusion_matrix)
  
  TP <- confusion_matrix["X1", "X1"]
  TN <- confusion_matrix["X0", "X0"]
  FP <- confusion_matrix["X1", "X0"]
  FN <- confusion_matrix["X0", "X1"]
  
  numerator <- (TP * TN) - (FP * FN)
  denominator <- sqrt(as.double(TP + FP) * as.double(TP + FN) * as.double(TN + FP) * as.double(TN + FN))
  
  # Print intermediate calculations for debugging
  cat("TP:", TP, "TN:", TN, "FP:", FP, "FN:", FN, "\n")
  cat("Numerator:", numerator, "Denominator:", denominator, "\n")
  
  if (is.na(denominator) || denominator == 0) {
    warning("Denominator is zero or NA in MCC calculation")
    return(NA)
  } else {
    return(numerator / denominator)
  }
}

# Function to compute AUC-ROC
compute_auc_roc <- function(df) {
  tryCatch({
    roc_obj <- roc(df$obs, df$p1, quiet = TRUE)
    as.numeric(auc(roc_obj))
  }, error = function(e) {
    warning("Error in AUC-ROC calculation: ", e$message)
    return(NA)
  })
}

compute_all_metrics <- function(df) {
  tryCatch({
    list(
      accuracy = compute_accuracy(df),
      balanced_accuracy = compute_balanced_accuracy(df),
      precision = compute_precision(df),
      recall = compute_recall(df),
      f1_score = compute_f1(df),
      mcc = compute_mcc(df),
      auc_roc = compute_auc_roc(df)
    )
  }, error = function(e) {
    warning("Error in compute_all_metrics: ", e$message)
    return(list(
      accuracy = NA,
      balanced_accuracy = NA,
      precision = NA,
      recall = NA,
      f1_score = NA,
      mcc = NA,
      auc_roc = NA
    ))
  })
}

# Compute metrics for each unique combination of validation, model, subsampling, and set
compute_metrics_by_group <- function(predictions) {
  predictions %>%
    group_by(validation, model, subsampling, set) %>%
    group_modify(~ {
      metrics <- compute_all_metrics(.x)
      # Print group information for debugging
      cat("Group:", paste(c(
        paste("validation:", .y$validation),
        paste("model:", .y$model),
        paste("subsampling:", .y$subsampling),
        paste("set:", .y$set)
      ), collapse = ", "), "\n")
      as_tibble(metrics)
    }) %>%
    ungroup()
}
# Suppress warnings
options(warn = -1)

# Compute metrics
metrics <- compute_metrics_by_group(predictions)

# Restore warnings
options(warn = 0)

# Display the results
print(metrics)

# Write results to CSV
write.csv(metrics, "~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /C.5 Conduct analysis plan/code/4 ml/code/ml_metrics_results.csv", row.names = FALSE)

# Additional diagnostic information
cat("\nUnique values in 'class' column:\n")
print(table(predictions$class))

cat("\nUnique values in 'obs' column:\n")
print(table(predictions$obs))

cat("\nSample of predictions data:\n")
print(head(predictions))

# Check for any infinite or NA values in p0 and p1 columns
cat("\nInfinite or NA values in p0 column:", sum(is.infinite(predictions$p0) | is.na(predictions$p0)), "\n")
cat("Infinite or NA values in p1 column:", sum(is.infinite(predictions$p1) | is.na(predictions$p1)), "\n")

# Print summary statistics for p0 and p1 columns
cat("\nSummary statistics for p0 column:\n")
print(summary(predictions$p0))
cat("\nSummary statistics for p1 column:\n")
print(summary(predictions$p1))