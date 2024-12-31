library(tidyverse)

# List of dataframes
# Named list of dataframes
sp.xgb.metrics = data.frame(sp.xgb.metrics)
sp.noxgb.metrics = data.frame(sp.noxgb.metrics)
xgb_merge = dplyr::full_join(sp.xgb.metrics, sp.noxgb.metrics)
sp.xgb_merge = xgb_merge %>% dplyr::filter(is.na(error_message)) %>% dplyr::mutate(validation = "spatial")
tm.metrics = tm.metrics %>% dplyr::mutate(validation = "temporal")

M = dplyr::full_join(sp.xgb_merge, tm.metrics)
M = M %>% dplyr::filter(is.na(error_message)) %>% dplyr::select(validation, model_name, subsample, predictor_sets, everything(), -runtime, -error_message)


# View the combined dataframe
glimpse(M)


# export combined spatial and temporal results
write.csv(M, "~/1 NHTSA Ped Crash.local-results/upload/ml-results-export.csv", row.names = FALSE)