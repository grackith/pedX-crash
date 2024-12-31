library(tidyverse)
library(ggplot2)
library(readr)

misclass <- read_csv("~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/misclass-in-both-vals-2024-11-13.csv")

misclass %>%
  group_by(idx) %>%
  mutate(avg_prob_diff = mean(prb_dff)) %>%
  arrange(desc(avg_prob_diff)) %>%
  ungroup()

# > dim(misclass)
# [1] 4100   26

table(misclass$errr_ty, misclass$vldtn_t)
# huge skew in mis classification types by validation strategy.
#                  spatial  temporal
# False Negative     874      340
# False Positive    1176     1710

misclass %>%
  ggplot(aes(x=vldtn_t, fill=errr_ty)) +
  geom_bar(position="dodge") +
  labs(x="Validation Type", y="Count", fill="Error Type") +
  theme_minimal()

misclass_85th <- misclass %>%
  group_by(idx, errr_ty, vldtn_t) %>%
  mutate(avg_prob_diff = mean(prb_dff)) %>%
  ungroup() %>%
  filter(avg_prob_diff >= quantile(avg_prob_diff, 0.85)) %>%
  arrange(desc(avg_prob_diff))

dim(misclass_85th)
# [1] 4100   27



# spatial temporal
# False Negative     874      340
# False Positive    1176     1710

#Dp: more false alarms in temporal validation strategy
#

# Export to CSV
#write_csv(misclass_85th, "~/Library/CloudStorage/GoogleDrive-gad9515@nyu.edu/Shared drives/HumanFuel/1 NHTSA Ped Crash Sept 2023 /paper/code/misclass-85th-percentile.csv")