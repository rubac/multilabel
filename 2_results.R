library(tidyverse)
all_single_test_pred <- read_csv("Downloads/all_single_test_pred.csv")
all_single_test_pred$justones <- 1
df <- pivot_wider(all_single_test_pred, names_from = new_label_1, values_from = justones)
df[is.na(df)] <- 0

result_df <- df %>%
  group_by(lfdn) %>%
  summarize(Concatenated_Text = paste(text, collapse = ", "))

result_df2 <- df %>%
  group_by(lfdn) %>%
  reframe(social_network_surrounding = max(social_network_surrounding),
          financial_situation =max(financial_situation),
          health = max(health),
          job =max(job),
          rest = max(rest),
          nonresponse = max(nonresponse),
          life_situation_living_conditions = max(life_situation_living_conditions),
          time_references = max(time_references),
          politics_security_society = max(politics_security_society),
          life_event = max(life_event))


X_factors <- read_csv("Downloads/DF_labels_to_factors.csv")
