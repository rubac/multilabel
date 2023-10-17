library(tidyverse)
all_single_test_pred <- read_csv("~/Downloads/all_single_test_pred (1).csv")
## make sure accuracy is correct
all_single_test_pred$correct <- ifelse(all_single_test_pred$labels==all_single_test_pred$single_predictions, 1, 0)

prop.table(table(all_single_test_pred$correct))

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

result_df2$concat_text <- result_df$Concatenated_Text


## stopped here. next step is to get the predictions in a similar format --- ten columns with just 1s and 0s


X_factors <- read_csv("Downloads/DF_labels_to_factors.csv")
