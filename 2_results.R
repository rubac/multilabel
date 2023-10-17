library(tidyverse)
all_single_test_pred <- read_csv("~/Downloads/all_single_test_pred.csv")
test_df_single <- read_csv( "~/bwSyncShare/Multilabel open q/all_single_test.csv")

all_single_test_pred$lfdn <- test_df_single$lfdn

unique_df <- all_single_test_pred %>% 
  distinct(labels, .keep_all = TRUE) %>% 
  select(labels, new_label_1)
print(unique_df)

all_single_test_pred <- all_single_test_pred %>% 
  mutate(char_predictions = case_when(
    single_predictions == 0 ~ "nonresponse",
    single_predictions == 1 ~ "health",
    single_predictions == 2 ~ "time_references",
    single_predictions == 3 ~ "social_network_surrounding",
    single_predictions == 4 ~ "rest",
    single_predictions == 5 ~ "financial_situation",
    single_predictions == 6 ~ "life_situation_living_conditions",
    single_predictions == 7 ~ "job",
    single_predictions == 8 ~ "politics_security_society",
    single_predictions == 9 ~ "life_event"
  ))



## make sure accuracy matches python result
all_single_test_pred$correct <- ifelse(all_single_test_pred$labels==all_single_test_pred$single_predictions, 1, 0)

prop.table(table(all_single_test_pred$correct))

all_single_test_pred$justones <- "1"
all_single_test_pred <- all_single_test_pred %>% 
  select(c(lfdn, text, label, dataset_type, new_label_1,
           labels, single_predictions, char_predictions, justones))

library(reshape2)
wide_pred <- dcast(all_single_test_pred, lfdn ~ char_predictions, value.var = "single_predictions")

wide_pred <- wide_pred %>%
  mutate_all(~ ifelse(. >= 1 & . <= 10, 1, .))

result_df <- all_single_test_pred %>%
  group_by(lfdn) %>%
  summarize(Concatenated_Text = paste(text, collapse = ", "))
wide_pred$text <- result_df$Concatenated_Text

wide_true <- dcast(test_df_single, lfdn ~ new_label_1, value.var = "exp_cond")
wide_true <- wide_true %>%
  mutate_all(~ ifelse(. >= 1 & . <= 10, 1, .))
wide_true$text <- result_df$Concatenated_Text


zero_one_loss <- function(list1, list2) {
  if (length(list1) == length(list2)) {
    num_elements <- length(list1)
    num_matches <- sum(sapply(1:num_elements, function(i) !identical(list1[[i]], list2[[i]])))
    match_percentage <- (num_matches / num_elements) * 100
    
    cat("Number of elements in the first list:", num_elements, "\n")
    cat("Number of nonmatches:", num_matches, "\n")
    cat("zero_one_loss:", match_percentage/100)
  } else {
    cat("Both lists should have the same number of vectors.\n")
  }
}



# Example usage:
list1 <- list(c(0, 1, 1, 0, 1),
              c(1, 0, 0, 1, 0),
              c(0, 0, 1, 1, 1),
              c(1, 0, 1, 0, 1),
              c(0, 1, 1, 0, 0))
list2 <- list(c(0, 1, 1, 0, 1),
              c(1, 0, 1, 1, 0),
              c(0, 0, 1, 1, 1),
              c(1, 0, 1, 0, 1),
              c(0, 1, 1, 0, 0))

zero_one_loss(list1, list2)





hamming_loss <- function(list1, list2) {
  total_mismatches <- 0
  total_elements <- 0
  
  for (i in 1:length(list1)) {
    vec1 <- list1[[i]]
    vec2 <- list2[[i]]
    
    if (length(vec1) != length(vec2)) {
      stop("Vectors in the input lists must have the same length")
    }
    
    mismatches <- sum(vec1 != vec2)
    total_mismatches <- total_mismatches + mismatches
    total_elements <- total_elements + length(vec1)
  }
  
  average_mismatches <- total_mismatches / total_elements
  return(average_mismatches)
}

accuracy <-  function(list1, list2) {
  if (length(list1) == length(list2)) {
    num_elements <- length(list1)
    num_matches <- sum(sapply(1:num_elements, function(i) !identical(list1[[i]], list2[[i]])))
    match_percentage <- (num_matches / num_elements) * 100
    
    cat("Number of elements in the first list:", num_elements, "\n")
    cat("Number of matches:", num_elements - num_matches, "\n")
    cat("accuracy:", 1-(match_percentage/100))
  } else {
    cat("Both lists should have the same number of vectors.\n")
  }
}


# Example usage:
list1 <- list(c(0, 1, 0, 1, 0), c(1, 0, 0, 1, 1))
list2 <- list(c(1, 0, 0, 1, 0), c(0, 1, 1, 1, 0))

result <- hamming_loss(list1, list2)
cat("Hamming loss:", result, "\n")

################################################################################
wide_pred_list <- wide_pred %>% 
  select(-c(lfdn, text))
wide_pred_list <- split(as.matrix(wide_pred_list), 1:nrow(wide_pred_list))
wide_pred_list <- lapply(wide_pred_list, as.vector)
head(wide_pred_list)

wide_true_list <- wide_true %>% 
  select(-c(lfdn, text))
wide_true_list <- split(as.matrix(wide_true_list), 1:nrow(wide_true_list))
wide_true_list <- lapply(wide_true_list, as.vector)
head(wide_true_list)

accuracy(wide_pred_list, wide_true_list)

zero_one_loss(wide_pred_list, wide_true_list)

hamming_loss(wide_pred_list, wide_true_list)
