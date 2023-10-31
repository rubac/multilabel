library(tidyverse)
single_results <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_100run.csv")
multi_results <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_till1000.csv")

single_results %>%
  summarise(across(accuracy:hamming_loss, list(mean=mean, sd=sd, median=median), na.rm=TRUE))
single_results <- single_results %>% 
  mutate(cond = "single")

multi_results <- multi_results %>% 
  filter(split_index<100) %>% ## first 100 observations, index starts at 0
  mutate(cond = "multi")
multi_results %>%
  summarise(across(accuracy:hamming_loss, list(mean=mean, sd=sd, median=median), na.rm=TRUE))

df.comb <- rbind(single_results, multi_results)

t.test(accuracy~cond, var.equal = F, alternative = "two.sided", data = df.comb)
t.test(hamming_loss~cond, var.equal = F, alternative = "two.sided", data = df.comb)
t.test(zero_one_loss~cond, var.equal = F, alternative = "two.sided", data = df.comb)

