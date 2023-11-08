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

df.comb.plot = df.comb %>% 
  group_by(cond) %>% 
  summarise(acc_se = sd(accuracy) / sqrt(length(accuracy)),
        zer_se = sd(zero_one_loss) / sqrt(length(zero_one_loss)),
        ham_se = sd(hamming_loss) / sqrt(length(hamming_loss)),
        acc_m = mean(accuracy),
        zer_m = mean(zero_one_loss),
        ham_m = mean(hamming_loss))


zer_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=zer_m), stat="identity", fill="gray", alpha=0.7) + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=zer_m-zer_se, ymax=zer_m+zer_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Condition", y = "Zero-one-loss") +
  scale_x_discrete(labels = c("Multi-label", "Single-label"))

acc_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=acc_m), stat="identity", fill="gray", alpha=0.7) + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=acc_m-acc_se, ymax=acc_m+acc_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Condition", y = "Accuracy") + 
  scale_x_discrete(labels = c("Multi-label", "Single-label"))

ham_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=ham_m), stat="identity", fill="gray", alpha=0.7) + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=ham_m-ham_se, ymax=ham_m+ham_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = "Condition", y = "Hamming-loss") +
  scale_x_discrete(labels = c("Multi-label", "Single-label"))


library(gridExtra)
combined_plot <- grid.arrange(acc_plot, ham_plot, zer_plot, ncol = 3)
ggsave("~/bwSyncShare/Multilabel open q/results/combined_plot.eps", combined_plot, device = cairo_ps)
