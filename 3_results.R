library(tidyverse)
# multi_results <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_multi.csv")
single_sample <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_samesize - sample from 3-5-10.csv")
single_three <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_samesize - three box.csv")
single_five <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_samesize - five box.csv")
single_ten <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_samesize - ten box.csv")
concat_three <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_threebox.csv")
concat_five <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_fivebox.csv")
concat_ten <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_tenbox.csv")

## need to correct hamming loss for some as there was an error in the computation in the python script
## need to divide by number of samples in test df
xxx <- read_csv("~/bwSyncShare/Multilabel open q/data/all_concat.csv")
threeboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="three box"])*0.2) # 0.2 is size of test df
fiveboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="five box"])*0.2) # 0.2 is size of test df
tenboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="ten box"])*0.2) # 0.2 is size of test df

concat_three$hamming_loss <- concat_three$hamming_loss/threeboxtestdf
concat_five$hamming_loss <- concat_five$hamming_loss/fiveboxtestdf
concat_ten$hamming_loss <- concat_ten$hamming_loss/tenboxtestdf

# Define a function that takes a dataset and its name, and performs the summarise and mutate operations
summarize_and_add_cond <- function(df, name) {
  summarized_df <- df %>%
    mutate(cond = name)
  return(summarized_df)
}

# multi_summary <- summarize_and_add_cond(multi_results, "Multi label")
single_sampled_summary <- summarize_and_add_cond(single_sample, "Single label, random sample")
single_three_summary <- summarize_and_add_cond(single_three, "Single label, three boxes")
single_five_summary <- summarize_and_add_cond(single_five, "Single label, five boxes")
single_ten_summary <- summarize_and_add_cond(single_ten, "Single label, ten boxes")
concat_three_summary <- summarize_and_add_cond(concat_three, "Concat. multi label, three boxes")
concat_five_summary <- summarize_and_add_cond(concat_five, "Concat. multi label, five boxes")
concat_ten_summary <- summarize_and_add_cond(concat_ten, "Concat. multi label, ten boxes")


df.comb <- rbind(single_sampled_summary,
                 single_three_summary,
                 single_five_summary,
                 single_ten_summary,
                 concat_three_summary,
                 concat_five_summary,
                 concat_ten_summary)




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
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Adjust the y-axis font size
    axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
    legend.text = element_text(size = 14),       # Adjust the legend font size
    axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Zero-one-loss") 


#+
#  scale_x_discrete(labels = c("Multi-label", "Single-label (five boxes)", "Single-label (ten boxes)", "Single-label (three boxes)"))

acc_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=acc_m), stat="identity", fill="gray", alpha=0.7) + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=acc_m-acc_se, ymax=acc_m+acc_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Adjust the y-axis font size
    axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
    legend.text = element_text(size = 14),       # Adjust the legend font size
    axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Accuracy") 

#+ 
#  scale_x_discrete(labels = c("Multi-label", "Single-label (five boxes)", "Single-label (ten boxes)", "Single-label (three boxes)"))

ham_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=ham_m), stat="identity", fill="gray", alpha=0.7) + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=ham_m-ham_se, ymax=ham_m+ham_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1), # Adjust the y-axis font size
    axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
    legend.text = element_text(size = 14),       # Adjust the legend font size
    axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Hamming-loss") 

#+
 # scale_x_discrete(labels = c("Multi-label", "Single-label (five boxes)", "Single-label (ten boxes)", "Single-label (three boxes)"))

library(gridExtra)
combined_plot_two <- grid.arrange(ham_plot, zer_plot, ncol = 2)

hist(single_three$zero_one_loss)
hist(single_five$zero_one_loss)
hist(single_ten$zero_one_loss)

hist(single_three$hamming_loss)
hist(single_five$hamming_loss)
hist(single_ten$hamming_loss)

hist(concat_three$zero_one_loss)
hist(concat_five$zero_one_loss)
hist(concat_ten$zero_one_loss)

hist(concat_three$hamming_loss)
hist(concat_five$hamming_loss)
hist(concat_ten$hamming_loss)



#ggsave("~/bwSyncShare/Multilabel open q/results/3_combined_plot_two.eps", combined_plot_two, device = cairo_ps, width = 10, height = 6)
# ggsave("~/bwSyncShare/Multilabel open q/results/3_combined_plot.eps", combined_plot, device = cairo_ps, width = 10, height = 6)
#ggsave("~/bwSyncShare/Multilabel open q/results/3_combined_plot_SM.eps", combined_plot_SM, device = cairo_ps, width = 10, height = 6)
