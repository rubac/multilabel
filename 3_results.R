library(tidyverse)
multi_results <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_multi.csv")
multi_to_single <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_multi_to_single.csv")
single_all <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_all.csv")
single_sample <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_sampled_3-5-10box.csv")
single_three <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_threebox.csv")
single_five <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_fivebox.csv")
single_ten <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_single_tenbox.csv")
concat_all <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_all.csv")
concat_three <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_threebox.csv")
concat_five <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_fivebox.csv")
concat_ten <- read_csv("~/bwSyncShare/Multilabel open q/results/test_results_concat_tenbox.csv")

## need to correct hamming loss for some as there was an error in the computation in the python script
## need to divide by number of samples in test df
xxx <- read_csv("~/bwSyncShare/Multilabel open q/data/all_concat.csv")
c_alltestdf <- round(length(xxx$lfdn)*0.2) # 0.2 is size of test df
c_threeboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="three box"])*0.2) # 0.2 is size of test df
c_fiveboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="five box"])*0.2) # 0.2 is size of test df
c_tenboxtestdf <- round(length(xxx$lfdn[xxx$exp_cond=="ten box"])*0.2) # 0.2 is size of test df
# 
# zzz <- read_csv("~/bwSyncShare/Multilabel open q/data/all_single.csv")
# s_alltestdf <- round(length(zzz$lfdn)*0.2) # 0.2 is size of test df
# s_sampletest<- round(553*0.2)/2.7 # 0.2 is size of test df; 553 size of sample of single label data
# s_threeboxtestdf <- round(length(zzz$lfdn[zzz$exp_cond=="three box"])*0.2)/2.7 # 0.2 is size of test df
# s_fiveboxtestdf <- round(length(zzz$lfdn[zzz$exp_cond=="five box"])*0.2)/2.7 # 0.2 is size of test df
# s_tenboxtestdf <- round(length(zzz$lfdn[zzz$exp_cond=="ten box"])*0.2)/2.7 # 0.2 is size of test df


yyy <- read_csv("~/bwSyncShare/Multilabel open q/data/Happy_onebox.csv")
oneboxdf <- round(length(yyy$lfdn)*0.2) # 0.2 is size of test df

concat_all$hamming_loss <- concat_all$hamming_loss/c_alltestdf
concat_three$hamming_loss <- concat_three$hamming_loss/c_threeboxtestdf
concat_five$hamming_loss <- concat_five$hamming_loss/c_fiveboxtestdf
concat_ten$hamming_loss <- concat_ten$hamming_loss/c_tenboxtestdf

multi_results$hamming_loss <- multi_results$hamming_loss/oneboxdf
# multi_to_single$hamming_loss <- multi_to_single$hamming_loss/oneboxdf

# single_all$hamming_loss <- single_all$hamming_loss/s_alltestdf
# single_three$hamming_loss <- single_three$hamming_loss/s_threeboxtestdf
# single_five$hamming_loss <- single_five$hamming_loss/s_fiveboxtestdf
# single_ten$hamming_loss <- single_ten$hamming_loss/s_tenboxtestdf
# single_sample$hamming_loss <- single_sample$hamming_loss/s_sampletest

# Define a function that takes a dataset and its name, and performs the summarise and mutate operations
summarize_and_add_cond <- function(df, name) {
  summarized_df <- df %>%
    mutate(cond = name)
  return(summarized_df)
}

multi_summary <- summarize_and_add_cond(multi_results, "Multi label")
multi_single_summary <- summarize_and_add_cond(multi_to_single, "Multi label as single label")
single_sampled_summary <- summarize_and_add_cond(single_sample, "Single label, random sample")
single_three_summary <- summarize_and_add_cond(single_three, "Single label, three boxes")
single_five_summary <- summarize_and_add_cond(single_five, "Single label, five boxes")
single_ten_summary <- summarize_and_add_cond(single_ten, "Single label, ten boxes")
single_all_summary <- summarize_and_add_cond(single_all, "Single label, all")
concat_three_summary <- summarize_and_add_cond(concat_three, "Concat. multi label, three boxes")
concat_five_summary <- summarize_and_add_cond(concat_five, "Concat. multi label, five boxes")
concat_ten_summary <- summarize_and_add_cond(concat_ten, "Concat. multi label, ten boxes")
concat_all_summary <- summarize_and_add_cond(concat_all, "Concat. multi label, all")

df.comb <- rbind(multi_summary,
                 multi_single_summary,
                 single_all_summary,
                 single_sampled_summary,
                 single_three_summary,
                 single_five_summary,
                 single_ten_summary,
                 concat_all_summary,
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
zer_plot

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
ham_plot

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
