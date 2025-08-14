library(tidyverse)
setwd("~/bwSyncShare/Multilabel_open_q/results")
multi_results <- read_csv("test_results_multi.csv")
multi_to_single <- read_csv("test_results_multi_to_single.csv")
single_sample <- read_csv("test_results_single_sampled_3-5-10box.csv")
concat_sample <- read_csv("test_results_concat_sample.csv")

## need to correct hamming loss for some as there was an error in the computation in the python script
## need to divide by number of samples in test df
concat_sample$hamming_loss <- concat_sample$hamming_loss / round(553*0.2) # 0.2 is size of test df
multi_results$hamming_loss <- multi_results$hamming_loss/round(553*0.2)

# multi_results <- read_csv("Revision results/Multi_rev.csv")
# multi_to_single <- read_csv("Revision results/Multi_to_single_rev.csv")
# single_sample <- read_csv("Revision results/Single_rev.csv")
# concat_sample <- read_csv("Revision results/Single_to_multi_rev.csv")



# Define a function that takes a dataset and its name, and performs the summarise and mutate operations
summarize_and_add_cond <- function(df, name) {
  summarized_df <- df %>%
    mutate(cond = name)
  return(summarized_df)
}

multi_summary <- summarize_and_add_cond(multi_results, "Single-box / Multi-label")
multi_single_summary <- summarize_and_add_cond(multi_to_single, "Disagg. Single-box / Single-label")
single_sampled_summary <- summarize_and_add_cond(single_sample, "Multi-box / Single-label")
concat_sampled_summary <- summarize_and_add_cond(concat_sample, "Concat. Multi-box / Multi-label")

#concat_sampled_summary <- concat_sampled_summary %>%
#  rename(
    #f1_macro = macro_f1,
#         accuracy = accuracy_av)
#multi_summary <- multi_summary %>% 
#  rename(f1_macro = macro_f1)

df.comb <- rbind(single_sampled_summary,
                 multi_summary,
                 multi_single_summary,
                 concat_sampled_summary)

df.comb.plot = df.comb %>% 
  group_by(cond) %>% 
  summarise(acc_se = sd(accuracy) / sqrt(length(accuracy)),
            zer_se = sd(zero_one_loss) / sqrt(length(zero_one_loss)),
            ham_se = sd(hamming_loss) / sqrt(length(hamming_loss)),
            #f1_se = sd(f1_macro) / sqrt(length(f1_macro)),
            acc_m = mean(accuracy),
            zer_m = mean(zero_one_loss),
            ham_m = mean(hamming_loss)
            #f1_m = mean(f1_macro)
            )

df.comb.plot$cond <- fct_relevel(df.comb.plot$cond,
                                 "Single-box / Multi-label",
                                 "Concat. Multi-box / Multi-label",
                                 "Multi-box / Single-label",
                                 "Disagg. Single-box / Single-label")

zer_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=zer_m), stat="identity", fill="gray") + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=zer_m-1.96*zer_se, ymax=zer_m+1.96*zer_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # Adjust the y-axis font size
    axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
    legend.text = element_text(size = 14),       # Adjust the legend font size
    axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Zero-one-loss") 
zer_plot

ham_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x=cond, y=ham_m), stat="identity", fill="gray") + # Adjust alpha here
  geom_errorbar(aes(x=cond, ymin=ham_m-1.96*ham_se, ymax=ham_m+1.96*ham_se), width=.5, alpha=1, size=.5) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1), # Adjust the y-axis font size
    axis.title.x = element_text(size = 16),      # Adjust the x-axis label font size
    legend.text = element_text(size = 14),       # Adjust the legend font size
    axis.title.y = element_text(size = 16),      # Adjust the y-axis label font size
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Hamming-loss") 
ham_plot


ham_plot2 <- ggplot(df.comb.plot[df.comb.plot$cond == "Single-box / Multi-label" | df.comb.plot$cond == "Concat. Multi-box / Multi-label", ]) +
  geom_bar(aes(x = cond, y = ham_m), stat = "identity", fill = "gray") +
  geom_errorbar(aes(x = cond, ymin = ham_m - 1.96*ham_se, ymax = ham_m + 1.96*ham_se), width = .5, alpha = 1, size = .5) +
  coord_cartesian(ylim = c(0.0069, 0.0075)) +  # Zoom into the range without removing data
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14), 
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  labs(x = "", y = "Hamming-loss")

ham_plot2

library(gridExtra)

# Combine the plots side by side
comb_plot <- grid.arrange(zer_plot, ham_plot, ncol = 2)

ggsave("comb_plot.eps", comb_plot, width = 10, height = 6)
ggsave("hamm_loss.eps", ham_plot2, width = 10, height = 6)





library(tidyverse)
# If not installed: install.packages("ragg")
library(ragg)
library(gridExtra)

setwd("~/bwSyncShare/Multilabel_open_q/results")

multi_results     <- read_csv("test_results_multi.csv")
multi_to_single   <- read_csv("test_results_multi_to_single.csv")
single_sample     <- read_csv("test_results_single_sampled_3-5-10box.csv")
concat_sample     <- read_csv("test_results_concat_sample.csv")

## Correct Hamming loss (divide by test-set size)
## (Your test size was 20% of 553; adjust here if needed)
test_n <- round(553 * 0.2)
concat_sample$hamming_loss <- concat_sample$hamming_loss / test_n
multi_results$hamming_loss <- multi_results$hamming_loss / test_n

summarize_and_add_cond <- function(df, name) {
  df %>% mutate(cond = name)
}

multi_summary           <- summarize_and_add_cond(multi_results, "Single-box / Multi-label")
multi_single_summary    <- summarize_and_add_cond(multi_to_single, "Disagg. Single-box / Single-label")
single_sampled_summary  <- summarize_and_add_cond(single_sample, "Multi-box / Single-label")
concat_sampled_summary  <- summarize_and_add_cond(concat_sample, "Concat. Multi-box / Multi-label")

df.comb <- bind_rows(single_sampled_summary,
                     multi_summary,
                     multi_single_summary,
                     concat_sampled_summary)

df.comb.plot <- df.comb %>%
  group_by(cond) %>%
  summarise(
    acc_se = sd(accuracy) / sqrt(length(accuracy)),
    zer_se = sd(zero_one_loss) / sqrt(length(zero_one_loss)),
    ham_se = sd(hamming_loss) / sqrt(length(hamming_loss)),
    acc_m  = mean(accuracy),
    zer_m  = mean(zero_one_loss),
    ham_m  = mean(hamming_loss),
    .groups = "drop"
  )

df.comb.plot$cond <- forcats::fct_relevel(
  df.comb.plot$cond,
  "Single-box / Multi-label",
  "Concat. Multi-box / Multi-label",
  "Multi-box / Single-label",
  "Disagg. Single-box / Single-label"
)

base_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

zer_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x = cond, y = zer_m), stat = "identity", fill = "gray50") +
  geom_errorbar(aes(x = cond, ymin = zer_m - 1.96 * zer_se, ymax = zer_m + 1.96 * zer_se),
                width = .5, size = .5) +
  base_theme +
  labs(x = "", y = "Zero-one loss")

ham_plot <- ggplot(df.comb.plot) +
  geom_bar(aes(x = cond, y = ham_m), stat = "identity", fill = "gray50") +
  geom_errorbar(aes(x = cond, ymin = ham_m - 1.96 * ham_se, ymax = ham_m + 1.96 * ham_se),
                width = .5, size = .5) +
  base_theme +
  labs(x = "", y = "Hamming loss")

ham_plot2 <- ggplot(
  df.comb.plot %>% filter(cond %in% c("Single-box / Multi-label", "Concat. Multi-box / Multi-label"))
) +
  geom_bar(aes(x = cond, y = ham_m), stat = "identity", fill = "gray50") +
  geom_errorbar(aes(x = cond, ymin = ham_m - 1.96 * ham_se, ymax = ham_m + 1.96 * ham_se),
                width = .5, size = .5) +
  coord_cartesian(ylim = c(0.0069, 0.0075)) +
  base_theme +
  labs(x = "", y = "Hamming loss")

# Combine plots side-by-side
comb_plot <- grid.arrange(zer_plot, ham_plot, ncol = 2)

# ---- HIGH-RES EXPORTS FOR PRODUCTION ----
# LINE DRAWINGS: 1200 dpi TIFF (preferred by journals)
ggsave("Figure_1.tif", plot = comb_plot,
       device = ragg::agg_tiff, dpi = 1200,
       width = 10, height = 6, units = "in", compression = "lzw")

ggsave("Figure_S1.tif", plot = ham_plot2,
       device = ragg::agg_tiff, dpi = 1200,
       width = 10, height = 6, units = "in", compression = "lzw")

# OPTIONAL: 300 dpi JPEG versions (halftones/photos requirement)
# (Comment out if you don't need JPEGs)
ggsave("Figure_1.jpg", plot = comb_plot,
       device = ragg::agg_jpeg, dpi = 300,
       width = 10, height = 6, units = "in")

ggsave("Figure_S1.jpg", plot = ham_plot2,
       device = ragg::agg_jpeg, dpi = 300,
       width = 10, height = 6, units = "in")

# (If you still want EPS for archival, you can keep your original ggsave calls too.)
