# Conservation Campaign A/B Test Analysis
# R Implementation

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(knitr)
  library(gridExtra)
  library(scales)
  library(effectsize)
  library(pwr)
  library(tidyr) 
  library(grid)  
})

cat(" CONSERVATION CAMPAIGN A/B TEST ANALYSIS \n\n")

#1. DATA LOADING AND INITIAL EXPLORATION
cat("1. LOADING AND EXPLORING DATA\n")
cat(rep("=", 50), "\n")

#Set file path
file_path <- "D:/TESTING DATA FOR A CONSERVATION CAMPAIGN/conservation_dataset.csv"
output_folder <- "D:/TESTING DATA FOR A CONSERVATION CAMPAIGN/"

#Load the dataset
tryCatch({
  df <- read_csv(file_path, show_col_types = FALSE)
  cat("✓ Data loaded successfully!\n")
  cat("Dataset shape:", nrow(df), "rows x", ncol(df), "columns\n")
}, error = function(e) {
  cat("Error loading file:", e$message, "\n")
  stop("Please check the file path.")
})

#Basic info about the dataset
cat("\nDataset Info:\n")
cat("- Total users:", format(nrow(df), big.mark = ","), "\n")
cat("- Columns:", paste(names(df), collapse = ", "), "\n")

#Clean column names (remove spaces and special characters)
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub(":", "", names(df))

cat("- Cleaned column names:", paste(names(df), collapse = ", "), "\n")

#Check data types
cat("\nData types:\n")
str(df)

#Check for missing values
missing_count <- sum(is.na(df))
if (missing_count > 0) {
  cat("\n Missing values found:", missing_count, "\n")
  sapply(df, function(x) sum(is.na(x)))
} else {
  cat("\n✓ No missing values found\n")
}

#2. DATA PREPROCESSING
cat("\n\n2. DATA PREPROCESSING\n")
cat(rep("=", 50), "\n")

#Display unique values in key columns
cat("Unique values in key columns:\n")
cat("- Message types:", paste(unique(df$message_type), collapse = ", "), "\n")
cat("- Engagement:", paste(unique(df$engaged), collapse = ", "), "\n")
if ("most_engagement_day" %in% names(df)) {
  cat("- Days:", paste(unique(df$most_engagement_day), collapse = ", "), "\n")
}

#Convert logical if needed
if (is.character(df$engaged)) {
  df$engaged <- as.logical(df$engaged)
}

#3. BASIC STATISTICS AND GROUP COMPARISON
cat("\n\n3. BASIC STATISTICS\n")
cat(rep("=", 50), "\n")

#Group sizes
group_sizes <- df %>% 
  count(message_type) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("Group sizes:\n")
for(i in 1:nrow(group_sizes)) {
  cat("- ", group_sizes$message_type[i], ": ", 
      format(group_sizes$n[i], big.mark = ","), 
      " users (", group_sizes$percentage[i], "%)\n", sep = "")
}

#Engagement statistics by group
engagement_stats <- df %>%
  group_by(message_type) %>%
  summarise(
    Total_Users = n(),
    Engaged_Users = sum(engaged),
    Engagement_Rate = mean(engaged),
    .groups = 'drop'
  )

cat("\nEngagement Statistics by Group:\n")
print(engagement_stats)

#Calculate key metrics for each group
control_data <- df %>% filter(message_type == "Generic")
treatment_data <- df %>% filter(message_type == "Personalized")

control_rate <- mean(control_data$engaged)
treatment_rate <- mean(treatment_data$engaged)
lift <- ((treatment_rate - control_rate) / control_rate) * 100

cat("\n📊 KEY METRICS:\n")
cat("Control (Generic) engagement rate:", sprintf("%.4f", control_rate), 
    "(", sprintf("%.2f", control_rate * 100), "%)\n")
cat("Treatment (Personalized) engagement rate:", sprintf("%.4f", treatment_rate), 
    "(", sprintf("%.2f", treatment_rate * 100), "%)\n")
cat("Absolute difference:", sprintf("%.4f", treatment_rate - control_rate), "\n")
cat("Relative lift:", sprintf("%.2f", lift), "%\n")

#4. STATISTICAL SIGNIFICANCE TESTING
cat("\n\n4. STATISTICAL SIGNIFICANCE TESTING\n")
cat(rep("=", 50), "\n")

#A. Chi-square test for independence
cat("A. Chi-square Test for Independence\n")
cat(rep("-", 40), "\n")

contingency_table <- table(df$message_type, df$engaged)
cat("Contingency Table:\n")
print(contingency_table)

chi2_test <- chisq.test(contingency_table)
cat("\nChi-square test results:\n")
cat("- Chi-square statistic:", sprintf("%.4f", chi2_test$statistic), "\n")
cat("- p-value:", sprintf("%.6f", chi2_test$p.value), "\n")
cat("- Degrees of freedom:", chi2_test$parameter, "\n")

#B. Two-proportion test using prop.test
cat("\nB. Two-Proportion Test (prop.test)\n")
cat(rep("-", 40), "\n")

control_successes <- sum(control_data$engaged)
control_total <- nrow(control_data)
treatment_successes <- sum(treatment_data$engaged)
treatment_total <- nrow(treatment_data)

prop_test <- prop.test(c(control_successes, treatment_successes), 
                       c(control_total, treatment_total))

cat("Two-proportion test results:\n")
cat("- Chi-square statistic:", sprintf("%.4f", prop_test$statistic), "\n")
cat("- p-value:", sprintf("%.6f", prop_test$p.value), "\n")

#C. Effect size (Cohen's h)
cat("\nC. Effect Size (Cohen's h)\n")
cat(rep("-", 40), "\n")

#Calculate Cohen's h manually
cohens_h <- 2 * (asin(sqrt(treatment_rate)) - asin(sqrt(control_rate)))
cat("Cohen's h (effect size):", sprintf("%.4f", cohens_h), "\n")

#Interpret effect size
effect_interpretation <- ifelse(abs(cohens_h) < 0.2, "small",
                                ifelse(abs(cohens_h) < 0.5, "medium", "large"))
cat("Effect size interpretation:", effect_interpretation, "\n")

#5. CONFIDENCE INTERVALS
cat("\n\n5. CONFIDENCE INTERVALS\n")
cat(rep("=", 50), "\n")

#Extract confidence intervals from prop.test
ci_matrix <- prop_test$conf.int
cat("95% Confidence Intervals:\n")
cat("- Difference in proportions: [", sprintf("%.4f", ci_matrix[1]), 
    ", ", sprintf("%.4f", ci_matrix[2]), "]\n", sep = "")

#Individual confidence intervals
control_ci <- binom.test(control_successes, control_total)$conf.int
treatment_ci <- binom.test(treatment_successes, treatment_total)$conf.int

cat("- Control (Generic): [", sprintf("%.4f", control_ci[1]), 
    ", ", sprintf("%.4f", control_ci[2]), "]\n", sep = "")
cat("- Treatment (Personalized): [", sprintf("%.4f", treatment_ci[1]), 
    ", ", sprintf("%.4f", treatment_ci[2]), "]\n", sep = "")

#6. POWER ANALYSIS
cat("\n\n6. POWER ANALYSIS\n")
cat(rep("=", 50), "\n")

#Calculate power for two-proportion test
effect_size_pwr <- ES.h(treatment_rate, control_rate)
n1 <- control_total
n2 <- treatment_total
h <- n1 / (n1 + n2)

power_result <- pwr.2p.test(h = effect_size_pwr, n = min(n1, n2), 
                            sig.level = 0.05, power = NULL)

cat("Power analysis results:\n")
cat("- Effect size (h):", sprintf("%.4f", effect_size_pwr), "\n")
cat("- Statistical power:", sprintf("%.4f", power_result$power), 
    "(", sprintf("%.1f", power_result$power * 100), "%)\n")

#7. ADDITIONAL ANALYSIS
cat("\n\n7. ADDITIONAL ANALYSIS\n")
cat(rep("=", 50), "\n")

#A. Analysis by day of week
if ("most_engagement_day" %in% names(df)) {
  cat("A. Engagement by Day of Week\n")
  cat(rep("-", 30), "\n")
  
  day_analysis <- df %>%
    group_by(most_engagement_day, message_type) %>%
    summarise(engagement_rate = mean(engaged), .groups = 'drop') %>%
    pivot_wider(names_from = message_type, values_from = engagement_rate)
  
  print(day_analysis)
} else {
  cat("A. Day of week data not available in this dataset\n")
}

#B. Analysis by hour
if ("most_engagement_hour" %in% names(df)) {
  cat("\nB. Engagement by Hour of Day\n")
  cat(rep("-", 30), "\n")
  
  hour_stats <- df %>%
    group_by(message_type) %>%
    summarise(
      count = n(),
      mean_hour = mean(most_engagement_hour),
      sd_hour = sd(most_engagement_hour),
      min_hour = min(most_engagement_hour),
      q25_hour = quantile(most_engagement_hour, 0.25),
      median_hour = median(most_engagement_hour),
      q75_hour = quantile(most_engagement_hour, 0.75),
      max_hour = max(most_engagement_hour),
      .groups = 'drop'
    )
  
  print(hour_stats)
} else {
  cat("\nB. Hour of day data not available in this dataset\n")
}

#C. Messages seen analysis
if ("total_messages_seen" %in% names(df)) {
  cat("\nC. Total Messages Seen Analysis\n")
  cat(rep("-", 30), "\n")
  
  messages_stats <- df %>%
    group_by(message_type) %>%
    summarise(
      count = n(),
      mean_messages = mean(total_messages_seen),
      sd_messages = sd(total_messages_seen),
      min_messages = min(total_messages_seen),
      q25_messages = quantile(total_messages_seen, 0.25),
      median_messages = median(total_messages_seen),
      q75_messages = quantile(total_messages_seen, 0.75),
      max_messages = max(total_messages_seen),
      .groups = 'drop'
    )
  
  print(messages_stats)
  
  #Wilcoxon test for messages seen
  wilcox_test <- wilcox.test(total_messages_seen ~ message_type, data = df)
  cat("\nWilcoxon rank-sum test for messages seen:\n")
  cat("- W statistic:", sprintf("%.2f", wilcox_test$statistic), "\n")
  cat("- p-value:", sprintf("%.6f", wilcox_test$p.value), "\n")
} else {
  cat("\nC. Total messages seen data not available in this dataset\n")
}

#8. CREATE AND SAVE VISUALIZATIONS
cat("\n\n8. CREATING AND SAVING VISUALIZATIONS\n")
cat(rep("=", 50), "\n")

#Set theme for plots
theme_set(theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                  axis.text = element_text(size = 11),
                  axis.title = element_text(size = 12)))

#Plot 1: Engagement rates by group
p1 <- engagement_stats %>%
  ggplot(aes(x = message_type, y = Engagement_Rate, fill = message_type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Engagement_Rate)), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Generic" = "#FF6B6B", "Personalized" = "#4ECDC4")) +
  labs(title = "Engagement Rate by Message Type",
       x = "Message Type",
       y = "Engagement Rate") +
  theme(legend.position = "none") +
  ylim(0, max(engagement_stats$Engagement_Rate) * 1.15)

#Plot 2: Sample sizes
p2 <- group_sizes %>%
  ggplot(aes(x = message_type, y = n, fill = message_type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = format(n, big.mark = ",")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Generic" = "#FF6B6B", "Personalized" = "#4ECDC4")) +
  scale_y_continuous(labels = comma_format()) +
  labs(title = "Sample Sizes by Group",
       x = "Message Type",
       y = "Number of Users") +
  theme(legend.position = "none")

#Plot 3: Engagement by day of week (if available)
if ("most_engagement_day" %in% names(df)) {
  day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  
  p3 <- df %>%
    mutate(most_engagement_day = factor(most_engagement_day, levels = day_order)) %>%
    group_by(most_engagement_day, message_type) %>%
    summarise(engagement_rate = mean(engaged), .groups = 'drop') %>%
    ggplot(aes(x = most_engagement_day, y = engagement_rate, fill = message_type)) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("Generic" = "#FF6B6B", "Personalized" = "#4ECDC4")) +
    labs(title = "Engagement Rate by Day of Week",
         x = "Day of Week",
         y = "Engagement Rate",
         fill = "Message Type") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1))
} else {
  #Alternative plot if day data not available
  p3 <- engagement_stats %>%
    ggplot(aes(x = message_type, y = Engagement_Rate, fill = message_type)) +
    geom_col(alpha = 0.8, width = 0.6) +
    scale_fill_manual(values = c("Generic" = "#FF6B6B", "Personalized" = "#4ECDC4")) +
    labs(title = "Engagement Rate Comparison",
         x = "Message Type",
         y = "Engagement Rate") +
    theme(legend.position = "none")
}

#Plot 4: Distribution of messages seen (if available)
if ("total_messages_seen" %in% names(df)) {
  p4 <- df %>%
    filter(total_messages_seen <= quantile(total_messages_seen, 0.95)) %>%  #Remove extreme outliers for better visualization
    ggplot(aes(x = total_messages_seen, fill = message_type)) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    scale_fill_manual(values = c("Generic" = "#FF6B6B", "Personalized" = "#4ECDC4")) +
    labs(title = "Distribution of Total Messages Seen",
         x = "Total Messages Seen",
         y = "Frequency",
         fill = "Message Type") +
    scale_y_continuous(labels = comma_format())
} else {
  # Alternative plot if messages data not available
  engagement_counts <- df %>%
    count(message_type, engaged) %>%
    mutate(engaged = ifelse(engaged, "Engaged", "Not Engaged"))
  
  p4 <- engagement_counts %>%
    ggplot(aes(x = message_type, y = n, fill = engaged)) +
    geom_col(alpha = 0.8, position = "dodge") +
    scale_fill_manual(values = c("Not Engaged" = "#FFB3B3", "Engaged" = "#B3E5E0")) +
    scale_y_continuous(labels = comma_format()) +
    labs(title = "Engagement Distribution by Group",
         x = "Message Type",
         y = "Count",
         fill = "Status")
}

#Combine plots and save
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, 
                              top = textGrob("Conservation Campaign A/B Test Results", 
                                             gp = gpar(fontsize = 16, fontface = "bold")))

#Save the combined plot
ggsave(filename = paste0(output_folder, "conservation_ab_test_results_R.png"), 
       plot = combined_plot, width = 15, height = 12, dpi = 300)

cat(" Combined visualization saved to:", paste0(output_folder, "conservation_ab_test_results_R.png"), "\n")

#Save individual plots as well
ggsave(filename = paste0(output_folder, "engagement_rates_R.png"), 
       plot = p1, width = 8, height = 6, dpi = 300)
ggsave(filename = paste0(output_folder, "sample_sizes_R.png"), 
       plot = p2, width = 8, height = 6, dpi = 300)
ggsave(filename = paste0(output_folder, "engagement_by_day_R.png"), 
       plot = p3, width = 10, height = 6, dpi = 300)
ggsave(filename = paste0(output_folder, "messages_distribution_R.png"), 
       plot = p4, width = 10, height = 6, dpi = 300)

cat(" Individual plots also saved to the same folder\n")

#9. RESULTS INTERPRETATION
cat("\n\n9. RESULTS INTERPRETATION\n")
cat(rep("=", 50), "\n")

alpha <- 0.05
p_value <- prop_test$p.value

cat(" STATISTICAL SIGNIFICANCE (α =", alpha, "):\n")

if (p_value < alpha) {
  significance <- "STATISTICALLY SIGNIFICANT"
  decision <- "REJECT the null hypothesis"
} else {
  significance <- "NOT STATISTICALLY SIGNIFICANT" 
  decision <- "FAIL TO REJECT the null hypothesis"
}

cat("- Result:", significance, "\n")
cat("- Decision:", decision, "\n")
cat("- p-value:", sprintf("%.6f", p_value), "\n")

cat("\n BUSINESS IMPACT:\n")
if (treatment_rate > control_rate) {
  direction <- "increased"
  recommendation <- "Consider implementing personalized messages"
} else {
  direction <- "decreased"
  recommendation <- "Stick with generic messages"
}

cat("- Personalized messages", direction, "engagement by", sprintf("%.2f", abs(lift)), "%\n")
cat("- Absolute difference:", sprintf("%.4f", abs(treatment_rate - control_rate)), "\n")
cat("- Effect size:", effect_interpretation, "\n")
cat("- Recommendation:", recommendation, "\n")

cat("\n⚠ CONSIDERATIONS:\n")
cat("- Sample sizes: Control =", format(control_total, big.mark = ","), 
    ", Treatment =", format(treatment_total, big.mark = ","), "\n")
cat("- Statistical power:", sprintf("%.1f%%", power_result$power * 100), "\n")

if (power_result$power < 0.8) {
  cat("   Power is below 80% - consider larger sample sizes for future tests\n")
}

min_group_size <- min(group_sizes$n)
max_group_size <- max(group_sizes$n)
if (min_group_size / max_group_size < 0.5) {
  cat("  ⚠ Unbalanced groups - ensure random assignment in future tests\n")
}

#10. SUMMARY RESULTS TABLE
cat("\n\n10. SUMMARY RESULTS\n")
cat(rep("=", 50), "\n")

summary_results <- data.frame(
  Metric = c("Test Type", "Control Group", "Treatment Group", "Control Rate", 
             "Treatment Rate", "Absolute Difference", "Relative Lift (%)", 
             "p-value", "Effect Size (Cohen's h)", "Statistical Power", 
             "Sample Size Control", "Sample Size Treatment", 
             "Statistically Significant", "Recommendation"),
  Value = c("A/B Test - Conservation Campaign", "Generic", "Personalized",
            sprintf("%.4f", control_rate), sprintf("%.4f", treatment_rate),
            sprintf("%.4f", treatment_rate - control_rate), sprintf("%.2f", lift),
            sprintf("%.6f", p_value), sprintf("%.4f", cohens_h),
            sprintf("%.4f", power_result$power), format(control_total, big.mark = ","),
            format(treatment_total, big.mark = ","), 
            ifelse(p_value < 0.05, "Yes", "No"), recommendation)
)

print(kable(summary_results, format = "simple"))

#Save summary results to CSV
write_csv(summary_results, paste0(output_folder, "ab_test_summary_results_R.csv"))
cat("\n Summary results saved to:", paste0(output_folder, "ab_test_summary_results_R.csv"), "\n")

#Save detailed data for further analysis
detailed_results <- df %>%
  group_by(message_type) %>%
  summarise(
    total_users = n(),
    engaged_users = sum(engaged),
    engagement_rate = mean(engaged),
    avg_messages_seen = ifelse("total_messages_seen" %in% names(df), 
                               mean(total_messages_seen), NA),
    .groups = 'drop'
  )

write_csv(detailed_results, paste0(output_folder, "group_statistics_R.csv"))
cat(" Detailed group statistics saved to:", paste0(output_folder, "group_statistics_R.csv"), "\n")

