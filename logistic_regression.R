library(readr)
library(mice)

data_path <- "./data/breast_cancer_data_encoded.csv"

# Load data
breast_cancer_data <- read_csv(data_path)

# Convert all numeric columns to numeric (float) type
breast_cancer_data <- breast_cancer_data %>%
  mutate_if(is.character, as.numeric)

# # Remove variables with >50% missing data
# missing_prop <- sapply(your_data, function(x) sum(is.na(x))/length(x))
# keep_vars <- names(missing_prop[missing_prop < 0.5])
# cleaned_data <- your_data[, keep_vars]

# Multiple Imputation
imputed_data <- mice(breast_cancer_data, m = 5, method = 'pmm', seed = 42, 
                     printFlag = FALSE)

# Fit logistic regression on imputed datasets
fit <- glm.mids(depression ~ ., data = imputed_data, family = binomial)

# Pool results
pooled_results <- pool(fit)
results_summary <- summary(pooled_results)

# # Coefficents and p-values
# plot_data <- results_summary %>%
#   filter(term != "(Intercept)") %>%
#   mutate(
#     # Calculate confidence intervals
#     ci_lower = estimate - 1.96 * std.error,
#     ci_upper = estimate + 1.96 * std.error,
#     # Determine significance
#     significant = ifelse(p.value < 0.05, "Significant", "Not Significant"),
#     # Clean term names
#     term_clean = case_when(
#       term == "age_told_had_cancer" ~ "Age at Cancer Diagnosis",
#       term == "gender" ~ "Gender",
#       term == "income" ~ "Income",
#       TRUE ~ stringr::str_replace_all(term, "_", " ") %>% 
#              stringr::str_to_title()
#     )
#   )
# # Forest plot of coefficients
# p1 <- ggplot(plot_data, aes(x = estimate, y = reorder(term_clean, estimate))) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
#   geom_point(aes(color = significant), size = 3) +
#   geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = significant), 
#                  height = 0.2) +
#   scale_color_manual(values = c("Significant" = "#E31A1C", 
#                                 "Not Significant" = "#1F78B4")) +
#   labs(
#     title = "Logistic Regression Coefficients",
#     subtitle = "Error bars show 95% confidence intervals",
#     x = "Log Odds (Coefficient)",
#     y = "Variables",
#     color = "Significance"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     axis.text = element_text(size = 11),
#     legend.position = "bottom"
#   )
# 
# # Odds Ratios Plot
# or_data <- plot_data %>%
#   mutate(
#     odds_ratio = exp(estimate),
#     or_ci_lower = exp(ci_lower),
#     or_ci_upper = exp(ci_upper)
#   )
# 
# p2 <- ggplot(or_data, aes(x = odds_ratio, y = reorder(term_clean, odds_ratio))) +
#   geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
#   geom_point(aes(color = significant), size = 3) +
#   geom_errorbarh(aes(xmin = or_ci_lower, xmax = or_ci_upper, color = significant), 
#                  height = 0.2) +
#   scale_color_manual(values = c("Significant" = "#E31A1C", 
#                                 "Not Significant" = "#1F78B4")) +
#   scale_x_log10() +
#   labs(
#     title = "Odds Ratios",
#     subtitle = "Error bars show 95% confidence intervals",
#     x = "Odds Ratio (log scale)",
#     y = "Variables",
#     color = "Significance"
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     axis.text = element_text(size = 11),
#     legend.position = "bottom"
#   )

# completed_data1 <- complete(imputed_data, 1)
head(results_summary)
