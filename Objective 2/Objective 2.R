# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(forcats)
library(broom)

# Load data
setwd("R:/TCD - Business Analytics/Semseter 2/ESG Analysis/Group Project")
df <- read_csv("Final.csv")

# Select return columns of interest
returns_cols <- c("returns_1y", "returns_3y", "returns_5y")

# Clean data (remove NAs for selected return columns)
df_clean <- df %>%
  select(is_esg, net_assets, net_expense_ratio, category, all_of(returns_cols)) %>%
  drop_na()

# Convert is_esg to numeric (1 = TRUE, 0 = FALSE)
df_clean$is_esg_numeric <- as.numeric(df_clean$is_esg)

# Convert category to factor
df_clean$category <- as.factor(df_clean$category)


# 1. Summary Statistics Table
summary_stats <- df_clean %>%
  group_by(is_esg) %>%
  summarise(across(all_of(returns_cols), list(mean = mean, sd = sd), na.rm = TRUE))

print("Summary Statistics:")
print(summary_stats)

# 2. T-tests for returns
for (col in returns_cols) {
  cat("\nT-test for", col, ":\n")
  print(t.test(df_clean[[col]] ~ df_clean$is_esg))
}

# 3. Coefficient of Variation
cv_df <- df_clean %>%
  group_by(is_esg) %>%
  summarise(across(all_of(returns_cols),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

cv_melt <- cv_df %>%
  mutate(across(ends_with("_mean"), ~ get(gsub("mean", "sd", cur_column())) / ., 
                .names = "cv_{.col}")) %>%
  select(is_esg, starts_with("cv_returns")) %>%
  pivot_longer(-is_esg, names_to = "metric", values_to = "cv") %>%
  mutate(period = gsub("cv_returns_|_mean", "", metric))

# 4. Density Plots for Return Distributions
custom_colors <- c("TRUE" = "#1b9e77", "FALSE" = "#d95f02")  # adjust hex codes as needed

for (col in returns_cols) {
  p <- ggplot(df_clean, aes(x = .data[[col]], fill = factor(is_esg))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = custom_colors, labels = c("Non-ESG Fund", "ESG Fund")) +
    labs(
      title = paste("Return Distribution:", col),
      x = col,
      fill = "Fund Type"
    ) +
    theme_minimal()
  
  print(p)
}

# 5. Boxplots for Returns
df_long <- df_clean %>%
  pivot_longer(cols = all_of(returns_cols), names_to = "period", values_to = "return")

ggplot(df_long, aes(x = is_esg, y = return, fill = is_esg)) +
  geom_boxplot() +
  facet_wrap(~period, scales = "free") +
  labs(title = "Boxplots of Returns by ESG Status",
       x = "ESG Fund", y = "Return (%)") +
  theme_minimal()

# 6. Violin plots to show distribution spread
ggplot(df_long, aes(x = is_esg, y = return, fill = is_esg)) +
  geom_violin(trim = FALSE) +
  facet_wrap(~period, scales = "free") +
  labs(title = "Violin Plots of Return Spread",
       x = "ESG Fund", y = "Return (%)") +
  theme_minimal()

# 7. Bar Chart: Mean Returns by ESG Status
mean_returns <- df_long %>%
  group_by(is_esg, period) %>%
  summarise(mean_return = mean(return, na.rm = TRUE))

ggplot(mean_returns, aes(x = period, y = mean_return, fill = is_esg)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(mean_return, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  labs(title = "Average Returns: ESG vs Non-ESG",
       y = "Mean Return (%)", x = "Return Period", fill = "ESG Status") +
  scale_fill_manual(values = c("TRUE" = "#91cf60", "FALSE" = "#fc8d59")) +  # subtle green and red
  theme_minimal()

# 8. CV Bar Chart
ggplot(cv_melt, aes(x = period, y = cv, fill = is_esg)) +
  geom_col(position = "dodge") +
  labs(title = "Coefficient of Variation (Risk per Unit Return)",
       x = "Return Period", y = "CV (SD / Mean)") +
  theme_minimal()

# 9. Expense Ratio Comparison Plot
ggplot(df_clean, aes(x = is_esg, y = net_expense_ratio, fill = is_esg)) +
  geom_boxplot() +
  labs(title = "Net Expense Ratio: ESG vs Non-ESG",
       x = "ESG Fund", y = "Expense Ratio") +
  theme_minimal()

# 10. Run regression for each return column
for (col in returns_cols) {
  formula <- as.formula(paste(col, "~ is_esg + net_assets + category"))
  model <- lm(formula, data = df_clean)
  cat("\n--------------------------------------------------\n")
  cat("Linear Regression for", col, "\n")
  print(summary(model))
}

# Run regression for 5-year returns
model_5y <- lm(returns_5y ~ is_esg_numeric + net_assets + category, data = df_clean)

# 1. Predicted Returns by ESG Status (Adjusted)
df_clean$predicted_5y <- predict(model_5y)

# Boxplot of predicted returns
ggplot(df_clean, aes(x = is_esg, y = predicted_5y, fill = is_esg)) +
  geom_boxplot() +
  labs(title = "Predicted 5-Year Returns by ESG Status (Adjusted for Fund Size and Category)",
       x = "ESG Fund", y = "Predicted 5-Year Return") +
  theme_minimal()

# 2. Regression Coefficients with Confidence Intervals
# Tidy the model
tidy_model_5y <- tidy(model_5y, conf.int = TRUE)

# Plot coefficient estimates
ggplot(tidy_model_5y, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Regression Coefficients for 5-Year Returns",
       x = "Coefficient Estimate", y = "Model Terms") +
  theme_minimal()
