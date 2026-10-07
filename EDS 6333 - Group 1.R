df <- read.csv("C:/Users/tutha/Downloads/teen_phone_addiction_dataset.csv",header = TRUE)
head(df)

#Sakina:
# Load required library
library(dplyr)
library(ggplot2)
library(patchwork)
library(reshape2)
library(tidyverse)
library(car)
library(onewaytests)
# List of key numerical variables
key_vars <- c(
  "Daily_Usage_Hours",
  "Academic_Performance",
  "Addiction_Level",
  "Apps_Used_Daily",
  "Time_on_Gaming",
  "Sleep_Hours", 
  "Social_Interactions",
  "Exercise_Hours", 
  "Time_on_Education", 
  "Time_on_Social_Media"
)
# Function to calculate mode in R
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Loop through each variable and print stats
for (col in key_vars) {
  
  if (col %in% names(df)) {
    cat("\nVariable:", col, "\n")
    
    x <- df[[col]]
    
    cat("  Mean:", round(mean(x, na.rm = TRUE), 2), "\n")
    cat("  Median:", round(median(x, na.rm = TRUE), 2), "\n")
    cat("  Mode:", get_mode(x), "\n")
    cat("  Standard Deviation:", round(sd(x, na.rm = TRUE), 2), "\n")
    cat("  Range:", round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 2), "\n")
    
  } else {
    cat("\n⚠ Column", col, "not found in the dataset.\n")
  }
}
numeric_vars <- c(
  "Daily_Usage_Hours",
  "Academic_Performance",
  "Addiction_Level",
  "Apps_Used_Daily",
  "Time_on_Gaming",
  "Sleep_Hours", 
  "Social_Interactions",
  "Exercise_Hours", 
  "Time_on_Education", 
  "Time_on_Social_Media"
)

# List to store the generated plots
all_plots <- list()

cat("\n--- Generating Visualization Plots ---\n")

for (col in numeric_vars) {
  if (col %in% names(df)) {
    
    # 1. Create Histogram + Density Plot (p_hist)
    p_hist <- ggplot(df, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      geom_density(alpha = 0.2) +
      labs(title = paste("Distribution of", col),
           x = col,
           y = "Frequency") +
      theme(plot.title = element_text(size = 10)) # Adjust title size for small plots
    
    # 2. Create Boxplot (p_box)
    p_box <- ggplot(df, aes_string(y = col)) +
      geom_boxplot(fill = "orange") +
      labs(title = paste("Outliers for", col),
           y = col) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            plot.title = element_text(size = 10))
    
    # 3. Combine Histogram and Boxplot using patchwork's | (side-by-side)
    # The plot title comes from the histogram (p_hist)
    # The combined plot is given a name and stored in the list.
    all_plots[[col]] <- p_hist + p_box + 
      plot_layout(widths = c(3, 1)) # Allocate 3 units width to hist, 1 to boxplot
  } else {
    cat("\nWarning: Column '", col, "' not found in the dataset, cannot visualize.\n")
  }
}

cat("\n--- Displaying All Plots in a Grid ---\n")


combined_figure <- wrap_plots(all_plots, ncol = 2)

# Print the final combined figure
print(combined_figure)

# --- 1. Define Variables ---
numeric_vars <- c(
  "Daily_Usage_Hours",
  "Academic_Performance",
  "Addiction_Level",
  "Apps_Used_Daily",
  "Time_on_Gaming",
  "Sleep_Hours", 
  "Social_Interactions",
  "Exercise_Hours", 
  "Time_on_Education", 
  "Time_on_Social_Media"
)

categorical_vars <- c("Gender", "Phone_Usage_Purpose")

# --- 2. Initialize Master Plot List ---
master_plot_list <- list()

cat("--- Generating All Visualizations ---\n")

# =========================================================================
# A. NUMERICAL VARIABLES (Histograms + Boxplots)
# =========================================================================
cat("\n--- Processing Numerical Variables ---\n")

for (col in numeric_vars) {
  if (col %in% names(df)) {
    
    # 1. Create Histogram + Density Plot (p_hist)
    p_hist <- ggplot(df, aes_string(x = col)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      geom_density(alpha = 0.2) +
      labs(title = paste("Distribution of", col)) +
      theme_minimal() +
      theme(plot.title = element_text(size = 10), axis.title.x = element_blank())
    
    # 2. Create Boxplot (p_box)
    p_box <- ggplot(df, aes_string(y = col)) +
      geom_boxplot(fill = "orange") +
      labs(title = paste("Outliers for", col), y = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 10)
      )
    
    # 3. Combine Histogram and Boxplot (side-by-side)
    combined_num_plot <- p_hist + p_box + plot_layout(widths = c(3, 1))
    
    # 4. Store combined plot in the master list
    master_plot_list[[paste0("num_", col)]] <- combined_num_plot
    
  } else {
    cat("\nWarning: Column '", col, "' not found in the dataset.\n")
  }
}

# =========================================================================
# B. CATEGORICAL VARIABLES (Bar Plots)
# =========================================================================
cat("\n--- Processing Categorical Variables ---\n")

for (col in categorical_vars) {
  if (col %in% names(df)) {
    
    # Generate and print frequency table (this will still appear in the Console)
    cat("\n### Frequency Table for", col, "\n")
    print(df %>% count(.data[[col]]) %>% mutate(Percentage = round(n / sum(n) * 100, 2)) %>% rename(Category = 1, Count = n))
    
    # 1. Create Bar Plot (p_bar)
    p_bar <- ggplot(df, aes_string(x = col, fill = col)) +
      geom_bar() +
      labs(
        title = paste("Frequency of", col),
        x = NULL,
        y = "Count"
      ) +
      theme_minimal() +
      guides(fill = "none") +
      theme(plot.title = element_text(size = 10))
    
    # 2. Store bar plot in the master list
    master_plot_list[[paste0("cat_", col)]] <- p_bar
    
  } else {
    cat("\nWarning: Column '", col, "' not found in the dataset.\n")
  }
}

# =========================================================================
# C. FINAL COMBINATION AND DISPLAY
# =========================================================================

cat("\n--- Displaying Final Combined Figure ---\n")

# Combine ALL plots from the master list into a single figure.
# Using 'ncol = 2' organizes the 7 total plots (5 numeric pairs + 2 bar plots) 
# into a readable grid.
final_combined_figure <- wrap_plots(master_plot_list, ncol = 2)

# Print the single, combined figure to the RStudio Plots pane
print(final_combined_figure)


cat("\n--- Done. Please view the full grid in the Plots pane. ---\n")

library(ggplot2)

# Gender Plot
ggplot(df, aes(x = Gender)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Gender",
       x = "Gender", y = "Count") +
  theme_minimal()

# Phone Usage Purpose Plot
ggplot(df, aes(x = Phone_Usage_Purpose)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Phone Usage Purpose",
       x = "Purpose", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sandra:
# Create high addiction indicator

df$High_Addiction <- ifelse(df$Addiction_Level >= 9, 1, 0)

# Overall probability of high addiction

overall_prob <- mean(df$High_Addiction)
overall_prob

# Define long usage

df$Long_Usage <- ifelse(df$Daily_Usage_Hours >= 6, 1, 0)

# Probability of high addiction among long usage teens

prob_high_long <- mean(df$High_Addiction[df$Long_Usage == 1])

# Probability of high addiction among short usage teens

prob_high_short <- mean(df$High_Addiction[df$Long_Usage == 0])

prob_high_long
prob_high_short

library(ggplot2)
library(scales)

prob_usage <- aggregate(High_Addiction ~ Long_Usage, df, mean)

ggplot(prob_usage, aes(x = factor(Long_Usage), y = High_Addiction)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = percent, limits = c(0,1)) +
  scale_x_discrete(labels = c("0" = "< 6 hours/day", "1" = "≥ 6 hours/day")) +
  labs(
    title = "Probability of High Addiction by Daily Phone Usage",
    x = "Daily Smartphone Usage",
    y = "Probability of High Addiction"
  ) +
  theme_minimal()

# Probability of high addiction by gender

prob_gender <- aggregate(High_Addiction ~ Gender, df, mean)
prob_gender

# Expected addiction level by gender

exp_gender <- aggregate(Addiction_Level ~ Gender, df, mean)
exp_gender

# Probability of high addiction by phone usage purpose

prob_purpose <- aggregate(High_Addiction ~ Phone_Usage_Purpose, df, mean)
prob_purpose

# Expected addiction level by phone usage purpose

exp_purpose <- aggregate(Addiction_Level ~ Phone_Usage_Purpose, df, mean)
exp_purpose

# Define heavy gaming usage

df$High_Gaming <- ifelse(df$Time_on_Gaming >= 2, 1, 0)

prob_highgaming <- mean(df$High_Addiction[df$High_Gaming == 1])
prob_lowgaming  <- mean(df$High_Addiction[df$High_Gaming == 0])

prob_highgaming
prob_lowgaming

#Thanh:
summary(df)

#get the count of unique values per column
sapply(df, function(x) length(unique(x)))
#1)	Research question: Are mean addiction levels the same across men, women, and others?
table(df$Gender)                   # group sizes
summary(df$Addiction_Level)
by(df$Addiction_Level, df$Gender, summary) #get a summary of Gender variable
unique(df$Gender)
boxplot(Addiction_Level ~ Gender, data = df, main = "Addiction Level by Gender",
        ylab = "Addiction Level", xlab = "Gender")

leveneTest(Addiction_Level ~ Gender, data = df)
anova_model <- aov(Addiction_Level ~ Gender, data = df)
summary(anova_model)
# P-value > alpha values, fail to reject null hypothesis. Therefore, there is no difference in mean addiction levels across the three gender groups

#2)Research question: Does parental control play a significant role in teenagers’ phone addiction level?
unique(df$Parental_Control)
table(df$Parental_Control)
summary(df$Addiction_Level)
by(df$Addiction_Level,df$Parental_Control,summary)
boxplot(Addiction_Level ~ Parental_Control, data = df)
t.test(Addiction_Level ~ Parental_Control, data = df, var.equal = TRUE)
#Based on the independent t-test, the p-value is 0.9557, which is significantly higher than the alpha value of 0.05, indicating that there is no evidence to support that parental control plays a significant role in phone addiction in teenagers.

#3) Research question  Do mean addiction levels differ across groups defined by daily usage hours? 
summary(df$Daily_Usage_Hours)
daily_usage_hours_groups <- cut(df$Daily_Usage_Hours, 
                                breaks = c(0, 3.7, 6.4, 11.5), 
                                labels = c("Low", "Medium", "High"))
daily_usage_hours_groups
daily_usage_hours_groups <- cut(
  df$Daily_Usage_Hours, 
  breaks = c(0, 3.7, 6.4, 11.5), 
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Optional: add the new factor to your dataset
df$Usage_Group <- daily_usage_hours_groups

# Check distribution of groups
table(df$Usage_Group)
boxplot(Addiction_Level ~ Usage_Group, data = df,
        main = "Addiction Level by Usage Group",
        ylab = "Addiction Level", xlab = "Phone Usage Group")
if(!require(car)) install.packages("car"); library(car)
leveneTest(Addiction_Level ~ Usage_Group, data = df)
anova_model_daily_usage <- aov(Addiction_Level ~ Usage_Group, data = df)
summary(anova_model_daily_usage)

leveneTest(Addiction_Level ~ Usage_Group, data = df)
welch.test(Addiction_Level ~ Usage_Group, data = df)

#p-value of 2.2e^-16 which is significantly lower than alpha value of 0.05. This indicates that at least one group differs from the others in mean addiction level

#4) Does the mean addiction level in the lower daily usage group differ from the high and medium daily usage groups?
df$App_Group <- cut(
  df$Apps_Used_Daily,
  breaks = c(5, 9, 17, 20),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Check distribution
table(df$App_Group)

anova_result <- aov(Addiction_Level ~ App_Group, data = df)
summary(anova_result)

leveneTest(Addiction_Level ~ App_Group, data = df)
oneway.test(Addiction_Level ~ App_Group, data = df)

low_high_app <- subset(df, App_Group %in% c("Low", "High"))

t.test(Addiction_Level ~ App_Group, data = low_high_app, var.equal = FALSE)

Jolene:
# EDA Correlation
# Select key numerical variables
num_vars <- df %>% 
  select(Age, Daily_Usage_Hours, Sleep_Hours, Academic_Performance,
         Screen_Time_Before_Bed, Phone_Checks_Per_Day,
         Apps_Used_Daily, Time_on_Social_Media, Time_on_Gaming,
         Time_on_Education, Addiction_Level)


# Calculate Pearson Correlation
corr <- cor(num_vars, use = "complete.obs", method = "pearson")

# Plot Correlation heatmap
melted <- melt(corr)
ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank()
  ) +
  labs(title = "Correlation Heatmap")

# ----------------------------------------------------

# 1. Daily_Usage_Hours (r = 0.60) has the strongest positive correlation with Addiction_Level.
# 2. Apps_Used_Daily (r = 0.32), Time_on_Social_Media (r = 0.31), and Time_on_Gaming (r = 0.27) show moderate positive correlations.
# 3. Sleep_Hours (r = –0.22) is moderately negatively correlated, indicating that students who sleep less tend to have higher addiction scores.
# 4. Screen_Time_Before_Bed, Time_on_Education, Academic_Performance show near-zero correlations, suggesting minimal linear association with addiction.

# ---------------------------------------------------------------------

# 3. Linear Model between Daily Usage hours and addition levels
# (1) Pearson correlation
cor(df$Daily_Usage_Hours, df$Addiction_Level, method = "pearson")

# (2) Spearman correlation
cor(df$Daily_Usage_Hours, df$Addiction_Level, method = "spearman")

# (3) simple linear model of Daily Usage
model <- lm(Addiction_Level ~ Daily_Usage_Hours, data = df)

# (4) evaluate model
summary(model)

# -------------------------------------------------------------------------------------
# 1. β₀ (Intercept) = 6.4, β₁ (Slope) = 0.49, R² = 0.361, p-value < 2.2e−16
# 2. For every additional hour of daily smartphone usage, the addiction score increases by 0.49 points on average.
# 3. The model explains 36.1% of the variation in addiction level, which is relatively strong for behavioral survey data.
# 4. The relationship is statistically significant and the strongest among all variables tested.
# 5. This confirms Daily_Usage_Hours as the primary predictor of smartphone addiction.
# -------------------------------------------------------------------------------------

# (5) Residual analysis
# Residuals and fitted plot
res <- resid(model)
fit <- fitted(model)

plot(fit, res,
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals vs Fitted")
abline(h = 0, col="red", lty=2)

# --------------------------------------------------------------------------------
# 1. residual plot shows a clear triangular (funnel-like) pattern, the variance of the errors is not constant across fitted values.
# 2. the linear regression assumptions of constant variance are violated, and the model does not fully capture the underlying relationship between the variables.
# 3. the relationship between Daily Usage Hours and Addiction Level is strong but not perfectly linear
# -----------------------------------------------------------------------------------

# histogram of residual
hist(res, main="Histogram of Residuals", xlab="Residuals", col = 'SkyBlue')

# -------------------------------------------------------------------------
# 1. Residuals are approximately centered around 0, which is good.
# 2. The distribution is left-skewed, with a long tail extending toward negative values. 
# 3. Residuals are not perfectly normal,as the shape is asymmetric rather than bell-shaped.
# -------------------------------------------------------------------------

# residual QQ plot
qqnorm(res)
qqline(res, col="red")
# -------------------------------------------------------------------------
# 1. The left tail drops far below the line, meaning the model produces extreme negative residuals more often than would be expected under normality.
# 2. The right tail falls slightly below the line as well, but the deviation is much smaller than on the left side.
# 3. The linear model violates the normality assumption due to heavy negative residuals, indicating skewness and suggesting that a simple linear model may not fully capture the relationship between usage hours and addiction levels.
# -------------------------------------------------------------------------

# Overall, although Daily Usage Hours is a strong and statistically significant predictor of Smartphone Addiction Level
# the linear model does not fully satisfy key regression assumptions. 
# The residual diagnostics reveal left-skewed errors, and non-normal tails. 
# This suggests that the relationship may be more complex than linear.
# --------------------------------------------------------------------------


## Additional Variables 
# (1) Pearson Correlation
pearson_corr <- cor(df[, c("Daily_Usage_Hours", "Sleep_Hours",
                           "Screen_Time_Before_Bed", "Phone_Checks_Per_Day",
                           "Apps_Used_Daily", "Time_on_Social_Media",
                           "Time_on_Gaming", "Time_on_Education",
                           "Addiction_Level")],
                    method = "pearson")

pearson_corr[,"Addiction_Level"]



# (2) Spearman correlation
spearman_corr <- cor(df[, c("Daily_Usage_Hours", "Sleep_Hours",
                            "Screen_Time_Before_Bed", "Phone_Checks_Per_Day",
                            "Apps_Used_Daily", "Time_on_Social_Media",
                            "Time_on_Gaming", "Time_on_Education",
                            "Addiction_Level")],
                     method = "spearman")

spearman_corr[,"Addiction_Level"]

# (3) Regression model and evaluation
# create function for modeling and evaluation
run_regression <- function(df, predictor, response = "Addiction_Level") {
  
  # form formula
  formula <- as.formula(paste(response, "~", predictor))
  
  # crate lm model
  model <- lm(formula, data = df)
  
  cat(" Linear Regression Summary\n")
  print(summary(model))
  
  # extract fitted and residuals
  res <- resid(model)
  fit <- fitted(model)
  
  # Residual analysis
  # Residuals vs Fitted 
  plot(fit, res,
       xlab="Fitted Values",
       ylab="Residuals",
       main=paste("Residuals vs Fitted\n(", predictor, ")"))
  abline(h=0, col="red", lty=2)
  
  # Residual Histogram
  hist(res,
       main=paste("Histogram of Residuals\n(", predictor, ")"),
       xlab="Residuals",
       col="skyblue")
  
  # QQ Plot
  qqnorm(res,
         main=paste("QQ Plot of Residuals\n(", predictor, ")"))
  qqline(res, col="red")
  
  return(model)
}

run_regression(df, "Sleep_Hours")
run_regression(df, "Apps_Used_Daily")
run_regression(df, "Time_on_Social_Media")

# The model performances of these variables are worse than daily usage hours.

