## Analysis Overview

### Descriptive Statistics and Visualization 
Summarizes ten key numerical variables (mean, median, mode, standard deviation, range) and visualizes distributions with histograms, boxplots, and bar charts for Gender and Phone Usage Purpose.

### Probability Analysis 
Defines "high addiction" as an addiction level of 9 or above and calculates the probability of high addiction by daily usage (6+ hours vs. less), gaming time (2+ hours vs. less), gender, and phone usage purpose, along with expected addiction levels by group.

### Hypothesis Testing 
Uses ANOVA, Welch's ANOVA, t-tests, and Levene's test. No significant difference was found across gender or parental control (p = 0.9557), but addiction levels differed significantly across daily usage groups (p < 2.2e-16).

### Correlation and Regression 
Daily usage hours had the strongest correlation with addiction level (r = 0.60). A linear regression showed each additional hour of use raises the addiction score by about 0.49 points (R² = 0.361). Residual diagnostics suggest the relationship is not fully linear.

## Key Finding
Daily phone usage time is the strongest predictor of teen smartphone addiction, while gender and parental control show no significant effect.

## How to Run
1. Download the Teen Phone Addiction dataset (CSV) and place it in your R working directory.
2. Install the required packages:
   `install.packages(c("tidyverse", "patchwork", "reshape2", "car", "onewaytests", "scales"))`
3. Run `teen_phone_addiction_analysis.R` in RStudio.
