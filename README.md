# Four-Mile Run: Training Effect Analysis in R

Statistical analysis of 19 runs completed by a single runner ("Kevin") using a Garmin Forerunner 610 GPS watch, exploring how heart rate, pace, and run duration relate to the watch's **Training Effect** score. Completed as the first assignment for the *Advanced Data Analysis* course, MSc in Statistics, Athens University of Economics and Business (AUEB).

## Repository Contents

| File | Description |
|---|---|
| `23_Four-Mile_Run_Dataset.csv` | Raw dataset — 19 observations, 14 variables (Run, Time, Pace, Calories Burned, Training Effect, Max HR, Avg HR, Avg Speed, Max Speed, HR Rest, HR Rest1, HR Rest2, HR Change1, HR Change2). |
| `FourMileRun_Project_Code.R` | Full R script: data cleaning, exploratory data analysis, normality/outlier tests, correlation analysis, and regression modeling. |
| `FourMileRun_Project_Report.pdf` | Written report with narrative interpretation, figures, and tables summarizing the analysis and findings. |

## Analysis Overview

- **Data preparation**: dropped an empty imported column, split `Time` and `Pace` into numeric components, and created a categorical `Training.Effect.Cat` variable (Minor, Maintaining, Improving, Highly Improving, Overreaching).
- **Exploratory analysis**: summary statistics, Shapiro-Wilk normality tests, QQ plots, boxplots, and outlier detection across all numeric variables.
- **Pairwise comparisons**: Pearson correlation matrix and scatter plots of key variable pairs; paired t-tests and a Wilcoxon test comparing related heart-rate metrics.
- **Regression modeling**: stepwise selection followed by manual refinement (including a quadratic term for Average HR) to model Training Effect. The final model:

  ```r
  lm(Training.Effect ~ Avg.HR + I(Avg.HR^2) + Time.In.Minutes, data = subset_data)
  ```

  achieved an Adjusted R² of 0.902 and passed diagnostic checks for normality (Shapiro-Wilk), homoscedasticity (Levene's test), and independence (runs test) of residuals.

## Key Findings

- Average Heart Rate is the strongest predictor of Training Effect, with a **nonlinear (quadratic)** relationship — Training Effect rises quickly at first as Avg HR increases, then levels off.
- Run duration (`Time.In.Minutes`) has a smaller but statistically significant negative effect on Training Effect.
- Maximum HR and post-run recovery HR metrics, despite correlating with Training Effect, were not significant predictors once Average HR was accounted for.

## Requirements

Built with R, using the following packages:

```r
psych, ggplot2, corrplot, car, dplyr, nortest, MASS, Hmisc, randtests, VGAM, BAS
```

## Usage

1. Clone the repository.
2. Open `FourMileRun_Project_Code.R` and update the `read.csv()` file path to point to your local copy of `23_Four-Mile_Run_Dataset.csv`.
3. Run the script in R or RStudio to reproduce the exploratory plots, statistical tests, and regression models described in the report.

## Author

Konstantinos Grammenos — MSc in Statistics, AUEB
Supervisor: Prof. I. Ntzoufras
