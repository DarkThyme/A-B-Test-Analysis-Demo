# Conservation Campaign A/B Test

This project analyzes the effectiveness of a conservation awareness campaign using A/B testing.  
The goal was to compare engagement rates between personalized messages and generic messages to determine which approach drives higher participation.

## Project Structure

### R Analysis
Located in the `R` folder:
- `R Script Testing.R` – R code for the analysis
- `conservation_dataset.csv` – Dataset used in the analysis
- Generated plots: sample sizes, engagement over time, engagement rates, message type distribution, and final A/B test results
- Output CSV with summary statistics

### Python Analysis
Located in the `Python` folder:
- `Conservation Campaign.py` – Python code for the analysis
- Generated plots from the Python script

## Required Packages

### R
Install the following R packages:
```r
install.packages(c("dplyr", "ggplot2", "readr", "knitr", "scales", "gridExtra", "effectsize", "pwr"))

### Python
Install the following Python packages:
```bash
pip install pandas numpy matplotlib seaborn scipy

## How to Interpret Results
- Sample Sizes – Number of participants in each group.

- Engagement Over Time – Participation trends throughout the campaign.

- Engagement Rates – Average engagement comparison between groups.

- Message Distribution – Frequency of message types sent.

- Statistical Results – Significance testing to determine if observed differences are meaningful.

- The analysis from both R and Python confirms that personalized messages resulted in a statistically significant increase in engagement compared to generic messages.

