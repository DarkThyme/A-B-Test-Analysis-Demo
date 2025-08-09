#Conservation Campaign A/B Test Analysis
#Python Implementation

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from scipy.stats import chi2_contingency, ttest_ind, mannwhitneyu
import warnings
warnings.filterwarnings('ignore')

#Set style for better visualizations
plt.style.use('default')
sns.set_palette("husl")

print(" CONSERVATION CAMPAIGN A/B TEST ANALYSIS \n")

#1. DATA LOADING AND INITIAL EXPLORATION
print("1. LOADING AND EXPLORING DATA")
print("=" * 50)

#Load the dataset
file_path = r"D:\TESTING DATA FOR A CONSERVATION CAMPAIGN\conservation_dataset.csv"
try:
    df = pd.read_csv(file_path)
    print(f"✓ Data loaded successfully!")
    print(f"Dataset shape: {df.shape}")
except FileNotFoundError:
    print(" File not found. Please check the file path.")
    #Create sample data for demonstration if file not found
    np.random.seed(42)
    sample_data = {
        'user_id': range(1000000, 1001000),
        'message_type': np.random.choice(['Personalized', 'Generic'], 1000, p=[0.7, 0.3]),
        'engaged': np.random.choice([True, False], 1000, p=[0.15, 0.85]),
        'total_messages_seen': np.random.randint(1, 500, 1000),
        'most_engagement_day': np.random.choice(['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'], 1000),
        'most_engagement_hour': np.random.randint(0, 24, 1000)
    }
    df = pd.DataFrame(sample_data)
    print("Using sample data for demonstration.")

#Basic info about the dataset
print(f"\nDataset Info:")
print(f"- Total users: {len(df):,}")
print(f"- Columns: {list(df.columns)}")
print(f"- Data types:\n{df.dtypes}")

#Check for missing values
missing_values = df.isnull().sum()
if missing_values.sum() > 0:
    print(f"\n  Missing values found:\n{missing_values[missing_values > 0]}")
else:
    print("\n✓ No missing values found")

#2. DATA PREPROCESSING
print("\n\n2. DATA PREPROCESSING")
print("=" * 50)

#Handle the 'engaged' column - convert to boolean if it's not already
if df['engaged'].dtype == 'object':
    #Handle different possible string representations
    if df['engaged'].dtype == 'object':
        unique_values = df['engaged'].unique()
        print(f"Unique engaged values before conversion: {unique_values}")
        
        #Try different mapping approaches
        if set(unique_values).issubset({'TRUE', 'FALSE'}):
            df['engaged'] = df['engaged'].map({'TRUE': True, 'FALSE': False})
        elif set(unique_values).issubset({'True', 'False'}):
            df['engaged'] = df['engaged'].map({'True': True, 'False': False})
        elif set(unique_values).issubset({'true', 'false'}):
            df['engaged'] = df['engaged'].map({'true': True, 'false': False})
        elif set(unique_values).issubset({'1', '0'}):
            df['engaged'] = df['engaged'].map({'1': True, '0': False})
        else:
            print(f" Unexpected engaged values: {unique_values}")
            
print(f"Engaged column type after processing: {df['engaged'].dtype}")
print(f"Engaged values: {df['engaged'].unique()}")

#First, let's examine the actual column names
print("Actual column names in dataset:")
print(df.columns.tolist())
print("\nFirst few rows of data:")
print(df.head())

#Handle different possible column naming conventions

column_mapping = {
    'user id': 'user_id',
    'message_type': 'message_type',  
    'engaged': 'engaged',  
    'total_messages_seen': 'total_messages_seen',  
    'most engagement day': 'most_engagement_day',
    'most engagement hour': 'most_engagement_hour'
}

print(f"\nColumn mapping identified:")
for actual_name, standard_name in column_mapping.items():
    if actual_name in df.columns:
        print(f"- {actual_name} -> {standard_name}")

#Rename columns to standard names for easier processing
df = df.rename(columns=column_mapping)

#Display unique values in key columns 
print("\nUnique values in key columns:")
print(f"- Message types: {df['message_type'].unique()}")
print(f"- Engagement: {df['engaged'].unique()}")
print(f"- Days: {df['most_engagement_day'].unique()}")

#3. BASIC STATISTICS AND GROUP COMPARISON
print("\n\n3. BASIC STATISTICS")
print("=" * 50)

#Group sizes
group_sizes = df['message_type'].value_counts()
print("Group sizes:")
for group, size in group_sizes.items():
    percentage = (size / len(df)) * 100
    print(f"- {group}: {size:,} users ({percentage:.1f}%)")

#Engagement rates by group
engagement_by_group = df.groupby('message_type')['engaged'].agg(['count', 'sum', 'mean']).round(4)
engagement_by_group.columns = ['Total_Users', 'Engaged_Users', 'Engagement_Rate']
print(f"\nEngagement Statistics by Group:")
print(engagement_by_group)

#Calculate key metrics for each group
control_group = df[df['message_type'] == 'Generic']
treatment_group = df[df['message_type'] == 'Personalized']

control_rate = control_group['engaged'].mean()
treatment_rate = treatment_group['engaged'].mean()
lift = ((treatment_rate - control_rate) / control_rate) * 100

print(f"\n KEY METRICS:")
print(f"Control (Generic) engagement rate: {control_rate:.4f} ({control_rate*100:.2f}%)")
print(f"Treatment (Personalized) engagement rate: {treatment_rate:.4f} ({treatment_rate*100:.2f}%)")
print(f"Absolute difference: {treatment_rate - control_rate:.4f}")
print(f"Relative lift: {lift:.2f}%")

#4. STATISTICAL SIGNIFICANCE TESTING
print("\n\n4. STATISTICAL SIGNIFICANCE TESTING")
print("=" * 50)

#A. Chi-square test for independence
print("A. Chi-square Test for Independence")
print("-" * 40)
contingency_table = pd.crosstab(df['message_type'], df['engaged'])
print("Contingency Table:")
print(contingency_table)

chi2, p_value_chi2, dof, expected = chi2_contingency(contingency_table)
print(f"\nChi-square test results:")
print(f"- Chi-square statistic: {chi2:.4f}")
print(f"- p-value: {p_value_chi2:.6f}")
print(f"- Degrees of freedom: {dof}")

#B. Two-proportion z-test
print("\nB. Two-Proportion Z-Test")
print("-" * 40)

def two_prop_ztest(x1, n1, x2, n2):
    """
    Perform two-proportion z-test
    """
    p1 = x1 / n1
    p2 = x2 / n2
    p_pool = (x1 + x2) / (n1 + n2)
    
    se = np.sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
    z = (p1 - p2) / se
    p_value = 2 * (1 - stats.norm.cdf(abs(z)))
    
    return z, p_value, p1, p2

#Calculate for our data
control_successes = control_group['engaged'].sum()
control_total = len(control_group)
treatment_successes = treatment_group['engaged'].sum()
treatment_total = len(treatment_group)

z_stat, p_value_z, p1, p2 = two_prop_ztest(control_successes, control_total, 
                                           treatment_successes, treatment_total)

print(f"Two-proportion z-test results:")
print(f"- Z-statistic: {z_stat:.4f}")
print(f"- p-value: {p_value_z:.6f}")

#C. Effect size (Cohen's h)
print("\nC. Effect Size (Cohen's h)")
print("-" * 40)

def cohens_h(p1, p2):
    """Calculate Cohen's h for effect size between two proportions"""
    h = 2 * (np.arcsin(np.sqrt(p1)) - np.arcsin(np.sqrt(p2)))
    return h

effect_size = cohens_h(treatment_rate, control_rate)
print(f"Cohen's h (effect size): {effect_size:.4f}")

#Interpret effect size
if abs(effect_size) < 0.2:
    effect_interpretation = "small"
elif abs(effect_size) < 0.5:
    effect_interpretation = "medium"
else:
    effect_interpretation = "large"
print(f"Effect size interpretation: {effect_interpretation}")

#5. CONFIDENCE INTERVALS
print("\n\n5. CONFIDENCE INTERVALS")
print("=" * 50)

def proportion_ci(successes, total, confidence=0.95):
    """Calculate confidence interval for a proportion"""
    p = successes / total
    z_score = stats.norm.ppf(1 - (1 - confidence) / 2)
    margin_error = z_score * np.sqrt(p * (1 - p) / total)
    return p - margin_error, p + margin_error

#95% confidence intervals
control_ci = proportion_ci(control_successes, control_total)
treatment_ci = proportion_ci(treatment_successes, treatment_total)

print(f"95% Confidence Intervals:")
print(f"- Control (Generic): [{control_ci[0]:.4f}, {control_ci[1]:.4f}]")
print(f"- Treatment (Personalized): [{treatment_ci[0]:.4f}, {treatment_ci[1]:.4f}]")

#Confidence interval for the difference
diff_se = np.sqrt((control_rate * (1 - control_rate) / control_total) + 
                  (treatment_rate * (1 - treatment_rate) / treatment_total))
diff_margin = 1.96 * diff_se
diff_lower = (treatment_rate - control_rate) - diff_margin
diff_upper = (treatment_rate - control_rate) + diff_margin

print(f"- Difference: [{diff_lower:.4f}, {diff_upper:.4f}]")

#6. POWER ANALYSIS
print("\n\n6. POWER ANALYSIS")
print("=" * 50)

#Calculate observed power
def calculate_power(p1, p2, n1, n2, alpha=0.05):
    """Calculate statistical power for two-proportion test"""
    p_pool = ((n1 * p1) + (n2 * p2)) / (n1 + n2)
    se_null = np.sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
    se_alt = np.sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
    
    z_alpha = stats.norm.ppf(1 - alpha/2)
    z_beta = (abs(p1 - p2) - z_alpha * se_null) / se_alt
    power = stats.norm.cdf(z_beta)
    
    return power

observed_power = calculate_power(control_rate, treatment_rate, control_total, treatment_total)
print(f"Observed statistical power: {observed_power:.4f} ({observed_power*100:.1f}%)")

#7. ADDITIONAL ANALYSIS
print("\n\n7. ADDITIONAL ANALYSIS")
print("=" * 50)

#A. Analysis by day of week (if available)
if 'most_engagement_day' in df.columns:
    print("A. Engagement by Day of Week")
    print("-" * 30)
    day_analysis = df.groupby(['most_engagement_day', 'message_type'])['engaged'].mean().unstack()
    print(day_analysis.round(4))
else:
    print("A. Day of week data not available in this dataset")

#B. Analysis by hour (if available)
if 'most_engagement_hour' in df.columns:
    print("\nB. Engagement by Hour of Day")
    print("-" * 30)
    hour_stats = df.groupby('message_type')['most_engagement_hour'].describe()
    print(hour_stats.round(2))
else:
    print("\nB. Hour of day data not available in this dataset")

#C. Messages seen analysis (if available)
if 'total_messages_seen' in df.columns:
    print("\nC. Total Messages Seen Analysis")
    print("-" * 30)
    messages_stats = df.groupby('message_type')['total_messages_seen'].describe()
    print(messages_stats.round(2))
    
    #Test if there's a significant difference in messages seen between groups
    control_messages = control_group['total_messages_seen']
    treatment_messages = treatment_group['total_messages_seen']
    
    #Use Mann-Whitney U test for non-parametric comparison
    u_stat, p_value_messages = mannwhitneyu(control_messages, treatment_messages, alternative='two-sided')
    print(f"\nMann-Whitney U test for messages seen:")
    print(f"- U-statistic: {u_stat:.2f}")
    print(f"- p-value: {p_value_messages:.6f}")
else:
    print("\nC. Total messages seen data not available in this dataset")

#8. RESULTS INTERPRETATION
print("\n\n8. RESULTS INTERPRETATION")
print("=" * 50)

alpha = 0.05
print(f" STATISTICAL SIGNIFICANCE (α = {alpha}):")

if p_value_z < alpha:
    significance = "STATISTICALLY SIGNIFICANT"
    decision = "REJECT the null hypothesis"
else:
    significance = "NOT STATISTICALLY SIGNIFICANT"
    decision = "FAIL TO REJECT the null hypothesis"

print(f"- Result: {significance}")
print(f"- Decision: {decision}")
print(f"- p-value: {p_value_z:.6f}")

print(f"\n BUSINESS IMPACT:")
if treatment_rate > control_rate:
    direction = "increased"
    recommendation = "Consider implementing personalized messages"
else:
    direction = "decreased"
    recommendation = "Stick with generic messages"

print(f"- Personalized messages {direction} engagement by {abs(lift):.2f}%")
print(f"- Absolute difference: {abs(treatment_rate - control_rate):.4f}")
print(f"- Effect size: {effect_interpretation}")
print(f"- Recommendation: {recommendation}")

print(f"\n  CONSIDERATIONS:")
print(f"- Sample sizes: Control = {control_total:,}, Treatment = {treatment_total:,}")
print(f"- Statistical power: {observed_power:.1%}")
if observed_power < 0.8:
    print("   Power is below 80% - consider larger sample sizes for future tests")
if group_sizes.min() / group_sizes.max() < 0.5:
    print("   Unbalanced groups - ensure random assignment in future tests")

#9. VISUALIZATION PREPARATION MESSAGE
print("\n\n9. CREATING VISUALIZATIONS...")
print("=" * 50)

#Create visualizations
fig, axes = plt.subplots(2, 2, figsize=(15, 12))
fig.suptitle('Conservation Campaign A/B Test Results', fontsize=16, fontweight='bold')

#Plot 1: Engagement rates by group
engagement_rates = df.groupby('message_type')['engaged'].mean()
bars1 = axes[0,0].bar(engagement_rates.index, engagement_rates.values, 
                      color=['#FF6B6B', '#4ECDC4'], alpha=0.8)
axes[0,0].set_title('Engagement Rate by Message Type')
axes[0,0].set_ylabel('Engagement Rate')
axes[0,0].set_ylim(0, max(engagement_rates.values) * 1.2)

#Add value labels on bars
for bar, rate in zip(bars1, engagement_rates.values):
    axes[0,0].text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.001,
                   f'{rate:.3f}', ha='center', va='bottom', fontweight='bold')

#Plot 2: Sample sizes
sample_sizes = df['message_type'].value_counts()
bars2 = axes[0,1].bar(sample_sizes.index, sample_sizes.values,
                      color=['#FF6B6B', '#4ECDC4'], alpha=0.8)
axes[0,1].set_title('Sample Sizes by Group')
axes[0,1].set_ylabel('Number of Users')

#Add value labels
for bar, size in zip(bars2, sample_sizes.values):
    axes[0,1].text(bar.get_x() + bar.get_width()/2, bar.get_height() + max(sample_sizes.values)*0.01,
                   f'{size:,}', ha='center', va='bottom', fontweight='bold')

#Plot 3: Engagement by day of week (if available)
if 'most_engagement_day' in df.columns:
    day_engagement = df.groupby(['most_engagement_day', 'message_type'])['engaged'].mean().unstack()
    day_engagement.plot(kind='bar', ax=axes[1,0], color=['#FF6B6B', '#4ECDC4'], alpha=0.8)
    axes[1,0].set_title('Engagement Rate by Day of Week')
    axes[1,0].set_ylabel('Engagement Rate')
    axes[1,0].set_xlabel('Day of Week')
    axes[1,0].legend(title='Message Type')
    axes[1,0].tick_params(axis='x', rotation=45)
else:
    #Alternative plot if day data not available
    hour_engagement = df.groupby('message_type')['engaged'].mean()
    bars3 = axes[1,0].bar(hour_engagement.index, hour_engagement.values,
                          color=['#FF6B6B', '#4ECDC4'], alpha=0.8)
    axes[1,0].set_title('Engagement Rate Comparison')
    axes[1,0].set_ylabel('Engagement Rate')
    for bar, rate in zip(bars3, hour_engagement.values):
        axes[1,0].text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.001,
                       f'{rate:.3f}', ha='center', va='bottom', fontweight='bold')

#Plot 4: Distribution of messages seen (if available)
if 'total_messages_seen' in df.columns:
    axes[1,1].hist([control_group['total_messages_seen'], treatment_group['total_messages_seen']], 
                   bins=30, alpha=0.7, label=['Generic', 'Personalized'], color=['#FF6B6B', '#4ECDC4'])
    axes[1,1].set_title('Distribution of Total Messages Seen')
    axes[1,1].set_xlabel('Total Messages Seen')
    axes[1,1].set_ylabel('Frequency')
    axes[1,1].legend()
else:
    #Alternative plot if messages data not available
    engagement_counts = df.groupby(['message_type', 'engaged']).size().unstack(fill_value=0)
    engagement_counts.plot(kind='bar', ax=axes[1,1], color=['#FFB3B3', '#B3E5E0'], alpha=0.8)
    axes[1,1].set_title('Engagement Distribution by Group')
    axes[1,1].set_xlabel('Message Type')
    axes[1,1].set_ylabel('Count')
    axes[1,1].legend(['Not Engaged', 'Engaged'])
    axes[1,1].tick_params(axis='x', rotation=0)

plt.tight_layout()
plt.show()

print("\n Analysis Complete!")
print("\nThis analysis provides a comprehensive A/B test evaluation including:")
print("- Statistical significance testing")
print("- Effect size calculation") 
print("- Confidence intervals")
print("- Power analysis")
print("- Additional exploratory analysis")
print("- Visualizations")

#Save results to a summary file (optional)
summary_results = {
    'test_type': 'A/B Test - Conservation Campaign',
    'control_group': 'Generic',
    'treatment_group': 'Personalized',
    'control_rate': control_rate,
    'treatment_rate': treatment_rate,
    'absolute_difference': treatment_rate - control_rate,
    'relative_lift_percent': lift,
    'p_value': p_value_z,
    'effect_size_cohens_h': effect_size,
    'statistical_power': observed_power,
    'sample_size_control': control_total,
    'sample_size_treatment': treatment_total,
    'statistically_significant': p_value_z < 0.05,
    'recommendation': recommendation
}

print(f"\n SUMMARY RESULTS:")
for key, value in summary_results.items():
    if isinstance(value, float):
        print(f"- {key}: {value:.4f}")
    else:
        print(f"- {key}: {value}")