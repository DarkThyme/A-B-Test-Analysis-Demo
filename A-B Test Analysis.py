import numpy as np
import pandas as pd
from scipy.stats import *

#Creating Data 
N_exp = 100000   # experiment group
N_con = 100000   # control group

c_exp = pd.Series(np.random.binomial(1,0.43,size= N_exp))
c_con = pd.Series(np.random.binomial(1,0.17,size= N_con))

#Creating identifiers
exp_id = pd.Series(np.repeat("exp", N_exp))
con_id = pd.Series(np.repeat("con", N_con))

df_exp = pd.concat([c_exp, exp_id], axis= 1)
df_con = pd.concat([c_con, con_id], axis= 1)

df_exp.columns = ["Click", "Group"]
df_con.columns = ["Click", "Group"]

df_ab_test = pd.concat([df_exp , df_con], axis= 0).reset_index(drop= True)

#print(df_ab_test)

#Calculating the number of events happening in control group and experiment group (counting 1s in both groups)

x_con = df_ab_test.groupby("Group")["Click"].sum().loc["con"]
x_exp = df_ab_test.groupby("Group")["Click"].sum().loc["exp"]

#Calculating the probability

p_con_h = x_con/N_con
p_exp_h = x_exp/N_exp

#print(p_con_h, p_exp_h)

#calculating the estimate of pooled success probability

p_pooled_h = (x_con + x_exp)/(N_con + N_exp)
#print(p_pooled_h)

#Calculating Pooled variance

pooled_variance = p_pooled_h * (1 - p_pooled_h) * (1/N_con + 1/N_exp)

SD = np.sqrt(pooled_variance)

Test_stat = (p_con_h - p_exp_h)/ SD

#Confidence Interval assumption
alpha = 0.05

# Calculating critical value from standard normal distribution using Inverse cumulative distribution function
Z_crit = norm.ppf(1- alpha / 2)

p_value = 2 * norm.sf(abs(Test_stat))

#print(p_value)

#Calculating Confidence Interval 
CI = [round((p_exp_h - p_con_h) - SD*Z_crit, 4), round((p_exp_h - p_con_h) + SD*Z_crit, 4)]  
#print(CI)




