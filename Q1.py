######## README ##############
# Author: Jingyi Wu
# Date: Apr 6, 2020
# Project: Acumen Data Anlysis
##############################

import pandas as pd
import numpy as np

# Import data from source dataset
data = pd.read_csv('https://raw.githubusercontent.com/Killshadows/Acumen/master/data.csv')
data.head()

data.columns


"""
Question 1

a. Are all the values in the data reasonable? Are there missing values?

"""

# Check missing values or abnormal values by giving brief summaries on the data
for i in data.columns:
   
    # print variables names:
    print('Variable:', i)
    
    # print brief summaries 
    print(data[i].value_counts().sort_index())
    
    # print warnings if there are missing data
    print('Non-null row count:', data[i].count())
    if data[i].count() < len(data[i]):
         print('MISSING VALUES!!!')
    elif data[i].count() == len(data[i]):
         print('no missing values')
            
    print('')
    
    

"""
Question 1

b. What are the characteristics of employees at Company A? Do these demographics change over time?

"""
# Plotting gender ratio over time

# Exclude NULL records and add count column
data_gender = data[['Quarter','Sex (Male=1)']].dropna()
data_gender.insert(2, 'count', np.ones(len(data_gender)))

# Group by and add calculate ratio
data_gender = data_gender.groupby(['Quarter','Sex (Male=1)'], as_index=False).count()
data_gender['total'] = data_gender.groupby('Quarter')['count'].transform('sum')
data_gender['ratio'] = data_gender['count'].div(data_gender['total'])

# Plotting
plot1 = (ggplot(data_gender, aes(x='Quarter', y='ratio')) +
        geom_point(alpha = 0.5) +
        geom_smooth(se=False, method="loess", color="grey") +
        labs(title="Gender Ratio Over Time",
             x="Quarter",
             y="Ratio") +
        scale_x_continuous(breaks=range(1, 13), minor_breaks=[]) + 
        facet_wrap('Sex (Male=1)')
)
plot1.save('/Users/killshadows/Desktop/gender.png')



# Plotting gender ratio over time

# Exclude NULL records and add count column
data_race = data[['Quarter','Race']].dropna()
data_race.insert(2, 'count', np.ones(len(data_race)))
data_race = data_race.groupby(['Quarter','Race'], as_index=False).count()

# Group by and add calculate ratio
data_race['total'] = data_race.groupby('Quarter')['count'].transform('sum')
data_race['ratio'] = data_race['count'].div(data_race['total'])

# Plotting
plot2 = (ggplot(data_race, aes(x='Quarter', y='ratio')) +
        geom_point(alpha = 0.5) +
        geom_smooth(se=False, method="loess", color="grey") +
        labs(title="Race Ratio Over Time",
             x="Quarter",
             y="Ratio") +
        scale_x_continuous(breaks=range(1, 13), minor_breaks=[]) + 
        facet_wrap('Race')
)
plot2.save('/Users/killshadows/Desktop/race.png')


# Plotting age distribution over time

# Exclude NULL and abnormal records and add count column
data_age = data[['Quarter','Age']].dropna()
data_age = data_age[data_age['Age']<100]
data_age.insert(2, 'count', np.ones(len(data_age)))
data_age = data_age.groupby(['Quarter','Age'], as_index=False).count()

# Plotting
plot3 = (ggplot(data_age, aes(x='Age')) +
        geom_density(kernel="gaussian") +
        labs(title="Age Distribution Over Time",
             x="Age") +
        scale_x_continuous()+
        facet_wrap('Quarter')
)
plot3.save('/Users/killshadows/Desktop/age.png')



# Summarize salary characteristics

# Exclude NULL records
data_sal = data[['Quarter','Employee Id','Salary']].dropna()
data_sal['Salary'] = data_sal['Salary'].replace('[\$,]', '', regex=True).astype('int')
data_sal = data_sal.groupby(['Quarter','Employee Id'], as_index=False).mean()

# Plotting
plot4 = (ggplot(data_sal, aes(x='Salary')) +
        geom_density(kernel="gaussian") +
        labs(title="Salary Distribution Over Time",
             x="Salary") +
        scale_x_continuous()+
        facet_wrap('Quarter')
)
plot4.save('/Users/killshadows/Desktop/salary.png')

# Describe the data
data_sal = data[['Quarter','Salary']].dropna()
data_sal['Salary'] = data_sal['Salary'].replace('[\$,]', '', regex=True).astype('int')
data_sal = data_sal.groupby(['Quarter']).describe()
data_sal