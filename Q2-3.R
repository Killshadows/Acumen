#######################
# Author: Jingyi Wu
# Date: Apr 6 2020
#######################

library(ggplot2)
library(plm)
library(lme4)
library(texreg)

# Import data
data = read.csv("https://raw.githubusercontent.com/Killshadows/Acumen/master/data.csv")

# drop abnormal values (score=10.0)
data = data[!data$Health.Score == 10.0, ]
colnames(data) = c("Obs","Quarter","Employee.Id","Gender","Race","Age","Hospital.Visit","Salary","Health.Score")
data$Salary = as.numeric(gsub('[$,]', '', data$Salary))

data_age = data[!data$Age > 100, ]

#######################
# Q2 
#######################

# Box plot and Scatter Plot
attach(data)
plot(as.factor(Gender),Health.Score, xlab="Gender", ylab="Health Score")
plot(as.factor(Race),Health.Score, xlab="Race", ylab="Health Score")
plot(as.factor(Hospital.Visit),Health.Score, xlab="Hospital.Visit", ylab="Health Score")
plot(Salary,Health.Score, xlab="Salary", ylab="Health Score",pch = 20)

attach(data_age)
plot(Age,Health.Score, xlab="Age", ylab="Health Score",pch = 20)


# Regressions on the relationship of score and characteristics variables
# regression - gender, hospital.visit, salary
asso_gender = lm(Health.Score~as.factor(Gender),data=data)
asso_hospital = lm(Health.Score~as.factor(Hospital.Visit),data=data)
asso_salary = lm(Health.Score~Salary,data=data)

# regression - age
asso_age = lm(Health.Score~Age,data=data_age)

# regression - race
# generate dummy variables to do regression
data_race = data[!is.na(data$Race),]
data_race$Race3 = as.numeric(data_race$Race == 3)
data_race$Race2 = as.numeric(data_race$Race == 2)
data_race$Race1 = as.numeric(data_race$Race == 1)

asso_race = lm(Health.Score~Race2+Race3,data=data_race)

texreg(list(asso_gender,asso_race,asso_age,asso_hospital,asso_salary))


#######################
# Q3
#######################

# Plot overtime variation
data$Quarter = as.factor(data$Quarter)
plot= ggplot(data=data,aes(x=Quarter,y=Health.Score)) + geom_boxplot() + xlab("Quarter") + ylab("Health Score")
plot

# Plot individual trends
plot2 = ggplot(data=data, aes(x=Quarter,y=Health.Score, group=Employee.Id, color=Gender)) + 
  geom_smooth(method = "lm",se = FALSE, size=0.2)
plot2

# Retrieve trend coefficients of grouped regressions
data$Quarter = as.numeric(data$Quarter)
fits = lmList(Health.Score ~ Quarter | Employee.Id, data=data)
coef = coef(fits)

summary(coef)
plot3 = ggplot(coef, aes(x=Quarter)) + geom_density(kernel = 'gaussian') + labs(title="Trend Coefficient Distribution",x="Coefficient")
plot3



# Fixed effect model
model1 = plm(Health.Score~Quarter, data=data, index = c("Employee.Id"), model = "within")
summary(model1)

# Fixed effect model controlling other variables
model2 = plm(Health.Score~Quarter+ Age + Hospital.Visit + Salary, data=data_age, index = c("Employee.Id"), model = "within")
summary(model2)

texreg(list(model1,model2))

