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
data<-data[!data$Health.Score == 10.0, ]
colnames(data) = c("Obs","Quarter","Employee.Id","Gender","Race","Age","Hospital.Visit","Salary","Health.Score")
data$Salary = as.numeric(gsub('[$,]', '', data$Salary))

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
fits = lmList(response )
summary(model1)

# Fixed effect model controlling other variables
data<-data[!data$Age > 100, ]
model2 = plm(Health.Score~Quarter+ Age + Hospital.Visit + Salary, data=data, index = c("Employee.Id"), model = "within")
summary(model2)

texreg(list(model1,model2))

