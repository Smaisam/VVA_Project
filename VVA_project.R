#Installing required packages
install.packages("survival")
install.packages("survminer")

#Importing packages
library(survminer)
library(survival)

#Importing data and storing it as a dataframe
df<-read.csv("VVA_Guacamole2.csv")

#Converting NA values to 400
df[6:10,3]<-400

#Creating status column, and filling it with 0 for samples that didn't turn brown and 1 for samples that did turn brown
df$status<-rep(NA,10)
df[1:5,4]<-1
df[6:10,4]<-0


#Calculating statistics for the time column( dependent variable)
sd(df$Time)
var(df$Time)
mean(df$Time)
median(df$Time)
quantile(df$Time)
quantile(df$Time)[4] - quantile(df$Time)[2]#interquartile range
max(df$Time)
min(df$Time)
max(df$Time)-min(df$Time)

#Histogram of time column
hist(df$Time, main="Distribution of Time variable", xlab="Time (minutes)")

#Making boxplot of time column
boxplot(df$Time)

#Boxplot of Time and Treatment column
boxplot(df$Time ~ df$Treatment, xlab= "Treatment", ylab="Time", names=c("No","Lemon Juice"), main= "Boxplot of Treated vs Untreated Samples")

#Survival analysis
##Create survival object

surv_object <- Surv(time = df$Time, event = df$status)

##Fit survival  curves
km_fit <- survfit(surv_object ~ Treatment, data = df)

##Potting the curve
plot(km_fit, col = c("red", "blue"), xlab = "Time (minutes)", ylab = "Survival probability", main = "Guacamole Browning")
legend("bottomleft", legend = c("No Lemon", "Lemon"), col = c("red", "blue"), lty = 1, cex=.6)

##Log rank test
survdiff(surv_object ~ Treatment, data = df)

#Unpaired sample t-test
#X=samples without lemon
#Y=samples with lemon
#X-Y < 0 
t.test(x=df$Time[1:5], y=df$Time[6:10], alternative = 'less', conf.level = 0.95)


