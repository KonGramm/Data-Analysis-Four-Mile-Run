#------------------------- Msc In Statistics AUEB ---------------------------
#
# Purpose: First Project in Advanced Data Analysis with R.
# Kevin decided to begin a running program. Kevin is interested 
# in determining how effective his training 
# regimen is at improving his cardiovascular fitness and how he might modify his effort 
# on individual runs in order to optimize overall health benefits.  
# The data that appear in this dataset were collected by a Global Positioning 
# System (GPS) watch worn by the runner of a four-mile course.  Among all the variables 
# that were collected, the relationship between training effect and average heart rate and 
# maximum heart rate is the primary focus.  Using heart rate measurements after each run, 
# an analysis of post-exercise heart rate recovery provides an indication of cardiovascular 
# fitness.

# ASSIGNMENT TASKS :

# 1. Perform an exploratory analysis to monitor the progress of Kevin.  
# 2. Use visual methods to assess Kevin's progress.  
# 3. Study the pairwise associations between the variables  
# 4. Construct a regression  model (or more) to assess the progress of Kevin. 
# 
# In order to be able to run this code you have to download the 23_Four-Mile_Run_Dataset csv file
# and assign to the variable named path, the file path where you have stored this csv file.
#
# Author: Konstantinos Grammenos
# 
# #Date: 16/12/2023
#
# Proffesor:I. Ntzoufras




#-----------------------------EXPLONATORY ANALYSIS----------------------------------------------

#-------------------- Import the necessary libraries--------------------------
library(psych)
library(ggplot2)
library(corrplot)
library(car)
library(dplyr)
library("nortest")
library(MASS)
library(Hmisc) 
library(randtests)
library(VGAM)
library(BAS)
library(car)

#------------------------ LOAD THE DATA ------------------------------------------------------------

FourMileRun<- read.csv("C:\\Users\\kosti\\OneDrive\\Desktop\\Data_Analysis\\Data Analysis Project 1\\23_Four-Mile_Run_Dataset.csv")
head(FourMileRun)
FourMileRun<- subset(FourMileRun, select = -X)
FourMileRun <- as.data.frame(FourMileRun)

#----------------------------- DESCRIBING THE DATA ----------------------------------------------------
str(FourMileRun)
summary(FourMileRun)
describe(FourMileRun)

#----------------------------- TRANSFORMING THE DATA --------------------------------------------------
breaks <- c(1, 1.9, 2.9, 3.9, 4.9, 5)
labels <- c("Minor","Maintaining","Improving","Highly Improving","Overreaching")
FourMileRun$Train.Effect.Cat <- cut(FourMileRun$Training.Effect,breaks=breaks,labels=labels,include.lowest = T)

# Splitting time into minutes and seconds
timesplit <- strptime(FourMileRun$Time, format = "%M:%S")
FourMileRun$Time.Minutes <- timesplit$min
FourMileRun$Time.Seconds <-  timesplit$sec

# Creating time in seconds and minutes columns
FourMileRun$Time.In.Seconds <- FourMileRun$Time.Minutes * 60 + FourMileRun$Time.Seconds
FourMileRun$Time.In.Minutes<- round(FourMileRun$Time.Seconds/60,digits=2) + FourMileRun$Time.Minutes

# Splitting Pace into minutes and seconds
pacesplit <- strptime(FourMileRun$Pace, format = "%M:%S")
FourMileRun$Pace.Minutes <- pacesplit$min
FourMileRun$Pace.Seconds <-  pacesplit$sec

# Creating pace in seconds and minutes columns
FourMileRun$Pace.In.Seconds <- FourMileRun$Pace.Minutes * 60 + FourMileRun$Pace.Seconds
FourMileRun$Pace.In.Minutes <- round(FourMileRun$Pace.Seconds / 60, digits=2) + FourMileRun$Pace.Minutes

#----------------------------- REDESCRIBING THE DATA ----------------------------------------------

# Describe only numeric or integer variables
classes <- sapply(FourMileRun, class)
quant_var <- FourMileRun[(classes == 'numeric') | (classes == 'integer')]


# Cleaning and formatting the describe output
quant_var_df <- quant_var[, -c(1, 2)]

quant_var_df <- round(quant_var_df, digits = 2)
row_names_to_delete <- c("Run", "Time.Minutes", "Pace.Minutes", "Time.Seconds", "Time.In.Seconds", "Pace.Seconds", "Pace.In.Seconds", "Training.Effect")
quant_var_df <- quant_var_df[!(rownames(quant_var_df) %in% row_names_to_delete), , drop = FALSE]

# Quantiles from 10% to 90% increasing by 10%
sapply(quant_var, quantile, probs = seq(0.1, 0.90, 0.1))

# Unique categories in Train.Effect.Cat
unique(FourMileRun$Train.Effect.Cat)
describe.by(FourMileRun, FourMileRun$Train.Effect.Cat)

# Summary statistics by Train Effect Category
summary_stats_by_Train_Effect_Cat <- FourMileRun %>%
  filter(Train.Effect.Cat %in% c("Minor", "Improving", "Highly Improving")) %>%
  group_by(Train.Effect.Cat) %>%
  dplyr::summarize(
    count = n(),
    mean_Max.HR = mean(Max.HR, na.rm = TRUE),
    sd_Max.HR = sd(Max.HR, na.rm = TRUE),
    mean_Avg.HR = mean(Avg.HR, na.rm = TRUE),
    sd_Avg.HR = sd(Avg.HR, na.rm = TRUE),
    mean_HR.Rest = mean(HR.Rest, na.rm = TRUE),
    sd_HR.Rest = sd(HR.Rest, na.rm = TRUE),
    mean_Calories.Burned = mean(Calories.Burned, na.rm = TRUE),
    sd_Calories.Burned = sd(Calories.Burned, na.rm = TRUE),
    mean_Avg.Speed = mean(Avg.Speed, na.rm = TRUE),
    sd_Avg.Speed = sd(Avg.Speed, na.rm = TRUE),
    mean_Pace.In.Minutes = mean(Pace.In.Minutes, na.rm = TRUE),
    sd_Pace.In.Minutes = sd(Pace.In.Minutes, na.rm = TRUE),
    mean_Time.In.Minutes = mean(Time.In.Minutes, na.rm = TRUE),
    sd_Time.In.Minutes = sd(Time.In.Minutes, na.rm = TRUE)
  )

# Display the summary statistics
summary_stats_by_Train_Effect_Cat



#----------------------------- FINDING OUTLIERS -------------------------------------------------------

numeric_or_integer <- sapply(FourMileRun, function(x) is.numeric(x) || is.integer(x))
y <- FourMileRun[, numeric_or_integer]
n <- ncol(y)
eda <- boxplot(y)
names(eda)
eda$stats
colnames(eda$stats) <- names(y)
rownames(eda$stats) <- c('LB', 'Q1', 'M', 'Q3', 'UB')
eda$stats
eda$conf

# Finding outliers for each variable
for (i in 1:length(y)) {
  y1 <- y[, i]  # Extract the ith column of the y dataframe
  
  # Computes outliers for the variable y1 using the boxplot method
  out <- boxplot(y1, plot = FALSE)$out
  
  # Checks if there are outliers in the variable
  if (length(out) != 0) {
    print('-------------------------------------------------------')
    print(paste('Outliers for variable', names(y)[i]))
    print(paste(length(out), 'outliers'))
    print(paste(round(100 * length(out) / sum(!is.na(y1)), 1), '% outliers', sep = ''))
    print(which(y1 %in% out))
  }
}



#----------------------------- PLOTTING HEART RATE RECOVERY OVER RUNS -------------------------------

ggplot(FourMileRun, aes(x = Run)) +
  geom_line(aes(y = `HR.Change1`), color = "blue") +
  geom_line(aes(y = `HR.Change2`), color = "red") +
  labs(title = "Heart Rate Recovery Over Runs", y = "Heart Rate Change")


#----------------------------- TESTING FOR NORMALITY -------------------------------------------------
# Setting up a 3x4 grid for QQ plots with smaller margins and adjusted label sizes
par(mfrow = c(3, 4), mar = c(3, 3, 2, 1) + 0.1, cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)

# QQ plots with adjusted axis text size
qqnorm(FourMileRun$Calories.Burned, main = "Calories Burned QQ plot", ylim = c(300, 440))
qqline(FourMileRun$Calories.Burned)

qqnorm(FourMileRun$Max.HR, main = "Max.HR QQ plot", ylim = c(145, 170))
qqline(FourMileRun$Max.HR)

qqnorm(FourMileRun$Avg.HR, main = "Avg.HR QQ plot")
qqline(FourMileRun$Avg.HR)

qqnorm(FourMileRun$Max.Speed, main = "Max.Speed QQ plot")
qqline(FourMileRun$Max.Speed)

qqnorm(FourMileRun$Avg.Speed, main = "Avg.Speed QQ plot")
qqline(FourMileRun$Avg.Speed)

qqnorm(FourMileRun$HR.Rest, main = "HR.Rest QQ plot")
qqline(FourMileRun$HR.Rest)

qqnorm(FourMileRun$HR.Rest1, main = "HR.Rest1 QQ plot")
qqline(FourMileRun$HR.Rest1)

qqnorm(FourMileRun$HR.Rest2, main = "HR.Rest2 QQ plot")
qqline(FourMileRun$HR.Rest2)

qqnorm(FourMileRun$HR.Change1, main = "HR.Change1 QQ plot")
qqline(FourMileRun$HR.Change1)

qqnorm(FourMileRun$HR.Change2, main = "HR.Change2 QQ plot")
qqline(FourMileRun$HR.Change2)

qqnorm(FourMileRun$Pace.In.Seconds, main = "Pace In Seconds QQ plot")
qqline(FourMileRun$Pace.In.Seconds)

qqnorm(FourMileRun$Time.In.Minutes, main = "Time In Minutes QQ plot")
qqline(FourMileRun$Time.In.Minutes)

#----------------------------- CLEANING DATA FOR PLOTS -----------------------------------------------

# Specifying columns to delete
columns_to_delete <- c("Run", "Time.Minutes", "Time.Seconds",
                  "Pace.Minutes", "Pace.Seconds", "Time.In.Seconds")
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

#Subsetting the data frame to exclude the specified columns
y <- y[, !names(y) %in% columns_to_delete]
p <- ncol(y)
par(mfrow = c(1, 1))

#----------------------------- VISUAL METHODS TO ASSESS PROGRESS --------------------------------------

#Barplot Of  percentages of each Training Effect Category
x<-100*table(FourMileRun$Train.Effect.Cat)/sum( table(FourMileRun$Train.Effect.Cat))
train_cat_perc <- barplot(x, ylab="Percentage(%)",col="aquamarine3", xlab="Training Effect Category",cex.lab=1.2, cex.axis=1.2,ylim=c(0,100))
barplot(x, ylab="Percentage(%)", col="aquamarine3", xlab="Training Effect Category", cex.lab=1.2, cex.axis=1.2, ylim=c(0, 100))

# Adjust the plot layout and margins
par(mfrow = c(2, 3), mar = c(5, 7, 4, 2), mgp = c(3, 1, 0), cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.6)

# Plot 1: Calories Burned Over Runs
plot(FourMileRun$Run, FourMileRun$Calories.Burned, type = "o", col = "forestgreen",
     xlab = "Run Number", ylab = "Calories", main = "Calories Burned Over Runs",
     las = 1, mgp = c(3, 1, 0))

# Plot 2: Pace In Seconds Over Runs
plot(FourMileRun$Run, FourMileRun$Pace.In.Seconds, type = "l", col = "red",
     xlab = "Run Number", ylab = "Pace", main = "Pace In Seconds Over Runs",
     las = 1, mgp = c(3, 1, 0))

# Plot 3: Training Effect Over Runs
plot(FourMileRun$Run, FourMileRun$Training.Effect, type = "o", col = "deepskyblue3",
     xlab = "Run Number", ylab = "Training Effect", main = "Training Effect Over Runs",
     las = 1)

# Plot 4: Time In Minutes Over Runs
plot(FourMileRun$Run, FourMileRun$Time.In.Minutes, type = "o", col = "coral",
     xlab = "Run Number", ylab = "Time", main = "Time In Minutes Over Runs",
     las = 1)

# Plot 5: Pace In Minutes Over Runs
plot(FourMileRun$Run, FourMileRun$Pace.In.Minutes, type = "o", col = "magenta3",
     xlab = "Run Number", ylab = "Pace", main = "Pace In Minutes Over Runs",
     las = 1)

# Plot 8: Pace In Minutes Over Runs
plot(FourMileRun$Run, FourMileRun$Pace.In.Minutes, type = "o", col = "forestgreen",
     xlab = "Run Number", ylab = "Pace", main = "Pace In Minutes Over Runs",
     las = 1)

#---------------------------- BOXPLOTS-----------------------------------------------------

# Adjust the plot layout, margins, and axis label sizes
par(mfrow = c(2, 3), mar = c(5, 7, 4, 2) + 0.1, cex.axis = 2, cex.lab = 2, cex.main = 1.6)

# Boxplot of Training Effect
boxplot(FourMileRun$Training.Effect, main="Boxplot of Training Effect", col="darkslategray3")

# Boxplot of Max Heart Rate during each run
boxplot(FourMileRun$Max.HR, main="Boxplot of Max HR During Run", col="burlywood1",ylim=c(145,170))

# Boxplot of Average Heart Rate during each run
boxplot(FourMileRun$Avg.HR, main="Boxplot of Avg HR During Run", col="mediumpurple1")

# Boxplot of Heart Rate 1 minute after each run
boxplot(FourMileRun$HR.Rest1, main="Boxplot of HR 1 Minute after Run", col="seagreen2")

# Boxplot of Heart Rate 2 minutes after each run
boxplot(FourMileRun$HR.Rest2, main="Boxplot of HR 2 Minutes after Run", col="deepskyblue3")


par(mfrow = c(1, 1))
# Plot 7: HR.Change1 and HR.Change2 Trend Over Runs
plot(FourMileRun$Run, FourMileRun$HR.Change1, type = "o", col = "deepskyblue3",
     xlab = "Run Number", ylab = "Heart Rate", main = "HR.Change1 and HR.Change2 Trend Over Runs", ylim = c(0,70),
     las = 1)

points(FourMileRun$Run, FourMileRun$HR.Change2, type = "o", col = "coral")
legend("topright", legend = c("HR.Change1", "HR.Change2"), col = c("deepskyblue3", "coral"), pch = 1, lty = 1, cex = 1)

# Plot 6: Heart Rate Trend Over Runs
plot(FourMileRun$Run, FourMileRun$Avg.HR, type = "o", col = "deepskyblue3",
     xlab = "Run Number", ylab = "Heart Rate", main = "Heart Rate Trend Over Runs", ylim = c(100,180),
     las = 1)
points(FourMileRun$Run, FourMileRun$Max.HR, type = "o", col = "coral")
legend("topright", legend = c("Avg HR", "Max HR"), col = c("deepskyblue3", "coral"), pch = 1, lty = 1, cex = 1)


par(mfrow = c(2, 3),cex.axis=1.8)
#Boxplot of the Calories that Kevin burned during these 19 runs
boxplot(FourMileRun$Calories.Burned,main="Calories Burned",col="cornflowerblue",ylim=c(300,440))

#Boxplot of Average Speed 
boxplot(FourMileRun$Avg.Speed,main="Avg Speed during run",col="seagreen2")

#Boxplot of Max Speed 
boxplot(FourMileRun$Max.Speed,main="Max Speed during run",col="darkslategray3")

#Boxplot of Heart Rate immidieately after each run
boxplot(FourMileRun$HR.Rest,main="HR after run",col="burlywood1")

#Boxplot of Heart Rate measured exactly 1 minute after each run
boxplot(FourMileRun$HR.Rest1,main="HR 1 Min after run",col="darkmagenta")

#Boxplot of Time in Minutes for each Run
boxplot(FourMileRun$Pace.In.Minutes,main="Pace in Minutes over runs",col="sienna")

#---------------------- SCATTER PLOTS ----------------------------------------------------------------------

par(mfrow = c(1, 1))

# Scatter plot of HR from start of rest and 1 minute later and HR from the start of rest and 2 minutes later
plot(FourMileRun$HR.Change1, FourMileRun$HR.Change2,
     xlab = "Change in HR from start of rest to 1 min later",
     ylab = "Change in HR from start of rest to 2 min later",
     main = "",
     cex.lab = 1.1, cex.main = 1.1, cex.axis = 1.1)
abline(lm(FourMileRun$HR.Change2 ~ FourMileRun$HR.Change1))

# Scatter plot of HR from start of rest and 1 minute later and HR from the start of rest and 2 minutes later
plot(FourMileRun$HR.Rest1, FourMileRun$HR.Rest2,
     xlab = "Heart Rate 1 Minute After the Run",
     ylab = "Heart Rate 2 Minutes After the Run",
     main = "",
     cex.lab = 1.1, cex.main = 1.1, cex.axis = 1.1)
abline(lm(FourMileRun$HR.Rest2 ~ FourMileRun$HR.Rest1))


plot(FourMileRun$Pace.In.Seconds, FourMileRun$Calories.Burned, pch = 16, col = "purple",
     xlab = "Pace In Seconds", ylab = "Calories Burned", main = "Scatter Plot of Pace vs Calories Burned")
abline(lm(FourMileRun$Calories.Burned~FourMileRun$Pace.In.Seconds))



plot(FourMileRun$Pace.In.Seconds, FourMileRun$Avg.HR, pch = 16, col = "blue",
     xlab = "Pace In Seconds", ylab = "Average HR", main = "Scatter Plot of Pace vs Average HR")
abline(lm(FourMileRun$Avg.HR~FourMileRun$Pace.In.Seconds))


plot(FourMileRun$Pace.In.Seconds, FourMileRun$Avg.Speed, pch = 16, col = "red",
     xlab = "Pace In Seconds", ylab = "Average Speed", main = "Scatter Plot of Pace vs Average Speed")
abline(lm(FourMileRun$Avg.Speed~FourMileRun$Pace.In.Seconds))

#--------------------------- PAIRWISE ASSOCIATIONS ----------------------------------------------

#Correlation between the main quantitative variables
#variables for correlation analysis
selected_vars <- FourMileRun[, c("Training.Effect", "Calories.Burned",  "Avg.HR","Max.HR", "Avg.Speed", "Max.Speed","HR.Rest","HR.Rest1","HR.Rest2","HR.Change1","HR.Change2","Pace.In.Minutes","Time.In.Minutes")]

cor.test(FourMileRun$Training.Effect,FourMileRun$Calories.Burned)

FourMileRun$Training.Effect

cor_test_result <- cor.test(FourMileRun$Training.Effect, FourMileRun$Calories.Burned)
#Null Hypothesis (Ho): There is no correlation between "Training.Effect" and "Calories.Burned.
# Access and save the p-value, we reject the null, pvalue =0.497
p_value <- cor_test_result$p.value

for (i in 1:length(selected_vars)) {
  for (j in (i + 1):length(selected_vars)-1) {
    variable1 <- colnames(selected_vars)[i]
    variable2 <- colnames(selected_vars)[j]
    if (i!=j){
      cor_test_result <- cor.test(selected_vars[, i], selected_vars[, j])
      p_value <- cor_test_result$p.value
    }
    cat("Correlation between", variable1, "and", variable2, ":", "\n")
    cat("P-Value:", p_value, "\n")
    cat("\n")
    
  }
}
# Calculate pairwise correlations
cor_matrix <- cor(selected_vars)

#Create the correlation plot
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.cex = 1.1,  number.cex = 1.1,
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

#---------------------------- SCATTER PLOTS OF CORRELATED VARIABLES -----------------------------------

par(mfrow = c(3, 3), mar = c(5,5, 4, 1))

plot(FourMileRun$Time.In.Minutes, FourMileRun$Calories.Burned, pch = 16, 
col = "blue", xlab = "Time In Minutes", ylab = "Calories Burned", 
main = "Time.In.Minutes vs Cal. Burned",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Calories.Burned~FourMileRun$Time.In.Minutes))


plot(FourMileRun$Pace.In.Minutes, FourMileRun$Calories.Burned, 
pch = 16, col = "purple", xlab = "Pace In Minutes", 
ylab = "Calories Burned", main = "Pace.In.Minutes vs Cal. Burned",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Calories.Burned~FourMileRun$Pace.In.Minutes))


plot(FourMileRun$Calories.Burned, FourMileRun$Avg.Speed, pch = 16,
col = "maroon", xlab = "Calories Burned", ylab = "Avg Speed",
main = "Calories Burned vs Avg Speed",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Avg.Speed~FourMileRun$Calories.Burned))

plot(FourMileRun$HR.Change1, FourMileRun$Max.HR, pch = 16,
col = "forestgreen", xlab = "HR.Change1", 
ylab = "Max.HR", main = "HR.Change1 vs Max.HR",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Max.HR~FourMileRun$HR.Change1))


plot(FourMileRun$Max.HR, FourMileRun$HR.Rest, pch = 16,
col = "darkmagenta", xlab = "Max.HR", ylab = "HR.Rest",
main = "Max.HR vs HR.Rest",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$HR.Rest~FourMileRun$Max.HR))


plot(FourMileRun$HR.Rest2, FourMileRun$HR.Rest, pch = 16,
col = "steelblue3", xlab = "HR.Rest2",
ylab = "HR.Rest", main = "HR.Rest2 vs HR.Rest",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$HR.Rest~FourMileRun$HR.Rest2))

plot(FourMileRun$Max.HR, FourMileRun$Training.Effect, pch = 16,
col = "darksalmon", xlab = "Max.HR", ylab = "Training Effect", 
main = "Max HR vs Training Effect",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Training.Effect~FourMileRun$Max.HR))

plot(FourMileRun$Avg.HR, FourMileRun$Training.Effect, 
pch = 16, col = "bisque3", xlab = "Avg HR", 
ylab = "Training Effect", main = "Avg HR vs Training Effect",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Training.Effect~FourMileRun$Avg.HR))

plot(FourMileRun$HR.Change1, FourMileRun$Training.Effect, pch = 16,
col = "cyan3", xlab = "HR.Change1", ylab = "Training Effect", 
main = "HR.Change1 vs Training Effect",cex.main = 1.5,cex.axis = 1.5,cex.lab=1.5)
abline(lm(FourMileRun$Training.Effect~FourMileRun$HR.Change1))

#-------------------------------NORMALITY TESTS--------------------------------------------

#Normality Test Lilliefors test
sapply(y,lillie.test)

#Normality Shapiro‐Wilks test
for (i in length(y)){
  print(shapiro.test(y[,i]))
}
sapply(y,shapiro.test)
#we see that the p value of the Shapiro Test for Calories.Burned is 0.93 so we do not reject Ho and assume normality for Calories.Burned
#we see that the p value of the Shapiro Test for Training Effect is  1.377171e-05 so
#we reject Ho and we can not assume normality for Training Effect
#Training Effect is not a continuous variable
shapiro.test(FourMileRun$Training.Effect)

#we see that the p value of the Shapiro Test for Avg.Hr is  0.03741015<0.05 so we reject Ho and we can not assume normality for Avg.Hr
#We have no normality for Avg.HR
#the sample is not large, we have 19 obs<50
wilcox.test(FourMileRun$Avg.HR,mu=130)

#we see that the p value of the Shapiro Test for Max.HR is 0.5495749 so we do  not reject Ho and assume normality for Max.HR
#t test fro Max.HR
t.test(FourMileRun$Max.HR,mu=155)
#we reject Ho so 

#we see that the p value of the Shapiro Test for Avg.Speed is 0.1130426 so we do  not reject Ho and assume normality for Avg.Speed

#we see that the p value of the Shapiro Test for Max.Speed is 0.452232  so we do  not reject Ho and assume normality for Max.Speed

#we see that the p value of the Shapiro Test for HR.Rest is  0.1022817  so we do  not reject Ho and assume normality for HR.Rest

#we see that the p value of the Shapiro Test for HR.Rest1 is 0.6875721  so we do  not reject Ho and assume normality for HR.Rest1

#we see that the p value of the Shapiro Test for HR.Rest2 is 0.4833235 so we do  not reject Ho and assume normality for HR.Rest2

#we see that the p value of the Shapiro Test for HR.Change1 is 0.9585424 so we do  not reject Ho and assume normality for HR.Change1

#we see that the p value of the Shapiro Test for HR.Change2 is 0.5785248 so we do  not reject Ho and assume normality for HR.Change2

#we see that the p value of the Shapiro Test for Pace.In.Seconds is 0.2295416   so we do  not reject Ho and assume normality for Pace.In.Seconds 
#same for Pace.In.Minutes and for Time.In.Minutes (we assume normality)

# Paired t-test for HR.Rest1 and HR.Rest2
result <- t.test(FourMileRun$HR.Rest1, FourMileRun$HR.Rest2, paired = TRUE)
result

#The mean of "HR.Rest1" is not equal to the mean of "HR.Rest2" in the population,as 
#we reject Ho that true mean difference is equal to 0, and we make an error bar for the mean difference
#pvalue is 0.03243<0.05
# Plotting error-bar of the mean difference
mean_diff <- result$estimate
ci_lower <- result$conf.int[1]
ci_upper <- result$conf.int[2]
plot_data <- data.frame(group = "Difference", mean_diff = mean_diff, ymin = ci_lower, ymax = ci_upper)

ggplot(plot_data, aes(x = group, y = mean_diff, ymin = ymin, ymax = ymax)) +
  geom_errorbar(width = 0.8, position = position_dodge(0.8)) +  # Increased width
  geom_point(position = position_dodge(0.8), size = 5) +        # Increased point size
  labs(title = "", x = "Group", y = "Mean Difference") +
  theme(
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

# Wilcoxon test for dependent samples
wilcox_result <- wilcox.test(FourMileRun$Avg.HR, FourMileRun$HR.Rest, paired = TRUE)
print(wilcox_result)

wilcox_result1 <- wilcox.test(FourMileRun$Avg.HR, FourMileRun$HR.Rest1, paired = TRUE)
print(wilcox_result1)

#strongly reject Ho, p-value is 0.0001424
#there is difference in the medians between the two paired groups
x<- FourMileRun$Avg.HR - FourMileRun$HR.Rest1

# Create a boxplot of the differences
boxplot(x, main = "Boxplot of the Difference between Avg.HR and HR.Rest1", ylab = "Difference",col="lightcoral")
mean(x)
median(x)
shapiro.test(x)
#differences follow a normal distrib.

wilcox_result2 <- wilcox.test(FourMileRun$Avg.HR, FourMileRun$HR.Rest2, paired = TRUE)
wilcox_result2
#strongly reject Ho, p-value is 0.0001417
#there is difference in the medians between the two paired groups
x1<- FourMileRun$Avg.HR - FourMileRun$HR.Rest2
boxplot(x1, main = "Boxplot of the Difference between Avg.HR and HR.Rest2", ylab = "Difference",col="magenta3")
mean(x1)
median(x1)
shapiro.test(x1)

# Paired t-test for time of runs
first_half_runs <- FourMileRun$Time.In.Minutes[1:(length(FourMileRun$Time.In.Minutes)/2)]
second_half_runs <- FourMileRun$Time.In.Minutes[(length(FourMileRun$Time.In.Minutes)/2 + 1):length(FourMileRun$Time.In.Minutes)]
shapiro.test(first_half_runs)
shapiro.test(second_half_runs)
result <- t.test(first_half_runs, second_half_runs, paired = TRUE)

par(mfrow = c(1, 1))
# Comparative Box Plots for Early and Later Runs
early_runs <- FourMileRun[FourMileRun$Run <= 10,]
later_runs <- FourMileRun[FourMileRun$Run >= (max(FourMileRun$Run) - 9),]
later_runs
# Setting up colors
colors <- c("forestgreen", "maroon")  # Blue for early runs, red for later runs

##-----------------------  Comparative Analysis of Pace --------------------------

# Comparative Box Plot for Pace
par(mfrow=c(3,2)) # Set up the plotting area to display three plots side by side

# Pace
boxplot(early_runs$Pace.In.Minutes, later_runs$Pace.In.Minutes,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Pace",
        ylab = "Pace In Minutes",
        col = colors)
##-----------------------  Comparative Analysis of Time --------------------------

boxplot(early_runs$Time.In.Minutes, later_runs$Time.In.Minutes,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Time",
        ylab = "Time In Minutes",
        col = colors)

##-----------------------  Comparative Analysis of Calories --------------------------
# Calories Burned
boxplot(early_runs$Calories.Burned, later_runs$Calories.Burned,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Calories Burned",
        ylab = "Calories Burned",
        col = colors)

##-----------------------  Comparative Analysis of HR --------------------------
boxplot(early_runs$Avg.HR, later_runs$Avg.HR,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Avg HR",
        ylab = "Average Heart Rate (bpm)",
        col = colors)

##-----------------------  Comparative Analysis of MAX HR --------------------------
boxplot(early_runs$Max.HR, later_runs$Max.HR,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Max HR",
        ylab = "Max Heart Rate (bpm)",
        col = colors)


##-----------------------  Comparative Analysis of Train Effect --------------------------
boxplot(early_runs$Training.Effect, later_runs$Training.Effect,
        names = c("Early Runs", "Later Runs"),
        main = "Comparative Analysis of Training Effect",
        ylab = "Training Effect",
        col = colors)

# Reset the plotting area
par(mfrow=c(1,1))


#----------------- ASSOCIATION ANALYSIS----------------------------

#we have very few observations so this analysis does not have a real meaning
anova1 <- aov(Max.HR~Train.Effect.Cat, data = FourMileRun)
summary(anova1)
# Residuals QQ plot for ANOVA1
qqnorm(residuals(anova1),main = "Residuals of Anova1 QQ plot")
qqline(residuals(anova1))

shapiro.test(residuals(anova1))
#p value=0.3275 so we do not reject Ho so there is normality for residuals of anova1

#-------------- Homogeneity of variances tests --------------------------------------------------

#Fligner-Killeen test. The test assumes a similar sample size across groups for accurate results, so not valid here
fligner.test(Max.HR~Train.Effect.Cat,data = FourMileRun)
#p value =0.2673 so we do not reject the null hypothesis, suggesting that there is no strong evidence of different variances
#levene test
leveneTest(Max.HR~Train.Effect.Cat,data = FourMileRun)
#p value =0.3905 so we do not reject the null hypothesis, suggesting that there is no strong evidence of different variances

#------------------ Function to create error bars -----------------------------------------------
myerrorbar<-function(x,y, horizontal=F){
  a<-0.05
  sdata <- split(x,y)
  means <- sapply( sdata,mean )
  sds <- sapply( split(x,y), sd )
  ns <- table(y)
  LB <- means + qnorm( a/2 ) * sds /sqrt(ns)
  UB <- means + qnorm( 1-a/2 ) * sds /sqrt(ns)
  nlev <- nlevels(y)
  if (horizontal) { errbar( levels(y), means, UB, LB ) 
  } else {
    errbar( 1:nlev, means, UB, LB, xlim=c(0,nlev+1), axes=F, xlab='' ) 
    axis(2)
    axis(1, at=0:(nlev+1), labels=c('',levels(y),''),las = 2,cex.axis=0.6)	
  }
  
}


#summary(res.anova1) will provide information about whether there are significant differences in the means of Max.HR across the levels of Train.Effect.Cat
#Test for equality of  means: ANOVA F-test
cur.formula <- as.formula("Max.HR~Train.Effect.Cat")
res.anova1 <- aov(cur.formula,data=FourMileRun)
summary(res.anova1)
#do not reject Ho so there are not significant differences between the means

x<-FourMileRun$Max.HR 
y<-FourMileRun$Train.Effect.Cat
myerrorbar( x, y ) 

#Testing for the association between Avg.HR and Train.Effect.Cat
anova2 <- aov(Avg.HR~Train.Effect.Cat, data = FourMileRun)
summary(anova2)

qqnorm(residuals(anova2),main = "Residuals of Anova2 QQ plot")
qqline(residuals(anova2))


shapiro.test(residuals(anova2))
#p value=0.9906 so we do not reject Ho so there is normality for residuals of anova2

#fligner.test(Avg.HR~Train.Effect.Cat,data = FourMileRun)
#p value =0.2685 so we do not reject the null hypothesis, suggesting that there is no strong evidence of different variances
#levene test
leveneTest(Avg.HR~Train.Effect.Cat,data = FourMileRun)
#p value =0.3096 so we do not reject the null hypothesis, suggesting that there is no strong evidence of different variances

#summary(res.anova2) will provide information about whether there are significant differences in the means of Avg.HR across the levels of Train.Effect.Cat
#Test for equality of  means: ANOVA F-test
cur.formula <- as.formula("Avg.HR~Train.Effect.Cat")
res.anova2 <- aov(cur.formula,data=FourMileRun)
summary(res.anova1)
#do not reject Ho so there are not significant differences between the means

x<-FourMileRun$Avg.HR 
y<-FourMileRun$Train.Effect.Cat
myerrorbar( x, y ) 

FourMileRun$Train.Effect.Cat.Short <- FourMileRun$Train.Effect.Cat
levels(FourMileRun$Train.Effect.Cat.Short) <- c("Min", "Main", "Imp", "H.Imp", "Over")

# --------------------------- BOX PLOTS PER TRAINING CATEGORY --------------------------------------------------

# Set up the plotting area and margins
par(mfrow = c(2, 3), mar = c(6, 6, 4, 2) + 0.1, cex.lab = 1.8, cex.axis = 1.5)

# Define colors for the plots
colors <- list(
  c("skyblue", "lightgreen", "lightcoral", "lightgoldenrodyellow", "lightsalmon"),
  c("skyblue", "lightgreen", "purple", "lightgoldenrodyellow", "lightsalmon"),
  c("skyblue", "lightgreen", "aquamarine3", "lightgoldenrodyellow", "lightsalmon"),
  c("skyblue", "lightgreen", "maroon", "lightgoldenrodyellow", "lightsalmon"),
  c("skyblue", "lightgreen", "orange", "lightgoldenrodyellow", "lightsalmon")
)

# Plot 1: Box Plot of Average Heart Rate by Training Effect Category
boxplot(Avg.HR ~ Train.Effect.Cat.Short, data = FourMileRun, outline = TRUE, col = colors[[1]],
        main = "Avg HR by Training Effect Category", 
        xlab = "Training Effect Category", ylab = "Average Heart Rate")

# Plot 2: Box Plot of Max Heart Rate by Training Effect Category
boxplot(Max.HR ~ Train.Effect.Cat.Short, data = FourMileRun, outline = TRUE, col = colors[[2]],
        main = "Max HR by Training Effect Category", 
        xlab = "Training Effect Category", ylab = "Max Heart Rate")

# Plot 3: Box Plot of HR.Rest by Training Effect Category
boxplot(HR.Rest ~ Train.Effect.Cat.Short, data = FourMileRun, outline = TRUE, col = colors[[3]],
        main = "HR.Rest by Training Effect Category", 
        xlab = "Training Effect Category", ylab = "HR.Rest")

# Plot 4: Box Plot of HR.Change1 by Training Effect Category
boxplot(HR.Change1 ~ Train.Effect.Cat.Short, data = FourMileRun, outline = TRUE, col = colors[[4]],
        main = "HR.Change1 by Training Effect Category", 
        xlab = "Training Effect Category", ylab = "HR.Change1")

# Plot 5: Box Plot of HR.Change2 by Training Effect Category
boxplot(HR.Change2 ~ Train.Effect.Cat.Short, data = FourMileRun, outline = TRUE, col = colors[[5]],
        main = "HR.Change2 by Training Effect Category", 
        xlab = "Training Effect Category", ylab = "HR.Change2")


par(mfrow = c(1, 1))
# Create a boxplot of the Calories Burned in each Training Effect Category
colors <- c("skyblue", "lightgreen", "coral2", "lightgoldenrodyellow", "lightsalmon")
boxplot(Calories.Burned  ~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Calories Burned by Training Effect Category", xlab = "Training Effect Category", ylab = "Calories Burned")


# Create a boxplot of the Time In Minutes in each Training Effect Category
colors <- c("skyblue", "lightgreen", "darkseagreen2", "lightgoldenrodyellow", "lightsalmon")
boxplot(Time.In.Minutes  ~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of ime In Minutes by Training Effect Category", xlab = "Training Effect Category", ylab = "ime In Minutes")


# Create a boxplot of the Pace In Minutes in each Training Effect Category
colors <- c("skyblue", "lightgreen", "deeppink3", "lightgoldenrodyellow", "lightsalmon")
boxplot(Pace.In.Minutes~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Pace In Minutes  by Training Effect Category", xlab = "Training Effect Category", ylab = "Pace In Minutes")


#Boxplot of the Avg Speed in each Training Effect Category
colors <- c("skyblue", "lightgreen", "azure4", "lightgoldenrodyellow", "lightsalmon")
boxplot(Avg.Speed  ~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Average Speed by Training Effect Category", xlab = "Training Effect Category", ylab = "Average Speed")

#Boxplot of the Max Speed in each Training Effect Category
colors <- c("skyblue", "lightgreen", "forestgreen", "lightgoldenrodyellow", "lightsalmon")
boxplot(Max.Speed  ~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Max Speed by Training Effect Category", xlab = "Training Effect Category", ylab = "Max Speed")

#Boxplot of the Heart Rate 1 Minute After Run in each Training Effect Category
colors <- c("skyblue", "lightgreen", "chocolate3", "lightgoldenrodyellow", "lightsalmon")
boxplot(HR.Rest1~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Heart Rate 1 Minute After Run  by Training Effect Category", xlab = "Training Effect Category", ylab = "Heart Rate 1 Minute After Run")

#------------------------------ MORE PLOTS ----------------------------------------------

#Hr difference between HR.Change2 and HR.Change1
HR.Diff <- FourMileRun$HR.Rest1 - FourMileRun$HR.Rest2
#hist(HR.Diff,main='Distribution of Heart Rate Difference Between 2 and one minute later',xlab='Heart Rate Difference',ylim=c(0,8),col="burlywood1")

# Create a boxplot of Heart Rate Difference Between 2 and 1 minute later in each Training Effect Category
colors <- c("skyblue", "lightgreen", "lightcoral", "lightgoldenrodyellow", "lightsalmon")

boxplot(HR.Diff ~ Train.Effect.Cat, data = FourMileRun, outline = TRUE, col = colors,cex.axis = 0.8,main = "Box Plot of Heart Rate Difference Between 2 and 1 minute later (Beats Per Minute) by Training Effect", xlab = "Training Effect Category", ylab = "HR.diff")

#Scatter plot of HR from start of rest and 1 minute later and
#and HR from the start of rest and 2 minutes later in each training effect category
plot(FourMileRun$HR.Rest1,FourMileRun$HR.Rest2,col=FourMileRun$Train.Effect.Cat,pch=as.numeric(FourMileRun$Train.Effect.Cat),xlab="Heart Rate 1 Minute AFter the Run",ylab = "Heart Rate 2 Minutes AFter the Run",main="Scatter Plot Of HR.Rest1 and HR.Rest2 by Training Effect Category",cex.main=0.8)
legend('bottomright', pch=1:5, col=1:5, legend=levels(FourMileRun$Train.Effect.Cat))
abline(lm(FourMileRun$HR.Rest2~FourMileRun$HR.Rest1))

#---------------------------------------- MODEL CONSTRUCTION --------------------------------------------------------
#Construct a regression model (or more) to assess the progress of Kevin.

#--------------------------- Model 1: All variables ----------------------------------------------------

progress.m1<- lm(Training.Effect ~ Calories.Burned +HR.Change1 +
                   HR.Change2 + Max.HR +Avg.HR + Avg.Speed + Max.Speed +
                   HR.Rest + HR.Rest1 + HR.Rest2 + Time.In.Minutes + Pace.In.Minutes , data = FourMileRun)
summary(progress.m1)
anova(progress.m1)
#from the anova we see that only HR.Change1, Max.HR and Avg.HR are statistically significant
# Residuals analysis
#(Unstandardized) Residuals
residuals(progress.m1)
#Studentized residuals
rstudent(progress.m1)
#Standardized residuals
rstandard(progress.m1)
res.progress.m1=residuals(progress.m1,type="pearson")
# Plot residuals vs. fitted values
plot(fitted(progress.m1),res.progress.m1)

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(progress.m1))
qqline(residuals(progress.m1))
shapiro.test(residuals(progress.m1))
# we have normality for the unstardardized residuals

# Histogram and QQ plots for different residuals
par(mfcol = c(2, 3))
allres <- list(progress.m1$res, rstandard(progress.m1), rstudent(progress.m1))
mt <- c('Unstandardized Residuals', 'Standardized Residuals (Int. Stud.)', 'Studentized Residuals (Ext. Stud.)')
for (i in 1:3) {
  x <- allres[[i]]
  hist(x, probability = TRUE, main = mt[i], col = "lightgreen")
  x0 <- seq(min(x), max(x), length.out = 100)
  y0 <- dnorm(x0, mean(x), sd(x))
  lines(x0, y0, col = 2, lty = 2)
  qqnorm(x, main = mt[i])
  qqline(x)
}
par(mfrow = c(1, 1))
qqPlot(rstudent(progress.m1))

# Independence check: time-sequence plot
par(mfrow = c(1, 2))
plot(progress.m1$res, type = 'l')
plot(rstandard(progress.m1), type = 'l')
runs.test(progress.m1$residuals)
# Homoscedasticity check
yhat <- fitted(progress.m1)
qyhat <- cut(yhat, breaks = 4)
leveneTest(rstandard(progress.m1) ~ qyhat)
# Non-linearity check
plot(progress.m1, which = 1)

#--------------------  Model 2: only with the Max.HR variable -----------------------------------

progress.m2 <- lm(Training.Effect~Max.HR  ,data = FourMileRun)
progress.m2.quadr <- lm(Training.Effect~Max.HR +I(Max.HR^2)  ,data = FourMileRun)
# Summarize the model
summary(progress.m2)

#Multiple R-squared:  0.3372 is relatively low
#we can see fromt the scatterplot weak linear fit
plot(FourMileRun$Max.HR, FourMileRun$Training.Effect, pch = 16, col = "forestgreen", xlab = "Max.HR", ylab = "Training.Effect", main = "Scatter Plot of Max.HR vs Training.Effect",cex.main = 0.9)
abline(lm(FourMileRun$Training.Effect~FourMileRun$Max.HR))


# Plot the quadratic curve
plot(Training.Effect ~ Max.HR, data = FourMileRun, main = "Quadratic Regression Plot", xlab = "Max.HR", ylab = "Training Effect")

# Add the quadratic curve
curve(predict(progress.m2.quadr, newdata = data.frame(Max.HR = x)), col = "red", lwd = 2, add = TRUE)

title("Quadratic Regression Plot")
legend("topleft", legend = "Quadratic Model", col = "red", lty = 1, lwd = 2)

#non linearity between Max.HR and training effect
#residuals vs. fitted values plot
plot(progress.m2, which=3)
plot(residuals(progress.m2))
abline(0, 0) 
#p value =0.004887 so we reject the null Hypothesis so we do not have normality for residuals
shapiro.test(residuals(progress.m2))

#--------------------  Model 2: only with the Avg.HR variable -----------------------------------

progress.m3 <- lm(Training.Effect~Avg.HR  ,data = FourMileRun)
summary(progress.m3)
#Multiple R-squared:0.6764 is relatively large so there is a moderately strong relationship
# Scatter plot for Model 3
plot(FourMileRun$Avg.HR, FourMileRun$Training.Effect, pch = 16, col = "darkmagenta", xlab = "Avg.HR", ylab = "Training.Effect", main = "Scatter Plot of Avg.HR vs Training.Effect",cex.main = 0.9)
abline(lm(FourMileRun$Training.Effect~FourMileRun$Avg.HR))

#analysis of variance
anova(progress.m3)

#p value =0.0418 so we reject the null Hypothesis so we do not have normality for residuals
shapiro.test(residuals(progress.m3))

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(progress.m3))
qqline(residuals(progress.m3))

par( mfcol=c(2,3) ) 
allres <- list(); allres[[1]] <- progress.m3$res
allres[[2]] <- rstandard(progress.m3)
allres[[3]] <- rstudent(progress.m3)
mt<-c(); mt[1] <- 'Unstandardized Residuals'
mt[2] <- 'Standardized Residuals (Int. Stud.)'
mt[3] <- 'Studentized Residuals (Ext. Stud.)'
for (i in 1:3){
  x<-allres[[i]]
  hist(x, probability=T, main=mt[i],col = "lightgreen")
  x0<-seq( min(x), max(x), length.out=100)
  y0<-dnorm( x0, mean(x), sd(x) )
  lines(x0,y0, col=2, lty=2)
  qqnorm(x, main=mt[i])
  qqline(x)
}

par(mfrow = c(1, 1))
#For studentized residuals use the qq.plot function in car which compared them with the t distribution
qqPlot(rstudent(progress.m3))

#Checking for independence,Simple time‐sequence plot
par( mfrow=c(1,2) )
plot(progress.m3$res, type='l')
plot(rstandard(progress.m3), type='l')

runs.test(progress.m3$residuals)
# p-value = 0.4596 so we do not reject H0:the sequence was produced in a random manner, so we have independence

#non linearity between Avg.HR and training effect
#residuals vs. fitted values plot
plot(progress.m3, which=3)

plot(x = progress.m3$fitted.values, y = progress.m3$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Homoscedasticity check
yhat <- fitted(progress.m3)
qyhat <- cut(yhat, breaks = 4)
leveneTest(rstandard(progress.m3) ~ qyhat)
leveneTest( rstandard(progress.m3)~qyhat )
#pvalue=0.6413 so we do not reject Ho The variances of the residuals are equal across quartiles of the fitted values.
#no evidence to suggest a significant difference in variances among the groups defined by the quartiles of the fitted values. assumption of homoscedasticity may holds

#------------------------------ Model 4: Avg.HR with quadratic effect -----------------------------

#because we see non-linearity between Avg.HR and Training Effect we add the quadratic effect
progress.m4 <- lm(Training.Effect~Avg.HR + I(Avg.HR^2)  ,data = FourMileRun)
# Summarize the model
summary(progress.m4)
#model may fit the data well
#Multiple R-squared:  0.8893 is large so we may have a good model
#analysis of variance
#the model parameters are extremely statistically significant
anova(progress.m4)
#non linearity between Max.HR and training effect
plot(progress.m4, which=3)

#p value =0.5502 so we do not reject the null Hypothesis so we do have normality for residuals
shapiro.test(residuals(progress.m4))

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(progress.m4))
qqline(residuals(progress.m4))

# Histogram and QQ plots for different residuals
par(mfcol = c(2, 3))
allres <- list(progress.m4$res, rstandard(progress.m4), rstudent(progress.m4))
mt <- c('Unstandardized Residuals', 'Standardized Residuals (Int. Stud.)', 'Studentized Residuals (Ext. Stud.)')
for (i in 1:3) {
  x <- allres[[i]]
  hist(x, probability = TRUE, main = mt[i], col = "lightgreen")
  x0 <- seq(min(x), max(x), length.out = 100)
  y0 <- dnorm(x0, mean(x), sd(x))
  lines(x0, y0, col = 2, lty = 2)
  qqnorm(x, main = mt[i])
  qqline(x)
}

par(mfrow = c(1, 1))
#residuals vs. fitted values plot
#no linearity 
plot(progress.m4, which=3)

#Checking for equality of variance in quartiles of fitted values
yhat <- fitted(progress.m4)
qyhat<- cut( yhat, breaks= 4 )
leveneTest( rstandard(progress.m4)~qyhat )
#pvalue=0.3165 so we do not reject Ho The variances of the residuals are equal
#across quartiles of the fitted values.
#no evidence to suggest a significant difference in variances among the groups 
#defined by the quartiles of the fitted values. assumption of homoscedasticity may holds


#---------------------------------- Scatter plots and regression ---------------------------------------

par(mfrow = c(1, 2))

# Scatter Plot of Avg HR vs Training Effect
plot(FourMileRun$Avg.HR, FourMileRun$Training.Effect, pch = 16, col = "bisque3", 
     xlab = "Avg HR", ylab = "Training Effect", main = "Avg HR vs Training Effect",
     cex.main = 1.5, cex.lab = 1.6, cex.axis = 1.6)
abline(lm(FourMileRun$Training.Effect ~ FourMileRun$Avg.HR))

# Quadratic Regression Plot
plot(Training.Effect ~ Avg.HR, data = FourMileRun, main = "Quadratic Regression Plot", 
     xlab = "Avg.HR", ylab = "Training Effect", cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.5)
curve(predict(progress.m4, newdata = data.frame(Avg.HR = x)), col = "red", lwd = 2, add = TRUE)
legend("topleft", legend = "Quadratic Curve", col = "red", lty = 1, lwd = 2, cex = 1)
par(mfrow = c(1, 1))
# Add the quadratic curve
curve(predict(progress.m4, newdata = data.frame(Avg.HR = x)), col = "red", lwd = 2, add = TRUE)


# Add legend
legend("topleft", legend = "Quadratic Curve", col = "red", lty = 1, lwd = 2, cex = 1)
par(mfrow = c(1, 1))
#no clear pattern
# Residuals vs Fitted Values
plot(x = progress.m4$fitted.values, y = progress.m4$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)


#----------------- MODEL SELECTION AND CONSTRUCTION FROM THE BEGINING ------------------------
# Fit the model
subset_data <- FourMileRun[, !(names(FourMileRun) %in% c("Time", "Time.Minutes", "Pace.Minutes", "Pace.Seconds", "Time.In.Seconds", "Pace", "Pace.In.Seconds", "Time.Seconds","Run","Train.Effect.Cat","Pace.In.Minutes"))]

#My response variable is ordinal
# Fit the full model
full_model <- lm(Training.Effect ~ ., data = subset_data)
#statistically significant only the Max.HR,Avg.HR and HR.Rest in summary
summary(full_model)
anova(full_model)

# Residuals analysis for full model
plot(full_model, which=2)
#non linearity
plot(full_model, which=3)

#------------------------- STEPWISE FOR MODEL SELECTION ---------------------------------------

null_model <- lm(Training.Effect ~ 1, data = subset_data)
step(full_model, scope = list(lower = null_model, upper = full_model), direction = 'both')

# Bayesian Model Averaging with BIC and AIC priors
bas_results_bic <- bas.lm(Training.Effect ~ ., data = subset_data, prior = "BIC")
plot(bas_results_bic)
image(bas_results_bic)

bas_results_aic <- bas.lm(Training.Effect ~ ., data = subset_data, prior = "AIC")
plot(bas_results_aic)
#image(bas_results_aic)

# Best model selection and evaluation
best_model <- lm(formula = Training.Effect ~ Max.HR + Avg.HR + HR.Rest + HR.Rest1 + 
                   Time.In.Minutes, data = subset_data)

vif(best_model)
X <- model.matrix(best_model)
summary(best_model)
anova(best_model)

# Residuals analysis for best model
shapiro.test(residuals(best_model))  # Normality test
qqnorm(residuals(best_model))
qqline(residuals(best_model))


# Histograms and QQ plots for residuals
par(mfcol = c(2, 3))
allres <- list(best_model$res, rstandard(best_model), rstudent(best_model))
mt <- c('Unstandardized Residuals', 'Standardized Residuals (Int. Stud.)', 'Studentized Residuals (Ext. Stud.)')
for (i in 1:3) {
  x <- allres[[i]]
  hist(x, probability = TRUE, main = mt[i], col = "lightgreen")
  x0 <- seq(min(x), max(x), length.out = 100)
  y0 <- dnorm(x0, mean(x), sd(x))
  lines(x0, y0, col = 2, lty = 2)
  qqnorm(x, main = mt[i])
  qqline(x)
}
par(mfrow = c(1, 1))

# Homoscedasticity check
yhat <- fitted(best_model)
qyhat <- cut(yhat, breaks = 4)
leveneTest(rstandard(best_model) ~ qyhat)
#pvalue=0.06913so we do not reject Ho The variances of the residuals are equal across quartiles of the fitted values.
#no evidence to suggest a significant difference in variances among the groups defined by the quartiles of the fitted values. assumption of homoscedasticity may holds

#---------------------------- MODEL WITH THE QUADRATIC EFFECT ---------------------------

best_model1 <- lm(formula = Training.Effect ~ Max.HR + Avg.HR + + I(Avg.HR^2) +HR.Rest + HR.Rest1 + 
                    Time.In.Minutes, data = subset_data)
summary(best_model1)
anova(best_model1)
#we see that HR.Rest and HR.Rest1 are highly statistically insignificant

#------------------------- Model without HR.Rest and HR.Rest1 -------------------------------

best_model2<- lm(formula = Training.Effect ~ Max.HR + Avg.HR + I(Avg.HR^2) +
                   Time.In.Minutes , data = subset_data)
summary(best_model2)
anova(best_model2)
AIC(progress.m4,progress.m1,best_model,best_model1,best_model2)

#p value =0.09369 so we do not reject the null Hypothesis so we do have normality for residuals
shapiro.test(residuals(best_model2))

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(best_model2))
qqline(residuals(best_model2))

par( mfcol=c(2,3) ) 
allres <- list(); allres[[1]] <- best_model2$res
allres[[2]] <- rstandard(best_model2)
allres[[3]] <- rstudent(best_model2)
mt<-c(); mt[1] <- 'Unstandardized Residuals'
mt[2] <- 'Standardized Residuals (Int. Stud.)'
mt[3] <- 'Studentized Residuals (Ext. Stud.)'
for (i in 1:3){
  x<-allres[[i]]
  hist(x, probability=T, main=mt[i],col = "lightgreen")
  x0<-seq( min(x), max(x), length.out=100)
  y0<-dnorm( x0, mean(x), sd(x) )
  lines(x0,y0, col=2, lty=2)
  qqnorm(x, main=mt[i])
  qqline(x)
}

par(mfrow = c(1, 1))

# Homoscedasticity check for best_model2
yhat <- fitted(best_model2)
qyhat <- cut(yhat, breaks = 4)
leveneTest(rstandard(best_model2) ~ qyhat)

# Independence check
par(mfrow = c(1, 2))
plot(best_model2$res, type = 'l')
plot(rstandard(best_model2), type = 'l')
runs.test(best_model2$residuals)

# p-value = 0.627 so we do not reject H0:the sequence was produced in a random manner, so we have independence
par(mfrow = c(1, 1))
#no clear pattern
plot(x =best_model2$fitted.values, y = best_model2$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

#----------------------------- Best model 3 with quadratic effect -----------------------------

best_model3<- lm(formula = Training.Effect ~  Avg.HR + I(Avg.HR^2) +
                   Time.In.Minutes , data = subset_data)
summary(best_model3)
anova(best_model3)
model_coefficients <- coef(best_model3)
formatted_coefficients <- format(model_coefficients, scientific = FALSE)
print(formatted_coefficients)

#p value =0.3185 so we do not reject the null Hypothesis so we do have normality for residuals
shapiro.test(residuals(best_model3))

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(best_model3))
qqline(residuals(best_model3))
par( mfcol=c(2,3) ) 
allres <- list(); allres[[1]] <- best_model3$res
allres[[2]] <- rstandard(best_model3)
allres[[3]] <- rstudent(best_model3)
mt<-c(); mt[1] <- 'Unstandardized Residuals'
mt[2] <- 'Standardized Residuals (Int. Stud.)'
mt[3] <- 'Studentized Residuals (Ext. Stud.)'
for (i in 1:3){
  x<-allres[[i]]
  hist(x, probability=T, main=mt[i],col = "lightgreen",cex.axis=2, cex.lab=2)
  x0<-seq( min(x), max(x), length.out=100)
  y0<-dnorm( x0, mean(x), sd(x) )
  lines(x0,y0, col=2, lty=2)
  qqnorm(x, main=mt[i], cex.axis=2, cex.lab=2)
  qqline(x)
}

#Checking for equality of variance in quartiles of fitted values
yhat <- fitted(best_model3)
qyhat<- cut( yhat, breaks= 4 )


leveneTest( rstandard(best_model3)~qyhat )
#pvalue= 0.4155 so we do not reject Ho The variances of the residuals are equal across quartiles of the fitted values.
#no evidence to suggest a significant difference in variances among the groups defined by the quartiles of the fitted values. assumption of homoscedasticity may holds


#------------------------------ INDEPENDENCE PLOTS FOR best_model3-----------------------------------------------------

 par(mfrow=c(1,2), cex.axis=1.8, cex.lab=1.8, cex.main=1.8)

# Plot unstandardized and standardized residuals
plot(best_model3$res, type = 'l', xlab = 'Index', ylab = 'Unstandardized Residuals', main = '')
plot(rstandard(best_model3), type = 'l', xlab = 'Index', ylab = 'Standardized Residuals', main = '')
runs.test(best_model3$residuals)

# p-value = 0.627 so we do not reject H0:the sequence was produced in a random manner, so we have independence
par(mfrow = c(1, 1), cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
#no pattern
# Residuals vs Fitted Values
plot(x = best_model3$fitted.values, y = best_model3$residuals,
xlab = "Fitted Values", ylab = "Residuals", main = "", cex = 1.5)
abline(h = 0, col = "red", lty = 2)
# Additional plots for residuals
par( mfrow=c(1,2) )
plot(best_model3$res, type='l')
plot(rstandard(best_model3), type='l')

# Plotting residuals vs fitted values
plot(best_model3, which = 1)

### ----------------------------- PROCESS FOR CENTERING THE PREDICTORS OF OUR MODEL -------------------------------------

# Calculate the mean of the explan. variables
mean_Avg_HR <- mean(subset_data$Avg.HR)
mean_Time_In_Minutes <- mean(subset_data$Time.In.Minutes)

# Center the predictors by subtracting their mean values
subset_data$Centered_Avg_HR <- subset_data$Avg.HR - mean_Avg_HR
subset_data$Centered_Time_In_Minutes <- subset_data$Time.In.Minutes - mean_Time_In_Minutes

# Fit the lm with centered predictors
centered_model <- lm(Training.Effect ~ Centered_Avg_HR + I(Centered_Avg_HR^2) + Centered_Time_In_Minutes, 
                     data = subset_data)
summary(centered_model)

#p value =0.3185 so we do not reject the null Hypothesis so we do have normality for residuals
shapiro.test(residuals(centered_model))

#Normality QQ plot for (Unstandardized) Residuals
qqnorm(residuals(centered_model))
qqline(residuals(centered_model))

# Histograms and QQ plots for residuals of centered_model
par(mfcol = c(2, 3))
allres <- list(centered_model$res, rstandard(centered_model), rstudent(centered_model))
mt <- c('Unstandardized Residuals', 'Standardized Residuals (Int. Stud.)', 'Studentized Residuals (Ext. Stud.)')
for (i in 1:3) {
  x <- allres[[i]]
  hist(x, probability = TRUE, main = mt[i], col = "lightgreen")
  x0 <- seq(min(x), max(x), length.out = 100)
  y0 <- dnorm(x0, mean(x), sd(x))
  lines(x0, y0, col = 2, lty = 2)
  qqnorm(x, main = mt[i])
  qqline(x)
}

# Homoscedasticity check for centered_model
yhat <- fitted(centered_model)
qyhat <- cut(yhat, breaks = 4)
leveneTest(rstandard(centered_model) ~ qyhat)

#pvalue= 0.4155 so we do not reject Ho The variances of the residuals are equal across quartiles of the fitted values.
#no evidence to suggest a significant difference in variances among the groups defined by the quartiles of the fitted values. assumption of homoscedasticity may holds

# Independence check for centered_model
#Checking for independence,Simple time‐sequence plot
par( mfrow=c(1,2) )
plot(centered_model$res, type='l')
plot(rstandard(centered_model), type='l')

runs.test(centered_model$residuals)
# p-value = 0.627 so we do not reject H0:the sequence was produced in a random manner, so we have independence
par(mfrow = c(1, 1))
#no clear pattern
plot(x =centered_model$fitted.values, y = centered_model$residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

par( mfrow=c(1,2) )
plot(centered_model$res, type='l')
plot(rstandard(centered_model), type='l')
par( mfrow=c(1,1) )
# Plotting residuals vs fitted values
plot(centered_model, which = 1)


#---------------------------------  MODEL COMPARISON USING AIC -------------------------------------------------

AIC(progress.m4,progress.m1,best_model,best_model1,best_model2,best_model3,centered_model)
#Model Predictions
# Model Predictions
# Construct a hypothetical new data frame for predictions
new_data <- data.frame(
  Avg.HR = c(132, 140, 145, 150, 154, 151, 137, 128, 135, 142, 147, 152),
  Time.In.Minutes = c(34.3, 32.5, 33.0, 33.5, 33.23, 33.45, 34.05, 34.18, 32.8, 33.3, 34.0, 33.6)
)
new_data$Centered_Avg_HR <- new_data$Avg.HR - mean_Avg_HR
new_data$Centered_Time_In_Minutes <- new_data$Time.In.Minutes - mean_Time_In_Minutes

# Make predictions using the centered model
predictions <- predict(best_model3, newdata = new_data)
new_data$Predicted_Training_Effect <- predictions
subset_new_data <- new_data[, !(names(new_data) %in% c("Centered_Avg_HR", "Centered_Time_In_Minutes"))]

# Ordinal Logistic Regression
subset_data_ordinal_model <- FourMileRun[, !(names(FourMileRun) %in% c("Time", "Time.Minutes", "Pace.Minutes", "Pace.Seconds", "Time.In.Seconds", "Pace", "Pace.In.Seconds", "Time.Seconds", "Run", "Pace.In.Minutes", "Training.Effect"))]

full_model_ordinal <- polr(Train.Effect.Cat ~ ., data = subset_data_ordinal_model, Hess = TRUE)
null_model_ordinal <- polr(Train.Effect.Cat ~ 1, data = subset_data_ordinal_model, Hess = TRUE)
stepwise_model <- stepAIC(null_model_ordinal, scope = list(lower = null_model_ordinal, upper = full_model_ordinal), direction = "both")

# Summary of stepwise model
summary(stepwise_model)
exp(coef(stepwise_model))

#meaning the odds of the probability of Minor  when Avg.HR is at its reference level (here we will say the mean)
#assume that the mean is the reference level

#When you increase the average heart rate (Avg.HR) by one unit the odds of being in a higher training effect category increase by a factor of 1.20
#the odds increase by 20.6%
#higher average heart rates, are associated with more significant improvements in training effect categories. 

# Perform Bayesian Model Averaging
bma_results <- bas.lm(Training.Effect ~ .,
          data=subset_data, modelprior=uniform(), method="MCMC", MCMC.iterations=10000)
# Summary of BMA results
summary(bma_results)
