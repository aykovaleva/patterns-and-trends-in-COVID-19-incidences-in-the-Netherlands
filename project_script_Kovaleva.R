# Case B: patterns and trends in COVID-19 incidences in The Netherlands
# My name is: Alesya Kovaleva 

# 08-01-2026 

# Set my working directory
setwd("/home/aykovaleva/Documents/Radboud/subjects/intro_to_R/final_project/raw_data")

##################################################################
# Exercise 1

# Read in the data
data_2020 = read.csv("COVID_data_2020.csv") # 827075 rows

# Remove errors in the data set
data_2020 = unique(data_2020) # removes duplicates; 826075 rows now
data_2020$Deceased[data_2020$Deceased == "Ja"] = "Yes" # replacing Ja with Yes

##################################################################
# Exercise 2

# remove cases belonging to the Unknown age group
data_2020 = data_2020[data_2020$Agegroup != "Unknown", ] # 826023 rows now

# creating a column with harmonised age groups called Age
young_groups = c("0-9", "10-19", "20-29", "30-39", "40-49", "<50")
data_2020$Age = ifelse (data_2020$Agegroup %in% young_groups, 
                             "<50", data_2020$Agegroup)

# calculate the total number of incidences per age group
incidence = aggregate(data_2020$CaseID, list(data_2020$Age), length)
# chagne the column names of the newly created dataframe
colnames(incidence) = c("Age", "Amount")

# calculate the total number of deaths per age group
deaths = aggregate(data_2020$CaseID, 
                       list(data_2020$Age, data_2020$Deceased), length)
colnames(deaths) = c("Age", "Deceased", "Amount")
deaths = subset(deaths, Deceased == "Yes")


#visualise the incidences and deaths per age group as a barplot
barplot(Amount ~ Age, incidence, 
        main = "Distribution of COVID-19 cases\n amongst different age groups", 
        xlab = "Age groups (years)", ylab = "Number of cases")
barplot(Amount ~ Age, deaths,
        main = "Distribution of COVID-19 deaths\n amongst different age groups", 
        xlab = "Age groups (years)", ylab = "Number of cases")

##################################################################
# Exercise 3

cases_stats = cbind(incidence, deaths$Amount)
colnames(cases_stats)[3] = "Deaths_Amount"
# finding the normalised frequency of death in each age group
cases_stats$norm_freq = cases_stats$Deaths_Amount / cases_stats$Amount 
  
# visualising as a bar plot
barplot(norm_freq ~ Age, cases_stats,
        main = "Rate of death in 2020\n for the different age groups", 
        xlab = "Age groups (years)", ylab = "Frequence")

##################################################################
# Prepping
# This would be the cleanest way to do it, 
# but there is no garuantee that the dplyr library is installed on a random PC
#library(dplyr)
#df_all <- bind_rows(lapply(filenames, read.csv))

filenames = c("COVID_data_2020.csv", "COVID_data_2021.csv", 
              "COVID_data_2022.csv", "COVID_data_2023.csv")

data_covid = data.frame() # creating an empty data frame
for (file in filenames){
  temp = read.csv(file)
  data_covid = rbind(data_covid, temp)} # binding all files together one by one

##################################################################
# Exercise 4

# convert the date column from character to Date
data_covid$Date_statistics <- as.Date(data_covid$Date_statistics, 
                                              format = '%Y-%m-%d')

#Calculate the number of incidences and deaths per day 
incidence_time = aggregate(data_covid$CaseID, 
                            list(data_covid$Date_statistics), length)
colnames(incidence_time) = c("date", "amount")

# creating a seperate dataframe for deaths 
data_covid_dead = subset(data_covid, Deceased == "Yes")
death_time = aggregate(data_covid_dead$CaseID, 
                       list(data_covid_dead$Date_statistics), length)
colnames(death_time) = c("date", "amount")

# plot the number of incidences over time in a line plot
plot(amount ~ date, incidence_time, 
     type = 'l', lwd = 1.5, col = "red",
     main = "Number of incidences over time", 
     xlab = "Date ", ylab = "Number of incidence")

# plot the number of death over time in a line plot
plot(amount ~ date, death_time, 
     type = 'l', lwd = 1.5, col = "blue",
     main = "Number of deaths over time", 
     xlab = "Date ", ylab = "Number of deaths")

##################################################################
# Exercise 5

# Calculate the amount of males and females that died after infection 
incidence_sex = aggregate(data_covid$CaseID, 
                          list(data_covid$Sex), length)

deaths_sex = aggregate(data_covid_dead$CaseID, 
                       list(data_covid_dead$Sex), length)

# unifying the deaths and incidence in one table for convenience
# and getting rid of Unknown from incidence_sex
sex_stats = cbind(incidence_sex[-3,], deaths_sex[,2])
colnames(sex_stats) = c("Sex", "Incidence", "Death")

# calculate the proprtion
sex_stats$Death_Proportion = sex_stats$Death / sex_stats$Incidence 

#sex_stats
#Males have a relatively higher chance of dying after infection
# ~ 0.08 % higher

##################################################################
# Exercise 6

# Calculate deaths per prvovince
deaths_province = aggregate(data_covid_dead$CaseID, 
                       list(data_covid_dead$Province), length)
# sort by death in order
deaths_province = deaths_province[order(deaths_province[, 2]), ]
# lowest number of deaths in Flevoland: 323
# highest number of deaths in Zuid-Holland: 5061
# The ranking might be explained by population density of each region

# Calculate incidence per province
incidence_province = aggregate(data_covid$CaseID, 
                          list(data_covid$Province), length)

# sort by number in order
incidence_province = incidence_province[order(incidence_province[, 2]), ]
#incidence_province
# It looks like the death numbers are highly (positively) dependent on incidence
# Zuid-Holland has the highest incidence
# Flevoland has the second lowest incidence

##################################################################
# Exercise 7

# Calculating reproduction number using the incidence_time data frame from Ex4
incidence_time$Rt <- NA_real_ # creating an new empty vector for Rt
for (i in 5 : nrow(incidence_time)){
  incidence_time$Rt[i] = incidence_time[i, 2] / incidence_time[i - 4, 2]
  }

# Plot the Rt over time in a line plot
plot(Rt ~ date, incidence_time, 
     type = 'l', lwd = 1.5, col = "dark orange",
     main = "Reproduction number over time", 
     xlab = "Date ", ylab = "The effective reproduction number")

##################################################################
# Exercise 8

# Recreate the line plot of exercise 7 
# but now without the Rt values of the last 14 days
plot(Rt ~ date, head(incidence_time, -14), 
     type = 'l', lwd = 1.5, col = "dark orange",
     main = "Reproduction number over time", 
     xlab = "Date ", ylab = "The effective reproduction number")
# adding a legend
legend("topright", legend = "Excluding the last 14\nunreliable days", bty = "n")

##################################################################
# Exercise 9

# Add a horizontal line at Rt = 1 
abline(h = 1)

# function par()
# control
# the Netherlands was experiencing the worst wave of COVID-19 
# in the middle of the year 2021
# with the highest incidence rate being on 2021-07-06 






























