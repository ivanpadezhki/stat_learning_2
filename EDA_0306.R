#importing dataset 

library(readxl)
diabetes_dataset__2019 <- read.csv("C:/Users/elisa/OneDrive/Desktop/statistical/PROJECT_stat/diabetes_dataset__2019.csv")
View(diabetes_dataset__2019)

#getting the values for categorical variables 

unique(diabetes_dataset__2019['Age'])
unique(diabetes_dataset__2019['Gender'])
unique(diabetes_dataset__2019['Family_Diabetes'])
unique(diabetes_dataset__2019['highBP'])
unique(diabetes_dataset__2019['PhysicallyActive'])
unique(diabetes_dataset__2019['Smoking'])
unique(diabetes_dataset__2019['Alcohol'])
unique(diabetes_dataset__2019['RegularMedicine'])
unique(diabetes_dataset__2019['JunkFood'])
unique(diabetes_dataset__2019['Stress'])
unique(diabetes_dataset__2019['BPLevel'])
unique(diabetes_dataset__2019['UriationFreq'])
unique(diabetes_dataset__2019['Diabetic'])

#Fix BPLevel column values 

diabetes_dataset__2019$BPLevel[diabetes_dataset__2019$BPLevel == 'High'] <- 'high'
diabetes_dataset__2019$BPLevel[diabetes_dataset__2019$BPLevel == 'Low'] <- 'low'
diabetes_dataset__2019$BPLevel[diabetes_dataset__2019$BPLevel == 'normal '] <- 'normal'
unique(diabetes_dataset__2019['BPLevel'])

#cast binary variables to dummies (0/1) by creating new columns with _num suffix

#Gender: Male=1, Female=0
diabetes_dataset__2019$Gender_num <- ifelse(diabetes_dataset__2019$Gender == 'Male', 1, 0)

#Diabetes: Yes=1, No=0
diabetes_dataset__2019$Family_Diabetes_num <- ifelse(diabetes_dataset__2019$Family_Diabetes == 'yes', 1, 0)

#HighBP: Yes=1, No=0
diabetes_dataset__2019$highBP_num <- ifelse(diabetes_dataset__2019$highBP == 'yes', 1, 0)

#Smoking: Yes=1, No=0

diabetes_dataset__2019$Smoking_num <- ifelse(diabetes_dataset__2019$Smoking == 'yes', 1, 0)

#Alcohol: Yes=1, No=0

diabetes_dataset__2019$Alcohol_num <- ifelse(diabetes_dataset__2019$Alcohol == 'yes', 1, 0)

#RegularMedicine: Yes=1, No=0

diabetes_dataset__2019$RegularMedicine_num <- ifelse(diabetes_dataset__2019$RegularMedicine == 'yes', 1, 0)

#UriationFreq: quite often=1, not much=0 

diabetes_dataset__2019$UriationFreq_num <- ifelse(diabetes_dataset__2019$UriationFreq == 'quite often', 1, 0)

#dropping modified columns 

diabetes_dataset__2019 <- diabetes_dataset__2019[,-2:-4]

# use this to check column indexes at each run
as.data.frame(colnames(diabetes_dataset__2019))

diabetes_dataset__2019 <- diabetes_dataset__2019[,-4:-5]
diabetes_dataset__2019 <- diabetes_dataset__2019[,-6]
diabetes_dataset__2019 <- diabetes_dataset__2019[,-11]

# Convert remaining categorical columns to factors with suffix _f
# 0 corresponding to lowest factor, 3 to highest

#Age

vec <- as.factor(diabetes_dataset__2019$Age)
f <- factor(vec, levels =  c('less than 40','40-49','50-59','60 or older'))
n <- as.numeric(f)
nn <- n - 1
diabetes_dataset__2019$Age_f <- nn

#Physically Active

vec <- as.factor(diabetes_dataset__2019$PhysicallyActive)
f <- factor(vec, levels =  c('none','less than half an hr','more than half an hr','one hr or more'))
n <- as.numeric(f)
nn <- n - 1
diabetes_dataset__2019$PhysicallyActive_f <- nn

#JunkFood
vec <- as.factor(diabetes_dataset__2019$JunkFood)
f <- factor(vec, levels =  c('occasionally','often','very often','always'))
n <- as.numeric(f)
nn <- n - 1
diabetes_dataset__2019$JunkFood_f <- nn

#Stress

vec <- as.factor(diabetes_dataset__2019$Stress)
f <- factor(vec, levels =  c('not at all','sometimes','very often','always'))
n <- as.numeric(f)
nn <- n - 1
diabetes_dataset__2019$Stress_f <- nn

#dropping modified columns 

diabetes_dataset__2019 <- diabetes_dataset__2019[,-1:-2]

# use this to check column indexes at each run
as.data.frame(colnames(diabetes_dataset__2019))

diabetes_dataset__2019 <- diabetes_dataset__2019[,-4:-5]

#cast BP level as factor (0,1,2) = (normal, low, high)

vec <- as.factor(diabetes_dataset__2019$BPLevel)
f <- factor(vec, levels =  c('normal','low','high'))
n <- as.numeric(f)
nn <- n - 1
diabetes_dataset__2019$BPLevel_f <- nn

#drop old BPLevel column 

diabetes_dataset__2019 <- diabetes_dataset__2019[,-4]

#NUMERIC VARIABLES EDA 

#BMI - continuous 

unique(diabetes_dataset__2019['BMI'])

#Sleep (hours) - continuous

unique(diabetes_dataset__2019['Sleep'])

#SoundSleep (hours) - continuous

unique(diabetes_dataset__2019['SoundSleep'])

#Pregancies (n. of pregnancies) - continuous (0-4)

unique(diabetes_dataset__2019['Pregancies'])

#Pdiabetes - DUMMY (1=had pregnancy diabetes, 0=didn't)

#Fixing the variable Pdiabetes 

unique(diabetes_dataset__2019['Pdiabetes'])

diabetes_dataset__2019$Pdiabetes[diabetes_dataset__2019$Pdiabetes == 'yes'] <- 1
diabetes_dataset__2019$Pdiabetes[diabetes_dataset__2019$Pdiabetes == 'no'] <- 0
diabetes_dataset__2019$Pdiabetes[diabetes_dataset__2019$Pdiabetes == ''] <- NA
diabetes_dataset__2019$Pdiabetes[diabetes_dataset__2019$Pdiabetes == ' no'] <- 0
unique(diabetes_dataset__2019['Pdiabetes'])

#DIABETIC TARGET VARIABLE - turn into dummy 1/0

unique(diabetes_dataset__2019['Diabetic'])

diabetes_dataset__2019$Diabetic[diabetes_dataset__2019$Diabetic == 'yes'] <- 1
diabetes_dataset__2019$Diabetic[diabetes_dataset__2019$Diabetic == 'no'] <- 0
diabetes_dataset__2019$Diabetic[diabetes_dataset__2019$Diabetic == ' no'] <- 0
diabetes_dataset__2019$Diabetic[diabetes_dataset__2019$Diabetic == ''] <- 
unique(diabetes_dataset__2019['Diabetic'])

#Check for missing values and outliers 

n_na = sum(is.na(diabetes_dataset__2019$Age_f))
n_na

n_na = sum(is.na(diabetes_dataset__2019$Gender_num))
n_na

n_na = sum(is.na(diabetes_dataset__2019$Family_Diabetes_num))
n_na

n_na = sum(is.na(diabetes_dataset__2019$PhysicallyActive_f))
n_na

n_na = sum(is.na(diabetes_dataset__2019$BMI))
n_na #4 NA values for BMI

n_na = sum(is.na(diabetes_dataset__2019$Smoking_num))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$Alcohol_num))
n_na

n_na = sum(is.na(diabetes_dataset__2019$Sleep))
n_na

n_na = sum(is.na(diabetes_dataset__2019$SoundSleep))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$RegularMedicine_num))
n_na 


n_na = sum(is.na(diabetes_dataset__2019$JunkFood_f))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$Stress_f))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$BPLevel_f))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$Pregancies))
n_na #42 missing values for Pregancies

n_na = sum(is.na(diabetes_dataset__2019$Pdiabetes))
n_na #1 missing value for Pdiabetes

n_na = sum(is.na(diabetes_dataset__2019$UriationFreq_num))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$highBP_num))
n_na 

n_na = sum(is.na(diabetes_dataset__2019$Diabetic))
n_na #1 missing value for diabetic 

n_na_tot = sum(is.na(diabetes_dataset__2019))
n_na_tot ##OK 

#Check that all columns have either numeric data type or factor 

class(diabetes_dataset__2019$BMI)

class(diabetes_dataset__2019$Sleep)

class(diabetes_dataset__2019$SoundSleep)

class(diabetes_dataset__2019$Pregancies)

class(diabetes_dataset__2019$Pdiabetes) #character 

#cast to numeric 

diabetes_dataset__2019$Pdiabetes = as.numeric(diabetes_dataset__2019$Pdiabetes)
class(diabetes_dataset__2019$Pdiabetes)

class(diabetes_dataset__2019$Diabetic) #character 

diabetes_dataset__2019$Diabetic = as.numeric(diabetes_dataset__2019$Diabetic)
class(diabetes_dataset__2019$Diabetic)

class(diabetes_dataset__2019$Age_f)

class(diabetes_dataset__2019$Gender_num)

class(diabetes_dataset__2019$Family_Diabetes_num)

class(diabetes_dataset__2019$highBP_num)

class(diabetes_dataset__2019$Smoking_num)

class(diabetes_dataset__2019$Alcohol_num)

class(diabetes_dataset__2019$RegularMedicine_num)

class(diabetes_dataset__2019$UriationFreq_num)

class(diabetes_dataset__2019$Age_f)

class(diabetes_dataset__2019$PhysicallyActive_f)

class(diabetes_dataset__2019$JunkFood_f)

class(diabetes_dataset__2019$Stress_f)

class(diabetes_dataset__2019$BPLevel_f)

####################################################################################

#GRAPHS 

#Is BMI connected to diabetes? --> YES 

diab_BMI = diabetes_dataset__2019$BMI[which(diabetes_dataset__2019$Diabetic==1)]
nodiab_BMI = diabetes_dataset__2019$BMI[which(diabetes_dataset__2019$Diabetic==0)]

boxplot(diab_BMI,nodiab_BMI, main = "BMI of diabetic and non-diabetic subjects",ylab = "BMI",names = c("diabetic", "not diabetic"))


#Is gender connected to diabetes? YES, males more likely (59% non-diabetic, 65% diabetic)

install.packages("ggplot2")
install.packages("tidyverse")
library(tidyverse)

non_diabetic_tot <- which(diabetes_dataset__2019$Diabetic==0)
length(non_diabetic_tot) #685 non-diabetic subjects 

diabetic_tot <- which(diabetes_dataset__2019$Diabetic==1)
length(diabetic_tot) #266 diabetic subjects 

gender_df <- data.frame(table(diabetes_dataset__2019$Diabetic,diabetes_dataset__2019$Gender_num))
names(gender_df) <- c("Diabetic","Gender","Count")

ggplot(data=gender_df, aes(x=Diabetic, y=Count, fill=Gender)) + geom_bar(stat="identity")


# Is urination frequency connected with diabetes? 

urinate_df <- data.frame(table(diabetes_dataset__2019$Diabetic,diabetes_dataset__2019$UriationFreq_num))
names(urinate_df) <- c("Diabetic","Urination_frequency","Count")

ggplot(data=urinate_df, aes(x=Diabetic, y=Count, fill=Urination_frequency)) + geom_bar(stat="identity")

