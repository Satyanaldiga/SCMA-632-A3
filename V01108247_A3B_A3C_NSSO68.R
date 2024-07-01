# Load the necessary libraries
library(tidyverse)
library(mice)
library(car)
library(ggplot2)
library(lattice)
library(caret)
library(glmnet)
library(Matrix)
library(pROC)
library(BSDA)
library(glue)

#setting the wd
setwd('C:\\Users\\SPURGE\\Desktop\\SCMA')
getwd()


# Reading the file into R
data <- read.csv("NSSO68.csv")
dim(data)

unique(data$Religion)


# Filtering for MP
MP <- data %>%
  filter(state == "10")

# Display dataset info
cat("Dataset Information:\n")
print(names(MP))
print(head(MP))
print(dim(MP))


# Finding missing values
missing_info <- colSums(is.na(MP))
cat("Missing Values Information:\n")
print(missing_info)


# Sub-setting the data
MPnew <- MP %>%
  select(state_1,Religion, District, Region, Sector,emftt_q, emftt_v)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(MPnew)))


dim(MPnew)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
MPnew$emftt_q <- impute_with_mean(MPnew$emftt_q)
MPnew$emftt_v <- impute_with_mean(MPnew$emftt_v)

dim(MPnew)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(MPnew)))

MP$Religion

MPnew$emftt_v

MP$Religion

unique(MP$Religion)
str(MP$Religion)

# Sub-setting the data
MP_pr <- MP %>%
  select(Religion, eggsno_q, fishprawn_q, goatmeat_q, beef_q, pork_q, chicken_q, othrbirds_q)

dim(MP_pr)
MP_pr$eggsno_q
data
names(MP_pr)
str(MP_pr)

# Fitting a probit regression to identify non-vegetarians. 

religion_mapping <- c("Hinduism", "Islam", "Christianity","Jainism","Others")
MP_pr$Religion <- factor(MP_pr$Religion, labels = religion_mapping)
table(MP_pr$Religion)

columns <- c('eggsno_q','fishprawn_q', 'goatmeat_q', 'beef_q','pork_q', 'chicken_q', 'othrbirds_q')
data1 <- MP[columns]
data1$target <- ifelse(data1$eggsno_q>0,1,0) 
probit_modet <- glm(target~., data = data1, family = binomial(link = "probit"))
summary(probit_modet)


# Performorming a Tobit regression analysis on "NSSO68.csv" 
df_MP = data[data$state_1 == 'MP',]
vars <- c("state_1","Religion", "District", "Region", "Sector","emftt_q", "emftt_v")

df_MP_p = df_MP[vars]
names(df_MP_p)

df_MP_p$price = df_MP_p$emftt_v / df_MP_p$emftt_q
names(df_MP_p)

summary(df_MP_p)

head(table(df_MP_p$emftt_q))

dim(df_MP_p)

names(MP)

#  dependent variable and independent variables
y <- MP$foodtotal_v
X <- MP[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]


#  data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

install.packages("censReg")
library(censReg)
# Fitting the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])


# Printing model summary
summary(model)
