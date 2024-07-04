# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

# Load the dataset
df <- read.csv("C:\\A3\\campaign_responses.csv")

# Convert categorical variables to factors
df$gender <- as.factor(df$gender)
df$employed <- as.factor(df$employed)
df$marital_status <- as.factor(df$marital_status)
df$responded <- as.factor(df$responded)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(df$responded, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]

# Logistic Regression
logit_model <- glm(responded ~ ., data = dfTrain, family = binomial)

# Summary of the logistic regression model
summary(logit_model)

# Predicting probabilities
prob <- predict(logit_model, newdata = dfTest, type = "response")

# Confusion Matrix
predicted <- ifelse(prob > 0.5, "Yes", "No")
confusionMatrix(as.factor(predicted), dfTest$responded)

# ROC Curve
roc_curve <- roc(dfTest$responded, prob)
plot(roc_curve)

# Decision Tree
tree_model <- rpart(responded ~ ., data = dfTrain, method = "class")

# Plot the tree
rpart.plot(tree_model)

# Predicting probabilities
tree_pred <- predict(tree_model, newdata = dfTest, type = "class")

# Confusion Matrix
confusionMatrix(tree_pred, dfTest$responded)

# ROC Curve for Decision Tree
tree_prob <- predict(tree_model, newdata = dfTest, type = "prob")[,2]
roc_curve_tree <- roc(dfTest$responded, tree_prob)
plot(roc_curve_tree, add = TRUE, col = "red")

# Load the dataset
data_nss <- read.csv("C:\\A1\\NSSO68.csv.crdownload")
# Create a binary variable for chicken consumption
data_nss$chicken_q <- ifelse(data_nss$chicken_q > 0, 1, 0)

# Verify the creation of 'chicken_binary'
table(data_nss$chicken_q)

# Probit regression model
probit_model <- glm(chicken_q ~ Age + Marital_Status + Education, data = data_nss, family = binomial(link = "probit"))

# Summary of the probit regression model
summary(probit_model)

# Load necessary libraries
library(dplyr)
library(haven)
library(maxLik)

# Load the data
data <- read.csv("C:\\A1\\NSSO68.csv.crdownload", stringsAsFactors = FALSE)

# Subset data 
df <- data %>%
  select(MPCE_URP, Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)

# Check for missing values
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Impute missing values for selected columns
columns_to_impute <- c('Whether_owns_any_land', 'Religion', 'Social_Group', 'Regular_salary_earner')

# Assuming using mode for imputation for categorical variables
for (col in columns_to_impute) {
  mode_value <- as.character(sort(table(df[[col]]), decreasing = TRUE)[1])
  df[[col]][is.na(df[[col]])] <- mode_value
}

# Drop rows with any remaining NaN values
df <- na.omit(df)

# Check for missing values
print(sum(is.na(df$MPCE_URP)))
print(sum(is.na(df$Whether_owns_any_land)))
print(sum(is.na(df$hhdsz)))
print(sum(is.na(df$Religion)))
print(sum(is.na(df$Social_Group)))
print(sum(is.na(df$Regular_salary_earner)))

# Convert the target variable to binary based on the specified condition
df$MPCE_URP <- ifelse(df$MPCE_URP < 380, 0, 1)

# Define the independent variables (X) and the dependent variable (y)
X <- df %>%
  select(Whether_owns_any_land, hhdsz, Religion, Social_Group, Regular_salary_earner)
X <- cbind(1, C)  # Add a constant term for the intercept
y <- df$MPCE_URP

# Define the Tobit model function
tobit_loglike <- function(params) {
  beta <- params[1:(length(params)-1)]
  sigma <- params[length(params)]
  XB <- as.matrix(X) %*% beta
  cens <- (y == 0) + (y == 1)
  uncens <- 1 - cens
  ll <- numeric(length(y))
  
  ll[cens == 1] <- log(dnorm(y[cens == 1], mean = XB[cens == 1], sd = sigma))
  ll[uncens == 1] <- log(dnorm(y[uncens == 1], mean = XB[uncens == 1], sd = sigma))
  
  return(-sum(ll))
}

# Initial parameter guesses
start_params <- c(rep(0, ncol(X)), 1)

# Fit the Tobit model
tobit_results <- maxLik(tobit_loglike, start = start_params, method = "BFGS")

# Print the summary of the model
summary(tobit_results)

install.packages("AER")
# Example data
set.seed(123)
n <- 100
X <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
beta_true <- c(1, 0.5, -0.5)
sigma_true <- 1
y_star <- as.matrix(X) %*% beta_true + rnorm(n, sd = sigma_true)
y <- pmax(y_star, 0)

# Define the Tobit log-likelihood function
tobit_loglike <- function(params) {
  beta <- params[1:ncol(X)]
  sigma <- params[ncol(X) + 1]
  y_hat <- as.matrix(X) %*% beta
  ll <- ifelse(y > 0,
               log(dnorm((y - y_hat) / sigma)) - log(sigma),
               log(pnorm(-y_hat / sigma)))
  return(sum(ll))
}

# Initial parameter guesses
start_params <- c(rep(0, ncol(X)), 1)

# Ensure that X is numeric
X <- as.matrix(X)

# Fit the Tobit model
tobit_results <- maxLik(tobit_loglike, start = start_params, method = "BFGS")

# Print the summary of the model
summary(tobit_results)

# Load necessary libraries
library(maxLik)
library(AER)

# Example data
set.seed(123)
n <- 100
X <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n)
)
beta_true <- c(1, 0.5, -0.5)
sigma_true <- 1
y_star <- as.matrix(X) %*% beta_true + rnorm(n, sd = sigma_true)
y <- pmax(y_star, 0)

# Define the Tobit log-likelihood function
tobit_loglike <- function(params) {
  beta <- params[1:ncol(X)]
  sigma <- params[ncol(X) + 1]
  y_hat <- as.matrix(X) %*% beta
  ll <- ifelse(y > 0,
               log(dnorm((y - y_hat) / sigma)) - log(sigma),
               log(pnorm(-y_hat / sigma)))
  return(sum(ll))
}

# Initial parameter guesses
start_params <- c(rep(0, ncol(X)), 1)

# Ensure that X is numeric
X <- as.matrix(X)

# Fit the Tobit model
tobit_results <- maxLik(tobit_loglike, start = start_params, method = "BFGS")

# Print the summary of the model
summary(tobit_results)

