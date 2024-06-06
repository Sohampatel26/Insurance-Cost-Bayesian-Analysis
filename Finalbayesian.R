install.packages("caret")
# Load necessary libraries
library(tidyverse)

# Load the dataset
insurance_data <- read.csv("/Users/soham/Desktop/insurance.csv")

# Inspect the first few rows of the dataset
head(insurance_data)

# Get a summary of the dataset
summary(insurance_data)

# Check the structure of the dataset
str(insurance_data)

#------EDA----------
# Load necessary libraries
library(ggplot2)

# Histograms for numerical variables
ggplot(insurance_data, aes(x = age)) + geom_histogram(binwidth = 1, fill = "blue", color = "black") + theme_minimal() + ggtitle("Distribution of Age")
ggplot(insurance_data, aes(x = bmi)) + geom_histogram(binwidth = 1, fill = "green", color = "black") + theme_minimal() + ggtitle("Distribution of BMI")
ggplot(insurance_data, aes(x = charges)) + geom_histogram(binwidth = 1000, fill = "red", color = "black") + theme_minimal() + ggtitle("Distribution of Charges")

# Bar plots for categorical variables
ggplot(insurance_data, aes(x = sex)) + geom_bar(fill = "blue", color = "black") + theme_minimal() + ggtitle("Distribution of Sex")
ggplot(insurance_data, aes(x = as.factor(children))) + geom_bar(fill = "green", color = "black") + theme_minimal() + ggtitle("Distribution of Children")
ggplot(insurance_data, aes(x = smoker)) + geom_bar(fill = "red", color = "black") + theme_minimal() + ggtitle("Distribution of Smoker")
ggplot(insurance_data, aes(x = region)) + geom_bar(fill = "purple", color = "black") + theme_minimal() + ggtitle("Distribution of Region")

# Scatter plots for relationships
ggplot(insurance_data, aes(x = age, y = charges)) + geom_point(color = "blue") + theme_minimal() + ggtitle("Charges vs Age")
ggplot(insurance_data, aes(x = bmi, y = charges)) + geom_point(color = "green") + theme_minimal() + ggtitle("Charges vs BMI")
ggplot(insurance_data, aes(x = children, y = charges)) + geom_point(color = "red") + theme_minimal() + ggtitle("Charges vs Children")

# Box plots for relationships
ggplot(insurance_data, aes(x = sex, y = charges)) + geom_boxplot(fill = "blue") + theme_minimal() + ggtitle("Charges by Sex")
ggplot(insurance_data, aes(x = smoker, y = charges)) + geom_boxplot(fill = "red") + theme_minimal() + ggtitle("Charges by Smoker")
ggplot(insurance_data, aes(x = region, y = charges)) + geom_boxplot(fill = "purple") + theme_minimal() + ggtitle("Charges by Region")

#----------Removing Outliers------------
# Calculate the IQR for charges
Q1 <- quantile(insurance_data$charges, 0.25)
Q3 <- quantile(insurance_data$charges, 0.75)
IQR <- Q3 - Q1

# Define the upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter out the outliers
insurance_data_clean <- insurance_data %>%
  filter(charges >= lower_bound & charges <= upper_bound)

# Check the new dimensions of the dataset
dim(insurance_data_clean)

# Function to remove outliers based on IQR
remove_outliers <- function(data, variable) {
  Q1 <- quantile(data[[variable]], 0.25)
  Q3 <- quantile(data[[variable]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data <- data %>%
    filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)
  return(data)
}

# Remove outliers for age, bmi, and children
insurance_data_clean <- remove_outliers(insurance_data_clean, "age")
insurance_data_clean <- remove_outliers(insurance_data_clean, "bmi")
insurance_data_clean <- remove_outliers(insurance_data_clean, "children")

# Check the new dimensions of the cleaned dataset
dim(insurance_data_clean)

#-------Encoding variables--------------
# Encode sex and smoker as binary variables
insurance_data_clean$sex <- ifelse(insurance_data_clean$sex == "male", 1, 0)
insurance_data_clean$smoker <- ifelse(insurance_data_clean$smoker == "yes", 1, 0)

# One-hot encode the region variable
insurance_data_clean <- insurance_data_clean %>%
  mutate(region_northwest = ifelse(region == "northwest", 1, 0),
         region_southeast = ifelse(region == "southeast", 1, 0),
         region_southwest = ifelse(region == "southwest", 1, 0)) %>%
  select(-region)

# Check the structure of the cleaned dataset
str(insurance_data_clean)

#-------------Scale the numerical variables so that they have a mean of 0 and standard deviation of 1-----------

# Load the necessary library for scaling
#library(caret)

# Define the variables to scale
#numeric_vars <- c("age", "bmi", "children", "charges")

# Scale the numerical variables
#preProcess_scale <- preProcess(insurance_data_clean[, numeric_vars], method = c("center", "scale"))
#insurance_data_scaled <- predict(preProcess_scale, insurance_data_clean)

# Check the summary of the scaled dataset
#summary(insurance_data_scaled)

# Store the scaling parameters for later use
#preProcess_scale_info <- list(mean = preProcess_scale$mean, std = preProcess_scale$std)

#str(preProcess_scale)


#--------Feature Selection----------
# Define a function for performing the t-test on numerical features
t_test_function <- function(feature, target) {
  t.test(feature, target)
}

# Define a function for performing the chi-square test on binary categorical features
chi_square_test_function <- function(feature, target) {
  chisq.test(table(feature, target))
}

# Define a function for performing the z-test on binary categorical features (alternative to chi-square)
z_test_function <- function(feature, target) {
  prop.test(table(feature, target))
}

# Define an empty vector to store significant feature names
significant_features <- c()

# Loop through each feature and perform the test
for (feature in names(insurance_data_clean)) {
  if (is.numeric(insurance_data_clean[[feature]])) {
    p_value <- t_test_function(insurance_data_clean[[feature]], insurance_data_clean$charges)$p.value
  } else {
    if (length(unique(insurance_data_clean[[feature]])) == 2) {
      p_value <- chi_square_test_function(insurance_data_clean[[feature]], insurance_data_clean$charges)$p.value
    } else {
      p_value <- NA  # For multi-level categorical features, we'll handle them separately
    }
  }
  
  if (!is.na(p_value) && p_value < 0.05) {
    significant_features <- c(significant_features, feature)
  }
}

# Print the significant features
print(significant_features)

# Create interaction terms
insurance_data_clean$age_smoker_interaction <- insurance_data_clean$age * insurance_data_clean$smoker
insurance_data_clean$bmi_smoker_interaction <- insurance_data_clean$bmi * insurance_data_clean$smoker
insurance_data_clean$age_sex_interaction <- insurance_data_clean$age * insurance_data_clean$sex

# Interaction terms with region variables
insurance_data_clean$age_region_northwest_interaction <- insurance_data_clean$age * insurance_data_clean$region_northwest
insurance_data_clean$age_region_southeast_interaction <- insurance_data_clean$age * insurance_data_clean$region_southeast
insurance_data_clean$age_region_southwest_interaction <- insurance_data_clean$age * insurance_data_clean$region_southwest
insurance_data_clean$bmi_region_northwest_interaction <- insurance_data_clean$bmi * insurance_data_clean$region_northwest
insurance_data_clean$bmi_region_southeast_interaction <- insurance_data_clean$bmi * insurance_data_clean$region_southeast
insurance_data_clean$bmi_region_southwest_interaction <- insurance_data_clean$bmi * insurance_data_clean$region_southwest

# Check the updated dataset
head(insurance_data_clean)

#-----Bayesian Hierarchical Analysis----------
# Load the libraries
library(brms)
library(rstan)

# Define the formula for the Bayesian hierarchical model
formula <- bf(charges ~ age + sex + bmi + children + smoker + 
                age_smoker_interaction + bmi_smoker_interaction + age_sex_interaction +
                age_region_northwest_interaction + age_region_southeast_interaction + age_region_southwest_interaction +
                bmi_region_northwest_interaction + bmi_region_southeast_interaction + bmi_region_southwest_interaction +
                (1 | region_northwest) + 
                (1 | region_southeast) + 
                (1 | region_southwest))



# Fit the Bayesian hierarchical model
fit_with_interactions <- brm(
  formula, 
  data = insurance_data_clean, 
  family = gaussian(),
  chains = 4, 
  iter = 2000, 
  warmup = 1000, 
  cores = 4
)

# Print the summary of the model
summary(fit_with_interactions)


#----------Predict Insurance Costs----------

# Predict insurance charges using the fitted model
predictions <- predict(fit_with_interactions)

# Combine the original data with the predicted values
predicted_data <- cbind(insurance_data_clean, Predicted_Charges = predictions)

# View the first few rows of the combined dataset
head(predicted_data)


## Calculate Mean Absolute Error (MAE)
MAE <- mean(abs(predicted_data$charges - predicted_data$Predicted_Charges.Estimate))

# Calculate Mean Squared Error (MSE)
MSE <- mean((predicted_data$charges - predicted_data$Predicted_Charges.Estimate)^2)

# Calculate Root Mean Squared Error (RMSE)
RMSE <- sqrt(MSE)

# Calculate R-squared
SS_Residual <- sum((predicted_data$charges - predicted_data$Predicted_Charges.Estimate)^2)
SS_Total <- sum((predicted_data$charges - mean(predicted_data$charges))^2)
R_squared <- 1 - (SS_Residual / SS_Total)

# Print the metrics
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("R-squared:", R_squared, "\n")