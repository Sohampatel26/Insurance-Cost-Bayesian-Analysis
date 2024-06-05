install.packages("rstan")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("bayesplot")
library(rstan)
library(ggplot2)
library(dplyr)
library(readr)
library(bayesplot)

# Load the dataset
data <- read_csv('/Users/soham/Desktop/insurance.csv')

# Standardize numerical predictors for better convergence
data <- data %>%
  mutate(
    age_std = (age - mean(age)) / sd(age),
    bmi_std = (bmi - mean(bmi)) / sd(bmi),
    sex_code = ifelse(sex == 'male', 1, 0),
    smoker_code = ifelse(smoker == 'yes', 1, 0)
  )

# Create dummy variables for regions
data <- data %>%
  mutate(region_northwest = ifelse(region == 'northwest', 1, 0),
         region_southeast = ifelse(region == 'southeast', 1, 0),
         region_southwest = ifelse(region == 'southwest', 1, 0))

# Stan model code
stan_code <- "
data {
  int<lower=0> N;  // number of data points
  vector[N] age_std;
  vector[N] bmi_std;
  vector[N] children;
  vector[N] sex_code;
  vector[N] smoker_code;
  vector[N] region_northwest;
  vector[N] region_southeast;
  vector[N] region_southwest;
  vector[N] charges;
}
parameters {
  real mu_a;
  real<lower=0> sigma_a;
  real a[3];  // group-level intercepts for regions
  real b_age;
  real b_bmi;
  real b_children;
  real b_sex;
  real b_smoker;
  real<lower=0> sigma;
}
model {
  // Priors
  mu_a ~ normal(0, 10);
  sigma_a ~ normal(0, 10);
  a ~ normal(mu_a, sigma_a);
  b_age ~ normal(0, 10);
  b_bmi ~ normal(0, 10);
  b_children ~ normal(0, 10);
  b_sex ~ normal(0, 10);
  b_smoker ~ normal(0, 10);
  sigma ~ normal(0, 10);
  
  // Likelihood
  for (i in 1:N) {
    charges[i] ~ normal(a[1] * region_northwest[i] +
                        a[2] * region_southeast[i] +
                        a[3] * region_southwest[i] +
                        b_age * age_std[i] +
                        b_bmi * bmi_std[i] +
                        b_children * children[i] +
                        b_sex * sex_code[i] +
                        b_smoker * smoker_code[i], sigma);
  }
}
"

# Prepare data for Stan
stan_data <- list(
  N = nrow(data),
  age_std = data$age_std,
  bmi_std = data$bmi_std,
  children = data$children,
  sex_code = data$sex_code,
  smoker_code = data$smoker_code,
  region_northwest = data$region_northwest,
  region_southeast = data$region_southeast,
  region_southwest = data$region_southwest,
  charges = log(data$charges + 1)
)

# Fit the model
fit <- stan(model_code = stan_code, data = stan_data, iter = 2000, warmup = 1000, chains = 2, cores = 2)

# Print summary of the results
print(fit, probs = c(0.025, 0.5, 0.975))

# Plotting the trace
traceplot(fit)

# Analyzing the results
library(bayesplot)
mcmc_trace(fit)
