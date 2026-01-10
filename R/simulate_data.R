# ------------------------------------------------------------
# Synthetic data generation for alopecia database project
#
# This script generates a fully synthetic clinical dataset based on the structural properties of an original REDCap dataset.
#
# This simulation preserves: 
# - Demographic variables: Age (simulated using a truncated normal distribution to ensure values >= 0)
#                          Sex, Race, and Ethnicity with proportions matching the original data.
# - Binary treatment assignment patterns, including single and combination JAK inhibitor therapies
# - Conditional outcome variables, with NA values for unassigned treatments.
# - Categorical and ordinal variables with observed distributions maintained.
#
# The dataset is intended for methodological demonstration and reproducible analyses,
# ensuring that no real patient data are exposed while reflecting realistic dependencies and outcome structures. 
# ------------------------------------------------------------

library(dplyr)
library(tidyverse)
library(truncnorm)

data <- read.csv("jak_inhibitor.csv")
set.seed(123)

n_sim <- 1000
ID_sim <- c(1:n_sim)
# Truncated normal distribution to allow age >= 0
age_sim <- rtruncnorm(n_sim, a = 0,
                      mean = mean(data$age, na.rm = TRUE),
                      sd = sd(data$age, na.rm = TRUE))
age_sim <- round(age_sim)
hist(age_sim)

ethnicity_probs <- prop.table(table(data$ethnicity))
ethnicity_probs
ethnicity_sim <- sample(names(ethnicity_probs), 
                        size = n_sim, replace = TRUE, 
                        prob = ethnicity_probs)
ethnicity_sim <- factor(ethnicity_sim, levels = names(ethnicity_probs))
prop.table(table(ethnicity_sim))

race_probs <- prop.table(table(data$race))
race_probs
race_sim <- sample(names(race_probs),
                   size = n_sim, replace = TRUE,
                   prob = race_probs)
race_sim <- factor(race_sim, levels = names(race_probs))
prop.table(table(race_sim))

sex_probs <- prop.table(table(data$sex))
sex_probs
sex_sim <- sample(names(sex_probs),
                  size = n_sim, replace = TRUE,
                  prob = sex_probs)
sex_sim <- factor(sex_sim, levels = names(sex_probs))
prop.table(table(sex_sim))

demographic_sim <- data.frame(ID = ID_sim,
                              age = age_sim,
                              ethnicity = ethnicity_sim,
                              race = race_sim,
                              sex = sex_sim)


jak_cols <- c("specify_jak_inhibitor___1",
              "specify_jak_inhibitor___2",
              "specify_jak_inhibitor___3")
jak_patterns <- apply(data[, jak_cols], 1, paste, collapse = "_")

pattern_probs <- prop.table(table(jak_patterns))
pattern_probs

sim_patterns <- sample(
  names(pattern_probs),
  size = n_sim,
  replace = TRUE,
  prob = pattern_probs
)
sim_mat <- do.call(
  rbind,
  strsplit(sim_patterns, "_")
)

sim_mat <- apply(sim_mat, 2, as.integer)
colnames(sim_mat) <- jak_cols
inh_res_sim <- as.data.frame(sim_mat)

for (k in 1:3) {
  inh_var <- paste0("specify_jak_inhibitor___", k)
  res_var <- paste0("result___", k)
  
  inh_res_sim[[res_var]] <- rep(NA_integer_, n_sim)
  
  used_real <- data[[inh_var]] == 1
  p_res <- mean(data[[res_var]][used_real], na.rm = TRUE)
  idx <- which(inh_res_sim[[inh_var]] == 1)
  
  if(length(idx) > 0) {
    inh_res_sim[idx, res_var] <- rbinom(length(idx),1,p_res)
  }
}

synthetic_data <- cbind(demographic_sim, inh_res_sim)
write.csv(synthetic_data, "synthetic_data.csv", row.names = FALSE)
