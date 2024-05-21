## Instructions -------

# 1. Change AHAH variable in line 38 as required (second variable in "select" function)
# 2. Change plot label names in line 117
# 3. Change QOF variable in lines 93, 102 and 115 as required
# 4. Run code (note outputs not currently saved)


## Add packages -------

# Function to install packages if needed

install_and_load_if_needed <- function(packages) {
  # Find out which packages are not installed
  not_installed <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install the missing packages
  if(length(not_installed)) install.packages(not_installed)
  
  # Load the packages
  sapply(packages, require, character.only = TRUE)
}

# Install / load packages

required_packages <- c("ggplot2", "dplyr", "tidyr")
install_and_load_if_needed(required_packages)

## Data processing -------

# Read in data

data.gp <- read.csv("gp-reg-pat-prac-lsoa-all.csv")

data.ahah <- read.csv("AHAH 2022 data.csv")

# Filter data for required variable
data.lookup <- data.ahah %>%
  select(lsoa11,ah3blue) %>% # Select required variables
  distinct() %>% # Remove duplicates
  rename("LSOA_CODE" = lsoa11)

# Initialise dataframe to store weighted average scores by GP practice
data_wa <- data.frame(
  practice_code = character(),
  score = numeric(),
  n.patients = numeric()
)

# List all GP practice codes
list <- data.gp %>%
  select(PRACTICE_CODE) %>% # Get practice codes
  distinct() %>% # Remove duplicates
  arrange(PRACTICE_CODE) %>% # Arrange in alphabetical order
  pull(PRACTICE_CODE) %>% # Extract practice codes
  as.list() # Convert to a list

# Initialise index for FOR loop
i = 1

# Calculate weighted average score by GP practice
for(i in 1:length(list)){

interim <- data.gp %>% # Take GP_LSOA data
  filter(PRACTICE_CODE == list[i]) %>% # Filter single GP practice
  left_join(data.lookup, by = "LSOA_CODE") %>%
  rename(score = names(.)[8]) %>% # Ensure consistency in score variable name
  mutate(freq_score = NUMBER_OF_PATIENTS * score) %>% # Calculate total score for each LSOA
  filter(!is.na(freq_score ))

total.score = sum(interim$freq_score) # Total score for GP practice 
total.patients = sum(interim$NUMBER_OF_PATIENTS) # Total number of patients in GP practice
average.score = total.score / total.patients # Weighted average score for GP practice

# Add practice score value to the dataframe
data_wa[i, ] <- list(list[i], average.score, total.patients)

i = i + 1

}

# Add QOF data
data_wa$practice_code <- as.character(data_wa$practice_code)# Convert from list type to character

data.qof <- read.csv("relevant qofs.csv") %>%
  inner_join(data_wa, by = c("Practice.code" = "practice_code")) %>% # Add score data to QOF data, only where values exist
  filter(n.patients > 500)


## Weighted least squares regression -------

# Fit standard regression
model <- lm(score ~ Prevalence.of.Obesity....2022.2023.in.age..18., data = data.qof)

# Calculate residuals
residuals <- resid(model)

# Estimate weights as the inverse of the squared residuals 
weights <- 1 / (residuals^2)

# Perform the weighted least squares regression using the calculated weight
wls.model <- lm(score ~ Prevalence.of.Obesity....2022.2023.in.age..18., data = data.qof, weights = weights)

# Display the summary of the WLS model
summary(wls.model)


## Plot -------

data.qof.2 <- data.qof # Make replicate of dataframe

data.qof.2$fitted <- fitted(wls.model) # Add fitted values to the data frame

# Plot
ggplot(data.qof.2, aes(x = score, y = Prevalence.of.Obesity....2022.2023.in.age..18.)) +
  geom_point(size = 0.3, colour = "#0D0B7E") +  # Add points
  labs(title = "Scatter Plot Title", x = "X LABEL", y = "Y LABEL") +  # Add labels
  #geom_smooth(method = "lm", se = FALSE, color = "#9B0404") + 
  #geom_line(aes(y = fitted), color = "#9B0404") + # Add line of best fit
  theme_minimal() 
