# Packages
library(dplyr)
library(ggplot2)

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

# Plot
ggplot(data.qof, aes(x = score, y = Prevalence.OF..non.diabetic.hyperglycaemia..2022.2023..in.age..18..)) +
  geom_point() +  # Add points
  labs(title = "Scatter Plot", x = "X LABEL", y = "Y LABEL") +  # Add labels
  theme_minimal() 

# Regression

model <- lm(score ~ Prevalence.OF..non.diabetic.hyperglycaemia..2022.2023..in.age..18.., data = data.qof)

summary(model)
