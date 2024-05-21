# Packages
library(dplyr)
library(ggplot2)

# Read in data

data.gp <- read.csv("gp-reg-pat-prac-lsoa-all.csv")

data.ahah <- read.csv("AHAH 2022 data.csv")

# Filter data for required variable
data.lookup <- data.ahah %>%
  select(lsoa11,ah3gamb) %>% # Select required variables
  distinct() %>% # Remove duplicates
  rename("LSOA_CODE" = lsoa11)

# Initialise dataframe to store weighted average scores by GP practice
data_wa <- data.frame(
  practice_code = character(),
  score = numeric()
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
data_wa[i, ] <- list(list[i], average.score)

i = i + 1

}
