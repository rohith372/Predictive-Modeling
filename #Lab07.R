

# Install and load packages
# install.packages("tidyverse")
library(tidyverse)
library(class)

# Set working directory
setwd("/Users/rohithpamidimukkala/Desktop/UofA/MIS 545/Lab07")

# Import csv
sedanSize <- read_csv(file = "SedanSize.csv",
                        col_types = 'cfnii',
                        col_name = TRUE)

# Display, structure, summary in console of tibble
print(sedanSize)
str(sedanSize)
summarize(sedanSize)

# Remove MakeModel feature
sedanSize <- sedanSize %>%
  select(-MakeModel)

# Split tibble into two
sedanSizeLabels <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)

# Display all histograms, and call the function
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value, fill = key),
                              color = "black") + facet_wrap (~ key, scales = "free") + 
    theme_minimal()
}
displayAllHistograms(sedanSize)

# Create sampleSet and set seed
set.seed(517)
sampleSet <-sample(nrow(sedanSize),
                   round(nrow(sedanSize) * 0.75),
                   replace = FALSE)

# Split data into testing and training, 
sedanSizeTraining <- sedanSize[sampleSet, ]
sedanSizeTrainingLabels <- sedanSizeLabels[sampleSet, ]
sedanSizeTesting <- sedanSize[-sampleSet, ]
sedanSizeTestingLabels <- sedanSizeLabels[-sampleSet, ]

# Generate k-Nearest Neighbors model
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeTrainingLabels$SedanSize,
                           k = 7)

# Display prediction and summary of prediction
print(sedanSizePrediction)
print(summary(sedanSizePrediction))

# Confusion Matrix
sedanSizeConfusionMatrix <- table()