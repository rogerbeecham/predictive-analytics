#-------------------------------------------------------------Install packages

#install.packages('tidyverse')
#install.packages('sf')
#install.packages('caret')
#install.packages('fastDummies')
#install.packages('randomForest')
#install.packages('pROC')

library(tidyverse)
library(sf)
library(caret)
library(fastDummies)
library(randomForest)
library(pROC)

#------------------------------------------------------------Download and Load data
# Download data and unzip
url_data <- "https://www.roger-beecham.com/datasets/"
download.file(paste0(url_data,"microsim_5927.zip"), "./data.zip")
unzip("./data.zip")

# Read in individual-level survey data and simulated data
individuals <- read_csv("./data/individuals.csv")
simulated_oac_age_sex <- read_csv("https://www.roger-beecham.com/datasets/microsim.csv")

# Attach individual attributes to simulated dataset
simulated_oac_age_sex <- simulated_oac_age_sex %>% select(person_id, zone) %>%
  inner_join(individuals)

glimpse(simulated_oac_age_sex)


# --------------------------------------------------------------Pre-process data

# Take a sample of data (the full data are too large to model )
simulated_oac_sample = createDataPartition(y = simulated_oac_age_sex$overseas_airport, p= 0.3, list = FALSE)
simulated_oac_age_sex <- simulated_oac_age_sex[simulated_oac_sample,]

# Aggregate income data
simulated_oac_age_sex$income_band = NA
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "11-15K"
                                  | simulated_oac_age_sex$household_income == "0-10K"] <- "<15k"
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "16-20K"
                                  | simulated_oac_age_sex$household_income == "21-25K"
                                  | simulated_oac_age_sex$household_income == "26-30K"] <- "16_30k"
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "31-35K"
                                  | simulated_oac_age_sex$household_income == "36-40K"
                                  | simulated_oac_age_sex$household_income == "41-50K"] <- "31_50k"
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "51-60K"
                                  | simulated_oac_age_sex$household_income == "61-70K"
                                  | simulated_oac_age_sex$household_income == "71-80K"] <- "51_80k"
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "81K Plus"] <- "81k plus"
simulated_oac_age_sex$income_band[simulated_oac_age_sex$household_income == "NA"
                                  | simulated_oac_age_sex$household_income == "Not Answered"] <- "NA_Not_Answered"

# Subset data to include only a subset of predictor variables
simulated_subset = simulated_oac_age_sex[c(3:6, 8, 12)]

# Convert to factors for use in Random Forest Model
simulated_subset$oac_grp  = as.factor(simulated_subset$oac_grp)
simulated_subset$sex      = as.factor(simulated_subset$sex)
simulated_subset$age_band  = as.factor(simulated_subset$age_band)
simulated_subset$number_children = as.factor(simulated_subset$number_children)
simulated_subset$overseas_airport = as.factor(simulated_subset$overseas_airport)
simulated_subset$income_band = as.factor(simulated_subset$income_band)

# Create dummy variables (this creates a column for each possible categorical outcome where 1 indicates presence of the variable)
simulated_oac_dummies = fastDummies::dummy_cols(simulated_subset, select_columns=c("overseas_airport"))

# Print column names
colnames(simulated_oac_dummies)
ML_data=simulated_oac_dummies[c(3:4, 6, 58)] # subset data to keep only data to be used in final model

# Remove rows with missing data
ML_data = ML_data[complete.cases(ML_data), ]

# Convert outcome variable to factor
ML_data$overseas_airport_PMI = as.factor(ML_data$overseas_airport_PMI)
# Set 1 as the first level (this will become the positive class in our model)
ML_data$overseas_airport_PMI = relevel(ML_data$overseas_airport_PMI, '1')


#-----------------------------------------------------------------------Partition Data

# Split into training and testing
intrain <- createDataPartition(y = ML_data$overseas_airport_PMI, p= 0.6, list = FALSE)
training <- ML_data[intrain,]
testing <- ML_data[-intrain,]

# Make names legal for algorithms
names(testing) <- make.names(names(testing))
names(training) <- make.names(names(training))


#---------------------------------------------------------------------Train  and test model
# Training
set.seed(1234) # allows reproducibility
trctrl <- trainControl(method = "cv")
mtry <- sqrt(ncol(training))
tunegrid <- expand.grid(.mtry=mtry)
# Train the model using the labelled data - THIS MAY TAKE UPTO 10 MINS
PMI_tree <- train(overseas_airport_PMI ~., data = training, method = "rf",
                        metric = "Kappa",
                        trControl=trctrl,
                        tunegrid=tunegrid)
PMI_tree

# Predict 'unseen' records
pred_PMI = predict(PMI_tree, newdata = testing)

# A confusion matrix allows us to view the actual labels vs predicted.
# Can you see the problem with our model?
confusionMatrix(table(pred_PMI,testing$overseas_airport_PMI))


#-----------------------------------------------------------------------Address class imbalance
# Let's now under sample the training dataset to address the class imbalance
# select all PMI airport trips in training set
PMI_airport = training[training$overseas_airport_PMI=='1',]
#select all 0s (i.e. not PMI)
not_PMI_airport = training[training$overseas_airport_PMI=='0',]

# Select a random sample of non PMI airport to add to PMI_airport (resampling at 1:1 ratio)
set1 = not_PMI_airport[sample(nrow(not_PMI_airport), 4506),]
training1 = rbind(set1, PMI_airport)
training1$overseas_airport_PMI = relevel(training1$overseas_airport_PMI, '1')

# Check numbers - they should now be equal
table(training1$overseas_airport_PMI)

# Retrain model
# Training
set.seed(1234) # allows reproducibility
trctrl <- trainControl(method = "cv")
mtry <- sqrt(ncol(training1))
tunegrid <- expand.grid(.mtry=mtry)
# Train the model using the labelled data - THIS MAY TAKE UPTO 10 MINS
PMI_tree_resampled <- train(overseas_airport_PMI ~., data = training1, method = "rf",
                  metric = "Kappa",
                  trControl=trctrl,
                  tunegrid=tunegrid)

# Summary of the train object
PMI_tree_resampled

# Predict 'unseen' records
pred_PMI_resampled = predict(PMI_tree_resampled, newdata = testing)

# Is this any better? #Yes increased True positives, but still could be improved!
confusionMatrix(table(pred_PMI_resampled,testing$overseas_airport_PMI))

# Examine the most predictive variables
varImp(PMI_tree_resampled)

# Create a simple plot
dotPlot(varImp(PMI_tree_resampled, useModel = T, scale = T))

# Create a nicer variable importance plot
df <- data.frame(matrix(unlist(varImp(PMI_tree_resampled)$importance),
                        ncol = max(lengths(varImp(PMI_tree_resampled)$importance)),
                        byrow = TRUE))
names(df) <- names(varImp(PMI_tree_resampled)$importance[[which(lengths(varImp(PMI_tree_resampled)$importance)>0)[1]]])

varImp(PMI_tree_resampled)$importance %>%
  map_df(as_tibble)

varImp_data = varImp(PMI_tree_resampled)$importance %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname))

varImp_data$rowname = rev(c("Age 65+","Income Na/ not answered", "Children 2", "Age 25-34", "Age 50-64", 'Age 35-49', 'Children 1', 'Children 3','Income 51-80k', 'Income 16-30k', 'Income 31-50k', 'Children 4', 'Income 81k+'))

ggplot(varImp_data, aes(x = reorder(rowname, Overall), y = as.numeric(Overall), fill = as.numeric(Overall))) +
  geom_bar(stat = 'identity')+
  geom_text(aes(label=sprintf("%0.2f", round(as.numeric(Overall), digits = 2))), position=position_dodge(width=0.9), color = "grey20", hjust=-0.25)+
  coord_flip()+
  labs(x = 'Variable', y ='Importance Score', fill = 'Strength') +
  theme(legend.position = c(0.95, 0.1))+
  theme(panel.background = element_rect(fill = 'white'))+
  scale_fill_gradientn(colours = c("gray87", 'lightskyblue', "skyblue2", "skyblue3","skyblue4"))

# How could we improve the model?
# 1) Replace the 'cv' method with 'repeatedcv', this allows us to vary the number of randomly selected predictor
# variables at each split (mtry).

# DO NOT RUN
# Due to the size of the data and the train method this model is too long to run in the practical
# but you could run it on your laptop (overnight)

#set.seed(1234) # allows reproducibility
#mtry <- sqrt(ncol(training1))
#tunegrid <- expand.grid(.mtry=mtry)
#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#PMI_tree_resampled <- train(overseas_airport_PMI ~., data = training1, method = "rf",
#                            metric = "Kappa",
#                            trControl=trctrl,
#                            tuneGrid=tunegrid)

#2) Instead of undersampling the training set, we could generate synthetic data points to address class imbalance.
# This would increase the amount of data available for training. As micro simulation has already been used to
# generate this data, this wouldn't be the best idea See SMOTE (Synthetic Minority Oversampling TEchnique)
