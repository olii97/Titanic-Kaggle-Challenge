# Loading packages
library("tidymodels")
library("themis")
library("knitr")
library("ranger")
library("doParallel")
library("vip")
library("skimr")
library("corrplot")
library("ggridges")



# Read data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

train <- train %>% mutate(Survived = as.factor(Survived))

glimpse(train)

skim(train) 

# Classification issue


train %>% count(Survived) %>% 
  mutate(prop = n / sum(n))

# Check all the variables
skim(train)

# Corrplot without age
corrplot(cor(train %>% select(PassengerId, Survived, Pclass,  SibSp, Parch, Fare)))

# Set folds
set.seed(82001)
cv_folds <- train %>% vfold_cv(v = 10, strata = Survived)

rf_recipe_downsample <- recipe(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train) %>% 
  update_role(Age, new_role = "ID") %>% 
  step_downsample(Survived) 
rf_recipe_downsample

# Using ranger package for machine learning
rf_model_tune <- rand_forest(mtry = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Setting workflow
rf_tune_wf <- workflow() %>%
  add_recipe(rf_recipe_downsample) %>%
  add_model(rf_model_tune)
rf_tune_wf

# setting the accuracy of the metrics
class_metrics <- metric_set(accuracy, kap, sensitivity, 
                            specificity, roc_auc)
registerDoParallel()

set.seed(99154345)
rf_tune_res <- tune_grid(
  rf_tune_wf,
  resamples = cv_folds,
  grid = tibble(mtry = 1:6),
  metrics = class_metrics
)

rf_tune_res %>%
  collect_metrics()

rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric %in% c("sens", "spec")) %>%
  ggplot(aes(x = mtry, y = mean, ymin = mean - std_err, ymax = mean + std_err, 
             colour = .metric)) +
  geom_errorbar() + 
  geom_line() +
  geom_point() +
  facet_grid(.metric ~ ., scales = "free_y") 


# Select best model
best_acc <- select_best(rf_tune_res, "accuracy")
rf_final_wf <- finalize_workflow(rf_tune_wf, best_acc)
rf_final_wf

rf_trained_workflow <- rf_final_wf %>% fit(train)


newdata <- test
naRowIndices <- rowSums(is.na(newdata)) >= 1
newNonNaData <- newdata[!naRowIndices ,]
predict(rf_trained_workflow, new_data = newNonNaData)


