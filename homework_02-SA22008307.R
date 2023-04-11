#### Section I 
#  reading env and fish data
rm (list = ls ())
library("ade4")
library("tidyverse")
library("caret")
data(doubs)

# summarizing fish abundance data by sites
env <- doubs$env
fish <- doubs$fish 
totalfish <- rowSums(fish)

# combining env and total fish 
library("dplyr")
env_fish <- cbind(env,totalfish = rowSums(fish))

# visualizing the features of the new env_fish
library("ggplot2")
featurePlot(
  x = env_fish[,1:11],
  y = env_fish[,12],
  plot = "scatter",
  layout = c(4,3),
  auto.key = list(columns = 4),
  type = c("p","smooth"),
  col = "blue")

# linear relationships
cor(env_fish[, -12], env_fish[, 12], method = "pearson")

# deleting no fish sites.
library("dplyr")
env_fish <- subset(env_fish,totalfish != 0)

# removing all rows where any column contains an outlier
env_fish <- env_fish[complete.cases(env_fish), ]

# identifying near zero-variance
zerovar <- nearZeroVar(env_fish[, 1:11])
zerovar
env_fish <- env_fish                #integer(0)
# env_fish <- env_fish[, -zerovar]   integer(!=0)  

# removing highly correlated features
env_fish_corr <- cor(env_fish[, 1:11])
high_Corr <- findCorrelation(env_fish_corr, cutoff = 0.75)
env_fish <- env_fish[, -high_Corr]

# detecting the collinearity
comboInfo <- findLinearCombos(env_fish)
# comboInfo$remove = NULL
# Running the following 2 lines codes will report an error
comboInfo$remove         
env_fish <- env_fish[, -comboInfo$remove]   

# scale and center the env
env_fish[, -6] <- scale(env_fish[, -6], center = TRUE, scale = TRUE)



#### Section  II
# splitting data into training and test sets
set.seed(100)
index_fish <- createDataPartition(env_fish$totalfish, p = 0.8, list = FALSE)
train <- env_fish[index_fish,]
test <- env_fish[-index_fish,]
nrow(train);prop.table(table(train$totalfish))

# Creating and evaluating a baseline model
fitControl = trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  search = "random")

model_1 = train(
  totalfish ~ ., 
  data = train,
  method = "rf",
  trControl = fitControl,
  tuneLength = 10,
  metric = "RMSE",
  verbose = FALSE
)

model_1

# prediction
model_predictions <- predict(model_1, newdata = test)
model_predictions
