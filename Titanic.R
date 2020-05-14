gc()

# Load packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(h2o)


# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

full  <- dplyr::bind_rows(train, test) # bind training & test data


### Missing Values ###
sapply(full, function(x){sum(is.na(x))})
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), ]

# Get rid of our missing passenger IDs
embark_fare <- full %>% 
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Show row 1044
full[1044, ]
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)



### Create More Variables
full <- transform(full, 
                  Surname =  sapply(full$Name,
                                      function(x) strsplit(x, split = '[,.]')[[1]][1]),
                  Title = gsub('(.*, )|(\\..*)', '', Name),
                  Fsize = SibSp + Parch + 1
                  )
full[, c('Surname', 'Title')] <- lapply(full[, c('Surname', 'Title')], as.character)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'


# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
par(new = TRUE)
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
full$CabinNum <- sapply(full$Cabin, function(x) length(strsplit(x, " ")[[1]]))

table(full$Deck)
table(full$CabinNum)

### Missing Ages
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')  

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age
full$Deck <- as.factor(as.character(mice_output$Deck))
full$Child <-  as.factor(ifelse(full$Age < 18, '1', '0'))
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

Ticket_Split <- sapply(full$Ticket, function(x){strsplit(x, " ")[[1]][1]})
full$Ticket_Var <- ifelse(substr(Ticket_Split, 1,1) %in% c('1','2','3','4','5','6','7','8','9'), 'Num',
                   ifelse(substr(Ticket_Split, 1,1) == 'A', 'A',
                   ifelse(substr(Ticket_Split, 1,1) == 'C', 'C',
                   ifelse(substr(Ticket_Split, 1,1) == 'P', 'P', 
                   ifelse(substr(Ticket_Split, 1,1) == 'S', 'S', 
                   ifelse(substr(Ticket_Split, 1,1) == 'W', 'W', 'ETC'))))))
var <- c('Child', 'Mother', 'Ticket_Var')
full[, var] <- lapply(full[, var], as.factor)
str(full)

# save(full, file = "full.Rdata")
# load("full.Rdata")
full$Survived <- factor(full$Survived)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309, !(colnames(full) %in% c("Survived"))]

h2o.init() ## H2O 시작
trn <- as.h2o(train)
tst <- as.h2o(test)

myX <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title",
         "FsizeD", "Deck", "CabinNum", "Child", "Mother", "Ticket_Var")
myY <- "Survived"

## gbm
gbm <- h2o.gbm(x = myX,
               y = myY,
               build_tree_one_node = T,
               training_frame =  trn,
               distribution = "bernoulli",
               ntrees = 500,
               max_depth = 2,
               min_rows = nrow(trn) * 0.2,
               learn_rate = 0.1,
               seed = 1234,
               nfolds = 10,
               keep_cross_validation_predictions = T)

## Distributed RandomForest
drf <- h2o.randomForest(x = myX,
                        y = myY,
                        training_frame = trn,
                        ntrees = 500,
                        max_depth = 7,
                        seed = 1234,
                        nfolds = 10)

## GLM
glm <- h2o.glm(x = myX,
                 y = myY,
                 training_frame = trn,
                 lambda = 1e-5,
                 family = "binomial",
                 nfolds = 10)

## deeplearning
start.time <- Sys.time()
DL <- h2o.deeplearning(x = myX,
                       y = myY,
                       training_frame = trn,
                       activation = "Rectifier",
                       # distribution = "bernoulli",
                       hidden = c(100, 100),
                       epochs = 300,
                       rate = 0.01,
                       variable_importances = TRUE,
                       seed = 1234,
                       nfolds = 10)
print(duration <- Sys.time() - start.time)


## gbm 결과
gbm
h2o.gainsLift(gbm)
h2o.gainsLift(gbm, xval = T)

## drf 결과
drf
h2o.gainsLift(drf)
h2o.gainsLift(drf, xval = T)

## glm 결과
glm
h2o.gainsLift(glm)
h2o.gainsLift(glm, xval = T)

## Deep Learning 결과
DL
h2o.gainsLift(DL)
h2o.gainsLift(DL, xval = T)

## 제출용 데이터에 적용
Survived <- h2o.predict(object = glm, tst)
as.data.frame(Survived)$predict

test$Survived <- as.data.frame(Survived)$predict

# write.csv(test[, c("PassengerId", "Survived")], "submit.csv", row.names = F)
# 
# h2o.shutdown()
# Y

