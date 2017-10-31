#Importing Libraries
library(rattle)
library(corrplot)
library(randomForest)
library(rpart)
library(readr)
library(rgl)
library(car)
library(xgboost)

#Import test and train
train <- read_csv("C:/Users/sayan/Desktop/train (1).csv")
test <- read_csv("C:/Users/sayan/Desktop/test (1).csv")
data <- train
valid <- test

#Data processing
names(data)
data$MSZoning[data$MSZoning == "RH"] <- 5
data$MSZoning[data$MSZoning == "RM"] <- 4
data$MSZoning[data$MSZoning == "RP"] <- 3
data$MSZoning[data$MSZoning == "RL"] <- 2
data$MSZoning[data$MSZoning == "RL"] <- 3
data$MSZoning[data$MSZoning == "C (all)"] <- 2
data$MSZoning[data$MSZoning == "FV"] <- 1
data$MSZoning <- as.integer(data$MSZoning)

data$Street[data$Street == "Pave"] <-1
data$Street[data$Street == "Grvl"] <-0
data$Street <- as.integer(data$Street)

data$LotShape[data$LotShape != "Reg"] <- 0
data$LotShape[data$LotShape == "Reg"] <- 1
data$LotShape <- as.integer(data$LotShape)

data$LandContour[data$LandContour == "HLS"] <- 4
data$LandContour[data$LandContour == "Lvl"] <- 3
data$LandContour[data$LandContour == "Bnk"] <- 2
data$LandContour[data$LandContour == "Low"] <- 1
data$LandContour <- as.integer(data$LandContour)

data$Utilities[data$Utilities == "AllPub"] <- 4
data$Utilities[data$Utilities == "NoSewr"] <- 3
data$Utilities[data$Utilities == "NoSeWa"] <- 2
data$Utilities[data$Utilities == "ELO"] <- 1
data$Utilities <- as.integer(data$Utilities)

#Mean Normalisation
data$LotArea <- (data$LotArea - mean(data$LotArea , na.rm = TRUE))/sd(data$LotArea , na.rm = TRUE)

#Correlations
correlation <- cor(data[,c(2,3,4,5,6,8,81)] , use = "everything")
corrplot(correlation , method="number" , type="lower")
corrplot.mixed(correlation)
scatterplot(data$SalePrice ~ data$MSZoning , data=data ,grid="FALSE")
plot3d(data$SalePrice , data$LotArea , data$MSZoning , size=3)

#Variables: YearBuilt OverallCond OverallQual Utilities

#test prep
test$MSZoning[test$MSZoning == "RM"] <- 4
test$MSZoning[test$MSZoning == "RP"] <- 3
test$MSZoning[test$MSZoning == "RH"] <- 5
test$MSZoning[test$MSZoning == "RL"] <- 2
test$MSZoning[test$MSZoning == "RL"] <- 3
test$MSZoning[test$MSZoning == "C (all)"] <- 2
test$MSZoning[test$MSZoning == "FV"] <- 1
test$MSZoning[is.na(test$MSZoning)] <- 2
test$MSZoning <- as.integer(test$MSZoning)

test$Street[test$Street == "Pave"] <-1
test$Street[test$Street == "Grvl"] <-0
test$Street <- as.integer(test$Street)

test$LotShape[test$LotShape != "Reg"] <- 0
test$LotShape[test$LotShape == "Reg"] <- 1
test$LotShape <- as.integer(test$LotShape)

test$LandContour[test$LandContour == "HLS"] <- 4
test$LandContour[test$LandContour == "Lvl"] <- 3
test$LandContour[test$LandContour == "Bnk"] <- 2
test$LandContour[test$LandContour == "Low"] <- 1
test$LandContour <- as.integer(test$LandContour)

test$Utilities[test$Utilities == "AllPub"] <- 4
test$Utilities[test$Utilities == "NoSewr"] <- 3
test$Utilities[test$Utilities == "NoSeWa"] <- 2
test$Utilities[test$Utilities == "ELO"] <- 1
test$Utilities <- as.integer(test$Utilities)

#Mean Normalisation
test$LotArea <- (test$LotArea - mean(test$LotArea , na.rm = TRUE))/sd(test$LotArea , na.rm = TRUE)

#initial fit random forest and decision tree
fit<-rpart(SalePrice ~  MSZoning+LotFrontage+MSSubClass+Street+LotShape, data=data , method="anova")
fancyRpartPlot(fit)
rf <- randomForest(SalePrice ~ MSZoning+Street+LotShape+LandContour+Utilities+YearBuilt+OverallCond+OverallQual , data = data, ntree=20000 )
plot(rf)

Prediction <- predict(fit , test , type ="vector")
submit <- data.frame(ID = test$Id , SalePrice =Prediction)
write.csv(submit, file="House.csv" , row.names=FALSE)

predict <- predict(rf , test )
submit <- data.frame(ID = test$Id , SalePrice = predict)
predict[is.na(pedict)] <- mean(predict, na.rm = "TRUE")
write.csv(submit, file="House1.csv" , row.names=FALSE)

#Alternate method to normalise
normal <- function(x)
{
  x <-(x-y)%/%z
}
y<- mean(data$LotArea , na.rm = TRUE)
z <- sd(data$LotArea , na.rm = TRUE)
sapply(data$LotArea , normal)
