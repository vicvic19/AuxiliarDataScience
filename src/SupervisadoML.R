rm(list=ls()) # clear the list of objects
graphics.off() # clear the list of graphs
options(digits = 3) # number of digits to display
options(scipen=999)


# Hacer clasificación:

# Veremos:

# * KNN
# * DECISION TREE


data <- read.csv("../data/breast_cancer.csv",sep=";")


library(caret) 
library(lattice)
library(ggplot2)
library(MLmetrics)
library(e1071)



# Definir semilla
set.seed(22)

intrain <- createDataPartition(y = data$class, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

rm(intrain)

# Creamos cross validation

training[["class"]]=as.factor(training[["class"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) # train control

###########
## KNN ####
###########

knn_fit <- train(class ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"), # definimos que escale primero
                 tuneLength = 15) # cantidad de k a ver

knn_fit

test_pred <- predict(knn_fit, newdata = testing)
test_pred


## Reporta sensitivity, specifity
confusionMatrix(test_pred, factor(testing$class))

## Reporta Recall, Precision y F1
confusionMatrix(test_pred, factor(testing$class), mode="prec_recall")

recall(test_pred, factor(testing$class))

precision(data = test_pred,reference = factor(testing$class))

F_meas(test_pred,factor(testing$class))

########################## ALTERNATIVA

library(class)

test_pred2<-knn(train=training,cl=training[,31],test=testing,k=5)

training2 <- scale(training[,-31])
testing2 <- scale(testing[,-31])

test_pred3<-knn(train=cbind(training2,training[,31]),cl=training[,31],test=cbind(testing2,testing[,31]),k=5)
test_pred3==test_pred



##############################
#### DECISION TREE  ##########
##############################

tree_fit <- train(class ~., data = training, method = "rpart",
                  parms=list(split="information"),
                  trControl=trctrl,
                  tuneLength = 10)
tree_fit
library(rattle)
library(rpart.plot)
library(RColorBrewer)
prp(tree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


my_tree <- rpart(class ~ .,data=training,method="class",parms = list(split="information"))
my_tree


## Reportes

printcp(my_tree) # display the results 
plotcp(my_tree) # visualize cross-validation results 
summary(my_tree) # detailed summary of splits


# plot tree 
plot(my_tree, uniform=TRUE, 
     main="Classification Tree for Breast Cancer")
      text(my_tree, use.n=TRUE, all=TRUE, cex=.8)


#install.packages("rattle")


fancyRpartPlot(my_tree, caption = NULL)


## Métricas de Decision Tree

test_pred_tree <- predict(tree_fit, newdata = testing)
test_pred_tree


## Reporta sensitivity, specifity
confusionMatrix(test_pred_tree, factor(testing$class))

## Reporta Recall, Precision y F1
confusionMatrix(test_pred_tree, factor(testing$class), mode="prec_recall")

recall(test_pred_tree, factor(testing$class))

precision(data = test_pred_tree,reference = factor(testing$class))

F_meas(test_pred_tree,factor(testing$class))

