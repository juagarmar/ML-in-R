#Algoritmo Support Vector Machine (SVM)

# Loading Data

fold <- "dataset"
file1_SVM <- "data.csv"
file2 <- "class.csv"
mydata <- read.csv(file=file.path(params$fold,params$file1_SVM))
clase <- read.csv(file=file.path(params$fold,params$file2))
dim(mydata)
# add class
lab.group <- c("EWS","BL","NB","RMS")
clase.f <- factor(clase$x,labels=lab.group)
mydata$clase <- clase.f

# Step 2 - Data Exploration
mydata.train <- mydata[train,]
mydata.test  <- mydata[-train,]
summary(mydata[, 2300:2309])

# Step 3 - Training the model:

# begin by training a simple linear SVM
#library(kernlab)
set.seed(params$seed.clsfier) # to guarantee repeatable results
mydata_model1 <- ksvm(clase ~ ., data = mydata.train,
                      kernel = "vanilladot")
           
# look at basic information about the model
mydata_model1

# Step 4 - Evaluation:

# predictions on testing dataset
mydata_predict1 <- predict(mydata_model1, mydata.test)
res <- table(mydata_predict1, mydata.test$clase)
#require(caret)
(conf_mat.s1 <- confusionMatrix(res))

Step 5 - Improve the model

# begin by training a Gaussian SVM
#library(kernlab)
set.seed(params$seed.clsfier) # to guarantee repeatable results
mydata_model2 <- ksvm(clase ~ ., data = mydata.train,
                      kernel = "rbfdot")
# look at basic information about the model
mydata_model2
# predictions on testing dataset
mydata_predict2 <- predict(mydata_model2, mydata.test)
res <- table(mydata_predict2, mydata.test$clase)

#require(caret)
(conf_mat.s2 <- confusionMatrix(res))

# 3-fold crossvalidation

###3-fold crossvalidation
set.seed(params$seed.clsfier) # to guarantee repeatable results
model_sc <- train(clase ~ ., mydata, method='svmLinear', 
               trControl= trainControl(method='cv', number=3), 
               tuneGrid= NULL, trace = FALSE)

model_sc




