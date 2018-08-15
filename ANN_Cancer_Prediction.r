#Classification and diagnostic prediction of cancers using gene expression profiling

#Load Data
fold <- "dataset"
file1_ANN <- "pcaComponents.csv"
file2 <- "class.csv"
mydata0 <- read.csv(file=file.path(params$fold,params$file1_ANN))
clase <- read.csv(file=file.path(params$fold,params$file2))

#use top 10
mydata <- mydata0[,1:10]  

# Data landscape
boxplot(mydata, las=2, col="lightsalmon2", main="PCA data")

# custom normalization function
normalize <- function(x) {
return((x - min(x)) / (max(x) - min(x)))
}

mydata_nrm <- as.data.frame(lapply(mydata, normalize))
summary(mydata_nrm)

lab.group <- c("EWS","BL","NB","RMS")
clase.f <- factor(clase$x,labels=lab.group)

table(clase.f)

# Create 4 news dummies variables
mydata_ann <- mydata_nrm

mydata_ann$EWS <- clase.f=="EWS"
mydata_ann$BL <- clase.f=="BL"
mydata_ann$NB <- clase.f=="NB"
mydata_ann$RMS <- clase.f=="RMS"

################## data spliting
set.seed(params$seed.train) #fijar la semilla para el generador pseudoaleatorio
n_train <- params$p.train
n <- nrow(mydata_ann)
train <- sample(n,floor(n*n_train))
mydata_ann.train <- mydata_ann[train,]
mydata_ann.test  <- mydata_ann[-train,]

# Train Data:

############### libraries loading
#require(neuralnet)

## Create a formula for a model with a large number of variables:
xnam <- names(mydata_ann[1:10])
(fmla <- as.formula(paste("EWS+BL+NB+RMS ~ ",  paste(xnam, collapse= "+"))))
# simple ANN with only a single hidden neuron
set.seed(params$seed.clsfier) # to guarantee repeatable results
mydata_model <- neuralnet(fmla,
                          data = mydata_ann.train,
                          hidden=1)
                          
# Graphic representation
plot(mydata_model, rep="best")
plotnet(mydata_model, alpha=0.6)

#Step 4 - Evualuation

# obtain model results
model_results <- compute(mydata_model, mydata_ann.test[1:10])$net.result

# Put multiple binary output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(model_results, 1, maxidx)
prediction <- factor(idx,levels=c(1,2,3,4),labels=lab.group )
res <- table(prediction, clase.f[-train])

#require(caret, quietly = TRUE)
(conf_matrix<- confusionMatrix(res))

Step 5 - Improve the model:

# a more complex neural network topology with 5 hidden neurons
set.seed(params$seed.clsfier) # to guarantee repeatable results
mydata_model2 <- neuralnet(fmla,
                          data = mydata_ann.train,
                          linear.output = TRUE,
                          hidden=3)
 
 # visualize the network topology
plot(mydata_model2, rep="best")
# ANN representation
plotnet(mydata_model2, alpha=0.6)

# evaluate the results as we did before (Confusion Matrix)
model_results2 <- compute(mydata_model2, mydata_ann.test[1:10])$net.result
idx <- apply(model_results2, 1, maxidx)
prediction2 <- factor(idx,levels=c(1,2,3,4),labels=lab.group )
#prediction2 <- c('EWS', 'BL',"NB","RMS")[idx]
res <- table(prediction2, clase.f[-train])
(conf_matrix3<- confusionMatrix(res))

# 3-fold crossvalidation

# Create new dataset
mydata_caret <- mydata
mydata_caret$clase <- clase.

###3-fold crossvalidation
set.seed(params$seed.clsfier) # to guarantee repeatable results
model <- train(clase ~ ., mydata_caret, method='nnet', 
               trControl= trainControl(method='cv', number=3), 
               tuneGrid= NULL, tuneLength=3 ,trace = FALSE)

plot(model,rep=best)

# ANN representation
plotnet(model, alpha=0.6)


