str(mydata_1)
colnames(mydata_1)<-c("octamer","cleaved")
mydata_1$octamer<-as.character(mydata_1$octamer)
mydata_1$cleaved<-as.factor(mydata_1$cleaved)
str(mydata_1)
table(mydata_1[,2])

################# data transformation
# There are 20 allowed letters in the 8 character string
# (the allowed alphabet): 'ARNDCQEGHILKMFPSTWYV'
alphabet<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")

#split data
f2<-function(x){
y<-unlist(strsplit(x,""))
sapply(y,function(x){match(alphabet,x,nomatch=0)})
}

out<-apply(as.data.frame(mydata_1[,1]),1,f2)
out<-t(out)

dim(out)
out[1:6,1:20]

# center data
out<-scale(out,center=T,scale=F)
#head(out)
out[1:6,1:20]

################## data spliting
set.seed(123) #fijar la semilla para el generador pseudoaleatorio
train<-sample(1:nrow(out),round(2*nrow(out)/3,0))
# create training and test data
out_training<-out[train,]
out_test<-out[-train,]
# create labels for training and test data
class_training<-mydata_1[train,2]
class_test<-mydata_1[-train,2]

############### libraries loading
#library(class) # knn
############## data prediction
test_pred <- knn(train =out_training, test = out_test, cl = class_training, k=3)

table(class_test)

#require(knitr, quietly = TRUE)
kable(resum, col.names=c("valor k", "# falsos negativos",
"# falsos positivos", "% mal clasificados"),
align= c("l","c","c","c"), caption= paste("Algoritmo kNN: ",
params$file1 ,sep=""))

str(mydata_2)

colnames(mydata_2)<-c("octamer","cleaved")
mydata_2$octamer<-as.character(mydata_2$octamer)
mydata_2$cleaved<-as.factor(mydata_2$cleaved)
str(mydata_2)

table(mydata_2[,2])

#Transformation
uut<-apply(as.data.frame(mydata_2[,1]),1,f2)
uut<-t(uut)
dim(uut)
uut[1:6,1:20]

# center data
uut<-scale(uut,center=T,scale=F)
#head(out)
uut[1:6,1:20]

# Data Setup:

# load the "gmodels" library
#library(gmodels)
# Create the cross tabulation of predicted vs. actual

############ evaluating model performance
CrossTable(x = mydata_1[,2], y = test_pred , prop.chisq=FALSE)
table(class_test)

############# Evaluation:

#require(knitr, quietly = TRUE) 
kable(resum, col.names=c("valor k", "# falsos negativos", "# falsos positivos", "% mal clasificados"),
align= c("l","c","c","c"), caption= paste("Algoritmo kNN: ", params$file2 , " y ", params$file1, sep=""))

