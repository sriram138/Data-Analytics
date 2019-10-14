dataset <- read.csv(file="crx.data",header = FALSE)
str(dataset)
View(dataset)
colnames(dataset)=c('GENDER','AGE','DEBIT','MARRIED','BANKCUSTOMER','EDUCATIONALLEVEL','ETHINICITY','YEARSEMPLOYED','PRIORDEFAULT','EMPLOYED','CREDDITSCORE','DRIVERSLICENCE','CITIZEN','ZIPCODE','INCOME','APPROVED')
dataset$APPROVED=factor(dataset$APPROVED,levels = c('+','-'),labels = c(0,1))
dataset$GENDER=factor(dataset$GENDER,levels = c('a','b'),labels = c('Male','Female'))
dataset$EMPLOYED=factor(dataset$EMPLOYED,levels = c('f','t'),labels = c(0,1))
dataset$PRIORDEFAULT=factor(dataset$PRIORDEFAULT,levels = c('f','t'),labels = c(0,1))
dataset$AGE <- as.numeric(paste(dataset$AGE))
dataset$AGE <- round(dataset$AGE)
dataset$YEARSEMPLOYED=round(dataset$YEARSEMPLOYED)
dataset$AGE=ifelse(is.na(dataset$AGE),ave(dataset$AGE, FUN = function(x)mean(x, na.rm=TRUE)),dataset$AGE)
library(ggplot2)
library(tidyr)
library(funModeling)
library(dplyr)
plot_num(dataset)
ggplot(data = dataset,aes(x=AGE,y=INCOME,color=APPROVED))+geom_point()
ggplot(data=dataset,aes(x=GENDER,y=YEARSEMPLOYED))+geom_boxplot()
ggplot(data=dataset,aes(x=AGE))+geom_histogram(fill='green',color='black')
ggplot(data=dataset,aes(x=CREDDITSCORE,fill=GENDER))+geom_bar()+xlim(0,25)+ylim(0,100)
ggplot(data=dataset,aes(x=YEARSEMPLOYED,fill=GENDER))+geom_histogram(binwidth = 5)
ggplot(data=dataset,aes(x=YEARSEMPLOYED,y=INCOME,color=YEARSEMPLOYED))+geom_line()
ggplot(data=dataset,aes(x=YEARSEMPLOYED))+geom_histogram(binwidth = 5)+facet_wrap(dataset$APPROVED)
ggplot(data=dataset,aes(x=YEARSEMPLOYED,fill=EMPLOYED))+geom_density(alpha=0.4)+xlim(0,25)
##Decission tree

library(caTools)
dataset_new=dataset[,c(1,2,8,10,11,15,16)]
View(dataset_new)
split = sample.split(dataset_new$APPROVED,
                     SplitRatio = 0.8)
split
training_set =subset(dataset_new, split==TRUE)
test_set =subset(dataset_new, split==FALSE)
dim(training_set)
dim(test_set)
library(rpart)
library(rpart.plot)
fit <- rpart(APPROVED~GENDER+YEARSEMPLOYED+EMPLOYED+CREDDITSCORE+INCOME,
             data = training_set,
             method = 'class')
rpart.plot(fit)
predict_unseen=predict(object = fit,
                       newdata=test_set,
                       type='class')
tab_mat= table(test_set$APPROVED,
               predict_unseen)
sum(diag(tab_mat))/sum(tab_mat)
#Logistic Regression
library(caTools)
set.seed(123)
data_log <- dataset[,c(8,11,16)]
View(data_log)
summary(data_log)
split = sample.split(data_log$APPROVED,
                     SplitRatio = 0.80)
split
training_set =subset(data_log, split==TRUE)
test_set =subset(data_log, split==FALSE)
View(training_set)
View(test_set)
training_set[-3] = scale(training_set[-3])
test_set[,1:2] = scale(test_set[,1:2])

classifier = glm(formula = APPROVED ~ .,
                 family = binomial(),
                 data = training_set)
prob_pred = predict(classifier,
                    type='response',
                    newdata=test_set[-3])
y_pred=ifelse(prob_pred>0.5,1,0)
y_pred
cm=table(test_set[,3],y_pred)
cm
prop.table(cm)
library(ElemStatLearn)
set=training_set

X1=seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
X2=seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set) =c("YEARSEMPLOYED", 
                      "CREDDITSCORE")

prob_set=predict(classifier, type="response", newdata=grid_set)
y_grid=ifelse(prob_set>0.5,1,0)
plot(set[,-3], main="Logistic Regression",
     xlab='Age', ylab='Estimatated Salary',
     xlim=range(X1), ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add=T)
points(grid_set, pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set, pch=21, bg=ifelse(set[,3]==1, 'green4', 'red3'))
plot(training_set[,-3])
##K-Means Clustering

dataset_clus= dataset_new[4:6]
View(dataset_clus)
library(cluster)
set.seed(3)
wcss = vector()
for(i in 1:10) 
  wcss[i]=sum(kmeans(dataset_clus,i)$withinss)
plot(1:10, wcss,
     type='b',
     main="The Elbow Method",
     xlab="Number of Cluster",
     ylab='WCSS')
kmeans=kmeans(x=dataset_clus,centers = 3)
y_kmeans = kmeans$cluster
clusplot(dataset_clus, y_kmeans,
         shade = T,
         lines = 0,
         color = T,
         main="Cluster of CREDIT APPROVAL",
         xlab = 'INCOME',
         ylab = 'CREDIT SCORE')

