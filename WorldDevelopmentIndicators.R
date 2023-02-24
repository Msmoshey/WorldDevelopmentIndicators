#number 1
library(caret)
library(dplyr)
library(cluster)
library(MASS)
library(ggplot2)
library(ISLR)
library(nnet)
library(tidyr)
library(binaryLogic)
#Import data set
WDIproject<- read.csv('C:/Users/user/Desktop/Final Project/project_data.csv', header=T)
#clean data 
#remove columns with more than 25% nas
sapply(WDIproject,function(x) sum(is.na(x)))
WDIproject<-WDIproject[,c(-6,-7,-9,-15,-19)]
#drop rows with na 
WDIproject<-drop_na(WDIproject)
WDIproject
#change covid death, comp.eduation to numeric
str(WDIproject)
WDIproject$Covid.deaths<-as.numeric(gsub(",","",WDIproject$Covid.deaths))
WDIproject$Comp.education<-as.numeric(gsub(",","",WDIproject$Comp.education))
WDIproject$Pop.total<-as.numeric(gsub(",","",WDIproject$Pop.total))
str(WDIproject)
#rename column country name for easy reference 
names(WDIproject)[names(WDIproject) == 'ï..Country.Name'] <- 'Country.Name'
head(WDIproject)

#Question 1
#Descriptive analysis of the data 
summary(WDIproject)

#boxplot of covid deaths per continent 
boxplot(WDIproject$Covid.deaths ~ WDIproject$Continent, outline=FALSE,las=1,
        main = "Covid Deaths by Continent", xlab = "Continents", ylab = "Covid Deaths",
        col = rainbow(ncol(WDIproject)),
        outbg = "red")
#Top Ten Covid case by Country
WDI<-WDIproject%>% arrange(desc(Covid.deaths))
WDI
toptenCD<- WDI[c(1:10),]
toptenCD

ggplot(data = toptenCD) +
  geom_bar(mapping = aes(x = Country.Name, y = reorder(Covid.deaths, Country.Name), fill = Country.Name), stat = "identity") +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = guide_legend(title = "Country")) +
  labs(
    x = "Country",
    y = "No Covid Deaths",
    title = "Top 10 ",
    caption = "Top 10 Covid deaths"
  )

last10<- WDIproject %>%
  arrange(Covid.deaths) %>%
  mutate(n = row_number()) %>% filter(n <= 10)  
ggplot(data = last10) +
  geom_bar(mapping = aes(x = Country.Name, y = reorder(Covid.deaths,Country.Name), fill = Country.Name), stat = "identity") +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = guide_legend(title = "Country")) +
  labs(
    x = "Country",
    y = "Covid Death",
    title = "Last 10",
    caption = "Lowest 10 Covid deaths"
  )

#Question 2
question2 <- WDIproject
idx <- sample(1:dim(question2)[1], 50);idx
dim<-question2[idx,]
dfX<-question2[,-(c(1,2,3))] #remove Continent, country and covid deaths column
str(dfX)
library(factoextra) #plot matrix
dfX<-scale(dfX) #scale sample
#calculating the distance matrix
dfx2<- dist(dfX, method = "euclidean")
fviz_dist(dfx2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#Optimal distance calculation
fviz_nbclust(dfX, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)
#K means Clustering
set.seed(123)
kmeans2 <- kmeans(dfX, centers = 2, nstart = 25)
kmeans3 <- kmeans(dfX, centers = 3, nstart = 25)
kmeans4 <- kmeans(dfX, centers = 4, nstart = 25)
kmeans2
str(kmeans2)
fviz_cluster(kmeans2, data = dfX)
fviz_cluster(kmeans3, data = dfX)
fviz_cluster(kmeans4, data = dfX)
f1 <- fviz_cluster(kmeans2, geom = "point", data = dfX) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = dfX) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = dfX) + ggtitle("k = 4")
library(gridExtra) # to arrange plot in grid style on one page
grid.arrange(f1, f2, f3, nrow = 2)

#Start my calculating the distance matrix
df <- dist(dfX, method = "euclidean")
#Apply hierarchical clustering for different linkage methods
fit.single <- hclust(df, method="single")
fit.complete <- hclust(df, method="complete")
fit.average <- hclust(df, method="average")
fit.centroid <-hclust(df, method="centroid")

#par(mfrow=c(2,2))
plot(fit.single)
# cut tree into k=4 clusters
groups.fit.single <- cutree(fit.single, k=4)
# draw dendrogram with red borders around the 4 clusters
rect.hclust(fit.single, k=4, border="red")

fit.centroid <-hclust(df, method="centroid")
#Checking how many observations are in each cluster
table(groups.fit.single)

plot(fit.complete)
groups.fit.complete <- cutree(fit.complete, k=4)

rect.hclust(fit.complete, k=4, border="red")
table(groups.fit.complete)


plot(fit.average)
groups.fit.average <- cutree(fit.average, k=4)
rect.hclust(fit.average, k=4, border="red")
table(groups.fit.average)
plot(fit.centroid)
groups.fit.centroid <- cutree(fit.centroid, k=4)
rect.hclust(fit.centroid, k=46, border="red")
table(groups.fit.centroid)

table(groups.fit.complete,question2$Continent)
ggplot(question2, aes(question2$Life.expec, question2$Birth.rate, color =(question2$Continent))) +
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = groups.fit.complete) +
  scale_color_manual(values = c('black', 'red', 'green', 'blue', 'yellow','pink'))

#question 3
#create mean of covid deaths 
meanCD<-mean(WDIproject$Covid.deaths)
meanCD
#make covid death binary 
WDIproject$Covid.deaths<-ifelse(WDIproject$Covid.deaths > meanCD,"High","Low")
WDIproject$Covid.deaths<-as.factor(WDIproject$Covid.deaths)
WDIproject<-data

train <- data[,3:14]
train
#fit in train model
glm.fit<-glm(Covid.deaths~.,data=train,family=binomial)
summary(glm.fit)
contrasts(train$Covid.deaths)
glm.probs <- predict(glm.fit,type="response")
glm.probs 
glm.predicted <- rep("High",153)
glm.predicted
glm.predicted[glm.probs>0.5]="Low"
table(glm.predicted, train$Covid.deaths)
mean(glm.predicted==train$Covid.deaths)
#using AIC
glmfit2<- stepAIC(glm.fit)
glmfit3<- glm(Covid.deaths ~ Mortality.rate + Health.exp.capita+
                Health.exp, family = binomial(), train)

hist(glmfit3$fitted.values,main = " Histogram ",xlab = "Probability of high Covid casuality")


#question4
WDIproject<- read.csv('C:/Users/user/Desktop/Final Project/project_data.csv', header=T)
WDIproject<-WDIproject[,c(-6,-7,-9,-15,-19)]
#drop rows with na 
WDIproject<-drop_na(WDIproject)
#categorical variables
WDIproject$Covid_deaths4 <- ifelse(WDIproject$Covid.deaths <= 200, 0, 3)
WDIproject$Covid_deaths4 <- ifelse(WDIproject$Covid.deaths > 200 &WDIproject$Covid.deaths <= 1000, 1, WDIproject$Covid_deaths4)
WDIproject$Covid_deaths4 <- ifelse(WDIproject$Covid.deaths > 1000 & WDIproject$Covid.deaths <= 2000, 2, WDIproject$Covid_deaths4) 
WDIproject$Covid_deaths4 <- as.factor(WDIproject$Covid_deaths4) 
WDIproject


set.seed(1000)
index4 <- caret::createDataPartition(WDIproject$Covid_deaths4, p = 0.70, list = FALSE)
train4 <- WDIproject[index4, ]
test4 <- WDIproject[-index4, ]
head(train4)
head(test4)
logistic_model34 <- glm(Covid_deaths ~Life.expec + Elect.access+ Mortality.rate + Pop.growth +
                          Pop.total + Pop.density +Health.exp.capita + Health.exp + GDP.growth +
                          GDP.capita + Birth.rate+ Comp.education, family = binomial(), data = train4)
summary(logistic_model34)

logit_24 <- stepAIC(logistic_model34)

logistic_train4 <- glm(Covid_deaths4 ~Life.expec + Elect.access + Mortality.rate + 
                         Pop.total + Pop.density + GDP.growth + Birth.rate, family = binomial(), train4)
summary(logistic_train4)
#final model
lo2 <- glm(Covid_deaths4~ Life.expec + Mortality.rate + Elect.access+ Pop.growth +
             Health.exp + Birth.rate+ Comp.education, family = binomial(), train4)
summary(lo2)
#lda
ldamode <- lda(factor(Covid_deaths4)~Life.expec + Mortality.rate + Elect.access+ Pop.growth +
                 Health.exp + Birth.rate+ Comp.education, train4) #this gave more accuracy compared to oldamode

plot(ldamode)
predmodel.train <- predict(ldamode, data=train4) #predict on train dataset
table(predmodel.train$class, train4$Covid_deaths4)
mean(predmodel.train$class==train4$Covid_deaths4) #check accuracy mean which was above 50%

sum(predmodel.train$posterior[,1]>=.5)

sum(predmodel.train$posterior[,1]<.5)

predmodel.test <- predict(ldamode, newdata=test4) #predict on test dataset
table(predmodel.test$class, test4$Covid_deaths4)
mean(predmodel.test$class==test4$Covid_deaths4)

sum(predmodel.test$posterior[,1]>=.5)

sum(predmodel.test$posterior[,1]<.5)
#QDA
qdamode <- qda(factor(Covid_deaths4)~Life.expec + Mortality.rate + Elect.access+ Pop.growth +
                 Health.exp + Birth.rate+ Comp.education, train4)
qdamode
mean(predmodel.train$class==train4$Covid_deaths4)
sum(predmodel.train$posterior[,1]>=.5)

sum(predmodel.train$posterior[,1]<.5)


#Multiclass Logistic Regression
formular <- "Covid_deaths4 ~Life.expec + Mortality.rate + Elect.access+ Pop.growth +
Health.exp + Birth.rate+ Comp.education"
library(nnet)
mod <- multinom(formular, data=WDIproject)
summary(mod)
# Predicting the values for train dataset
train4$ClassPredicted <- predict(mod, newdata = train4, "class")
# Building classification table
tab <- table(train4$Covid_deaths4, train4$ClassPredicted)
# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

# Predicting the class for test dataset
test4$ClassPredicted <- predict(mod, newdata = test4, "class")
# Building classification table
tab4 <- table(test4$Covid_deaths4, test4$ClassPredicted)
tab4

round((sum(diag(tab4))/sum(tab4))*100,2)

# extracting coefficients from the model and exponentiate
exp(coef(mod))

head(probability.table <- fitted(mod))

#Question 5
WDI<- read.csv('C:/Users/user/Desktop/Final Project/project_data.csv', header=T)
WDI
question4<-WDI[,c(-7,-6,-15,-19,-9)]
question4$Covid.deaths<-as.numeric(gsub(",","",question4$Covid.deaths))
question4$Comp.education<-as.numeric(gsub(",","",question4$Comp.education))
library(tidyr)
question4<-drop_na(question4)
question5<-question4
question5

question5$Covid_deaths4 <- ifelse(question5$Covid.deaths <= 200, 0, 3)
question5$Covid_deaths4 <- ifelse(question5$Covid.deaths > 200 & question5$Covid.deaths <= 1000, 1, question5$Covid_deaths4)
question5$Covid_deaths4 <- ifelse(question5$Covid.deaths > 1000 & question5$Covid.deaths <= 2000, 2, question5$Covid_deaths4) 
question5$Covid_deaths4 <- as.factor(question5$Covid_deaths4) 

ggplot(data = question5) +
  geom_bar(mapping = aes(x = Covid.deaths, y = reorder(Continent, Covid.deaths), fill = Covid_deaths4),stat = "identity")+ guides(fill = guide_legend(title = "Continent")) +
  labs(
    x = "Covid Casuality",
    y = "Continent",
    title = "Covid Casualty by Continent",
    caption = "Covid deaths by Continent"
  )
library(ggplot2)
#check correlation
cor(question5$Life.expec, question5$Covid.deaths)

cor(question5$Mortality.rate, question5$Covid.deaths)

cor(question5$Birth.rate, question5$Covid.deaths)

par(mfrow=c(1, 2)) # divide graph area in 2 columns
boxplot(question5$Life.expec, main="Life Expectancy", sub=paste("Outlier rows: ", boxplot.stats(question5$Life.expec)$out))
boxplot(question5$Covid.deaths, main="Covid Deaths", sub=paste("Outlier rows: ", boxplot.stats(question5$Covid.deaths)$out))












                                
                                