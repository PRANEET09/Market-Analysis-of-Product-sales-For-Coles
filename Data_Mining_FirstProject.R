#make sure all this three packages are installed for our analysis
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
#install.packages("arules")

setwd("C:/Users/Praneet Shetty/Desktop/Ebooks/Notes/Semester 2/Data Mining/Market basket -Project")
coles=read.csv("Simulated Coles Data.csv",header = TRUE)

#Exploring the Data
dim(coles)
names(coles)
str(coles)

summary(coles) #there are 1165 missing found in majority variable
#Missing Values

library(mice)
md.pattern(coles)

#wee see large 1165 values  of NA in present in every variables Dataset
#sum(which(is.na(coles$ReceiptID)))
coles=slice(coles,1:58100)
summary(coles)

###Analysing every column
###receiptID
#looking for Duplicate Values
sum(which(!is.na(coles$ReceiptID[duplicated(coles)]))) #no duplicate values 


#sex
table(coles$sex)
sum(is.na(coles$sex))
coles$sex=factor(coles$sex)
coles$sex=factor(coles$sex,levels = c(1,2),labels = c("Male","Female"))
ggplot(coles)+geom_bar(aes(sex))+xlab("Sex")+ggtitle("Distribution of Customers Gender")

#on looking at the dataset we see that values for age,homeown,pmethod
#values rows 58000 are inputed in sequential order ,so treating as missing values

#nchildren
table(coles$nchildren)
sum(is.na(coles$nchildren))
e=which(coles$ReceiptID>=658001)
coles$nchildren[e]=NA
sum(is.na(coles$nchildren))
summary(coles$nchildren)
index=which(is.na(coles$nchildren))
coles$nchildren[index]=mean(coles$nchildren,na.rm =TRUE)
#rounding off,since continous variable we not deleting missing values
coles$nchildren=round(coles$nchildren)
table(coles$nchildren)
str(coles$nchildren)

#pmethod
table(coles$pmethod)
coles$pmethod=factor(coles$pmethod)
coles$pmethod=factor(coles$pmethod,levels = c(1,2,3,4),labels = c("Cash","Credit Card","Eftpos","other"))
#all the errors were converted to Missing values
table(coles$pmethod)
ggplot(coles)+geom_bar(aes(pmethod,fill=pmethod))+xlab("Payment Method")+ggtitle("Payment Method Distributions")
sum(is.na(coles$pmethod))

#homeown
table(coles$homeown)
sum(is.na(coles$homeown))
coles$homeown=factor(coles$homeown)
coles$homeown=factor(coles$homeown,levels = c(1,2,3),labels = c("Yes","No","Other"))
table(coles$homeown)
sum(is.na(coles$homeown))
ggplot(coles)+geom_bar(aes(homeown,fill=homeown))+xlab("Hpme Own")+ggtitle("Distributions of Customers owning a home or not")

str(coles)


#Value
str(coles)
sum(is.na(coles$Value))
summary(coles$Value)
#displaying the Boxplot
boxplot(coles$Value)
title("boxplot of Value")#before extreme removal
#3 extremes found,so replacing it with mean
coles$Value[coles$Value>500]=mean(coles$Value)
boxplot(coles$Value) #After extreme removal
title("boxplot of Value")
coles$Value=round(coles$Value)

#age
str(coles)
sum(is.na(coles$age))
coles$age=round(coles$age,0)
ggplot(coles)+geom_histogram(aes(age))+ggtitle("Distribution of the age")
summary(coles$age)

#income
str(coles)
sum(is.na(coles$income))
ggplot(coles)+geom_freqpoly(aes(income))+xlim(c(0,100000))+ggtitle("Frequency plot of the Income ")
boxplot(coles$income)
title("Box plot of the income")
coles$income[coles$income>600000]=mean(coles$income)
summary(coles$income)

#postcode
summary(coles$PostCode)
table(coles$PostCode)

#Since 9792 missing value  we not selecting it for further analysis
summary(coles)

#For Food items Variables
coles_purchase=select(coles,-(Value:nchildren))
x=names(coles_purchase)
coles[x]=lapply(coles[x], factor)
str(coles)

###Thus wee see that Columns Fruit and Fruit juice have extra levels.

x=levels(coles_purchase$fruit)
coles[] = lapply(coles, function(x) {
  levels(x)[levels(x) %in% c("11","2","3","4","6","7")] <- "1"
  levels(x)[levels(x) %in% c("", "o")] <- "0"
  x
})
#All the extra values are replaced as 1 and values like "o" and null as 0
str(coles)
#Deleting all the missing values
coles=na.omit(coles)  #a,, missing values are deleted
summary(coles)

###Clustering
#Since Custering only works on continous variables
str(coles)
##We will be selecting age,value,income,nchildren for clustering 
#exploring this 4 variables

#We can see 2 outliers
plot(coles$income,coles$age)

summary(coles$income)
boxplot(coles$income)
title("Boxplot of Income")
#manyvalues are outside whiskers which can be treated as outliers
#plot(outVals)

ggplot(coles)+geom_point(aes(x=income,y=Value,alpha=age))+ggtitle("Relationship between Value ,Income and Age")


mean(coles$Value)
summary(coles$Value)
boxplot(coles$Value)
title("Box plot of Value")
coles=na.omit(coles)

ggplot(coles)+geom_point(aes(x=Value,y=age))+ggtitle("Relationship between Age and value")
ggplot(coles)+geom_point(aes(x=income,y=age))+ggtitle("Relationship between Age and Income")
o=filter(coles,age<18&nchildren>0)

summary(coles$Value)
#making the Dataset free from outliers

ggplot(coles)+geom_point(aes(x=Value,y=income))+ggtitle("Relationship between Income and value")
x <- coles$Value
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
coles$Value[coles$Value < (qnt[1] - H)] =caps[1]
coles$Value[coles$Value > (qnt[2] + H)]= caps[2]


y <- coles$income
qnt <- quantile(y, probs=c(.25, .75), na.rm = T)
caps <- quantile(y, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(y, na.rm = T)
coles$income[coles$income < (qnt[1] - H)] =caps[1]
coles$income[coles$income >(qnt[2] + H)]= caps[2]


z=coles$age
qnt <- quantile(z, probs=c(.25, .75), na.rm = T)
caps <- quantile(z, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(z, na.rm = T)
coles$age[coles$age < (qnt[1] - H)] =caps[1]
coles$age[coles$age >(qnt[2] + H)]= caps[2]
caps[2]


boxplot(coles$Value)
title("Boxplot of Value after handling outliers")
summary(coles$income)
boxplot(coles$income)
title("Boxplot of income after handling outliers")

boxplot(coles$age)
title("Boxplot of age after outlier handling")

ggplot(coles)+geom_point(aes(x=income,y=Value))+ggtitle("Relationship between income and value after outlier removal")


ggplot(coles)+geom_point(aes(x=Value,y=age))
ggplot(coles)+geom_point(aes(x=income,y=age))
#ggplot(coles)+geom_point(aes(x=income,y=Value,alpha=age))

#Doing the cluster
coles_cluster=select(coles,Value,income,age)
#Checkong number of clusters
wss=(nrow(coles_cluster)-1)*sum(apply(coles_cluster [,c(1:3)],2, var))#calculating within-group sum of squares
#wss=NULL
for (i in 2:8) 
  wss[i]=sum(kmeans(coles_cluster [,c(1:3)],centers=i)$withinss)#creating a vector called "wss" for 2-4 clusters solution
par(bg="light yellow")
plot(1:8,wss,type="b",xlab="Number of clusters",ylab="Within-group sum of squares",col="blue")#plotting number of clusters against within-group sum of squares

###Running Kmeans
ClusterA=kmeans(coles_cluster[],6)

##AD Kmeans cluster solution to the dataset
coles_cluster$Cluster=as.factor(ClusterA$cluster)
table(coles_cluster$Cluster)


#Adding the cluster to the main Data
coles$cluster=as.factor(ClusterA$cluster)

#looking ho the cluster breaks  thdata into several clusters
###Now that 6 cluster have been created ,break the df according to clusters
cluster1=(filter(coles,cluster==1))
cluster2=(filter(coles,cluster==2))
cluster3=(filter(coles,cluster==3))
cluster4=(filter(coles,cluster==4))
cluster5=(filter(coles,cluster==5))
cluster6=(filter(coles,cluster==6))

17496/57987

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)
summary(cluster5)
summary(cluster6)
#Storing the Clusters for further analysis in market basket
write.csv(cluster1, file = "cluster1.csv")
write.csv(cluster2, file = "cluster2.csv")
write.csv(cluster3, file = "cluster3.csv")
write.csv(cluster4, file = "cluster4.csv")
write.csv(cluster5, file = "cluster5.csv")

write.csv(cluster6, file = "cluster6.csv")

write.csv(coles, file = "coles.csv")

#nt(aes(x=income,y=Value,color = age))+ggtitle("Relationship between Income and value")

################################END##################
    

