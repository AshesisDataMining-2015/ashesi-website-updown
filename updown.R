setwd('C:/Users/user/Documents/Data Mining/Project');
#pingdom <- read.csv('./pingdom-2015-04-01-clean.csv');
numdom <- read.csv('./pingdom.csv', header=TRUE,);
View(numdom$ResponseTime);
length(numdom);
length(numdom$ResponseTime);
# Setting Variables
numRows = length(numdom$ResponseTime);

# Determining Uptime Percentage
upTimes <- numdom$Status;
upTimes
upTime <- sum(upTimes == 1)

upTimePercentage <- (upTime / numRows)*100;
upTimePercentage

#---------------------------------------------------------------------#
# Grouping rows by location
locationGroup<-aggregate(numdom,by=list(numdom$Location),FUN=mean)

groupTime <- locationGroup[,c('Location','ResponseTime')]
barplot(groupTime$ResponseTime, main="Mean Server Response", xlab="Server Locations",names.arg=groupTime$Location)

#--------------------------------------------------------------------#


# Fastest probe location
locationTime <- numdom[,c('Location','ResponseTime')] # Extracting columns
locationTime

# Prepare Data
locationTime <- na.omit(locationTime) # listwise deletion of missing
locationTime <- scale(locationTime) # standardize variables

# Determine number of clusters
wss <- (nrow(locationTime)-1)*sum(apply(locationTime,2,var))
for (i in 2:15) 
  wss[i] <- sum(kmeans(locationTime, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(locationTime, 5) # 5 cluster solution
# get cluster means 
aggregate(locationTime,by=list(fit$cluster),FUN=mean)
# append cluster assignment
locationTime <- data.frame(locationTime, fit$cluster)
library("ggplot2", lib.loc="~/R/win-library/3.1")

qplot(locationGroup$Location, locationGroup$ResponseTime)

ggplot(groupTime)
ggplot(numdom, aes(groupTime, fill = groupTime ) ) + geom_bar()
summary(numdom)


#Geolocating maps
