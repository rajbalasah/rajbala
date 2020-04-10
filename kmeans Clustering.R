####################################################2nd Assignment####################################
#Choose file and assign
library(readxl)
library(openxlsx)
library(plyr)
airlines <-read.xlsx("E://Rajbala//Excelr//Assignment//clustering//EastWestAirlines.xlsx",sheet=2)

# View file and verify columns
View (airlines)
colnames (airlines)
str(airlines)

# Removing Id and Awards column
normalised <- scale(airlines[ ,2:11])
View(normalised)

#verify scree plot
windows()
wss1 <-(nrow(normalised)-1)*sum(apply(normalised, 2, var))
for (i in 2:20)wss1 [i]<- sum (kmeans(normalised,centers=i)$withinss)
plot (1:20,wss1,type="b",xlab="Number of clusters",ylab="Within group")

#verify distance
d1 <- dist(normalised,method="euclidean")


#Cluster dendrogram with average linkage
windows()
groupa <-hclust(d1,method="average")
plot(groupa,hang=-1)

#Cluster dendrogram with complete linkage
groupc <-hclust(d1,method="complete")
plot(groupc,hang=-1)

#########K MEANS##########
k=kmeans(normalised,7)
install.packages("animation")
library(animation)
window()
km <-kmeans.ani(normalised,7)
# line chart
wss <-c()
for (i in 2:15) 
{
  wss[i] <-sum (kmeans(normalised,centers = i )$withinss) 
}
plot (1:15,wss,type="b",xlab="no of clusters",ylab="avg distance")

km$cluster
table(km$cluster)
?table
cluster=data.frame("Airline"=normalised[,1],"cluster"=km$cluster)

library(dplyr)
c1=cluster%>%dplyr::filter(cluster==1)
View(c1)


