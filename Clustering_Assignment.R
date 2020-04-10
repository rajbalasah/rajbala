
#############################################1st Assignment#########################
# Choose file and assign
crime <-read.csv("E://Rajbala//Excelr//Assignment//clustering//crime_data.csv")

#View file and verify required columns
View (crime)

#normalised data as assault can influence output as well as exclude X column

normalisedata <-scale(crime[ ,2:5])
View (normalisedata)

summary(normalisedata)
#scree plot
wss <-(nrow(normalisedata)-1)*sum(apply(normalisedata, 2, var))
for (i in 2:20)wss [i]<- sum (kmeans(normalisedata,centers=i)$withinss)
plot (1:20,wss,type="b",xlab="number of clusters",ylab="Within group")

#form Cluster with euclidean
d <-dist(normalisedata,method="euclidean")
print (d,digit=3)

#Cluster dendrogram with average linkage
groupa <-hclust(d,method="average")
plot(groupa,hang=-1)

#Cluster dendrogram with complete linkage
groupc <-hclust(d,method="complete")
plot(groupc,hang=-1)
#fit <-hclust(d,method="average")

# display dendrogram

haclusta <-cutree(groupa,k=8)
rect.hclust(groupa,k=8,border = "red")

haclustc <-cutree(groupc,k=8)
rect.hclust(groupc,k=8,border = "red")

# Cluster Membership

#Membership
membershipa <-as.matrix(haclusta)
membershipc <-as.matrix(haclustc)
table(haclusta,haclustc)

# Cluster Mean
aggregate(normalisedata,list(haclusta),mean)
aggregate(normalisedata,list(haclustc),mean)

aggregate(crime [ ,2:5],list(haclusta),mean)

# silhoutte Plot
plot (silhouette(cutree(groupa,6),d))

