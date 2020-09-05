install.packages("data.table")
library(data.table)

airlines<-read.csv(file.choose())
View(airlines)
str(airlines)


ncol(airlines)
colnames(airlines)

nrow(airlines)

summary(airlines)


############## Hclustering Packages##############

install.packages("tidyverse")
library(tidyverse)# data manipulation

install.packages("clusters")
library(cluster)# clustering algorithms

install.packages("factoextra")
library(factoextra) # clustering visualization

install.packages("dendextend")
library(dendextend) # for comparing two dendrograms


############# Data preparation ##########

#airlines_1<-na.omit(airlines)
#?na.omit

#A<-scale(airlines_1)
#head(A)

sub_airlines<-airlines[,2:12]
view(sub_airlines)
colnames(sub_airlines)

norm_airlines<-scale(sub_airlines)

######### Hirerachical Clustering #############


distance_airlines<-dist(norm_airlines,method="euclidean")

str(distance_airlines)

clust_airlines<-hclust(distance_airlines,method="complete")

plot(clust_airlines,cex=0.6,hang = -1)

#********** alternative way ********#

HC1<-agnes(norm_airlines,method = "complete")
HC1$ac# Agglomerative coefficient, which measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure).
#[1] 0.984681

#****************** methods to assess**************#
#m <- c( "average", "single", "complete", "ward")
#names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
#ac <- function(x) {
#  agnes(norm_airlines, method = x)$ac
#}

#map_dbl(m, ac)
#?map_dbl

HC2 <- agnes(norm_airlines, method = "ward")
pltree(HC2, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

######****************** compute divisive hierarchical clustering ***************#####
HC3<-diana(norm_airlines)#allows us to perform divisive hierarchical clustering. 
#diana is a function frm package "Clusters"

# Divise coefficient; amount of clustering structure found
HC3$dc######[1] 0.9823148,nearly equal to 1


# plot dendrogram
pltree(HC3, cex = 0.6, hang = -1, main = "Dendrogram of diana")


# Ward's method
HC4 <- hclust(distance_airlines, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(HC4, k = 4)


# Number of members in each cluster
table(sub_grp) #sub_grp
#   1    2    3    4 
#2446 1380   43  130 


#airlines
#mutate(cluster=sub_grp)

plot(HC4, cex = 1)
rect.hclust(HC4, k = 4, border = 2:5)




### kind of Scatter plot Visualization   #####

fviz_cluster(list(data = airlines, cluster = sub_grp))  
?fviz_cluster
#Cut tree is done in 4 groups or clusters for better visualization we can also do into more clusters

#performing for cluster of 10,k=10

#sub_grp2<-cutree(HC4,k=10)
#fviz_cluster(list(data=airlines,cluster=sub_grp2))

# Compute distance matrix
res.dist <- dist(airlines, method = "euclidean")

### Compute 2 hierarchical clusterings side by side ###

#HC1 <- hclust(res.dist, method = "complete")
#HC2 <- hclust(res.dist, method = "ward.D2")

# Create and compare two dendrograms,side by side
#dend1 <- as.dendrogram (HC1)
#dend2 <- as.dendrogram (HC2)

#tanglegram(dend1, dend2)


############## determining optimal clusters ###########

fviz_nbclust(airlines,FUN=hcut,method = "wss")

############### Average Silhouette Method ############
fviz_nbclust(airlines,FUN=hcut,method = "silhouette")

###### Gap Statistic Method #########
#gap_stat <- clusGap(airlines, FUN = hcut, nstart = 25, K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)



##########********* kmeans clustering ***********###########

airlines_kmeans<-kmeans(norm_airlines,5)
str(airlines_kmeans)

airlines_kmeans$centers

airlines_new1<-cbind(airlines,sub_grp)
airlines_new2<-cbind(airlines_new1,airlines_kmeans$cluster)
colnames(airlines_new2)


#aggregate(airlines_new2[,2:12],by= list(airlines_new2$airline_kmeans$cluster), FUN = mean)


# Using Clara function(Clustering for Large Applications) to find cluster

X1 <- clara(norm_airlines,5) #Using Centroid

?clara#Computes a "clara" object, a list representing a clustering of the data into k clusters.

clusplot(X1)


#using Partition Arround Medoids to find cluster

X2 <- pam(norm_airlines,5) # Using Medoids

?pam#Partitioning (clustering) of the data into k clusters "around medoids", a more robust version of K-means.

clusplot(X2)
