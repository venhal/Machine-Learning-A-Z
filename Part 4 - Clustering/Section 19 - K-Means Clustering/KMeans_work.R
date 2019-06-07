# K-means clustering

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
# i means the number of clusters
for (i in 1:10) wcss[i] = kmeans(dataset, i)$tot.withinss
# visualizing
plot(x = 1:10, 
     y = wcss, 
     type = 'b', 
     main = paste('The Elbow Method'), 
     xlab = 'Number of clusters', 
     ylab = 'wcss')

# fitting the k-means to the dataset
kmeans = kmeans(dataset, 5, iter.max = 300, nstart = 10)
y_kmeans = kmeans$cluster

#visualizing the cluster
#install.packages('cluster')
library(cluster)
clusplot(dataset, 
         y_kmeans,
         lines = 0,
         shade = TRUE, 
         color = TRUE, 
         labels = 2, 
         plotchar = TRUE,
         span = TRUE,
         main = paste('Cluster Of Customers'), 
         xlab = 'Annual Income', 
         ylab = 'Spending Score')