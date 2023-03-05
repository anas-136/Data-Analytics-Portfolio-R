library(readxl)
Chipotle <- read_excel("D:/documents/IBA/IMA/Chipotle.xlsx")
View(Chipotle)
str(Chipotle)
summary(Chipotle)

#Data Cleaning#
sum(is.na(Chipotle))
chipotle_data <- na.omit(Chipotle)
View(chipotle_data)
chipotle_data$top1 <- NULL

#To decide the number of clusters we want to analyse we will use the elbow method.
k.max <- 10 #10 is the maximum number of clusters we will be checking
wss <- rep(NA, k.max)
nCluster <- list()
for(i in 1:k.max) {               
  chipotle_elbow <- kmeans(chipotle_data, i)
  wss[i] <- chipotle_elbow$tot.withinss  
  nCluster[[i]] <- chipotle_elbow$size
}
#Scree plot for elbow function
plot(1:k.max,wss,type = "b", xlab = "Number of Clusters K", ylab =  "Total Within Clusters Sum of Squares",
     main = "Number of Clusters Using Elbow Method")

#Number of clusters will be 3

#K-means cluster Analysis
chipotle_cluster_demogrpahic <- kmeans(chipotle_data[c("income", "age", "female")], 3)
chipotle_cluster_demogrpahic

library(ggplot2)

plot(chipotle_data[c("age", "income")], col = chipotle_cluster_demogrpahic$cluster,
     main = "Customer Segmentation")

