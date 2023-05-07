#################################
# Loading Necessary Libraries   #   
#################################

library(VIM)#---------------------------------------------------to find out missing values
library(ggplot2)#-----------------------------------------------to plot graphs
library(lubridate)
library(dplyr)
library(stats)
library(ggfortify)
library(magrittr)
library(factoextra)
library(tidyr)
library(reshape2)
library(rrcov)
library(cluster)

##############################
# Data Reading and Loading   #
##############################

#reading data of .RDS format
data=readRDS('chorizondata.rds')

#To display sampled data
head(data)

#To display whole data in table format
View(data)

#To see structure of data
print(str(data))
#All are numeric variables

#Dimensions of the data
print(dim(data))
# [1] 617  44

###################
# Data Analysis   #
###################

#Checking for missing values using VIM library function 'aggr'
a=aggr(data)

summary(a)
# Missings per variable: 
#   Variable Count
# ID     0
# XCOO     0
# YCOO     0
# Ag     0
# Al     0
# As     0
# B     0
# Ba     0
# Be     0
# Bi     0
# Ca     0
# Cd     0
# Co     0
# Cr     0
# Cu     0
# Fe     0
# Hg     0
# K     0
# La     0
# Mg     0
# Mn     0
# Mo     0
# Na     0
# Ni     0
# P     0
# Pb     0
# Rb     0
# S     0
# Sb     0
# Sc     0
# Si     0
# Sr     0
# Th     0
# Tl     0
# U     0
# V     0
# Y     0
# Zn     0
# C     0
# H     0
# N     0
# LOI     0
# pH     0
# Cond     0
# cluster     0
#As we can see that no feature or column of dataset has any single missing value in them




#Checking for Duplicated Data
flag=0
dup=duplicated(data)
for (i in 1:length(dup)) {
  if (dup[i]==TRUE){
    
    print(paste('There is a duplicate at ',i))
  }
  
}
if (flag==0){
  print('There are literally no duplicated values')
}
# [1] "THere are literally no duplicated values"
#This indicates no values are duplicated

########################
# Data Visualisation   #
########################

#Plotting Distributions of Required Features 

#We do not require below columns because they do not act as support numeric vectors for distribution plotting
not_needed_columns_temporary=c('ID','XCOO','YCOO','Ag','Al','B','Be','Bi','Co','K','Mo','Na','Cu','Fe')

#So we remove them for plotting
work_data=data%>%select(-not_needed_columns_temporary)



print(colnames(work_data))
# [1] "As"   "Ba"   "Ca"   "Cd"   "Cr"   "Hg"   "La"   "Mg"   "Mn"   "Ni"   "P"    "Pb"   "Rb"   "S"   
# [15] "Sb"   "Sc"   "Si"   "Sr"   "Th"   "Tl"   "U"    "V"    "Y"    "Zn"   "C"    "H"    "N"    "LOI" 
# [29] "pH"   "Cond"
work_data_long <- melt(work_data, id.vars = NULL)


# create a faceted histogram or density plot
plot <- ggplot(work_data_long, aes(x = value)) +
  geom_histogram() +   # or geom_density()
  facet_wrap(~ variable, scales = "free")

# print the plot to the console
print(plot)

ggsave("feature_distributions.png", plot = plot,limitsize = FALSE, width = 10, height = 8, dpi = 300, units = "in")
#To save png file to local storage

#Outlier detection for clustering
cov.rob <- covMcd(work_data)
mah.dist <- mahalanobis(work_data, cov.rob$center, cov.rob$cov)

# Identify outliers based on threshold
threshold <- qchisq(0.95, df = ncol(work_data))
outliers <- which(mah.dist > threshold)

# Print number of outliers
cat("Number of outliers:", length(outliers), "\n")
#Number of outliers: 290 

# It is common actually to find outliers in real life elemental distribution dataset.So log transformation needs to be applied to converted the distributions to a near normal one
# Generally it is not compulsory for data to be normally distributed for clustering but skewed data to symmetric data is beneficial.
# If the original data follows a log-normal distribution or approximately so, then the log-transformed data follows a normal or near normal distribution.
# So lets apply log transformation to all variables of work_data because some features have skewed distributions
work_data_log=work_data|>mutate_all(log)

work_data_log_long <- melt(work_data_log, id.vars = NULL)

# create a faceted histogram or density plot
plot <- ggplot(work_data_log_long, aes(x = value)) +
  geom_histogram() +   # or geom_density()
  facet_wrap(~ variable, scales = "free")

# print the plot to the console
print(plot)
#Now we can observe all the feature distributions resemble bell shape distribution aka normal distribution

ggsave("feature_distributions_with_log_transform.png", plot = plot,limitsize = FALSE, width = 10, height = 8, dpi = 300, units = "in")
#To save png file to local storage

#scaling 
# work_data_log=scale(work_data_log)



########################################
###      CLUSTERING PROCEDURE        ###
########################################




####################################
#    Hierarchial clustering        #
####################################

# So for distance based heirarchial(here) we need 2 sequencial ways :
# 1-->To measure distances between rows(records ) and clusters
# 2-->An algorithm to use these distances

#Finding the distance.(but the output.txt is too big as it contains distances between all the rows)
#Calculating euclidean distances to know how similar(near) or not similar(far) from eachother
#after apply manual transformation
means=apply(work_data_log,2,mean)
sds=apply(work_data_log, 2,sd)
nor=scale(work_data_log,center = means,scale = sds)

#Calculating euclidean distances to know how similar(near) or not similar(far) from eachother
distance=dist(nor)
sink("output.txt")  # Open a connection to a file called output.txt
options(max.print = 20000000)
print(distance)
sink()  # Close the connection and restore output to the console

#Hierarchial agglomerative clustering
clust_data=hclust(distance)
plot(clust_data,hang=-1)
rect.hclust(clust_data,k=3)#Visual representation of three clusters

#Cluster population
count_of_each_cluster_corresponding=cutree(clust_data,3)# 3 indicates 3 clusters,it can be changed
table(count_of_each_cluster_corresponding)
# count_of_each_cluster_corresponding
# 1    2    3 
# 539  75   3 

aggregate(nor,list(count_of_each_cluster_corresponding),mean)
#Helps in understanding more contribution of feature to 
#cluster by taking difference of max and min values of each feature



####################################
#   Non Hierarchial clustering     #
####################################

# So for distance based clustering/Non heirarchial(here) we need 3 sequencial ways :
# 1-->To measure distances between rows(records ) and clusters
# 2-->An algorithm to use these distances
# 3-->To find Optimal Number of clusters (for Non hierarchial)

####################################
#        K means clustering        #
####################################

#Scaling the data
scaled_work_data_log=scale(work_data_log)

#Scree plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(scaled_work_data_log)
# cluster count 2 looks like an optimal cluster count using elbow method

#Applying kmeans
KM=kmeans(scaled_work_data_log,centers=4,iter.max = 100,nstart=100)
#vis
fviz_nbclust(scaled_work_data_log,kmeans,method='wss')
fviz_nbclust(scaled_work_data_log,kmeans,method='silhouette')
fviz_nbclust(scaled_work_data_log,kmeans,method='gap_stat')

#cluster biplot
fviz_cluster(kmeans(work_data_log,centers=4,iter.max = 100,nstart=100),data=work_data_log)
#Cluster with scaled data
fviz_cluster(kmeans(scaled_work_data_log,centers=4,iter.max = 100,nstart=100),data=scaled_work_data_log)
#NOTE : if centers=2, we can  see 2 clusters play visually well to separate without overlapping ,the centers paramter can be changed

clusplot(scaled_work_data_log,KM$cluster)

####################################
#     clustering Validation        #
####################################
# In order to obtain meaningful clusters 
# 1-->Understand cluster summary statistics
# 2-->Shouldn't include categorical variables
# 3-->Considering clustering variation based on population remaining consistent for
# each cluster
# E.g:For less number of clusters the perfect consistency should be maintained where 
# for a bit more cluster count,
# slighter population inconsistencies might not affect at large scale

#All the above methods are considered at most possible cases

# we are preferring hierarchical methods here so that data itself dissolves into optimal
# cluster count after algorithm application.Because we in hand might not know perfect 
# cluster count for this data and there is not even any kind of constraint 
#that is enabling us to calculate them at before hand.