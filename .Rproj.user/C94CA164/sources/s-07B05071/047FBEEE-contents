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

##############################
# Data Reading and Loading   #
##############################

#reading data of .RDS format
data=readRDS('chorizondata.rds')

#To display sampled data
head(data)

#To display whole data in table format
View(data)

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
not_needed_columns_temporary=c('ID','XCOO','YCOO')

#So we remove them for plotting
work_data=data%>%select(-not_needed_columns_temporary)

work_data_long <- melt(work_data, id.vars = NULL)


# create a faceted histogram or density plot
plot <- ggplot(work_data_long, aes(x = value)) +
  geom_histogram() +   # or geom_density()
  facet_wrap(~ variable, scales = "free")

# print the plot to the console
print(plot)
#Ba,Zn,Cd,Rb,S,Sb,Sc,Si,C have fat distribution when compared to other elements

ggsave("all_distributions.png", plot = plot,limitsize = FALSE, width = 10, height = 8, dpi = 300, units = "in")
#To save png file to local storage

#Generally it is not compulsory for data to be normally distributed for clustering but skewed data to symmetric data is beneficial.
#If the original data follows a log-normal distribution or approximately so, then the log-transformed data follows a normal or near normal distribution.
#So lets apply log transformation to all variables of work_data because some features have skewed distributions
work_data_log=work_data|>mutate_all(log)

work_data_log_long <- melt(work_data_log, id.vars = NULL)

# create a faceted histogram or density plot
plot <- ggplot(work_data_log_long, aes(x = value)) +
  geom_histogram() +   # or geom_density()
  facet_wrap(~ variable, scales = "free")

# print the plot to the console
print(plot)
#Now we can observe all the feature distributions resemble bell shape distribution aka normal distribution

ggsave("all_distributions_with_log_transform.png", plot = plot,limitsize = FALSE, width = 10, height = 8, dpi = 300, units = "in")
#To save png file to local storage

#scaling 
work_data_log=scale(work_data_log)


##########################################################################################################################################################################
# K means clustering

kmeans(work_data,centers=4,iter.max = 100,nstart=100)
#vis
fviz_nbclust(work_data,kmeans,method='wss')
fviz_nbclust(work_data,kmeans,method='silhouette')
fviz_nbclust(work_data,kmeans,method='gap_stat')

#cluster biplot
fviz_cluster(kmeans(work_data,centers=5,iter.max = 100,nstart=100),data=work_data)

clusters=kmeans(work_data,centers=4,iter.max = 100,nstart=100)
data=data|>mutate(cluster=clusters$cluster)
data|>ggplot(aes(x=Ag,y=H,col=as.factor(cluster)))+geom_point()
#####################################################################################
#  Hierarchial Clustering

#distance
work_data.dist=dist(work_data)
work_data.dist

#hierarchial clustering
hc.out=hclust(work_data.dist,method = 'complete')
hc.out
png("myplot.png", width=80, height=60, units="in", res=300)

plot(hc.out)

rect.hclust(hc.out,k=6,border=2:5)
##########################################################################################################################################################################

library(ggmap)
library(tidyverse)
library(leaflet)
world_map=map_data('world')
leaflet()|>addTiles()|>addMarkers(lng = data$YCOO/10000.0,lat = data$XCOO/10000.0)
new_data=readRDS('chorizondata.rds')
new_data$XCOO=new_data$XCOO/10000.0
new_data$YCOO=new_data$YCOO/10000.0
View(new_data)
# map <- get_stamenmap(c(left = min(new_data$XCOO), bottom = min(new_data$YCOO), right = max(new_data$XCOO), top = max(new_data$YCOO)), zoom = 4, maptype = "toner-lite")
# ggplot(new_data, aes(x = XCOO, y = YCOO)) +
#   geom_point() +
#   ggmap(map)

register_google(key = "AIzaSyA_jzi9ROqFIiHbxnuRlS7zStntOAqLMTA")
map <- get_googlemap(center = "New York City", zoom = 10)

# Create a ggmap object
ggmap(map)

# Create a ggplot object
ggplot(new_data, aes(x = XCOO, y = YCOO)) +
  geom_point()

# Combine the two using the `annotation_raster` function
ggmap(map) +
  annotation_raster(map, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point(data=new_data, aes(x=XCOO, y=YCOO))
##########################################################################################################################################################################

