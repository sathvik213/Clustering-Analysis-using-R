library(tclust)
data("geyser2")
plot(geyser2)

#Hierachisches Clustern
?hclust
h <- hclust(dist(geyser2), method = "single") #ein punkt zu einem anderen, diese kann varrieren und die Distanz anders berechnen
plot(h, cex = 0.6)
cl <- cutree(h, 5)
plot(geyser2, col=cl, pch =cl)

h2 <- hclust(dist(geyser2), method = "complete")
plot(h2, cex = 0.4)
cl <- cutree(h2, 4)
plot(geyser2, col=cl, pch =cl)

h3 <- hclust(dist(geyser2), method = "average")
plot(h3, cex = 0.4)
cl <- cutree(h3, 4)
plot(geyser2, col=cl, pch =cl)

#Partionierende Methoden, zufälliger start
set.seed(123)
cl<- kmeans(geyser2, centers=4, nstart=10)
plot(geyser2, col=cl$cluster, pch = cl$cluster) #schlecht!

library(cluster)
set.seed(123)
cl <- pam(geyser2, k=4)
plot(geyser2, col = cl$cluster, pch =cl$cluster)

#trimmed k-means
library(trimcluster)
set.seed(123)
cl <- trimcluster::trimkmeans(geyser2, k=4, trim = 0.05, runs =10)
plot(geyser2, col = cl$classification, pch = cl$classification)

set.seed(123)
cl <- tclust(geyser2, k=4, alpha=0.01, nstart =10)
plot(geyser2, col=cl$cluster + 1, pch=cl$cluster +1)

library(mclust)
cl <- Mclust(geyser2, G=2:10)
plot(cl, what = "BIC")
mclustModelNames("VII")
#kreisförmige cluster, unterschiedlich gross
#es wurde nicht das kompleyeste modell genommen
#einschränkungen gewahlt Vorteil: weniger parameter schätzen

par(pty="s")
plot(cl, what="classification")
#man sieht kreise
plot(cl, what="uncertainty")
#nicht alle beobachtungen liegen gut im jeweiligen cluster
#das waren alles beispiele anhand von 2dim daten
#wir sahen visuel wie gut die clusterung war
#für höheredimensionale daten (mehr als 3 variablen)
#können wir die clusterguete nicht mehr visuell beurteilen
#na ja etwas schon: mit Tours 
#cluster validity measures

data(Nclus, package = "flexclust")
plot(Nclus)



library(NbClust)
install.packages("NbClust")
stats <- NbClust(geyser2, min.nc = 2, max.nc = 12,
                 method = "kmeans", index ="hartigan")
names(stats)
stats$Best.nc
stats$All.index
plot(2:12,stats$All.index,type="o")
plot(geyser2)



#das Best.nc ist fraglich hier, visuell würde man den knick bei 4 nehmen



stats <- NbClust(data=geyser2, min.nc = 2, max.nc = 12, method ="kmeans", index = "ch")
plot(2:12, stats$All.index, type= "o")
stats$Best.nc



stats <- NbClust(data=geyser2, min.nc = 2, max.nc = 12, method ="kmeans")
stats$All.index
stats$Best.nc




set.seed(123)
cl <- kmeans(geyser2, nstart = 10, centers = 4)
s <- cluster::silhouette(cl$cluster, dist(geyser2))
library(factoextra)
install.packages("factoextra")



fviz_silhouette(s) + scale_color_manual(values = c("black", "red", "green", "blue"))
plot(geyser2, col =gray(1-s[,"sil_width"]), pch=cl$cluster)
#Je heller die Beobachtung im Plot, desto schlechter liegt sie im Cluster. Genauer:
#der durchschnittliche Abstand zum naechstgelegenen Cluster
#ist nicht mehr recht viel grösser als ..... leider zu langsam zum abschreiben

set.seed(123)
cl <- kmeans(iris[, 1:4], 3, nstart =10)
table(cl$cluster, iris$Species)
cl2<- Mclust(iris[, 1:4], G=4)
table(cl$classification, iris$Species)
iris$kmeans <- cl$cluster
iris$Mclust <-cl2$classification

library(fpc)
?cluster.stats
cluster.stats(alt.clustering = as.integer(iris$Species),
              clustering=cl$cluster),
d=dist(iris[, 1:4]))$corrected.rand

#Fuzz clustering
library(e1071)
cl <- cmeans(geyser2, centers =4)
plot(geyser2, col=cl$cluster, pch=cl$cluster)
par(mfrow= c(2,2), mar = c(4, 4, 0.1, 0.1))
for(i in 1:4){
  plot(geyser2, col = gray(1-cl$membership[, i]), pch=cl$cluster)
}
##links oben: membership zu cluster 1, je dunkler desto höhere membership
## rechts oben : membership zu cluster 2, je dunkler desto höher mem....









