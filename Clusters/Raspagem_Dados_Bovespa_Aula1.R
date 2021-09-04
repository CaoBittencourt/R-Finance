# https://xsliulab.github.io/Workshop/week10/r-cluster-book.pdf
# http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

#### estatísticas, em prisões por 100.000 residentes por agressão, 
# assassinato e estupro em cada um dos 50 estados dos EUA 
# em 1973.
# Load data
data(USArrests)
# Compute distances and hierarchical clustering
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(hc)
# Default plot
plot(hcd, type = "rectangle", ylab = "Height")


install.packages("ape")
??ape
library("ape")


# Cut the dendrogram into 4 clusters
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)




# https://rpubs.com/Kassio_Ferreira/kmeans
#  K-means implementado por Kássio Ferreira e João Eudes
#  INPUT: uma MATRIZ de dados, em que os objetos estão nas linhas e as varíaveis nas colunas.
#  OUTPUT: uma lista com os k grupos e a classificação de cada objeto.
#

kMeans1 <- function(dados, k=2){
  
  # função que calcula a distância euclidiana
  euc.dist <- function(x1, x2) sum((x1 - x2) ^ 2)
  
  # labels
  rotulo = 1:k
  rownames(dados)[nrow(dados)]= 1
  
  # random centroids
  for(i in 1:nrow(dados)){
    rownames(dados)[i] <- sample(rotulo,1)
  }
  
  centroids <- colMeans(dados[rownames(dados) == 1, ])
  
  for(j in 2:k){
    centroids <- rbind(centroids, colMeans(dados[rownames(dados) == j, ]))
  }
  
  rownames(centroids) = 1:k #  identifica o centroide de cada grupo
  
  for(i in 1:nrow(dados)){
    distancias = NULL
    for(j in 1:k){
      distancias[j] = euc.dist(dados[i,], centroids[j,])
    }
    names(distancias) = 1:k
    #   print(distancias)
    rownames(dados)[i] = as.numeric(names(distancias[distancias == min(distancias)]))
    
    # recalcula as medias
    
    centroids <- colMeans(dados[rownames(dados) == 1, ])
    
    for(z in 2:k){
      centroids <- rbind(centroids, colMeans(dados[rownames(dados) == z, ]))
    }
    
    
  }
  #  centroids
  
  return(list(centroides = centroids, grupo1 = dados[rownames(dados) == 1, ],
              grupo2 = dados[rownames(dados) == 2, ],
              grupo3 = dados[rownames(dados) == 3, ],
              clusters = as.numeric(rownames(dados))))
}

dados = as.matrix(iris[ ,1:4])
head(dados)
exemplo = kMeans1(dados,k=4)
# exemplo$grupo1
# exemplo$grupo2
# exemplo$grupo3
plot(dados,col = exemplo$clusters+1,  main="K-Means result with 4 clusters", pch=20, cex=2)



### R base
### https://feliperego.github.io/blog/2015/07/14/K-Means-Clustering-With-R
?kmeans

# Load necessary libraries
library(datasets)

# Inspect data structure
str(attitude)

# Subset the attitude data
dat = attitude[,c(3,4)]

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(dat, 2, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1), 
     main="K-Means result with 2 clusters", pch=20, cex=2)

# Check for the optimal number of clusters given the data

mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

### But from the example above, we can say that after 6 clusters the observed

# Perform K-Means with the optimal number of clusters identified from the Elbow method
?set.seed 
# the default random seed is 626 integers, so only print a few runif(1);
set.seed(7)
km2 = kmeans(dat, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
plot(dat, col =(km2$cluster +1) ,
     main="K-Means result with 6 clusters", pch=20, cex=2)