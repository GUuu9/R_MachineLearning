install.packages("igraph")

# Shortest path
library(igraph)
NW1 <- matrix(c(0,10,20,20,0,0,0,
                0,0,5,15,35,0,0,
                0,0,0,0,35,0,30,
                0,0,0,0,20,15,0,
                0,0,0,0,0,0,15,
                0,0,0,0,10,0,20,
                0,0,0,0,0,0,0),
              byrow=T, ncol=7)
rownames(NW1) <- colnames(NW1) <- c("S",1:5,"T")

NW1 <- matrix(c(0,0,0,0,0,0,0,1,0,0,0,
                0,0,0,2,1,0,0,0,0,0,2,
                0,0,0,0,0,0,1,0,0,0,2,
                0,2,0,0,1,0,1,0,0,0,0,
                0,1,0,1,0,1,0,1,0,0,0,
                0,0,0,0,1,0,0,0,0,0,0,
                0,0,1,2,0,0,0,1,0,0,0,
                1,0,0,0,1,0,1,0,1,0,0,
                0,0,0,0,0,0,0,1,0,1,0,
                0,0,0,0,0,0,0,0,1,0,0,
                0,2,2,0,0,0,0,0,0,0,0),
              byrow=T, ncol=11)
rownames(NW1) <- colnames(NW1) <- c("S",1:9,"T")

ig <- graph.adjacency(NW1, mode="directed", weighted=TRUE)
plot(ig, edge.label=E(ig)$weight)

ShortPth <- get.shortest.paths(ig, "S", "T", mode="all", output="both")
ecol <- rep("gray80", ecount(ig))
ecol[unlist(ShortPth$epath)] <- "orange"
ew <- rep(2, ecount(ig))
ew[unlist(ShortPth$epath)] <- 4
vcol <- rep("gray40", vcount(ig))
vcol[unlist(ShortPth$vpath)] <- "gold"
plot(ig, vertex.color=vcol, edge.color=ecol, edge.width=ew, edge.arrow.mode=0)
shortest.paths(ig,"S")

# Shortest path
library(igraph)
NW1 <- matrix(c(
  0, 120, 90, 0, 0, 0, 0,0,
  120,0,0,60,0,0,0,0,
  90,0,0,0,0,60,0,0,
  0,60,0,0,60,0,60,0,
  0,0,0,60,0,90,0,0,
  0,0,60,0,90,0,120,0,
  0,0,0,60,0,120,0,90,
  0,0,0,0,0,0,90,0
  
                
),
byrow=T, ncol=8)
rownames(NW1) <- colnames(NW1) <- c("T",1:6,"S")

ig <- graph.adjacency(NW1, mode="directed", weighted=TRUE)
plot(ig, edge.label=E(ig)$weight)

ShortPth <- get.shortest.paths(ig, "S", "T", mode="all", output="both")

ecol <- rep("gray80", ecount(ig))
ecol[unlist(ShortPth$epath)] <- "orange"
ew <- rep(2, ecount(ig))
ew[unlist(ShortPth$epath)] <- 4
vcol <- rep("gray40", vcount(ig))
vcol[unlist(ShortPth$vpath)] <- "gold"
plot(ig, vertex.color=vcol, edge.color=ecol, edge.width=ew, edge.arrow.mode=0)
shortest.paths(ig,"S")



# Minimal spanning tree
library(igraph)
nw.mst <- matrix(c(0,5,3,0,6,5,
                   0,0,5,8,3,7, 
                   0,0,0,7,4,7, 
                   0,0,0,0,6,0, 
                   0,0,0,0,0,8, 
                   0,0,0,0,0,0),
                 byrow=T, ncol=6)
nw.mst <- nw.mst + t(nw.mst)
colnames(nw.mst) <- rownames(nw.mst) <- as.character(1:6)
g <- graph.adjacency(nw.mst, mode="undirected", weighted=TRUE)
plot(g, edge.label=E(g)$weight)
mst.g <- mst(g)
plot(mst.g, edge.label=E(mst.g)$weight)
sum(E(mst.g)$weight) # 길이

# TSP
#install.packages("TSP")
library(TSP)
nw.tsp <- matrix(c(0,5,3,Inf,6,5,
                   0,0,5,8,3,7, 
                   0,0,0,7,4,7, 
                   0,0,0,0,6,Inf,
                   0,0,0,0,0,8,
                   0,0,0,0,0,0),
                 byrow=T, ncol=6)
nw.tsp <- nw.tsp + t(nw.tsp)
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(1:6)
tsp <- TSP(nw.tsp) 
n_of_cities(tsp) 
labels(tsp)
tour <- solve_TSP(tsp) 
names(tour)
tour # 길이


library(TSP)
nw.tsp <- matrix(c(0,	1,	3,	5,	8,	8,	7,	6,	3,	1,	3,	6,	12,	11,	3,	11,	9,	10,	10,	18,	16,	2,
                   0,	0,	1,	4,	7,	8,	7,	6,	3,	2,	4,	7,	13,	12,	12,	12,	10,	11,	12,	20,	17,	2,
                   0,	0,	0,	5,	7,	9,	8,	7,	4,	3,	5,	8,	15,	13,	13,	14,	12,	13,	13,	21,	19,	2,
                   0,	0,	0,	0,	2,	10,	10,	9,	6,	5,	8,	11,	17,	16,	16,	16,	14,	15,	15,	23,	21,	5,
                   0,	0,	0,	0,	0,	10,	11,	11,	8,	8,	10,	13,	19,	18,	18,	18,	16,	17,	18,	25,	23,	7,
                   0,	0,	0,	0,	0,	0,	2,	3,	5,	7,	10,	13,	19,	18,	9,	18,	16,	17,	18,	26,	23,	6,
                   0,	0,	0,	0,	0,	0,	0,	2,	5,	6,	9,	12,	18,	17,	8,	17,	16,	17,	17,	25,	22,	6,
                   0,	0,	0,	0,	0,	0,	0,	0,	3,	5,	8,	11,	17,	16,	6,	16,	14,	15,	15,	23,	21,	4,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	4,	4,	7,	14,	13,	13,	13,	11,	12,	12,	20,	18,	1,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	2,	5,	11,	10,	10,	9,	8,	9,	9,	15,	16,	2,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	4,	10,	9,	9,	7,	8,	9,	8,	14,	14,	3,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	8,	7,	6,	7,	5,	6,	6,	12,	12,	5,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	2,	3,	4,	5,	7,	6,	14,	11,	11,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	4,	3,	3,	6,	4,	10,	10,	10,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	3,	4,	4,	4,	12,	10,	10,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	3,	3,	1,	7,	7,	10,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	3,	8,	8,	8,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	10,	11,	11,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	8,	9,	10,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	3,	16,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	16,
                   0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0
),
                 byrow=T, ncol=22)
nw.tsp <- nw.tsp + t(nw.tsp)
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(1:22)                                                     
tsp <- TSP(nw.tsp) 
n_of_cities(tsp) 
labels(tsp)
tour <- solve_TSP(tsp) 
names(tour)
tour # 길이
