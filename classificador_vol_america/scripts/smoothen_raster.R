smoothen <- function(rast){
  s <- rast
  cells <- cellFromRowColCombine(rast, c(1:nrow(rast)),c(1:ncol(rast)))
  values(s) <- sapply(cells, test, rast=rast)
}

test <- function(x, rast){
  adj <- adjacent(rast, x, 8, include=TRUE)
  m <- mean(rast[adj[,2]][rast[adj[,2]]>rast[x]-0.1&rast[adj[,2]]<rast[x]+0.1], na.rm=TRUE)
  m
}