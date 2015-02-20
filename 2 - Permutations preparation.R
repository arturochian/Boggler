library(igraph)
library(RSQLite)

# A graph will represent legit connections between letters
temp.graph <- graph.lattice(length = c(4,4), dim = 1, directed = FALSE, nei = 1)
plot(temp.graph)

# We need to add diagonal connections; we'll get the adjacency matrix and add them to it
adjacency.matrix <- get.adjacency(graph=temp.graph, type = "both", attr = NULL, edges = FALSE, sparse = TRUE)

# define pairs of links to add
pairs <- list(c(1,6), c(2,5), c(2,7), c(3,6), c(3,8), c(4,7), c(5,10),
              c(6,9), c(7,10), c(6,11), c(8,11), c(7,12), c(9,14), c(10,13),
              c(10,15), c(11,14), c(11,16), c(12,15))

for(i in seq_along(pairs)) {
  adjacency.matrix[pairs[[i]][1], pairs[[i]][2]] <- 1
}

boggle.graph <- graph.adjacency(adjmatrix = adjacency.matrix, diag = TRUE,mode = "undirected")

# Make sure we have the correct graph
plot(boggle.graph)

# Generate all legit paths with this recursive function
# credits: http://stackoverflow.com/users/919872/zelazny7
# reference: http://stackoverflow.com/questions/28609703/boggle-cheat-erm-solutioning-with-graphs-in-r/
getPaths <- function(v, g, L) {
  paths <- list()

  recurse <- function(g, v, path = NULL) {
    path <- c(v, path)

    if (length(path) >= L) {
      paths[[length(paths) + 1]] <<- rev(path)
    } else {
      for (i in neighbors(g, v)) {
        if (!(i %in% path)) recurse(g, i, path)
      }
    }
  }
  recurse(g, v)
  return(paths)
}

boggle.paths <- list()
for(i in 3:16) {
  allPaths <- lapply(V(boggle.graph), getPaths, boggle.graph, i)
  boggle.paths[[i]]  <- do.call(rbind, lapply(allPaths, function(x) do.call(rbind, x)))
}
rm(allPaths)
save(boggle.paths, file="boggle paths.RData")

# Check the size of the lists
sizes <- mapply(FUN = nrow, boggle.paths)
range(sizes)
plot(sizes,type="b")

# write the paths to the database
# Note: this will take about 400Mb of disk space
con <- RSQLite::dbConnect(SQLite(),"Boggler.sqlite")
lapply(3:16, function(i) dbWriteTable(con, name = paste("paths", i, sep="_"), value = as.data.frame(boggle.paths[[i]])))

dbDisconnect(con)

