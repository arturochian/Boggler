.libPaths("D:/Copy/R/Win-library/3.1")
library(RSQLite)

con <- dbConnect(SQLite(), "Boggler.sqlite")

# Depending on mode (via RScript or via interactive sessions),
# get the 16 letters from the Boggle game
if(length(commandArgs(TRUE))==1) {
  bog.letters <- strsplit(paste(commandArgs(TRUE)[1],collapse=""),"")[[1]]
} else {
  bog.letters <- strsplit(paste(scan(file = "", what = "character", nlines = 4),collapse=""),"")[[1]]
}

# For testing
# bog.letters <- as.character(strsplit(letters,""))[sample(x = 1:26, size = 16, replace = TRUE)]

# bog.letters <- tolower(bog.letters)

# Plot the board
# http://www.r-bloggers.com/going-viral-with-rs-igraph-package/
# http://lists.nongnu.org/archive/html/igraph-help/2007-07/msg00011.html
library(igraph)
boggle.graph <- graph.lattice(length = c(4,4), dim = 1, directed = FALSE)
plot(boggle.graph,
     layout = layout.grid,
     vertex.label=toupper(bog.letters[c(13:16,9:12,5:8,1:4)]),
     vertex.size = 60,
     vertex.shape = "square",
     vertex.color="white",
     vertex.frame.color= "black",
     vertex.label.color = "black",
     vertex.label.family = "sans",
     #edge.label.family="Palatino",
     vertex.label.cex=3,
     edge.width=2,
     edge.color="white")

findWords <- function(n.letters) {
  paths <- dbReadTable(con, paste("paths", n.letters, sep="_"))
  candidates <- as.data.frame(apply(X = paths, MARGIN = 1, FUN = function(x) paste(bog.letters[x], collapse="")))
  query <- paste("SELECT DISTINCT mot FROM dict WHERE taille IS", n.letters, "AND mot IN ('", paste(candidates[,1], collapse="','"),"');")
  return(dbGetQuery(con, query))
}

for(i in 3:10)  print(findWords(i))

