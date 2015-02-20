.libPaths("D:/Copy/R/Win-library/3.1")
library(RSQLite)

# Depending on mode (via RScript or via interactive sessions),
# get the 16 letters from the Boggle game
if(length(commandArgs(TRUE))==1) {
  letters <- strsplit(paste(commandArgs(TRUE)[1],collapse=""),"")[[1]]
} else {
  letters <- strsplit(paste(scan(file = "", what = "character", nlines = 4),collapse=""),"")[[1]]
}

#letters <- as.character(strsplit(letters,""))[1:16]

con <- dbConnect(SQLite(), "Boggler.sqlite")
# dbListTables(con)

findWords <- function(n.letters) {
  paths <- dbReadTable(con, paste("paths", n.letters, sep="_"))
  candidates <- as.data.frame(apply(X = paths, MARGIN = 1, FUN = function(x) paste(letters[x], collapse="")))
  query <- paste("SELECT DISTINCT mot FROM dict WHERE taille IS", n.letters, "AND mot IN ('", paste(candidates[,1], collapse="','"),"');")
  return(dbGetQuery(con, query))
}

findWords(3)
findWords(5)
findWords(8)
