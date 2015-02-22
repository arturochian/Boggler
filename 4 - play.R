.libPaths("D:/Copy/R/Win-library/3.1")
library(RSQLite)
library(igraph)

#shuffle.mode <- "free"
shuffle.mode <- "traditional"

con <- dbConnect(SQLite(),"Boggler.sqlite")

time.allowed <- 120

# randomize letters
if(shuffle.mode=="free") {
  wgts <- dbReadTable(con, "letter_weights")[[2]]
  bog.letters <- sample(x = letters, size = 16, replace = TRUE,
                        prob = wgts)
} else {
  dice <- list()
  dice[[1]] <- "elupst"
  dice[[2]] <- "zdvnea"
  dice[[3]] <- "sdtnoe"
  dice[[4]] <- "amoris"
  dice[[5]] <- "fxraoi"
  dice[[6]] <- "moqabj"
  dice[[7]] <- "fsheei"
  dice[[8]] <- "hrsnei"
  dice[[9]] <- "etnkou"
  dice[[10]] <- "tarilb"
  dice[[11]] <- "tieaoa"
  dice[[12]] <- "acepdm"
  dice[[13]] <- "rlasec"
  dice[[14]] <- "uliwer"
  dice[[15]] <- "vgtnie"
  dice[[16]] <- "lenuyg"

  bog.letters <- sapply(sapply(dice,strsplit,""), sample, size=1)
}

# visualization
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
     vertex.label.cex=3,
     edge.width=2,
     edge.color="white")

findWords <- function(n.letters) {
  if(n.letters==3)
    cat("Veuillez patienter.")
  else
    cat(".")
  paths <- dbReadTable(con, paste("paths", n.letters, sep="_"))
  candidates <- as.data.frame(apply(X = paths, MARGIN = 1, FUN = function(x) paste(bog.letters[x], collapse="")))
  query <- paste("SELECT DISTINCT mot FROM dict WHERE taille IS", n.letters, "AND mot IN ('", paste(candidates[,1], collapse="','"),"');")
  return(dbGetQuery(con, query))
}

calc.points <- function(word) {
  switch(EXPR = as.character(nchar(word)), "3"=1, "4"=1, "5"=2,
         "6"=3, "7"=5, "8"=11, "9"=20, "10"=50, "11"=100, "12"=150,
         "13"=200, "14"=250, "15"=250, "16"=250)
}


good.words <- as.character(unlist(lapply(3:8, findWords)))
time.start <- Sys.time()
word.list <- data.frame(word=character(),points=numeric(),stringsAsFactors = FALSE)
max.points <- sum(unlist(lapply(good.words, calc.points)))

repeat {
  word <- scan(what = "character", nlines = 1, quiet = TRUE)

  if(length(word) == 0)
    break

  if(word %in% good.words && !word %in% word.list$word) {
    pts <- calc.points(word)
    cat(pts, "pts")
    word.list[nrow(word.list)+1,] <- list(word, pts)
  } else {
    cat("Mot non valide ou déjà entré")
  }

  time.diff <- Sys.time()-time.start
  units(time.diff) <- "secs"
  if(time.diff < time.allowed) {
    cat("\n", time.allowed - time.diff, " secs restantes")
  } else {
    cat("\nTotal: ", sum(word.list$points), " points!")
    cat("\nMaximum possible: ", max.points)
    break
  }
}

print(word.list)
