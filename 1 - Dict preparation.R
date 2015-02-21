library(RSQLite)

# Source for words:
# http://www.pallier.org/ressources/dicofr/dicofr.html
# http://www.pallier.org/ressources/dicofr/liste.de.mots.francais.frgut.txt"

words <- read.table(file = "liste.de.mots.francais.frgut.txt",header=FALSE, stringsAsFactors = FALSE)

# Remove diacritics
words <- iconv(words$V1, to="ASCII//TRANSLIT//IGNORE")

# Remove words with a dash
words <- grep("-", x = words, fixed = TRUE, value = TRUE, invert = TRUE)

# Get unique words
words <- unique(words)

# Keep only words having between 3 and 16 letters
words <- words[(nchar(words) >= 3) & (nchar(words) <= 16)]

# Prepare table for database
dict <- data.frame(mot=words, taille=nchar(words))
write.table(dict, file="dict.txt", row.names = FALSE)

con <- dbConnect(SQLite(),"Boggler.sqlite")
dbWriteTable(con, name = "dict", dict, overwrite = TRUE)
dbSendQuery(con, "CREATE UNIQUE INDEX mots ON dict(mot);")
dbSendQuery(con, "CREATE INDEX taille ON dict(taille);")
dbDisconnect(con)
