library(RSQLite)

con <- dbConnect(SQLite(),"Boggler.sqlite")

# Source for words:
# http://www.pallier.org/ressources/dicofr/dicofr.html
# http://www.pallier.org/ressources/dicofr/liste.de.mots.francais.frgut.txt"

words <- read.table(file = "liste.de.mots.francais.frgut.txt",header=FALSE, stringsAsFactors = FALSE)

# Remove diacritics
words <- iconv(words$V1, to="ASCII//TRANSLIT//IGNORE")

# all lowercase
words <- tolower(words)

# Remove words with a dash
words <- grep("-", x = words, fixed = TRUE, value = TRUE, invert = TRUE)

# Get unique words
words <- unique(words)

# Keep only words having between 3 and 16 letters
words <- words[(nchar(words) >= 3) & (nchar(words) <= 16)]

# Prepare table for database
dict <- data.frame(mot=words, taille=nchar(words))
write.table(dict, file="dict.txt", row.names = FALSE)

dbWriteTable(con, name = "dict", dict, overwrite = TRUE)
dbSendQuery(con, "CREATE UNIQUE INDEX mots ON dict(mot);")
dbSendQuery(con, "CREATE INDEX taille ON dict(taille);")

# Create weights for letter randomization
# based on frequency of letters but "smoothed"
# This is used when playing only
all.letters <- strsplit(paste(dict$mot,collapse=""),"")
tmp.wgt <- prop.table(table(all.letters))
ltr.wgt <- seq(from = 0.002, to = 0.075, length.out = 26)
# sum(ltr.wgt)
names(ltr.wgt) <- names(tmp.wgt[order(tmp.wgt)])
ltr.wgt <- ltr.wgt[order(names(tmp.wgt[order(tmp.wgt)]))]
wgts <- data.frame(letter=names(ltr.wgt), weight=ltr.wgt)
rownames(wgts) <- NULL
dbWriteTable(con, name = "letter_weights", value = wgts, row.names = FALSE, overwrite = TRUE)

dbDisconnect(con)
