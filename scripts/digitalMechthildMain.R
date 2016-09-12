## -----------------------------------------
## load packages and libraries
## -----------------------------------------

source("scripts/digitalMechthildFunctions.r")

## -----------------------------------------
## set directory, get file names
## -----------------------------------------

input.dir <- "texts/text"
input.files.v <- dir(path = input.dir, pattern = ".*txt")

## -----------------------------------------
## global variables
## -----------------------------------------

cleaned.text.l <- list()
raw.text.freqs.l <- list()
relative.text.freqs.l <-list()

## -----------------------------------------
## read files, get chapter positions, process text
## -----------------------------------------

show.files.func(input.files.v)
for (i in 1:length(input.files.v)) {
  
  #input text
  book.text.v <- read.book.func(input.dir, input.files.v[i])
  
  #grep chapters
  chapter.positions.v <- grep("^CHAPTER \\d", book.text.v)
  
  #put END on final chapter in book
  book.text.v <- c(book.text.v, "END")
  final.position.v <- length(book.text.v)
  chapter.positions.v <- c(chapter.positions.v, final.position.v)
  
  #clean text
  cleaned.text.l[[input.files.v[i]]] <- clean.text.func(book.text.v, chapter.positions.v)
}

## -----------------------------------------
## calculate word frequencies
## -----------------------------------------

#raw frequencies
for (i in 1:length(cleaned.text.l)) {
  raw.text.freqs.l[[input.files.v[i]]] <- calc.raw.freqs.func(book.text.v,
                                                              chapter.positions.v,
                                                              cleaned.text.l[[i]])
}

#relative frequencies
for (i in 1:length(cleaned.text.l)) {
  relative.text.freqs.l[[input.files.v[i]]] <- calc.rel.freqs.func(book.text.v,
                                                              chapter.positions.v,
                                                              cleaned.text.l[[i]])
}

## -----------------------------------------
## get data for "pine", "minne" and "lichamen"
## -----------------------------------------

#relative frequencies pulled into list
pine.l <- lapply(relative.text.freqs.l[[1]], '[', 'pine')
minne.l <- lapply(relative.text.freqs.l[[1]], '[', 'minne')
lichamen.l <- lapply(relative.text.freqs.l[[1]], '[', 'lichamen')

#create matrices
pine.m <- do.call(rbind, pine.l)
minne.m <- do.call(rbind, minne.l)
lichamen.m <- do.call(rbind, lichamen.l)

#merge matrices and name columns
pine.lichamen.m <- cbind(pine.m[,1], lichamen.m[,1])
minne.lichamen.m <- cbind(minne.m[,1], lichamen.m[,1])
pine.minne.m <- cbind(pine.m[,1], minne.m[,1])
colnames(minne.lichamen.m) <- c("minne", "lichamen")
colnames(pine.lichamen.m) <- c("pine", "lichamen")
colnames(pine.minne.m) <- c("pine", "minne")

#plot barchart
barplot(minne.lichamen.m, beside = TRUE, col = 'grey')
barplot(pine.lichamen.m, beside = TRUE, col = 'grey')
barplot(pine.minne.m, beside = TRUE, col = 'grey')