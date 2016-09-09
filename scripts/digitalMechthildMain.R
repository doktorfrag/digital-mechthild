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

book.text.v
chapter.positions.v
cleaned.text.l <- list()
raw.text.freqs.l <- list()
relative.text.freqs.l <-list()

## -----------------------------------------
## read files, get chapter positions, process text
## -----------------------------------------

show.files.func(input.files.v)
for (i in 1:length(input.files.v)) {
  
  #get raw text and grep chapters
  book.text.v <- read.book.func(input.dir, input.files.v[i])
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