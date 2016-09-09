## -----------------------------------------
## function takes vector of file names and prints the name
## -----------------------------------------

show.files.func <- function(file.name.v){
  for (i in 1:length(file.name.v)) {
    cat(i, file.name.v[i], "\n", sep = " ")
  }
}

## -----------------------------------------
## function reads in raw text and returns
## -----------------------------------------

read.book.func <- function(input.dir, input.file.v){
  book.text.v <- scan(paste(input.dir,
                          input.files.v,
                          sep = "/"),
                    what = "character",
                    sep = "\n")
  return(book.text.v)
}

## -----------------------------------------
## function cleans raw text and returns list
## -----------------------------------------

clean.text.func <- function(book.text.v, chapter.positions.v){
  chapter.word.vector.l <- list()
  for(i in 1:length(chapter.positions.v)){
    if(i != length(chapter.positions.v)){
      
      #get chapter title and start/end points of chapter
      chapter.title <- book.text.v[chapter.positions.v[i]]
      start <- chapter.positions.v[i]+1
      end <- chapter.positions.v[i+1]-1
      
      #read chapter lines, convert to lowercase, and remove non-words
      chapter.lines.v <- book.text.v[start:end]
      chapter.words.v <- tolower(paste(chapter.lines.v, collapse = " "))
      chapter.words.l <- strsplit(chapter.words.v, "\\W")
      
      # remove whitespaces and insert into list according to chapter title
      chapter.word.v <- unlist(chapter.words.l)
      chapter.word.v <- chapter.word.v[which(chapter.word.v != "")]
      chapter.word.vector.l[[chapter.title]] <- chapter.word.v
    }
  }
  return(chapter.word.vector.l)
}

## -----------------------------------------
## function calculates raw frequencies and returns list
## -----------------------------------------

calc.raw.freqs.func <- function(book.text.v, chapter.positions.v, input.text.l){
  chapter.raws.l <- list()
  for (i in 1:length(input.text.l)) {
    chapter.title <- book.text.v[chapter.positions.v[i]]
    raw.text.freqs.t <- table(input.text.l[[i]])
    chapter.raws.l[[chapter.title]] <- raw.text.freqs.t
  }
  return(chapter.raws.l)
}

calc.rel.freqs.func <- function(book.text.v, chapter.positions.v, input.text.l){
  chapter.rel.freq.l <- list()
  for (i in 1:length(input.text.l)) {
    chapter.title <- book.text.v[chapter.positions.v[i]]
    chapter.freqs.t <- table(input.text.l[[i]])
    chapter.rel.freq.l[[chapter.title]] <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
  }
  return(chapter.rel.freq.l)
}