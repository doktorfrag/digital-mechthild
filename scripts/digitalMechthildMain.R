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
## get data for "pine" and "minne"
## -----------------------------------------

#relative frequencies pulled into list
pine.l <- lapply(relative.text.freqs.l[[1]], '[', 'pine')
minne.l <- lapply(relative.text.freqs.l[[1]], '[', 'minne')

#create matrices
pine.m <- do.call(rbind, pine.l)
minne.m <- do.call(rbind, minne.l)

#merge matrices, name columns, clear NA values
pine.minne.m <- cbind(pine.m[,1], minne.m[,1])
colnames(pine.minne.m) <- c("pine", "minne")
pine.minne.m[which(is.na(pine.minne.m))] <- 0
print(pine.minne.m)

#plot barchart
barplot(pine.minne.m, beside = TRUE, col = 'grey')

## -----------------------------------------
## calculate correlation between "pine" and "minne"
## -----------------------------------------

pine.minne.df <- as.data.frame(pine.minne.m)
pine.minne.cor <- cor.test(pine.minne.df$pine, pine.minne.df$minne, method = "pearson")
print(pine.minne.cor)

## -----------------------------------------
## test correlation with randomization
## -----------------------------------------

random.cor.pine.minne.v <- NULL
for (i in 1:100000) {
  random.cor.pine.minne.v <- c(random.cor.pine.minne.v, 
                               cor(sample(pine.minne.df$pine), pine.minne.df$minne))
}

cat("Minumum: ", min(random.cor.pine.minne.v), "\n")
cat("Maximum: ", max(random.cor.pine.minne.v), "\n")
cat("Range: ", range(random.cor.pine.minne.v), "\n")
cat("Mean: ", mean(random.cor.pine.minne.v), "\n")
cat("Standard Deviation: ", sd(random.cor.pine.minne.v), "\n")

## -----------------------------------------
## print pretty histogram
## -----------------------------------------

h <- hist(random.cor.pine.minne.v, breaks = 100, col = "grey",
          xlab = "Correlation Coefficient",
          main = "Histogram of Random Correlation Coefficients w/ Normal Curve \n Book 1 of DfL: 'pine' and 'minne'",
          plot = TRUE)
xfit <- seq(min(random.cor.pine.minne.v), max(random.cor.pine.minne.v), length = 1000)
yfit <- dnorm(xfit, mean = mean(random.cor.pine.minne.v), sd = sd(random.cor.pine.minne.v))
yfit <- yfit * diff(h$mids[1:2]) * length(random.cor.pine.minne.v)
lines(xfit, yfit, col = "black", lwd = 2)