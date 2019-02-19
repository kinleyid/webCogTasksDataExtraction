
hyphens.to.list <- function(input){
  out <- list()
  for (i in 1:length(input)) {
    out[[i]] <- as.numeric(
      unlist(
        strsplit(
          as.character(input[i]), split='-'
        )
      )
    )
  }
  return(out)
}

extract.RDS.metrics <- function(full.path) {
  # Returns a vector of the following metrics:
  # Reverse span
  # Weighted proportion correct
  input.data.frame <- read.table(full.path,
                                 header = T,
                                 sep = ',')
  input.data.frame <- input.data.frame[input.data.frame$Trial > 0, ]
  cols.to.process <- c('NumbersShown', 'Input')
  for (column in cols.to.process) {
    input.data.frame[[column]] <- hyphens.to.list(input.data.frame[[column]])
  }
  cresps <- sapply(input.data.frame$NumbersShown, rev)
  curr.length <- length(input.data.frame$NumbersShown[[1]])
  curr.n.correct <- 0
  digit.span <- 0
  min.n.correct <- 3
  curr.sum.correct <- 0
  prev.sum.correct <- 0
  for (i in 1:nrow(input.data.frame)) {
    if (length(input.data.frame$NumbersShown[[i]]) > curr.length) {
      curr.length <- length(input.data.frame$NumbersShown[[i]])
      curr.n.correct <- 0
      prev.sum.correct <- curr.sum.correct
      curr.sum.correct <- 0
    }
    if (!any(is.na(input.data.frame$Input[[i]]))) {
      if (all(input.data.frame$Input[[i]] == cresps[[i]])) {
        curr.n.correct <- curr.n.correct + 1
      }
    }
    curr.sum.correct <- curr.sum.correct + sum(input.data.frame$Input[[i]] == cresps[[i]], na.rm = T)
    if (i == nrow(input.data.frame)) {
      if (curr.n.correct >= min.n.correct) {
        digit.span <- curr.length
      } else {
        digit.span <- curr.length - 1
      }
      weighted.correct <- prev.sum.correct
    }
  }
  metrics <- c(digit.span, weighted.correct)
  metrics <- digit.span
  # metric.names <- c('DigitSpan', 'WeightedProportionCorrect')
  metric.names <- 'DigitSpan'
  names(metrics) <- metric.names
  return(metrics)
}

data <- get.data(trial.directories,
                 file.pattern,
                 task.name,
                 load.data.func,
                 get.metrics.func)

write.csv(data, file = 'C:/Users/Isaac/Projects/VarsityCog/Data/Consolidated/Tidy data/RDS.csv', row.names = F)