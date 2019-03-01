
extract.NBack.metrics <- function(full.path) {
  # Returns proportion correct responses
  data <- read.table(full.path,
                     header = T,
                     sep = ',')
  data <- subset(data, Trial > 0)
  
  # Get true n back value
  data$NBack <- sapply(data$NumbersShown,
    function(x) {
      x <- strsplit(as.character(x), '-')[[1]]
      x <- as.numeric(x[length(x) - 2])
      return(x)
    }
  )
  
  return(data.frame(
    n.back.ppn.correct = sum(data$Input == data$NBack) / dim(data)[1]
  ))
  
}