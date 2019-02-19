
extract.RVP.metrics <- function(full.path) {
  # Returns a vector of the following metrics:
  # True positive probability
  # False positive probability
  # False negative probability
  # Mean reaction time for true positives
  input.data.frame <- read.table(full.path,
                                 header = T,
                                 sep = ',')
  sum.true.pos <- 0
  sum.false.pos <- 0
  sum.false.neg <- 0
  sum.rt <- 0
  n.seq <- 0
  resp.allowance <- 1800 # milliseconds
  patterns <- list(c(2,4,6), c(3,5,7), c(4,6,8))
  # Ignore practice round
  input.data.frame <- input.data.frame[-(1:which(input.data.frame$Event == 'Task start')), ]
  resp.idx <- input.data.frame$Event == 'Space'
  resp.table <- input.data.frame[resp.idx, ]
  stim.table <- input.data.frame[!resp.idx, ]
  stim.table$Event <- as.double(as.vector(stim.table$Event))
  stim.idxs <- c()
  resp.idxs <- c()
  seqs <- list()
  for (stim.idx in 1:nrow(stim.table)) {
    if (stim.idx < 3) {
      next
    }
    last.sequence <- stim.table$Event[(stim.idx-2):stim.idx]
    if (any(sapply(patterns, identical, last.sequence))) {
      n.seq <- n.seq + 1
      seqs[[length(seqs) + 1]] <- last.sequence
      pres.time <- stim.table$Time[stim.idx]
      stim.idxs <- c(stim.idxs, stim.idx)
      for (resp.idx in 1:nrow(resp.table)) { # determine if they responded in time
        resp.time <- resp.table$Time[resp.idx]
        if (resp.time >= pres.time) {
          if (resp.time <= pres.time + resp.allowance) {
            resp.idxs <- c(resp.idxs, resp.idx)
            sum.true.pos <- sum.true.pos + 1
            sum.rt <- sum.rt + resp.time - pres.time
          }
          break
        }
      }
    }
  }
  extra.counts <- 0
  if (any(diff(stim.idxs) < 6)) {
    # x <- array(0, dim = nrow(stim.table))
    # x[stim.idxs] = c(10000, diff(stim.idxs))
    # stim.table$indicator = x
    # stim.table
    doubled.stim.1 <- stim.table$Time[stim.idxs[c(diff(stim.idxs) == 1, F)]]
    doubled.stim.2 <- stim.table$Time[stim.idxs[c(F, diff(stim.idxs) == 1)]]
    extra.counts <- 0
    for (i in 1:length(doubled.stim.1)) {
      extra.counts <- extra.counts + sum(resp.table$Time >= doubled.stim.1[i]
                                         & resp.table$Time >= doubled.stim.2[i]
                                         & resp.table$Time <= doubled.stim.2[i] + resp.allowance
                                         & resp.table$Time <= doubled.stim.1[i] + resp.allowance)
    }
    sum.true.pos <- sum.true.pos - extra.counts
    n.seq <- n.seq - length(doubled.stim.1)
  }
  sum.false.pos <- nrow(resp.table) - sum.true.pos
  sum.false.neg <- n.seq - sum.true.pos
  metrics <- c(sum.true.pos/n.seq, sum.false.pos, sum.false.neg/n.seq, sum.rt/(n.seq + extra.counts))
  names(metrics) <- c('RVP_TruePositives', 'RVP_FalsePositives', 'RVP_FalseNegative', 'RVP_MeanTotalRT')
  return(metrics)
}

data <- get.data(trial.directories,
                 file.pattern,
                 task.name,
                 load.data.func,
                 get.metrics.func)

write.csv(data, file = 'C:/Users/Isaac/Projects/VarsityCog/Data/Consolidated/Tidy data/RVP.csv', row.names = F)
