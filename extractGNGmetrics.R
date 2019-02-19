
extract.GNG.metrics <- function(full.path) {
  data <- read.table(full.path,
                                 header = T,
                                 sep = ',',
                                 quote = '~')
  # Discard practice data
  task.start <- which(data$event == 'task_start')
  data <- data[task.start:dim(data)[1], ]
  
  # Calculate n. true positives, n.false negatives, and mean reaction time
  go.trials <- data$event == 'go'
  n.go <- sum(go.trials)
  n.true.pos <- 0
  true.pos.rts <- c()
  for (trial.idx in which(go.trials)) {
    # If the following event was a response, this is a true positive
    if (trial.idx != dim(data)[1]) {
      if (data$event[trial.idx + 1] == 'response') {
        n.true.pos <- n.true.pos + 1
        true.pos.rts <- c(true.pos.rts,
                          data$time[trial.idx + 1] - data$time[trial.idx])
      }
    }
  }
  
  # Calculate n. false positives
  nogo.trials <- data$event == 'nogo'
  n.nogo <- sum(nogo.trials)
  n.false.pos <- 0
  for (trial.idx in which(nogo.trials)) {
    # If the following event was a response, this is a false positive
    if (trial.idx != dim(data)[1]) {
      if (data$event[trial.idx + 1] == 'response') {
        n.false.pos <- n.false.pos + 1
      }
    }
  }
  
  metrics <- c(mean(true.pos.rts),
               n.true.pos / n.go,
               n.false.pos / n.nogo)
  names(metrics) <- c('mean_RT',
                      'p_true_pos',
                      'p_false_pos')
  return(metrics)
}
