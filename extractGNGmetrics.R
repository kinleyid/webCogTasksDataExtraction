
extract.GNG.metrics <- function(full.path) {
  # Extracts the following metrics from go/no-go raw CSVs:
  # Mean reaction time for true positives
  # Proportion true positives
  # Mean reaction time for false positives
  # Proportion false positives
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
  false.pos.rts <- c()
  for (trial.idx in which(nogo.trials)) {
    # If the following event was a response, this is a false positive
    if (trial.idx != dim(data)[1]) {
      if (data$event[trial.idx + 1] == 'response') {
        n.false.pos <- n.false.pos + 1
        false.pos.rts <- c(false.pos.rts,
                           data$time[trial.idx + 1] - data$time[trial.idx])
      }
    }
  }
  
  metrics <- c(mean(true.pos.rts),
               n.true.pos / n.go,
               mean(false.pos.rts),
               n.false.pos / n.nogo)
  names(metrics) <- c('mean_true_pos_RT',
                      'p_true_pos',
                      'mean_false_pos_RT',
                      'p_false_pos')
  return(metrics)
}
