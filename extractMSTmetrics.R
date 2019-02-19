
extract.MST.metrics <- function(full.path) {
  # Returns a vector containing the probabilities of each
  # possible response type given each possible image type
  input.data.frame <- read.table(full.path,
                                 header = T,
                                 sep = ',')
  input.data.frame <- subset(input.data.frame, Trial > 0 & Phase == 'testing')
  resps <- c('Similar', 'New', 'Old')
  image.types <- c('lure', 'foil', 'repeat')
  lure.bins <- 1:5
  contingency.table <- array(data = 0,
                             dim = c(length(image.types),
                                     length(resps),
                                     length(lure.bins)),
                             dimnames = list(resps,
                                             image.types,
                                             lure.bins))
  for (i1 in 1:length(resps)) {
    for (i2 in 1:length(image.types)) {
      for (i3 in 1:length(lure.bins)) {
        contingency.table[resps[i1], image.types[i2], lure.bins[i3]] <- 
          contingency.table[resps[i1], image.types[i2], lure.bins[i3]] + sum(
          input.data.frame$RESP == resps[i1]
          & input.data.frame$ImageType == image.types[i2]
          & input.data.frame$LureBin == lure.bins[i3]
        )
      }
    }
  }
  metrics <- array(dim = length(resps)*length(image.types))
  intended.names <- c()
  # Compute probability of response types without accounting for lure difficulty
  cnt <- 1
  for (i1 in 1:length(resps)) {
    for (i2 in 1:length(image.types)) {
      intended.names[cnt] <- sprintf('p_%s_g_%s)', tolower(resps[i1]), tolower(image.types[i2]))
      metrics[cnt] <- sum(contingency.table[resps[i1], image.types[i2], ]) / sum(contingency.table[, image.types[i2], ])
      cnt <- cnt + 1
    }
  }
  names(metrics) <- intended.names
  # # Compute probability of correct lure detection at different lure difficulty levels
  # new.metrics <- c()
  # new.names <- c()
  # for (curr.diff in 1:5) {
  #   new.names[curr.diff] <- sprintf('p_Similar_g_Lure_bin_%d)', curr.diff)
  #   new.metrics[curr.diff] <- sum(contingency.table['Similar', 'lure', curr.diff]) / sum(contingency.table[, 'lure', curr.diff])
  # }
  # metrics <- c(metrics, new.metrics)
  # names(metrics) <- c(intended.names, new.names)
  metrics
}
