
extract.CWI.metrics <- function(full.path) {
  # Returns a vector of the following metrics:
  # Mean RT on matching (congruent) trials
  # Probability of correct response on matching (congruent) trials
  # Meant RT on non-matching (incongruent) trials
  # Probability of correct respnose on non-matching (incongruent) trials
  curr.data <- read.table(full.path,
                          header = T,
                          sep = ',')
  curr.data <- subset(curr.data, Trial > 0)
  match.idx <- with(curr.data, Word == Colour)
  rts <- with(curr.data, ResponseTime - PresentationTime)
  is.correct <- with(curr.data, Colour == Selection)
  curr.metrics <- c(mean(rts[match.idx]),
                    # mean(rts[match.idx & is.correct]),
                    sum(is.correct[match.idx]) / sum(match.idx),
                    mean(rts[!match.idx]),
                    # mean(rts[!match.idx & is.correct]),
                    sum(is.correct[!match.idx]) / sum(!match.idx))
                    # mean(rts))
  names(curr.metrics) <- c('Stroop_MeanRTMatching',
                           'Stroop_p_CorrectMatching',
                           'Stroop_MeanRTNonMatching',
                           'Stroop_p_CorrectNonMatching')
  return(curr.metrics)
}
