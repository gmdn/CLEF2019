# Requires features from a trained system
# Requires stem_query
#source("stem_query.R")

rank_documents_stem <- function(features, m = 1, display = FALSE) {
  
  # compute coordinates
  x <- sparse_bm25[, features] %*% bim_rel[features]
  y <- sparse_bm25[, features] %*% bim_nonrel[features]
  
  if(display) {
    plot(x, y, col = relevance_feedback + 2)
    abline(0, m)
  }
  
  # rank scores
  scores <- y - m * x
  
  # smaller scores, smaller probability of non-relevance
  ranking <- order(scores)
  
  # get pids
  pids <- documents[ranking]
  
  return(list(pids = pids, scores = scores[ranking]))
  
}