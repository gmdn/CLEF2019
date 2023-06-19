get_next_k_docs <- function(ranking, qrels, relevance_feedback, k = 0) {
  
  # find documents to judge
  doc_to_judge <- qrels$pid[relevance_feedback == -1]
  
  # find top k docs
  if(k == 0) { # rank all judged docs
    top_k <- na.omit(match(ranking$pids, doc_to_judge))
  } else { # get top k only
    top_k <- na.omit(match(ranking$pids, doc_to_judge))[1:k]  
  }
  
  return(doc_to_judge[top_k])
  
}