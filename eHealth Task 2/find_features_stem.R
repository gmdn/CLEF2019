# Requires stem_query
#source("stem_query.R")

find_features_stem <- function(query) {
  
  # stem query terms
  query_stem <- stem_query(query)
  
  # find the indexes of the terms (stems) involved (query + expansion)
  idx_query_terms <- which(is.element(terms, 
                                      query_stem$term))
  
  return(idx_query_terms)
  
}