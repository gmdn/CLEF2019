require(tm)
require(SnowballC)
#require(textstem)
require(tidytext)

stem_query <- function(query) {
  
  if (is.character(query)) {
    query <- tibble(text = query)
  }
  
  query_stems <- query %>%
    unnest_tokens(term, text, 
                  token = "words",
                  strip_numeric = TRUE) %>%
    filter(!(term %in% stopwords())) %>%
    mutate(term = wordStem(term, language = "en"))

  return(query_stems)
  
}

