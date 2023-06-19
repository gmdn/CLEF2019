estimate_p_sd <- function(num_of_documents, samples) {
  
  N <- num_of_documents #nrow(list_of_docs)
  doc_sampled <- samples
  
  n <- length(doc_sampled)
  z <- sum(doc_sampled)
  
  p_hat <- z / n
  
  sd_hat <- sqrt((p_hat * (1 - p_hat) / (n - 1)) * ((N - n) / N))  
  
  z_alpha <- qnorm(p = 95/100, mean = 0, sd = 1)
  
  # confidence intervals
  conf_hi <- p_hat + z_alpha * sd_hat
  conf_lo <- p_hat - z_alpha * sd_hat
  
  conf_hi <- floor(conf_hi * N)
  conf_lo <- floor(conf_lo * N)
  
  if (conf_lo < 0) {
    conf_lo <- 0
  }
  if (conf_hi > N) {
    conf_hi <- N
  }
  
  return(c(floor(p_hat * N), conf_lo, conf_hi))
  
}


library(dplyr)

run <- read.table(file = "./runs/local/stem_original_m10p50f0t1500p1m10s10", 
                  header = F, stringsAsFactors = F)

names(run) <- c("topic", "Q0", "document", "rank", "score", "run_name", "relevance")
str(run)

# num_of_docs <- run %>%
#   group_by(topic) %>%
#   tally()
# 
# num_of_rels <- run %>% 
#   group_by(topic) %>%
#   tally(relevance)
# 
# num_of_rels_samples <- run %>%
#   group_by(topic) %>%
#   filter(score == 0) %>%
#   tally(relevance)
# 
# 
# run %>%
#   group_by(topic) %>%
#   filter(score != 0) %>%
#   tally(relevance)
# 
# #filter(Q0 == 1)

topics <- unique(run$topic)

# create tibble
df <- data_frame(topic = topics, 
                 num_docs = 0,
                 docs_read = 0,
                 num_rel = 0,
                 sampled = 0,
                 est_rel = 0,
                 range_min = 0,
                 range_max = 0,
                 found_rel = 0) 

df

for (topic in topics) {
  
  topic <- topics[29]
  
  run_topic <- run[run$topic == topic, ]
  
  num_docs <- nrow(run_topic)
  
  num_rel <- sum(run_topic$relevance)
  
  sampled <- run_topic$relevance[run_topic$score == 0]
  
  estimates <- estimate_p_sd(num_of_documents = num_docs, samples = doc_samples)
  
  threshold <- which(run_topic$Q0 == 1)
  
  if (length(threshold) == 0) {
    threshold <- nrow(run_topic)
  } 
  
  num_rel_found <- sum(run_topic$relevance[run_topic$rank <= threshold])
  
  df[df$topic == topic, 2:9] <- c(num_docs, threshold, num_rel, length(sampled), estimates[1], estimates[2], estimates[3], num_rel_found)
  
}

df

library(xtable)

print(xtable(df, digits = 0), include.rownames = FALSE)
