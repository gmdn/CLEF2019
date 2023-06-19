library(dplyr)
library(ggplot2)

results <- read.table(file = paste0("./runs/", run_name), 
                      header = FALSE, 
                      stringsAsFactors = FALSE)

names(results) <- c("topic", "mode", "document", "rank", "score", "runid", "relevance")

results_augmented <- results %>% mutate(cum_relevance = cumsum(relevance))

str(results_augmented)

results_augmented %>% 
  ggplot(aes(x = rank, y = cum_relevance)) + 
  geom_point() + 
  geom_line(col = "red")

rel_docs_found <- sum(results$relevance)

###########

df <- selection_strategy

#df %>% filter(selection_strategy$sampled == T) %>% group_by(relevance_feedback) %>% tally()
#df %>% filter(selection_strategy$ranked == T) %>% group_by(relevance_feedback) %>% tally()

df %>% #filter(selection_strategy != "unjudged") %>%
  group_by(strategy) %>%
  mutate(round = 1:n(), cum_rel = cumsum(relevance)) %>%
  ggplot(aes(x = round, y = cum_rel, group = strategy, color = strategy)) +
  geom_line()

            
table(qrels$relevancy)

############

# Size of population
N <- num_of_documents

# number of samples
n <- sum(df$strategy == "sampled")

# number of positive observation
pos_obs <- sum(df$relevance[df$strategy == "sampled"])

# estimate of proportion
p_hat <- pos_obs / n

# alpha
alpha <- 0.05

# z score
z_alpha <- qnorm(p = 1 - alpha/2, mean = 0, sd = 1)

# confidence intervals
conf_hi <- p_hat + z_alpha * sqrt((p_hat * (1 - p_hat) / (n - 1)) * ((N - n) / N))
conf_lo <- p_hat - z_alpha * sqrt((p_hat * (1 - p_hat) / (n - 1)) * ((N - n) / N))

if (conf_lo < 0) {
  conf_lo <- 0
}

# size of intervals
size_conf_hi <- ceiling(conf_hi * N)
size_conf_lo <- floor(conf_lo * N)

if (size_conf_lo == 0 & pos_obs > 0) {
  size_conf_lo <- pos_obs
}

print(paste0("The collection contains ", N, " documents"))
print(paste0("You sampled ", n, " documents"))
print(paste0("We expect to find ", format(p_hat * N, digits = 1), " relevant documents"))
print(paste0("The confidence interval is between"))
print(paste0(size_conf_lo, " and ", size_conf_hi))
print(paste0("with ", (1 - alpha) * 100, "% confidence "))
print("")
print(paste0("The total number of relevant docs is ", sum(qrels$relevancy)))
print(paste0("You found ", rel_docs_found, " relevant documents"))
print(paste0("in ", dim(results)[1], " documents"))

      