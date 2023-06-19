library(readr)
library(tibble)
library(dplyr)
library(ggplot2)

source("load_data.R")

# read all local runs
path_to_folder <- "./runs/to_evaluate/"

experiments <- list.files(path_to_folder)

results <- tibble(topic = character(),
                  Q0 = integer(),
                  document = character(),
                  rank = integer(),
                  score = double(),
                  run = character(),
                  relevance = integer()
)

for (experiment in experiments) { 
  
  exp <- read_delim(file = paste0(path_to_folder, "/", experiment), 
                    delim = " ", 
                    col_names = c("topic", 
                                  "Q0", 
                                  "document", 
                                  "rank", 
                                  "score", 
                                  "run",
                                  "relevance"), 
                    col_types = cols(col_character(),
                                     col_integer(),
                                     col_character(),
                                     col_integer(),
                                     col_double(),
                                     col_character(),
                                     col_integer()))
  
  results <- results %>%
    bind_rows(exp)
  
  #break
  
}


# subset experiments
res_bm25 <- results %>% filter(grepl(x = run, pattern = "bm25"))

topics <- sort(unique(qrels_abs_test$topic))

results_bm25 <- res_bm25 %>%
  group_by(run, topic) %>%
  summarize(num_docs = n(), 
            num_rels = sum(relevance), 
            num_shown = 0,
            rels_found = 0) %>%
  ungroup()

last_shown <- res_bm25 %>%
  filter(Q0 == 1) %>% 
  select(run, topic, rank)


for (run in unique(res_bm25$run)) { # run <- unique(res_bm25$run)[1]
  print(run)
  for(topic in topics) { # topic <- topics[1]
    #print(topic)
    last_r <- last_shown %>% filter(run == !!run & topic == !!topic)
    if (nrow(last_r) == 0) {
      print(topic)
      results_bm25 <- results_bm25 %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  num_docs, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   num_rels, rels_found))  
    } else {
      
      r_found <- res_bm25 %>% 
        filter(run == !!run & topic == !!topic) %>% 
        summarize(rels_found = sum(relevance[1:last_r$rank]))
      
      #print(last_r)
      #print(r_found)
      
      results_bm25 <- results_bm25 %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  last_r$rank, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   r_found$rels_found, rels_found))
    }
  }
}

levels <- c("2019_baseline_bm25_t200",
            "2019_baseline_bm25_t400",
            "2019_baseline_bm25_t600",
            "2019_baseline_bm25_t800",
            "2019_baseline_bm25_t1000",
            "2019_baseline_bm25_t2000",
            "2019_baseline_bm25_t3000")

library(ggplot2)
results_bm25 %>%
  mutate(recall = rels_found / num_rels) %>%
  ggplot(aes(x = topic, y = recall)) +
  geom_line(aes(group = factor(run, levels), colour = factor(run, levels))) +
  guides(colour=guide_legend(title="run")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



results_bm25 %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(total_shown = sum(num_shown), 
            avg_recall = mean(recall)) %>%
  arrange(total_shown) %>%
  ggplot(aes(x = total_shown, y = avg_recall)) +
  geom_point()
  

results_orig_p10 %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(total_shown = sum(num_shown), 
            avg_recall = mean(recall)) %>%
  arrange(total_shown) %>%
  ggplot(aes(x = total_shown, y = avg_recall)) +
  geom_point()

