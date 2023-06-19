# subset experiments
res_dist <- results %>% filter(grepl(x = run, pattern = "distributed"))

topics <- sort(unique(qrels_abs_test$topic))

results_dist <- res_dist %>%
  group_by(run, topic) %>%
  summarize(num_docs = n(), 
            num_rels = sum(relevance), 
            num_shown = 0,
            rels_found = 0) %>%
  ungroup()

last_shown <- res_dist %>%
  filter(Q0 == 1) %>% 
  select(run, topic, rank)


for (run in unique(res_dist$run)) { # run <- unique(res_dist$run)[1]
  print(run)
  for(topic in topics) { # topic <- topics[1]
    #print(topic)
    last_r <- last_shown %>% filter(run == !!run & topic == !!topic)
    if (nrow(last_r) == 0) {
      #print(topic)
      results_dist <- results_dist %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  num_docs, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   num_rels, rels_found))  
    } else {
      
      r_found <- res_dist %>% 
        filter(run == !!run & topic == !!topic) %>% 
        summarize(rels_found = sum(relevance[1:last_r$rank]))
      
      #print(last_r)
      #print(r_found)
      
      results_dist <- results_dist %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  last_r$rank, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   r_found$rels_found, rels_found))
    }
  }
}

levels <- c("2019_distributed_effort_m10p10f0t100p1m10s10",
            "2019_distributed_effort_m10p10f0t200p1m10s10",
            "2019_distributed_effort_m10p10f0t300p1m10s10",
            "2019_distributed_effort_m10p10f0t400p1m10s10",
            "2019_distributed_effort_m10p10f0t500p1m10s10",
            "2019_distributed_effort_m10p10f0t1000p1m10s10",
            "2019_distributed_effort_m10p10f0t1500p1m10s10")

labels <- c("2019_distributed_effort_t100",
            "2019_distributed_effort_t200",
            "2019_distributed_effort_t300",
            "2019_distributed_effort_t400",
            "2019_distributed_effort_t500",
            "2019_distributed_effort_t1000",
            "2019_distributed_effort_t1500")

library(ggplot2)
results_dist %>%
  mutate(recall = rels_found / num_rels) %>%
  ggplot(aes(x = topic, y = recall)) +
  geom_line(aes(group = factor(run, levels), colour = factor(run, levels, labels))) +
  guides(colour=guide_legend(title="run")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

