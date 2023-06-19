# subset experiments
res_orig_p10 <- results %>% filter(grepl(x = run, pattern = "original_m10p10"))

topics <- sort(unique(qrels_abs_test$topic))

results_orig_p10 <- res_orig_p10 %>%
  group_by(run, topic) %>%
  summarize(num_docs = n(), 
            num_rels = sum(relevance), 
            num_shown = 0,
            rels_found = 0) %>%
  ungroup()

last_shown <- res_orig_p10 %>%
  filter(Q0 == 1) %>% 
  select(run, topic, rank)


for (run in unique(res_orig_p10$run)) { # run <- unique(res_orig_p10$run)[1]
  print(run)
  for(topic in topics) { # topic <- topics[1]
    #print(topic)
    last_r <- last_shown %>% filter(run == !!run & topic == !!topic)
    if (nrow(last_r) == 0) {
      #print(topic)
      results_orig_p10 <- results_orig_p10 %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  num_docs, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   num_rels, rels_found))  
    } else {
      
      r_found <- res_orig_p10 %>% 
        filter(run == !!run & topic == !!topic) %>% 
        summarize(rels_found = sum(relevance[1:last_r$rank]))
      
      #print(last_r)
      #print(r_found)
      
      results_orig_p10 <- results_orig_p10 %>% 
        mutate(num_shown = ifelse(run == !!run & topic == !!topic, 
                                  last_r$rank, num_shown),
               rels_found = ifelse(run == !!run & topic == !!topic, 
                                   r_found$rels_found, rels_found))
    }
  }
}

levels <- c("2018_stem_original_m10p10f0t100p1m10s10",
            "2018_stem_original_m10p10f0t200p1m10s10",
            "2018_stem_original_m10p10f0t300p1m10s10",
            "2018_stem_original_m10p10f0t400p1m10s10",
            "2018_stem_original_m10p10f0t500p1m10s10",
            "2018_stem_original_m10p10f0t1000p1m10s10",
            "2018_stem_original_m10p10f0t1500p1m10s10")

labels <- c("2018_stem_original_t100",
            "2018_stem_original_t200",
            "2018_stem_original_t300",
            "2018_stem_original_t400",
            "2018_stem_original_t500",
            "2018_stem_original_t1000",
            "2018_stem_original_t1500")

library(ggplot2)
results_orig_p10 %>%
  mutate(recall = rels_found / num_rels) %>%
  ggplot(aes(x = topic, y = recall)) +
  geom_line(aes(group = factor(run, levels), colour = factor(run, levels, labels))) +
  guides(colour=guide_legend(title="run")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

