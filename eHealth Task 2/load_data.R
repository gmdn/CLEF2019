require(tibble)
require(readr)
require(dplyr)

# load test queries (abstract only)
file_path <- "~/Documents/GitHub/tar/2019-TAR/Task2/"

topic_types <- list.dirs(file_path, recursive = F, full.names = F)

qrels_abs_test <- tibble(topic = character(),
                        iter = character(),
                        pid = character(),
                        relevant = integer())

for (topic_type in topic_types) { 
  
  if (topic_type == "Training") 
    next

  for (dir_qrel in list.files(paste0(file_path, topic_type))) {
    
    print(dir_qrel)
    
    qrel_table <- read_table(file = paste0(file_path, "Testing/", dir_qrel, "/qrels/full.test.", tolower(dir_qrel), ".abs.2019.qrels"), 
                             col_names = c("topic", 
                                           "iter", 
                                           "pid", 
                                           "relevant",
                                           "empty"),
                             col_types = cols("c", "c", "c", "i", "l")) %>%
      select(topic, iter, pid, relevant)
    
    qrels_abs_test <- qrels_abs_test %>%
      bind_rows(qrel_table)
  
    
  }
  
}
