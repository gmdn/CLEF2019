library(ggplot2)
library(dplyr)
library(gridExtra)

#source("load_data.R")

# set hyper-parameters
# m1: angular coefficient during ranking
m1 <- 1.0
# p: percent of documents to read from the pool
p <- 50
# f: number of features to add each round
f  <- 0
# m2 angular coefficient after top k documents
m2 <- 1.0
# thresh: number of documents that a physician is willing to read
thresh <- 1500
# prec: minimum level of precision during classification phase
prec <- 0.1
# sample every n docs
sample_n <- 10

# create run identifier
id <- paste0("m", m1 * 10, "p", p, "f", f, "t", thresh, "p", prec * 10, "m", m2 * 10, "s", sample_n)

# give run name
run_id <- paste0("stem_original_", id)

run <- read.table(paste0("./runs/local/", run_id),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  colClasses = c("character",
                                 "numeric",
                                 "character",
                                 "numeric",
                                 "numeric",
                                 "character",
                                 "numeric"))

names(run) <- c("topic_id",
                "interaction",
                "pid",
                "rank",
                "score",
                "run_id",
                "relevance")

#str(run)

#topics <- sort(unique(qrel_abs_test$topic))
topics <- unique(run$topic)

# build measures data frame
eval_measures <- data.frame(num_docs          = rep(0, length(topics)),
                            num_rels          = rep(0, length(topics)),
                            rels_found        = rep(0, length(topics)),
                            #wss_95            = rep(0, length(topics)), # TODO
                            last_rel          = rep(0, length(topics)),
                            num_shown         = rep(0, length(topics)),
                            #wss_100           = rep(0, length(topics)), # TODO
                            num_feedback      = rep(0, length(topics)),
                            precision_at_1    = rep(0, length(topics)),
                            precision_at_5    = rep(0, length(topics)),
                            precision_at_10   = rep(0, length(topics)),
                            precision_at_15   = rep(0, length(topics)),
                            precision_at_20   = rep(0, length(topics)),
                            precision_at_30   = rep(0, length(topics)),
                            precision_at_50   = rep(0, length(topics)),
                            precision_at_100  = rep(0, length(topics)),
                            precision_at_200  = rep(0, length(topics)),
                            precision_at_500  = rep(0, length(topics)),
                            precision_at_1000 = rep(0, length(topics)),
                            recall            = rep(0, length(topics)),
                            recall_at_R       = rep(0, length(topics)),
                            recall_at_R_100   = rep(0, length(topics)),
                            recall_at_2R      = rep(0, length(topics)),
                            recall_at_2R_100  = rep(0, length(topics)),
                            recall_at_4R      = rep(0, length(topics)),
                            recall_at_4R_100  = rep(0, length(topics)))

# give rows topic ids
row.names(eval_measures) <- topics

# build cost-recall dataframe
cost_recall <- matrix(0, nrow = 101, ncol = length(topics))
colnames(cost_recall) <- topics

# build plot
df_empty <- data.frame()
ggp_cost_recall  <- ggplot(df_empty) + geom_point() + xlim(0, 1) + ylim(0, 1)
ggp_precision_at <- ggplot(df_empty) + geom_point() + ylim(0, 1)

for(topic in topics) {
#{
  #topic <- topics[2] or topic <- topics[which(topics == "CD007394")]
  #topic <- topics[2]

  # extract run data for this topic
  run_topic <- run[run$topic_id == topic, ]
  
  ## get docs
  #qrel_topic <- qrel_abs_test[qrel_abs_test$topic == topic, ]
  
  # extract pids of relevant documents
  #qrel_topic_pids <- qrel_topic[qrel_topic$relevancy == 1, "pid"]
  
  # number of documents
  num_docs <- dim(run_topic)[1]
  
  # number of relevant documents #### LEIF
  num_rels <- sum(run_topic$relevance)
  
  # find cut
  threshold <- which(run_topic$interaction == 1)
  
  if (length(threshold) == 0) {
    threshold <- dim(run_topic)[1]
  }
  
  # threshold run  
  run_topic_thres <- run_topic[1:threshold, ]
  
  # number of documents #### LEIF
  num_shown <- nrow(run_topic_thres)
  
  # extract pids from run
  #run_topic_pids_thres <- run_topic_thres[, "pid"]
  
  # find relevant docs of run
  #run_topic_rel <- intersect(run_topic_pids_thres, qrel_topic_pids)
  
  # number of relevant docs retrieved ### LEIF
  rels_found <- sum(run_topic_thres$relevance)
  
  # find index of last relevant ### LEIF
  last_rel <- max(which(run_topic_thres$relevance == 1))
  if (last_rel == -Inf) {
    last_rel <- num_shown + 1
  }
  
  # find number of feedback ### LEIF
  num_feedback <- num_shown #sum(run_topic$interaction == "AF")
  
  # create relevance vector
  #relevance <- rep(0, num_shown)
  relevance <- run_topic$relevance[1:threshold]
  
  # update relevance with true relevant docs 
  #relevance[which(is.element(run_topic_pids, run_topic_rel))] <- 1
  
  # compute precision at 1
  precision_at_1 <- relevance[1]
  
  # compute recall at 5
  precision_at_5 <- sum(relevance[1:5])/5
  
  # compute recall at 10
  precision_at_10 <- sum(relevance[1:10])/10
  
  # compute recall at 15
  if(length(relevance) < 15) {
    precision_at_15 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_15 <- sum(relevance[1:15])/15  
  }
  
  # compute recall at 20
  if(length(relevance) < 20) {
    precision_at_20 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_20 <- sum(relevance[1:20])/20  
  }
  
  # compute recall at 30
  if(length(relevance) < 30) {
    precision_at_30 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_30 <- sum(relevance[1:30])/30  
  }
  
  # compute recall at 50
  if(length(relevance) < 50) {
    precision_at_50 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_50 <- sum(relevance[1:50])/50  
  }
  
  # compute recall at 100
  if(length(relevance) < 100) {
    precision_at_100 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_100 <- sum(relevance[1:100])/100  
  }
  
  # compute recall at 200
  if(length(relevance) < 200) {
    precision_at_200 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_200 <- sum(relevance[1:200])/200  
  }
  
  # compute recall at 500
  if(length(relevance) < 500) {
    precision_at_500 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_500 <- sum(relevance[1:500])/500  
  }
  
  # compute recall at 1000
  if(length(relevance) < 1000) {
    precision_at_1000 <- sum(relevance[1:length(relevance)])/length(relevance)
  } else {
    precision_at_1000 <- sum(relevance[1:1000])/1000  
  }
  
  # compute recall
  recall <- rels_found/num_rels
  
  # compute recall at R
  recall_at_R <- sum(relevance[1:num_rels], na.rm = TRUE)/num_rels
  
  # compute recall at R + 100
  recall_at_R_100 <- sum(relevance[1:(num_rels + 100)], na.rm = TRUE)/num_rels
  
  # compute recall at 2R
  recall_at_2R <- sum(relevance[1:(2 * num_rels)], na.rm = TRUE)/num_rels
  
  # compute recall at 2R + 100
  recall_at_2R_100 <- sum(relevance[1:((2 * num_rels) + 100)], na.rm = TRUE)/num_rels
  
  # compute recall at 4R
  recall_at_4R <- sum(relevance[1:(4 * num_rels)], na.rm = TRUE)/num_rels
  
  # compute recall at 4R + 100
  recall_at_4R_100 <- sum(relevance[1:((4 * num_rels) + 100)], na.rm = TRUE)/num_rels
  
  # # compute docs at recall 70
  # docs_at_recall70 <- min(which(cumsum(relevance) >= true_rel_docs * 0.7))
  # 
  # # compute docs at recall 80
  # docs_at_recall80 <- min(which(cumsum(relevance) >= true_rel_docs * 0.8))
  # 
  # # compute docs at recall 90
  # docs_at_recall90 <- min(which(cumsum(relevance) >= true_rel_docs * 0.9))
  # 
  # # compute docs at recall 100
  # docs_at_recall100 <- min(which(cumsum(relevance) == true_rel_docs))
  
  eval_measures[topic, "num_docs"]          <- num_docs
  eval_measures[topic, "num_rels"]          <- num_rels
  eval_measures[topic, "rels_found"]        <- rels_found
  eval_measures[topic, "last_rel"]          <- last_rel
  eval_measures[topic, "num_shown"]         <- num_shown
  eval_measures[topic, "num_feedback"]      <- num_feedback
  eval_measures[topic, "precision_at_1"]    <- precision_at_1
  eval_measures[topic, "precision_at_5"]    <- precision_at_5
  eval_measures[topic, "precision_at_10"]   <- precision_at_10
  eval_measures[topic, "precision_at_15"]   <- precision_at_15
  eval_measures[topic, "precision_at_20"]   <- precision_at_20
  eval_measures[topic, "precision_at_30"]   <- precision_at_30
  eval_measures[topic, "precision_at_50"]   <- precision_at_50
  eval_measures[topic, "precision_at_100"]  <- precision_at_100
  eval_measures[topic, "precision_at_200"]  <- precision_at_200
  eval_measures[topic, "precision_at_500"]  <- precision_at_500
  eval_measures[topic, "precision_at_1000"] <- precision_at_1000
  eval_measures[topic, "recall"]            <- recall
  eval_measures[topic, "recall_at_R"]       <- recall_at_R
  eval_measures[topic, "recall_at_R_100"]   <- recall_at_R_100
  eval_measures[topic, "recall_at_2R"]      <- recall_at_2R
  eval_measures[topic, "recall_at_2R_100"]  <- recall_at_2R_100
  eval_measures[topic, "recall_at_4R"]      <- recall_at_4R
  eval_measures[topic, "recall_at_4R_100"]  <- recall_at_4R_100
  
  # plot normalized cost-recall plot
  df <- data.frame(cost = (1:num_shown)/num_docs,
                   recall = cumsum(run_topic_thres$relevance)/num_rels)
  #plot(df, type = "l")
  #df <- data.frame(cost = (1:length(relevance))/length(relevance), 
  #                 recall = cumsum(relevance)/sum(relevance))
  
  # compute recall at fixed costs (0.1, 0.2, ...)
  costs <- rep(0, 101)
  
  for(i in 1:101) {

    find_min <- min(which(df$cost >= (i - 1)/100))
    
    if (find_min == Inf) {
      costs[i] <- recall
    } else {
      costs[i] <- df$recall[find_min]
    }
    
  }
  # save into cost_recall matrix
  cost_recall[, topic] <- costs
  
  ggp_cost_recall <- ggp_cost_recall + geom_line(data = data.frame(cost = seq(0, 1, 0.01),
                                                                   recall = costs),
                                                 aes(x = cost, y = recall), colour = "lightgrey")

  
  df_precision_at <- data.frame(at = c(1, 5, 10, 15, 20, 30, 50, 100, 200, 500, 1000), 
                                precision = as.numeric(eval_measures[topic, 7:17]))
  
  ggp_precision_at <- ggp_precision_at + geom_line(data = df_precision_at, 
                                                   aes(x = at, y = precision), 
                                                   colour = "lightgrey")
  
}

ggp_cost_recall <- ggp_cost_recall + 
  geom_line(data = data.frame(cost = seq(0, 1, 0.01), 
                              recall = rowMeans(cost_recall)),
            aes(x = cost, y = recall), 
            colour = "red") +
  geom_abline(slope = 0, intercept = mean(eval_measures$recall_at_R),
              colour = "blue")

#print(ggp_cost_recall)


ggp_precision_at <- ggp_precision_at + 
  geom_line(data = data.frame(at = c(1, 5, 10, 15, 20, 30, 50, 100, 200, 500, 1000),
                              precision = colMeans(eval_measures[, 7:17])),
            aes(x = at, y = precision), 
            colour = "red") +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 30, 50, 100, 200, 500, 1000),
                     trans = "log10")


#print(ggp_precision_at)

grid.arrange(ggp_cost_recall, ggp_precision_at, top = run_id)

avg_measures <- colMeans(eval_measures)

print(format(colMeans(eval_measures), digits = 3, nsmall = 3))

#print(format(colMeans(eval_measures), digits = 3, nsmall = 3))
#save(eval_measures, file = paste0("./runs/", run_id, "_eval.RData"))

mean(eval_measures$rels_found / eval_measures$num_rels)

eval_measures[, c("num_rels", "rels_found", "num_shown")] %>% 
  mutate(recall = rels_found/num_rels) %>% 
  summarise(docs_shown = sum(num_shown), avg_recall = mean(recall))
