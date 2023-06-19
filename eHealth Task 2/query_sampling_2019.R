# set seed for a replication random sampling 
set.seed(123)

library(Matrix)
library(tibble)
library(readr)
library(dplyr)
library(tidytext)
library(tm)
library(SnowballC)

source("load_data.R")
source("stem_query.R")
source("find_features_stem.R")
source("rank_documents_stem.R")
source("get_next_k_docs.R")


# set hyper-parameters
# m1: angular coefficient during ranking
m1 <- 1.0
# p: percent of documents to read from the pool
p <- 10
# f: number of features to add each round
f  <- 0
# m2 angular coefficient after top k documents
m2 <- 1.0
# thresh: number of documents that a physician is willing to read
thresh <- 400
# prec: minimum level of precision during classification phase
prec <- 0.1
# sample every n docs
sample_n <- 10

# create run identifier
run_id <- paste0("m", m1 * 10, "p", p, "f", f, "t", thresh, "p", prec * 10, "m", m2 * 10, "s", sample_n)

# give run name
run_name <- paste0("2019_distributed_effort_", run_id)

# stop if you already have this run
if(file.exists(paste0("./runs/local/", run_name))) {
  stop(paste0("run ", run_name, " already exists"))
}

# define BM25 smoothing (hyper-)parameters (rel: relevant, nonrel: non relevant)
alpha_rel <- 1.0
beta_rel  <- 0.01
alpha_nonrel <- 1.0
beta_nonrel  <- 1

# build trec-like run dataframe
run <- tibble(topic_id = character(),
              #interaction = character(),
              threshold = numeric(),
              pid = character(),
              rank = numeric(),
              score = numeric(), #invert scores for treceval
              run_id = character(),
              relevance = numeric())

# get topics
topics <- list.files("../../indexing/read_pubmed/pubmed_data/test/")

# compute number of documents for each pool
docs_per_topic <- qrels_abs_test %>% 
  group_by(topic) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(topic) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(show = ceiling(prop * (thresh * length(topics))))

#topic <- topics[10] 
#{
for(topic in topics) {
#{
  # display topic
  print(paste0("[", which(topics == topic), "] ranking pids for topic ", topic))
  
  # load index for topic
  #load(paste0("../index_collection/indexes/test/", topic, "_index.RData"))
  load(paste0("../../indexing/index_collection/indexes/test/", topic, "_index.RData"))
  
  # generate sparse binary
  sparse_binary <- sparse_tf
  sparse_binary@x <- rep(1, length(sparse_binary@x))
  
  documents <- sparse_tf@Dimnames[[1]]
  terms <- sparse_tf@Dimnames[[2]]
  #print(num_of_documents)
  
  # subset qrels 
  qrels <- qrels_abs_test %>%
    filter(topic == !!topic) %>%
    select(pid, relevant)
  
  # subset qrels with pids only
  qrels <- qrels %>%
    filter(pid %in% documents)
  
  
  # find pids that are not in qrels
  pids_not_in_qrels <- setdiff(documents, qrels$pid)
  
  # for those pids, set relevance to zero
  if(length(pids_not_in_qrels) > 0) {
    qrels <- rbind(qrels, data.frame(pid = pids_not_in_qrels, 
                                     relevant = rep(0, length(pids_not_in_qrels))))  
  }
  
  # sort qrels in the same way dtm pids are sorted
  qrels <- qrels[order(qrels$pid), ]
  
  # initialize relevance feedback vector
  # -1 : not judged 
  #  0 : not relevant
  #  1 : relevant
  relevance_feedback <- rep.int(-1, times = num_of_documents)
  
  # initialize selection strategy dataframe
  strategy <- factor(levels = c("ranked", "sampled"))
  #selection_strategy <- data.frame(id = numeric(), relevance = numeric(), strategy = strategy)
  selection_strategy <- tibble(id = integer(), 
                               relevance = integer(), 
                               strategy = strategy)
  
  # In the first pass, we estimate the parameters of the non-relevant documents 
  # using the whole collection.
  # (WE SHOULD ADD NUMBER OF FEATURES IN THE RDATA file, future work)
  theta_rel <- rep(alpha_rel / (alpha_rel + beta_rel), times = dim(sparse_binary)[2])
  theta_nonrel <- (colSums(sparse_binary) + alpha_nonrel) / (num_of_documents + alpha_nonrel + beta_nonrel)
  
  # BIM weight for relevant and non relevant set (log(theta / (1 - theta)))
  bim_rel <- log(theta_rel) - log(1 - theta_rel)
  bim_nonrel <- log(theta_nonrel) - log(1 - theta_nonrel)
  
  # read original query
  query_original <- readLines(paste0("../../indexing/read_pubmed/pubmed_data/test/", 
                                     topic, 
                                     "/topic_title.txt"))
  #query_variant_keywords <- readLines(paste0("../../source/read_pubmed/pubmed_data/test_variant/", topic, "/topic_title_variant_FB.txt"))
  #query_variant_readable <- readLines(paste0("../../source/read_pubmed/pubmed_data/test_variant/", topic, "/topic_title_variant_FV.txt"))  
  
  # get features
  features <- find_features_stem(query_original)
  # terms_stem[features]
  #features_variant_keyword <- c() ## find_features(query_variant_keywords)
  #features_variant_readable <- c() ## find_features(query_variant_readable)
  
  # find number of documents to assess
  k <- ceiling(num_of_documents * p / 100)
  
  # get maximum number of documents to judge for this topic
  thresh_topic <- docs_per_topic %>% 
    filter(topic == !!topic)
  
  # adjust if necessary
  if (k > thresh_topic$show & thresh > 0) { # if k is greater than threshold
    k <- thresh_topic$show
  }
  if (k > num_of_documents) { # if k is greater than number of available documents (legacy code?)
    k <- num_of_documents
  }
  print(paste0("number of docs to judge: ", k))
  
  # prepare samples
  samples_not_ranked <- sample(1:num_of_documents, replace = FALSE)
  next_sample_to_read <- 1
  
  # set number of relevant documents
  num_of_rel_documents <- 0
  
  #####################################################################
  ##### START FEEDBACK
  #####################################################################
  # i <- 1
  for(i in 1:k) {
    
    # alternate between ranking and sampling of the next document to judge
    if (i %% sample_n != 0) { # rank documents and select first doc
      
      # rank documents with original query
      ranking <- rank_documents_stem(features, m = m1)
      #ranking_variant1 <- rank_documents(features_variant1, m = m1)
      
      # set the number of top k documents to assess (k = 1)
      pids_feedback <- get_next_k_docs(ranking, qrels, relevance_feedback, k = 1)
      
      # get scores of top k
      scores <- ranking$scores[which(ranking$pids %in% pids_feedback)]
      
      # get indexes of documents to get relevance scores
      idx_pids_feedback <- which(qrels$pid %in% pids_feedback)
      
      # update relevance feedback vector
      relevance_feedback[idx_pids_feedback] <- qrels$relevant[idx_pids_feedback]
      
      # update selection strategy vector
      selection_strategy[i, ] <- data.frame(idx_pids_feedback, 
                                            relevance_feedback[idx_pids_feedback],
                                            "ranked")
      
    } else {
      
      idx_pids_feedback <- samples_not_ranked[next_sample_to_read]
      next_sample_to_read <- next_sample_to_read + 1
      
      # check whether this doc has already been judged
      if (relevance_feedback[idx_pids_feedback] == -1) {
        
        # get pids
        pids_feedback <- qrels$pid[idx_pids_feedback]
        
        # set score (zero, since it has not been ranked)
        scores <- 0
        
        # update relevance feedback vector
        relevance_feedback[idx_pids_feedback] <- qrels$relevant[idx_pids_feedback]
        
        # update selection strategy vector
        selection_strategy[i, ] <- data.frame(idx_pids_feedback, 
                                              relevance_feedback[idx_pids_feedback],
                                              "sampled")
        
      } else {
        
        # update selection strategy vector
        selection_strategy[i, ] <- data.frame(idx_pids_feedback, 
                                              relevance_feedback[idx_pids_feedback],
                                              "sampled")
        
        # continue for loop (skip following code)
        next
        
      }
      
    }
    
    # update run item
    run_update <- data.frame(topic_id = topic,
                             interaction = 0,
                             pid = pids_feedback,
                             rank = sum(relevance_feedback != -1), # (sum(relevance_feedback != -1) + 1):(sum(relevance_feedback != -1) + 1),
                             score = -scores, #invert scores for treceval
                             run_id = run_name,
                             relevance = relevance_feedback[idx_pids_feedback])
    
    # write run on file
    write.table(run_update, file = paste0("./runs/local/", run_name),
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE,
                append = T)
    
    # update if necessary
    if (num_of_rel_documents < sum(relevance_feedback == 1)) {
      
      # compute number of relevant documents
      num_of_rel_documents <- sum(relevance_feedback == 1)
      
      # estimate the parameters for the relevant set and the relevant features
      if(num_of_rel_documents > 1) {
        # document frequency
        doc_freq <- colSums(sparse_binary[relevance_feedback == 1, ])
        # compute relevant parameters
        theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
      } else if (num_of_rel_documents == 1) {
        doc_freq <- sparse_binary[relevance_feedback == 1, ]
        theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
      }
      # re-estimate the parameters for the non-relevant set (including unjudged)
      theta_nonrel <- (colSums(sparse_binary[relevance_feedback < 1, ]) + alpha_nonrel) / (num_of_documents - num_of_rel_documents + alpha_nonrel + beta_nonrel)
      
      # BIM weight for relevant and non relevant set (log(theta / (1 - theta)))
      bim_rel <- log(theta_rel) - log(1 - theta_rel)
      bim_nonrel <- log(theta_nonrel) - log(1 - theta_nonrel)
      
    }
    
    # select most relevant features (include query terms)
    if(num_of_rel_documents > 0) {
      # select features with the higest difference between probabilities (of rel and nonrel)
      p_q <- theta_rel - theta_nonrel
      # if number of features (parameter) is greater than 0
      if (f > 0) {
        features <- union(features, 
                          head(order(p_q, decreasing = TRUE), f))
      } else { # increment f at each round
        features <- union(features,
                          head(order(p_q, decreasing = TRUE), i))
      }
    }
  }
  #####################################################################
  ##### END FEEDBACK
  #####################################################################
  
  
  ##################################################################
  ##### START CLASSIFICATION
  ##################################################################
  
  # recompute number of rel and non-rel documents
  num_of_rel_documents <- sum(relevance_feedback == 1)
  num_of_nonrel_documents <- sum(relevance_feedback == 0)
  
  # estimate the parameters for the relevant set and the relevant features
  if(num_of_rel_documents > 1) {
    # document frequency
    doc_freq <- colSums(sparse_binary[relevance_feedback == 1, ])
    # compute relevant parameters
    theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
    # find non-zero features (legacy)
    #rel_features <- which(doc_freq > 0)
    features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 1, ]) > 0)
    
  } else if (num_of_rel_documents == 1) {
    doc_freq <- sparse_binary[relevance_feedback == 1, ]
    theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
    # (legacy)
    #rel_features <- which(doc_freq > 0)
    features_nonzero <- which(sparse_binary[relevance_feedback == 1, ] > 0)
    
  } else { # use non-relevant information
    # doc_freq <- colSums(sparse_binary[relevance_feedback == 0, ])
    #  (legacy)
    #rel_features <- which(doc_freq > 0)
    print("found no relevant docs")
    features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 0, ]) > 0)
  }
  
  # re-estimate the parameters for the non-relevant set
  if(num_of_nonrel_documents > 1) {
    theta_nonrel <- (colSums(sparse_binary[relevance_feedback == 0, ]) + alpha_nonrel) / (num_of_nonrel_documents + alpha_nonrel + beta_nonrel)  
  } else if(num_of_nonrel_documents == 1) {
    theta_nonrel <- (sparse_binary[relevance_feedback == 0, ] + alpha_nonrel) / (num_of_nonrel_documents + alpha_nonrel + beta_nonrel)  
  } else {
    theta_nonrel <- (alpha_nonrel) / (alpha_nonrel + beta_nonrel)
  }
  
  # BIM weight for relevant and non relevant set (log(theta / (1 - theta)))
  bim_rel <- log(theta_rel) - log(1 - theta_rel)
  bim_nonrel <- log(theta_nonrel) - log(1 - theta_nonrel)
  
  # compute coordinates only on relevant features
  x <- sparse_bm25[, features_nonzero] %*% bim_rel[features_nonzero]
  y <- sparse_bm25[, features_nonzero] %*% bim_nonrel[features_nonzero]
  plot(x, y, col = relevance_feedback + 2, cex = 0.5)
  
  # get indexes of relevant and non-relevant documents
  idx_doc_rel <- which(relevance_feedback == 1)
  idx_doc_nonrel <- which(relevance_feedback == 0)
  
  # indexes of judged documents
  idx_doc_judged <- which(relevance_feedback > -1)
  
  # indexes of to be judged documents
  idx_doc_to_judge <- which(relevance_feedback < 0)
  
  # if there is a sufficient number of relevant documents
  if(length(idx_doc_rel) >= 3) {
    # chech the rotation of the distribution of relevant docs
    lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_rel],
                                                         y = y[idx_doc_rel]))
  } else {
    # otherwise chech the rotation of the distribution of judged docs
    lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_judged],
                                                         y = y[idx_doc_judged]))
  }
  #abline(lm_relevant, lty = 2, lwd = 0.7, col = "green")
  
  # find non relevant interpolation
  lm_nonrelevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_nonrel],
                                                          y = y[idx_doc_nonrel]))
  #abline(lm_nonrelevant, lty = 2, lwd = 0.5, col = "red")
  
  # get slopes
  m_rel <- lm_relevant$coefficients[2]
  m_nonrel <- lm_nonrelevant$coefficients[2]
  
  # get higher slope
  m_classify <- ifelse(m_rel > m_nonrel, m_rel, m_nonrel)
  
  # adjust slope if necessary
  if (m_classify < 1.0) {
    m_classify <- 1.0
  }
  
  # if there is at least one relevant document
  if (length(idx_doc_rel) > 0) {
    # find "highest" relevant point (least relevant point according to m_rel)
    q_classify <- sort(y[idx_doc_rel] - m_classify * x[idx_doc_rel], decreasing = T)
  } else {
    # otherwise use most relevant document among *non-relevants* documents
    q_classify <- sort(y[idx_doc_nonrel] - m_classify * x[idx_doc_nonrel], decreasing = F)
  }
  abline(q_classify[1], m_classify, lty = 3, lwd = 1)
  
  # find how many documents are below this point
  num_doc_below_line <- sum(y[idx_doc_to_judge] <= m_classify * x[idx_doc_to_judge] + q_classify[1])
  
  # set new threshold (geometric series, user will read at most 2 * threshold documents)
  thr <- floor(thresh_topic$show / 2)
  
  # keep reading until threshold is zero 
  while (thr > 0) {
    
    # keep count of documents to judge at this round  
    num_doc_to_judge <- thr
    
    # keep track of the remaning documents
    if (num_doc_to_judge > sum(relevance_feedback == -1)) {
      num_doc_to_judge <- sum(relevance_feedback == -1)
    }
    
    while (num_doc_to_judge > 0) {
      
      # keep track of the index "i" for selection strategy
      i <- i + 1
      
      # alternate between ranking and sampling of the next document to judge
      if (i %% sample_n != 0) { # rank documents and select first doc
        
        # rank documents with non zero features and m_rel
        ranking <- rank_documents_stem(features_nonzero, m = m_rel)
        
        # set the number of top k documents to assess (k = 1)
        pids_feedback <- get_next_k_docs(ranking, qrels, relevance_feedback, k = 1)
        
        # get scores of top k
        scores <- ranking$scores[which(ranking$pids %in% pids_feedback)]
        
        # get indexes of documents to get relevance scores
        idx_pids_feedback <- which(qrels$pid %in% pids_feedback)
        
        # update relevance feedback vector
        relevance_feedback[idx_pids_feedback] <- qrels$relevant[idx_pids_feedback]
        
        # update selection strategy vector
        selection_strategy[i, ] <- data.frame(idx_pids_feedback, 
                                              relevance_feedback[idx_pids_feedback],
                                              "ranked")
        
        # update docs below line (in case no relevant documents were found)
        num_doc_below_line <- num_doc_below_line - 1
        
        
      } else {
        
        idx_pids_feedback <- samples_not_ranked[next_sample_to_read]
        next_sample_to_read <- next_sample_to_read + 1
        
        # check whether this doc has already been judged
        if (relevance_feedback[idx_pids_feedback] == -1) {
          
          # get pids
          pids_feedback <- qrels$pid[idx_pids_feedback]
          
          # set score (zero, since it has not been ranked)
          scores <- 0
          
          # update relevance feedback vector
          relevance_feedback[idx_pids_feedback] <- qrels$relevant[idx_pids_feedback]
          
          # update selection strategy vector
          selection_strategy[i, ] <- data.frame(idx_pids_feedback, 
                                                relevance_feedback[idx_pids_feedback],
                                                "sampled")
          
        } else {
          
          # update selection strategy vector
          selection_strategy[i, ] <- tibble(idx_pids_feedback, 
                                                relevance_feedback[idx_pids_feedback],
                                                "sampled")
          
          # continue for loop (skip following code)
          #next
          
        }
        
      }
      
      # update run item
      run_update <- data.frame(topic_id = topic,
                               interaction = 0,
                               pid = pids_feedback,
                               rank = sum(relevance_feedback != -1), # (sum(relevance_feedback != -1) + 1):(sum(relevance_feedback != -1) + 1),
                               score = -scores, #invert scores for treceval
                               run_id = run_name,
                               relevance = relevance_feedback[idx_pids_feedback])
      
      # write run on file
      write.table(run_update, file = paste0("./runs/local/", run_name),
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE,
                  append = T)
      
      # update only if necessary
      if (num_of_rel_documents < sum(relevance_feedback == 1)) {
        
        # compute number of relevant documents
        num_of_rel_documents <- sum(relevance_feedback == 1)
        
        # estimate the parameters for the relevant set and the relevant features
        if(num_of_rel_documents > 1) {
          # document frequency
          doc_freq <- colSums(sparse_binary[relevance_feedback == 1, ])
          # compute relevant parameters
          theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
          
          features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 1, ]) > 0)
          
        } else if (num_of_rel_documents == 1) {
          doc_freq <- sparse_binary[relevance_feedback == 1, ]
          theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
          
          features_nonzero <- which(sparse_binary[relevance_feedback == 1, ] > 0)
        } else {
          features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 0, ]) > 0)
        }
        # re-estimate the parameters for the non-relevant set (including unjudged)
        theta_nonrel <- (colSums(sparse_binary[relevance_feedback < 1, ]) + alpha_nonrel) / (num_of_documents - num_of_rel_documents + alpha_nonrel + beta_nonrel)
        
        # BIM weight for relevant and non relevant set (log(theta / (1 - theta)))
        bim_rel <- log(theta_rel) - log(1 - theta_rel)
        bim_nonrel <- log(theta_nonrel) - log(1 - theta_nonrel)
        
        # features non zero
        # features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 1, ]) > 0)
        
        # compute coordinates only on relevant features
        x <- sparse_bm25[, features_nonzero] %*% bim_rel[features_nonzero]
        y <- sparse_bm25[, features_nonzero] %*% bim_nonrel[features_nonzero]
        #plot(x, y, col = relevance_feedback + 2, cex = 0.5)
        
        # get indexes of relevant documents
        idx_doc_rel <- which(relevance_feedback == 1)
        idx_doc_nonrel <- which(relevance_feedback == 0)
        
        # indexes of judged documents
        idx_doc_judged <- which(relevance_feedback > -1)
        
        # indexes of to be judged documents
        idx_doc_to_judge <- which(relevance_feedback < 0)
        
        # if there is a sufficient number of relevant documents
        if(length(idx_doc_rel) >= 3) {
          # chech the rotation of the distribution of relevant docs
          lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_rel],
                                                               y = y[idx_doc_rel]))
        } else {
          # otherwise chech the rotation of the distribution of judged docs
          lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_judged],
                                                               y = y[idx_doc_judged]))
        }
        #abline(lm_relevant, lty = 2, lwd = 0.5)
        
        # find non relevant interpolation
        lm_nonrelevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_nonrel],
                                                                y = y[idx_doc_nonrel]))
        
        # get slopes
        m_rel <- lm_relevant$coefficients[2]
        m_nonrel <- lm_nonrelevant$coefficients[2]
        
        # get higher slope
        m_classify <- ifelse(m_rel > m_nonrel, m_rel, m_nonrel)
        
        # adjust slope if necessary
        if (m_classify < 1.0) {
          m_classify <- 1.0
        }
        
        # if there is at least one relevant document
        if (length(idx_doc_rel) > 0) {
          # find "highest" relevant point (least relevant point according to m_rel)
          q_classify <- sort(y[idx_doc_rel] - m_classify * x[idx_doc_rel], decreasing = T)
        } else {
          # otherwise use most relevant document among *non-relevants* documents
          q_classify <- sort(y[idx_doc_nonrel] - m_classify * x[idx_doc_nonrel], decreasing = F)
        }
        
        # find how many documents are below this point
        num_doc_below_line <- sum(y[idx_doc_to_judge] <= m_classify * x[idx_doc_to_judge] + q_classify[1])
        
      } # endif found new relevant
      
      # update documents to judge
      num_doc_to_judge <- num_doc_to_judge - 1
      
    } # endif num_doc_below_line
    
    # recompute coordinates (changes in non rel docs)
    num_of_rel_documents <- sum(relevance_feedback == 1)
    
    # estimate the parameters for the relevant set and the relevant features
    if(num_of_rel_documents > 1) {
      # document frequency
      doc_freq <- colSums(sparse_binary[relevance_feedback == 1, ])
      # compute relevant parameters
      theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
      
      # recompute non zero features
      features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 1, ]) > 0)
      
    } else if (num_of_rel_documents == 1) {
      doc_freq <- sparse_binary[relevance_feedback == 1, ]
      theta_rel <- (doc_freq + alpha_rel) / (num_of_rel_documents + alpha_rel + beta_rel)
      
      # recompute non zero features
      features_nonzero <- which(sparse_binary[relevance_feedback == 1, ] > 0)
      
    } else {
      
      # recompute non zero features
      features_nonzero <- which(colSums(sparse_binary[relevance_feedback == 0, ]) > 0)
      
    }
    # re-estimate the parameters for the non-relevant set (including unjudged)
    theta_nonrel <- (colSums(sparse_binary[relevance_feedback < 1, ]) + alpha_nonrel) / (num_of_documents - num_of_rel_documents + alpha_nonrel + beta_nonrel)
    
    # BIM weight for relevant and non relevant set (log(theta / (1 - theta)))
    bim_rel <- log(theta_rel) - log(1 - theta_rel)
    bim_nonrel <- log(theta_nonrel) - log(1 - theta_nonrel)
    
    # compute coordinates only on relevant features
    x <- sparse_bm25[, features_nonzero] %*% bim_rel[features_nonzero]
    y <- sparse_bm25[, features_nonzero] %*% bim_nonrel[features_nonzero]
    #plot(x, y, col = relevance_feedback + 2, cex = 0.5)
    
    # get indexes of relevant documents
    idx_doc_rel <- which(relevance_feedback == 1)
    idx_doc_nonrel <- which(relevance_feedback == 0)
    
    # indexes of judged documents
    idx_doc_judged <- which(relevance_feedback > -1)
    
    # indexes of to be judged documents
    idx_doc_to_judge <- which(relevance_feedback < 0)
    
    # if there is a sufficient number of relevant documents
    if(length(idx_doc_rel) >= 3) {
      # chech the rotation of the distribution of relevant docs
      lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_rel],
                                                           y = y[idx_doc_rel]))
    } else {
      # otherwise chech the rotation of the distribution of judged docs
      lm_relevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_judged],
                                                           y = y[idx_doc_judged]))
    }
    #abline(lm_relevant, lty = 2, lwd = 0.5)
    
    # find non relevant interpolation
    lm_nonrelevant <- lm(formula = y ~ x, data = data.frame(x = x[idx_doc_nonrel],
                                                            y = y[idx_doc_nonrel]))
    
    # get slopes
    m_rel <- lm_relevant$coefficients[2]
    m_nonrel <- lm_nonrelevant$coefficients[2]
    
    # get higher slope
    m_classify <- ifelse(m_rel > m_nonrel, m_rel, m_nonrel)
    
    # adjust slope if necessary
    if (m_classify < 1.0) {
      m_classify <- 1.0
    }
    
    # if there is at least one relevant document
    if (length(idx_doc_rel) > 0) {
      # find "highest" relevant point (least relevant point according to m_rel)
      q_classify <- sort(y[idx_doc_rel] - m_classify * x[idx_doc_rel], decreasing = T)
    } else {
      # otherwise use most relevant document among *non-relevants* documents
      q_classify <- sort(y[idx_doc_nonrel] - m_classify * x[idx_doc_nonrel], decreasing = F)
    }
    #abline(q_classify[1], m_classify, lty = 3, lwd = 1)
    
    # find how many documents are below this point
    num_doc_below_line <- sum(y[idx_doc_to_judge] <= m_classify * x[idx_doc_to_judge] + q_classify[1])
    
    # update threshold
    thr <- floor(thr / 2)
    
  } # endif num_doc_to_judge
  
  ##################################################################
  ##### END CLASSIFICATION
  ##################################################################
  
  if (sum(relevance_feedback == -1) > 0) {
    # complete run with the remaining PIDs (only for CLEf 2018)
    ranking <- rank_documents_stem(features_nonzero, m = m_rel)
    
    # all the remaining docs
    pids_feedback <- get_next_k_docs(ranking, qrels, relevance_feedback)
    
    # get scores of top k
    scores <- ranking$scores[which(ranking$pids %in% pids_feedback)]
    
    # get indexes of documents to get relevance scores
    idx_pids_feedback <- which(qrels$pid %in% pids_feedback)
    
    # get last rank
    last_rank <- run_update$rank
    
    # update run item
    run_update <- data.frame(topic_id = topic,
                             interaction = c(1, rep(0, length(pids_feedback) - 1)),
                             pid = pids_feedback,
                             rank = (last_rank + 1):num_of_documents, # (sum(relevance_feedback != -1) + 1):(sum(relevance_feedback != -1) + 1),
                             score = -scores, #invert scores for treceval
                             run_id = run_name,
                             relevance = qrels$relevant[idx_pids_feedback])
    
    # write run on file
    write.table(run_update, file = paste0("./runs/local/", run_name),
                row.names = FALSE,
                col.names = FALSE,
                quote = FALSE,
                append = T)
  }
  
  print(paste0("rel docs found = ", num_of_rel_documents))
  print(paste0("recall = ", num_of_rel_documents/sum(qrels$relevant)))
  
 } #endif topic

# reload run and remove last column (for CLEF evaluation)
run_local <- read.table(paste0("./runs/local/", run_name), 
                        stringsAsFactors =  F, 
                        header = F)
str(run_local)

write.table(run_local[, 1:6], 
            file = paste0("./runs/clef/", run_name),
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE)

