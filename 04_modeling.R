df = fread("text_total_handled.csv", data.table = FALSE)
head(df, 1)

# # 영단어 확인
# df_each = unlist(strsplit(df$text, split = " "))
# df_each = unique(df_each)
# length(df_each)
# 
# df_each_eng = grep(pattern = "[a-zA-Z]", x = df_each, value = TRUE)
# head(df_each_eng, 200)

nouns = c()
seq_num = seq(101, nrow(df), 100)
seq_num_max = max(seq_num)
for(n_text in seq_num){
  # n_text = 100
  cat(paste0("\r", "====== extracting nouns (", n_text, "/", seq_num_max,") ======"))
  nouns = c(nouns, extractNoun(df$text[(n_text - 100):n_text]))
}
nouns = c(nouns, extractNoun(df$text[34302:nrow(df)]))
nouns[[1]]

# 한글자 제거
nouns_gt1 = lapply(nouns, FUN = function(x){x[nchar(x) > 1]})
nouns_gt1 = lapply(nouns_gt1, FUN = function(x){x[!grepl(pattern = "^[0-9].*?[0-9]$", x = x)]})
nouns_gt1[1:2]

#### 명사 확인 ####
unique_noun_check(nouns_gt1, pattern = "하이마트")
unique_noun_check(nouns_gt1, pattern = "하이닉스")
unique_noun_check(nouns_gt1, pattern = "알리바바")
unique_noun_check(nouns_gt1, pattern = "lg|엘지")
unique_noun_check(nouns_gt1, pattern = "마켓")
unique_noun_check(nouns_gt1, pattern = "11번가")
unique_noun_check(nouns_gt1, pattern = "쿠팡")
unique_noun_check(nouns_gt1, pattern = "쿠팡[^맨]")
unique_noun_check(nouns_gt1, pattern = "티몬")
unique_noun_check(nouns_gt1, pattern = "gs")
unique_noun_check(nouns_gt1, pattern = "세븐")
unique_noun_check(nouns_gt1, pattern = "현대")
unique_noun_check(nouns_gt1, pattern = "cj")
unique(unlist(nouns_gt1[130:140]))

#### __ ● 개별 명사 치환 ####

nouns_gt1 = lapply(nouns_gt1, FUN = "gsub", pattern = "쇼셜", replacement = "소셜")
nouns_gt1 = lapply(nouns_gt1, FUN = "gsub", pattern = "마켓.{0,}", replacement = "마켓")
nouns_gt1 = lapply(nouns_gt1, FUN = "gsub", pattern = "지마켓", replacement = "g마켓")

#### __ ● 일괄 치환 ####

noun_pairs = data.frame(origin = c("하이마트",     "하이닉스",   "알리바바", 
                                   "엘지", "lg유플러스", "lg생활건강", "lg전자",
                                   "g마켓", "b2b마켓", "소셜마켓", "11번가",
                                   "박원순", "쿠팡맨", "쿠팡[^맨]", "티몬",
                                   "gs25", "gs[가-힣]", "gs[a-z]", "세븐일레븐",
                                   "h몰", "현대[^h]", "cj오쇼핑", "cj[^오]", 
                                   "11st", "11번가"),
                        change = c("롯데하이마트", "sk하이닉스", "알리바바", 
                                   "lg",   "lg유플러스", "lg생활건강", "lg전자", 
                                   "g마켓", "b2b마켓", "소셜마켓", "11번가",
                                   "박원순", "쿠팡맨", "쿠팡", "티몬",
                                   "gs25", "gs", "english", "세븐일레븐",
                                   "hmall", "현대", "cj오쇼핑", "cj", 
                                   "11번가", "11번가"))

for(n_pairs in 1:nrow(noun_pairs)){
  cat(paste0("\r", "==== ", n_pairs, "/", nrow(noun_pairs), " ===="))
  # n_pairs = 1
  nouns_gt1 = lapply(nouns_gt1, 
                     FUN = "gsub", 
                     pattern = paste0(".{0,}", noun_pairs[n_pairs, "origin"], ".{0,}"), 
                     replacement = noun_pairs[n_pairs, "change"])
}

corpus = Corpus(VectorSource(nouns_gt1))
dtm = DocumentTermMatrix(corpus)

# dtm_matrix = as.matrix(dtm_tfidf)
# dtm_matrix = as.matrix(dtm)
# dd = inspect(dtm[1:6, intersect(colnames(dtm), c("g마켓", "11번가"))])
# dd = inspect(dtm[1:2, ])
# class(dd)

lda_result = LDA(dtm, control = list(seed = 1228), k = 10) # 상당히 오래걸림(1시간 이상)

# 10개 토픽의 상위 100개 단어 저장
lda_k10_terms_100 = as.data.frame(terms(lda_result, 100))
head(lda_k10_terms_100, 10)
# write.csv(lda_k10_terms_100, "lda_k10_terms_100.csv", row.names = FALSE)

lda_result_posterior = posterior(lda_result)
lda_result_posterior = round(lda_result_posterior$topics, 5)
colnames(lda_result_posterior) = paste0("topic_", 1:10)
lda_result_posterior = as.data.frame(lda_result_posterior)
lda_result_posterior[, "topic_n"] = apply(lda_result_posterior, MARGIN = 1, FUN = "which.max")
head(lda_result_posterior)
# write.csv(lda_result_posterior, "lda_k10_posterior.csv", row.names = FALSE)

#### 토픽별 단어 추출 ####
# topic 1
dtm[which(lda_result_posterior$topic_n == 1), 1:dtm$ncol] %>% 
  as.matrix() %>%
  apply(MARGIN = 2, FUN = "sum") %>%
  .[order(-.)] %>%
  .[. > 0] -> topic_words

df_topic_words_count = data.frame(obs = 1:length(topic_words),
                                    words = names(topic_words),
                                    count = as.numeric(topic_words))

write.csv(df_topic_words_count, "lda_k10_topic_1_word_count.csv", row.names = FALSE)

# topic 2 ~ 10
for(n_topic in 2:10){
  cat(paste0("\r", "====== topic: ", n_topic, " ======"))
  dtm[which(lda_result_posterior$topic_n == n_topic), 1:dtm$ncol] %>% 
    as.matrix() %>%
    apply(MARGIN = 2, FUN = "sum") %>%
    .[order(-.)] %>%
    .[. > 0] -> topic_words
  
  df_topic_words_count = data.frame(obs = 1:length(topic_words),
                                    words = names(topic_words),
                                    count = as.numeric(topic_words))
  
  write.csv(df_topic_words_count, 
            paste0("lda_k10_topic_", n_topic, "_word_count.csv"), 
            row.names = FALSE)
}
