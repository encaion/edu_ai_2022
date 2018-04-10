post = fread("lda_k10_doc_info_total.csv", 
             select = c("year", "month", "press", "topic_n"),
             data.table = FALSE)
head(post)

post[, "topic_n"] = as.character(post$topic_n)
post[, "sum"] = 1

post_agg = aggregate(sum ~ year + topic_n, data = post, FUN = "sum")
head(post_agg)

ggplot(data = post_agg, aes(x = year,
                            y = sum)) + 
  geom_col(aes(fill = factor(topic_n, levels = 1:10)),
           position = "dodge") + 
  labs(fill = "토픽", x = NULL, y = "기사 개수") + 
  scale_x_continuous(expand = c(0.01, 0.01),
                     breaks = 2014:2017,
                     labels = 2014:2017) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  theme(legend.title = element_text(size = 15),
        axis.text.x = element_text(size = 15))

#### 연도별 월별 ####
post[, "topic_n"] = as.character(post$topic_n)
post[, "sum"] = 1

year = 2017
post_agg = aggregate(sum ~ month + topic_n, data = post[post$year == year, ], FUN = "sum")
head(post_agg)

ggplot(data = post_agg, aes(x = month,
                            y = sum)) + 
  geom_hline(yintercept = seq(0, 200, 50), size = 2, color = "#FFFFFF") +
  geom_line(aes(color = factor(topic_n, levels = 1:10)),
            size = 1.2) + 
  geom_point(aes(color = factor(topic_n, levels = 1:10)),
             size = 3, alpha = 0.5) +
  labs(color = "토픽", x = NULL, y = "기사 개수",
       title = paste0(year, "년 월별 토픽별 추이")) + 
  scale_x_continuous(expand = c(0.02, 0.02),
                     breaks = 1:12,
                     labels = 1:12) + 
  scale_y_continuous(expand = c(0.03, 0.03)) + 
  theme(legend.title = element_text(size = 15),
        axis.text.x  = element_text(size = 15),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.title   = element_text(size = 25, face = "bold"))

#### word cloud ####
df = fread("lda_k10_topic_1_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10)

df = fread("lda_k10_topic_2_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'pentagon')

df = fread("lda_k10_topic_3_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_4_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_5_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_6_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_7_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_8_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_9_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')

df = fread("lda_k10_topic_10_word_count.csv", data.table = FALSE)
wordcloud2(data = df[, -1], minSize = 10, shape = 'square')
