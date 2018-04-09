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
