#### 데이터 로딩 ####
doc_info = fread("text_total_handled.csv", 
                 select = c("date", "press", "title"),
                 data.table = FALSE)
head(doc_info)

post = fread("lda_k10_posterior.csv", data.table = FALSE, nrows = 34329)
head(post)

#### 자료 조합 ####
post[, "year"   ] = as.numeric(substr(doc_info$date, 1, 4))
post[, "month"  ] = as.numeric(substr(doc_info$date, 6, 7))
post[, "press"  ] = doc_info$press
post[, "title"  ] = doc_info$title
head(post)

unique(post$press)
unique(post$month)

# write.csv(post, "lda_k10_doc_info_total.csv", row.names = FALSE)