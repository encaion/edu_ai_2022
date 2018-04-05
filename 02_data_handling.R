#### 날짜 결측치 확인 ####
df = fread("text_total.csv", data.table = FALSE, encoding = "UTF-8")
head(df)

df[c(236, 7453), ]
