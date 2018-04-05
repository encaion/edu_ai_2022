#### 날짜 결측치 처리 ####
df = fread("text_total.csv", data.table = FALSE, encoding = "UTF-8")
df[c(236, 7453), "date"] = df[c(236, 7453) - 1, "date"]
head(df)
