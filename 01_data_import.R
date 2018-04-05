text_2014 = read_xlsx("2014_2017.xlsx", sheet = 1, 
                      col_types = c("text", "text", "text", "date", "skip"),
                      col_names = c("press", "title", "text", "date"),
                      skip = 1)
head(text_2014, 2)

text_2015 = read_xlsx("2014_2017.xlsx", sheet = 2, 
                      col_types = c("text", "text", "text", "date", "skip"),
                      col_names = c("press", "title", "text", "date"),
                      skip = 1)
head(text_2015, 2)

text_2016 = read_xlsx("2014_2017.xlsx", sheet = 3, 
                      col_types = c("text", "text", "text", "date", "skip"),
                      col_names = c("press", "title", "text", "date"),
                      skip = 1)
head(text_2016, 2)

text_2017 = read_xlsx("2014_2017.xlsx", sheet = 4, 
                      col_types = c("text", "text", "text", "date", "skip"),
                      col_names = c("press", "title", "text", "date"),
                      skip = 1)
head(text_2017, 2)

text_total = rbind(text_2014, 
                   text_2015,
                   text_2016,
                   text_2017)
head(text_total)

text_total[, "title"] = gsub(pattern = "\\,", replacement = "@@", x = text_total$title)
text_total[,  "text"] = gsub(pattern = "\\,", replacement = "@@", x = text_total$text)

fwrite(text_total, "text_total.csv")

  
  