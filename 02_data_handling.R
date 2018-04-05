#### 날짜 결측치 처리 ####
df = fread("text_total.csv", data.table = FALSE, encoding = "UTF-8")
df = df[-c(236, 7453), ]
head(df)

#### 제목 처리 ####
df[1:20, "title"]

#### __ ● 국가 처리 ####
# grep(pattern = "한[[:punct:]]{1}중", df$title, value = TRUE)
# grep(pattern = "중[[:punct:]]{1}일", df$title, value = TRUE)
# grep(pattern = " 일 ", df$title, value = TRUE)
# grep(pattern = "한[[:punct:]]{1}미", df$title, value = TRUE)
# grep(pattern = "중[[:punct:]]{1}미", df$title, value = TRUE)
# grep(pattern = "한\\ㆍ", df$title, value = TRUE)
# grep(pattern = "인니", df$title, value = TRUE)

df[, "title"] = gsub(pattern = "한.중.일", 
                     replacement = " 한국 중국 일본 ",
                     x = df$title)

df[, "title"] = gsub(pattern = "한.중.미", 
                     replacement = " 한국 중국 미국 ",
                     x = df$title)


df[, "title"] = gsub(pattern = "미.중.일", 
                     replacement = " 미국 중국 일본 ",
                     x = df$title)

df[, "title"] = gsub(pattern = "한[[:punct:]]{1}미", 
                     replacement = " 한국 미국 ",
                     x = df$title)

df[, "title"] = gsub(pattern = "( 중|중|[[:punct:]]{1}중)[[:punct:]]{1}(일 |일|일[[:punct:]]{1}일)", 
                     replacement = " 중국 일본 ",
                     x = df$title)

df[, "title"] = gsub(pattern = "( 한|한|[[:punct:]]{1}한)[[:punct:]]{1}(중 |중|중[[:punct:]]{1}중)", 
                     replacement = " 중국 일본 ",
                     x = df$title)

df[, "title"] = gsub("한ㆍ중", " 한국 중국 ", df$title)
df[, "title"] = gsub("한ㆍ일ㆍ중", " 한국 일본 중국 ", df$title)
df[, "title"] = gsub("한ㆍ", " 한국 ", df$title)
df[, "title"] = gsub("인니", " 인도네시아 ", df$title)

df[, "title"] = gsub(" 한중", " 한국 중국 ", df$title)
df[, "title"] = gsub(" 한미", " 한국 미국 ", df$title)
df[, "title"] = gsub(" 한일", " 한국 일본 ", df$title)
df[, "title"] = gsub("한중일", " 한국 중국 일본 ", df$title)

#### __ ● 특수문자 ####
regextract(pattern = "[[:punct:]]", text = df$title) %>%
  unique() -> txt_punct

df[, "title"] = gsub(pattern = "\\[.*?]|\\{.*?}|\\(.*?)|<.*?>",
                     replacement = "",
                     x = df$title)

t(t(txt_punct))

# 기타 특수문자 확인
# grep(pattern = "〈", df$title, value = TRUE)
# grep(pattern = "「", df$title, value = TRUE)
# grep(pattern = txt_punct[47], df$title, value = TRUE)
# grep(pattern = "\\$", df$title, value = TRUE)


df_punct_pairs = data.frame(punct = c("$", "↑"),
                            kr = c("달러", "상승"))

# for(n_pairs in 1:nrow(df_punct_pairs)){
#   cat(paste0("\r", "substitute: ", n_pairs))
#   df[, "title"] = gsub(pattern     = paste0("\\", df_punct_pairs[n_pairs, "punct"]),
#                        replacement = paste0(" ", df_punct_pairs[n_pairs, "kr"], " "),
#                        x = df[, "title"])
# }

df[, "title"] = gsub(pattern = "[[:punct:]]", replacement = " ", x = df$title)

#### __ ● 한자 ####
# 한자 확인
# title_cn = gsub(pattern = "[^一-龠]", replacement = "", df$title)
# title_cn = paste0(title_cn, collapse = "")
# title_cn = unlist(strsplit(title_cn, split = ""))
# title_cn = unique(title_cn)
# title_cn
# 
# for(n_cn in 1:length(title_cn)){
#   grep(pattern = title_cn[n_cn], df$title, value = TRUE) %>%
#     head(2) %>%
#     print()
#   print("-----")
# }

# 한자 치환
df_cn_pairs = data.frame(cn = c("印泥",	"印尼",	"露語",	"中企",	"韓流",	"直購",	"日本",	"印",	"中",	"韓",	"日",	"臺",	"美",	"英",	"亞",	"獨",	"伊",	"銀",	"株",	"兆",	"社",	"上",	"新",	"比",	"逆",	"食",	"政",	"業",	"女",	"門",	"乳",	"毒",	"勢",	"稅",	"價"),
                         kr = c("인도네시아",	"인도네시아",	"전화",	"중소기업",	"한류",	"직구",	"일본",	"인도",	"중국",	"한국",	"일본",	"대만",	"미국",	"영국",	"아시아",	"독일",	"이탈리아",	"은행",	"주식",	"조원",	"회사",	"상승",	"새로운",	"대비",	"역",	"식품",	"정부",	"업",	"여자",	"문",	"유",	"독",	"세력",	"세금",	"가격"))
df_cn_pairs

for(n_pairs in 1:nrow(df_cn_pairs)){
  cat(paste0("\r", "substitute: ", n_pairs))
  df[, "title"] = gsub(pattern     = df_cn_pairs[n_pairs, "cn"],
                       replacement = paste0(" ", df_cn_pairs[n_pairs, "kr"], " "),
                       x = df[, "title"])
}

# 나머지 한자 처리
df[, "title"] = gsub(pattern = "[一-龠]", replacement = " ", df$title)
df[1:20, "title"]

#### __ ● 기타 기호 ####
df[, "title"] = gsub(pattern = "[^0-9a-zA-Z가-힣 ]", replacement = " ", df$title)
df[1:20, "title"]

#### __ ● 소문자 ####
df[, "title"] = tolower(df$title) 

#### __ ● 고유 단어 확인 ####
word_unique = unique(unlist(strsplit(df$title, split = " ")))
length(word_unique)
word_unique[401:600]

# grep(pattern = "독신", x = df$title, value = TRUE)
# grep(pattern = "싱글", x = df$title, value = TRUE)
# grep(pattern = "ft ", x = df$title, value = TRUE)
# grep(pattern = "데이 ", x = df$title, value = TRUE)
# regextract(pattern = " .{1,8}데이 ", df$title) %>%
#   gsub(pattern = " ", replacement = "") %>%
#   unique()
# grep(pattern = "commerce", x = df$title, value = TRUE)
# grep(pattern = "커머스", x = df$title, value = TRUE)
# regextract(pattern = " [a-z가-힣]{1,5}커머스 ", df$title) %>%
#   gsub(pattern = " ", replacement = "") %>%
#   unique()
# 
# grep(pattern = "m커머스", x = df$title, value = TRUE)
# grep(pattern = "d커머스", x = df$title, value = TRUE)
# grep(pattern = "t커머스", x = df$title, value = TRUE)
# grep(pattern = "네모커머스", x = df$title, value = TRUE)
# 
# grep(pattern = "중기", x = df$title, value = TRUE)
# 
# grep(pattern = "티몰", x = df$title, value = TRUE)
# 
# grep(pattern = "[0-9]조 ", x = df$title, value = TRUE)

# 종목:  종목
# 엔터:  엔터테인먼트
# 독신자의 날: 싱글데이
# 솔로데이: 싱글데이
# 원데이:
# e commerce, e 커머스, e쇼핑몰, e 쇼핑몰: 이커머스
# v커머스: 브이커머스
# m커머스: 엠커머스
# d커머스: 디커머스
# t커머스: 티커머스
# ise커머스: 아이에스이커머스
# 네모커머스: 
# 중기: 중소기업
# 티몰글로벌: 티몰
# [0-9]조  -> [0-9]조원
# [0-9]억  -> [0-9]억원
# fta:  fta 

df[, "title"] = gsub(pattern = "종목 ", replacement = " 종목 ", df$title)
df[, "title"] = gsub(pattern = "엔터 ", replacement = " 엔터테인먼트 ", df$title)

df[, "title"] = gsub(pattern = "독신자의 날", replacement = " 싱글데이 ", df$title)
df[, "title"] = gsub(pattern = "솔로데이", replacement = " 솔로데이 ", df$title)
df[, "title"] = gsub(pattern = "원데이", replacement = "", df$title)

df[, "title"] = gsub(pattern = "e commerce|e 커머스|e쇼핑몰|e 쇼핑몰|전자상거래|전자 상거래", replacement = " 이커머스 ", df$title)
df[, "title"] = gsub(pattern = "v커머스", replacement = " 브이커머스 ", df$title)
df[, "title"] = gsub(pattern = "m커머스", replacement = " 엠커머스 ", df$title)
df[, "title"] = gsub(pattern = "d커머스", replacement = " 디커머스 ", df$title)
df[, "title"] = gsub(pattern = "t커머스", replacement = " 티커머스 ", df$title)
df[, "title"] = gsub(pattern = "ise커머스", replacement = " 아이에스이커머스 ", df$title)
df[, "title"] = gsub(pattern = "네모커머스", replacement = "", df$title)
df[, "title"] = gsub(pattern = "드림커머스", replacement = "", df$title)

df[, "title"] = gsub(pattern = "중기", replacement = " 중소기업 ", df$title)
df[, "title"] = gsub(pattern = "중진공", replacement = " 중소기업 진흥공단 ", df$title)

df[, "title"] = gsub(pattern = "중기", replacement = " 중소기업 ", df$title)

df[, "title"] = gsub(pattern = "티몰", replacement = " 티몰 ", df$title)

df[, "title"] = gsub(pattern = " fta", replacement = " fta ", df$title)

df[, "title"] = gsub(pattern = "매출", replacement = " 매출 금액 ", df$title)
df[, "title"] = gsub(pattern = "조원 |억원 |만원 ", replacement = " 금액 ", df$title)
df[, "title"] = gsub(pattern = "[0-9]{1}조 ", replacement = " 조원 금액 ", df$title)
df[, "title"] = gsub(pattern = "[0-9]{1}억 ", replacement = " 조원 금액 ", df$title)

df[, "title"] = gsub(pattern = " 페이|페이 ", replacement = " 페이 ", df$title)


#### __ ● 공백 제거 ####
df[, "title"] = gsub(pattern = " {1,}", replacement = " ", df$title)
df[, "title"] = gsub(pattern = "^ | $", replacement = "", df$title)
df[1:20, "title"]


#### 본문 처리 ####
df[1:20, "text"]

#### __ ● 국가 처리 ####
# grep(pattern = "한[[:punct:]]{1}중", df$text, value = TRUE)
# grep(pattern = "중[[:punct:]]{1}일", df$text, value = TRUE)
# grep(pattern = " 일 ", df$text, value = TRUE)
# grep(pattern = "한[[:punct:]]{1}미", df$text, value = TRUE)
# grep(pattern = "중[[:punct:]]{1}미", df$text, value = TRUE)
# grep(pattern = "한\\ㆍ", df$text, value = TRUE)
# grep(pattern = "인니", df$text, value = TRUE)

df[, "text"] = gsub(pattern = "한.중.일", 
                    replacement = " 한국 중국 일본 ",
                    x = df$text)

df[, "text"] = gsub(pattern = "한.중.미", 
                    replacement = " 한국 중국 미국 ",
                    x = df$text)


df[, "text"] = gsub(pattern = "미.중.일", 
                    replacement = " 미국 중국 일본 ",
                    x = df$text)

df[, "text"] = gsub(pattern = "한[[:punct:]]{1}미", 
                    replacement = " 한국 미국 ",
                    x = df$text)

df[, "text"] = gsub(pattern = "( 중|중|[[:punct:]]{1}중)[[:punct:]]{1}(일 |일|일[[:punct:]]{1}일)", 
                    replacement = " 중국 일본 ",
                    x = df$text)

df[, "text"] = gsub(pattern = "( 한|한|[[:punct:]]{1}한)[[:punct:]]{1}(중 |중|중[[:punct:]]{1}중)", 
                    replacement = " 중국 일본 ",
                    x = df$text)

df[, "text"] = gsub("한ㆍ중", " 한국 중국 ", df$text)
df[, "text"] = gsub("한ㆍ일ㆍ중", " 한국 일본 중국 ", df$text)
df[, "text"] = gsub("한ㆍ", " 한국 ", df$text)
df[, "text"] = gsub("인니", " 인도네시아 ", df$text)

df[, "text"] = gsub(" 한중", " 한국 중국 ", df$text)
df[, "text"] = gsub(" 한미", " 한국 미국 ", df$text)
df[, "text"] = gsub(" 한일", " 한국 일본 ", df$text)
df[, "text"] = gsub("한중일", " 한국 중국 일본 ", df$text)

#### __ ● 특수문자 ####
regextract(pattern = "[[:punct:]]", text = df$text) %>%
  unique() -> txt_punct
t(t(txt_punct))

df[, "text"] = gsub(pattern = "\\[.*?]|\\{.*?}|\\(.*?)|<.*?>",
                    replacement = "",
                    x = df$text)

df[, "text"] = gsub(pattern = "\\＜.*?＞|\\【.*?】",
                    replacement = "",
                    x = df$text)

punct_p = gregexpr(pattern = "▶", text = df$text)
punct_p = unlist(lapply(punct_p, function(x){return(x[1])}))
text_nchar = nchar(df$text)

df[, "text"] = ifelse(punct_p > text_nchar * 0.7, 
                      substr(df$text, start = 1, stop = punct_p - 1),
                      df$text)


# 기타 특수문자 확인
# grep(pattern = "〈", df$text)
# grep(pattern = "＜", df$text)
# grep(pattern = "「", df$text)
# grep(pattern = txt_punct[47], df$text)
# grep(pattern = "\\$", df$text)
# gregexpr(pattern = "▶", df$text[7])

df_punct_pairs = data.frame(punct = c("$", "↑"),
                            kr = c("달러", "상승"))

# for(n_pairs in 1:nrow(df_punct_pairs)){
#   cat(paste0("\r", "substitute: ", n_pairs))
#   df[, "text"] = gsub(pattern     = paste0("\\", df_punct_pairs[n_pairs, "punct"]),
#                        replacement = paste0(" ", df_punct_pairs[n_pairs, "kr"], " "),
#                        x = df[, "text"])
# }

df[, "text"] = gsub(pattern = "[[:punct:]]", replacement = " ", x = df$text)

#### __ ● 한자 ####
# 한자 확인
# text_cn = gsub(pattern = "[^一-龠]", replacement = "", df$text)
# text_cn = paste0(text_cn, collapse = "")
# text_cn = unlist(strsplit(text_cn, split = ""))
# text_cn = unique(text_cn)
# text_cn
# 
# for(n_cn in 1:length(text_cn)){
#   grep(pattern = text_cn[n_cn], df$text, value = TRUE) %>%
#     head(1) %>%
#     print()
#   print("-----")
# }

# 한자 치환
df_cn_pairs = data.frame(cn = c("印泥",	"印尼",	"露語",	"中企",	"韓流",	"直購",	"日本",	"印",	"中",	"韓",	"日",	"臺",	"美",	"英",	"亞",	"獨",	"伊",	"銀",	"株",	"兆",	"社",	"上",	"新",	"比",	"逆",	"食",	"政",	"業",	"女",	"門",	"乳",	"毒",	"勢",	"稅",	"價"),
                         kr = c("인도네시아",	"인도네시아",	"전화",	"중소기업",	"한류",	"직구",	"일본",	"인도",	"중국",	"한국",	"일본",	"대만",	"미국",	"영국",	"아시아",	"독일",	"이탈리아",	"은행",	"주식",	"조원",	"회사",	"상승",	"새로운",	"대비",	"역",	"식품",	"정부",	"업",	"여자",	"문",	"유",	"독",	"세력",	"세금",	"가격"))
df_cn_pairs

for(n_pairs in 1:nrow(df_cn_pairs)){
  cat(paste0("\r", "substitute: ", n_pairs))
  df[, "text"] = gsub(pattern     = df_cn_pairs[n_pairs, "cn"],
                      replacement = paste0(" ", df_cn_pairs[n_pairs, "kr"], " "),
                      x = df[, "text"])
}

# 나머지 한자 처리
df[, "text"] = gsub(pattern = "[一-龠]", replacement = " ", df$text)
df[1:20, "text"]


#### __ ● 기타 기호 ####
df[, "text"] = gsub(pattern = "[^0-9a-zA-Z가-힣 ]", replacement = " ", df$text)
df[1:20, "text"]

#### __ ● 소문자 ####
df[, "text"] = tolower(df$text) 

#### __ ● 고유 단어 확인 ####
word_unique = unique(unlist(strsplit(df$text, split = " ")))
length(word_unique)
word_unique[401:600]

# grep(pattern = "독신", x = df$text, value = TRUE)
# grep(pattern = "싱글", x = df$text, value = TRUE)
# grep(pattern = "ft ", x = df$text, value = TRUE)
# grep(pattern = "데이 ", x = df$text, value = TRUE)
# regextract(pattern = " .{1,8}데이 ", df$text) %>%
#   gsub(pattern = " ", replacement = "") %>%
#   unique()
# grep(pattern = "commerce", x = df$text, value = TRUE)
# grep(pattern = "커머스", x = df$text, value = TRUE)
# regextract(pattern = " [a-z가-힣]{1,5}커머스 ", df$text) %>%
#   gsub(pattern = " ", replacement = "") %>%
#   unique()
# 
# grep(pattern = "m커머스", x = df$text, value = TRUE)
# grep(pattern = "d커머스", x = df$text, value = TRUE)
# grep(pattern = "t커머스", x = df$text, value = TRUE)
# grep(pattern = "네모커머스", x = df$text, value = TRUE)
# 
# grep(pattern = "중기", x = df$text, value = TRUE)
# 
# grep(pattern = "티몰", x = df$text, value = TRUE)
# 
# grep(pattern = "[0-9]조 ", x = df$text, value = TRUE)
 
# 종목:  종목
# 엔터:  엔터테인먼트
# 독신자의 날: 싱글데이
# 솔로데이: 싱글데이
# 원데이:
# e commerce, e 커머스, e쇼핑몰, e 쇼핑몰: 이커머스
# v커머스: 브이커머스
# m커머스: 엠커머스
# d커머스: 디커머스
# t커머스: 티커머스
# ise커머스: 아이에스이커머스
# 네모커머스: 
# 중기: 중소기업
# 티몰글로벌: 티몰
# [0-9]조  -> [0-9]조원
# [0-9]억  -> [0-9]억원
# fta:  fta 

df[, "text"] = gsub(pattern = "종목 ", replacement = " 종목 ", df$text)
df[, "text"] = gsub(pattern = "엔터 ", replacement = " 엔터테인먼트 ", df$text)

df[, "text"] = gsub(pattern = "독신자의 날", replacement = " 싱글데이 ", df$text)
df[, "text"] = gsub(pattern = "솔로데이", replacement = " 솔로데이 ", df$text)
df[, "text"] = gsub(pattern = "원데이", replacement = "", df$text)

df[, "text"] = gsub(pattern = "e commerce|e 커머스|e쇼핑몰|e 쇼핑몰|전자상거래|전자 상거래", replacement = " 이커머스 ", df$text)
df[, "text"] = gsub(pattern = "v커머스", replacement = " 브이커머스 ", df$text)
df[, "text"] = gsub(pattern = "m커머스", replacement = " 엠커머스 ", df$text)
df[, "text"] = gsub(pattern = "d커머스", replacement = " 디커머스 ", df$text)
df[, "text"] = gsub(pattern = "t커머스", replacement = " 티커머스 ", df$text)
df[, "text"] = gsub(pattern = "ise커머스", replacement = " 아이에스이커머스 ", df$text)
df[, "text"] = gsub(pattern = "네모커머스", replacement = "", df$text)
df[, "text"] = gsub(pattern = "드림커머스", replacement = "", df$text)

df[, "text"] = gsub(pattern = "중기", replacement = " 중소기업 ", df$text)
df[, "text"] = gsub(pattern = "중진공", replacement = " 중소기업 진흥공단 ", df$text)

df[, "text"] = gsub(pattern = "중기", replacement = " 중소기업 ", df$text)

df[, "text"] = gsub(pattern = "티몰", replacement = " 티몰 ", df$text)

df[, "text"] = gsub(pattern = " fta", replacement = " fta ", df$text)

df[, "text"] = gsub(pattern = "매출", replacement = " 매출 금액 ", df$text)
df[, "text"] = gsub(pattern = "조원 |억원 |만원 ", replacement = " 금액 ", df$text)
df[, "text"] = gsub(pattern = "[0-9]{1}조 ", replacement = " 조원 금액 ", df$text)
df[, "text"] = gsub(pattern = "[0-9]{1}억 ", replacement = " 조원 금액 ", df$text)

df[, "text"] = gsub(pattern = " 페이|페이 ", replacement = " 페이 ", df$text)

df[, "text"] = gsub(pattern = " 기자 ", replacement = "", df$text)

#### __ ● 공백 제거 ####
df[, "text"] = gsub(pattern = " {1,}", replacement = " ", df$text)
df[, "text"] = gsub(pattern = "^ | $", replacement = "", df$text)
df[1:20, "text"]

# write.csv(df, "text_total_handled.csv", row.names = FALSE)
