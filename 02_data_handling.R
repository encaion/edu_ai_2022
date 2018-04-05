#### 날짜 결측치 처리 ####
df = fread("text_total.csv", data.table = FALSE, encoding = "UTF-8")
df = df[-c(236, 7453), ]
head(df)

#### 제목 처리 ####
df[1:20, "title"]

#### __ ● 국가 처리 ####
grep(pattern = "한[[:punct:]]{1}중", df$title, value = TRUE)
grep(pattern = "중[[:punct:]]{1}일", df$title, value = TRUE)
grep(pattern = " 일 ", df$title, value = TRUE)
grep(pattern = "한[[:punct:]]{1}미", df$title, value = TRUE)
grep(pattern = "중[[:punct:]]{1}미", df$title, value = TRUE)
grep(pattern = "한\\ㆍ", df$title, value = TRUE)
grep(pattern = "인니", df$title, value = TRUE)

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
txt_punct = regextract(pattern = "[[:punct:]]", text = df$title) %>%
  unique()

df[, "title"] = gsub(pattern = "\\[.*?\\]|\\{.*?\\}|\\(.*?\\)|<.*?>",
                     replacement = "",
                     x = df$title)

t(t(txt_punct))

# 기타 특수문자 확인
grep(pattern = "〈", df$title, value = TRUE)
grep(pattern = "「", df$title, value = TRUE)
grep(pattern = txt_punct[47], df$title, value = TRUE)
grep(pattern = "\\$", df$title, value = TRUE)


df_punct_pairs = data.frame(punct = c("$", "↑"),
                            kr = c("달러", "상승"))

for(n_pairs in 1:nrow(df_punct_pairs)){
  cat(paste0("\r", "substitute: ", n_pairs))
  df[, "title"] = gsub(pattern     = paste0("\\", df_punct_pairs[n_pairs, "punct"]),
                       replacement = paste0(" ", df_punct_pairs[n_pairs, "kr"], " "),
                       x = df[, "title"])
}

df[, "title"] = gsub(pattern = "[[:punct:]]", replacement = " ", x = df$title)

#### __ ● 한자 ####
# 한자 확인
title_cn = gsub(pattern = "[^一-龠]", replacement = "", df$title)
title_cn = paste0(title_cn, collapse = "")
title_cn = unlist(strsplit(title_cn, split = ""))
title_cn = unique(title_cn)
title_cn

for(n_cn in 1:length(title_cn)){
  grep(pattern = title_cn[n_cn], df$title, value = TRUE) %>%
    head(2) %>%
    print()
  print("-----")
}

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

#### __ ● 공백 제거 ####
df[, "title"] = gsub(pattern = " {1,}", replacement = " ", df$title)
df[, "title"] = gsub(pattern = "^ | $", replacement = "", df$title)
df[1:20, "title"]

#### __ ● 소문자 ####
df[, "title"] = tolower(df$title) 

#### __ ● 고유 단어 확인 ####
word_unique = unique(unlist(strsplit(df$title, split = " ")))
length(word_unique)
word_unique[201:400]

grep(pattern = "독신", x = df$title, value = TRUE)
grep(pattern = "싱글", x = df$title, value = TRUE)

# 종목:  종목
# 엔터:  엔터테인먼트
# 독신자의 날: 싱글데이
# e commerse, e 커머스, e쇼핑몰, e 쇼핑몰: 이커머스
# 중기: 중소기업
# 티몰글로벌: 티몰
# [0-9]조  -> [0-9]조원
# [0-9]억  -> [0-9]억원
# fta:  fta 

# 5. 쓸모없는 띄어쓰기를 제거한다. (두 칸, 처음/마지막 띄어쓰기)
# 6. 단어만 추출하여 그 목록을 확인하고 추가 처리가 필요한 단어를 선별한다.