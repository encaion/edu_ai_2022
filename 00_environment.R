#### packages ####
library("readxl")
library("dplyr")
library("data.table")
library("reshape2")

library("tm")
library("KoNLP")
library("topicmodels")

library("ggplot2")
library("wordcloud2")
library("qgraph")


# workcloud2 설치는 다음 코드 실행
# library("devtools")
# install_github("Lchiffon/wordcloud2")

#### options ####
options(stringsAsFactors = FALSE,
        scipen = 100)

#### etc ####
date_origin = "1899-12-30"

#### load functions ####
source("03_functions.R")
