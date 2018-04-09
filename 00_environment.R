#### packages ####
library("readxl")
library("dplyr")
library("data.table")
# library("lubridate")

library("tm")
library("KoNLP")
library("topicmodels")

#### options ####
options(stringsAsFactors = FALSE,
        scipen = 100)

#### etc ####
date_origin = "1899-12-30"

#### load functions ####
source("03_functions.R")