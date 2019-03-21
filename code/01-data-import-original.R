################################################################################
## IMPORT OF ORIGINAL DATA - AVAILABLE MARCH 2019 ##############################
################################################################################
## This file extracts and merges all available data for all three countries as
## it stands at the start of the project. 
## Additional data is manually added as it becomes available in the files 
## 02-data-import-new-sco/eng/wal.R 
################################################################################
## DO NOT MODIFY THIS FILE & DO NOT SOURCE THIS FILE
################################################################################
## INPUTS:
## Outputs of 00-data-sources.R (five .rds files with metadata info)
## Manually downloaded excel, csv and pdf files in data/01-raw/orig.*.*
################################################################################
## 0. MASTER TABLE SETUP #######################################################
## 1. ENGLAND DATA IMPORT ######################################################
## 1.1 ENGLAND outturn data ####################################################
## 1.2 ENGLAND budget data #####################################################
## 2. SCOTLAND DATA IMPORT #####################################################
## 2.1 SCOTLAND incomes and expenditures #######################################
## 2.2 SCOTLAND penalty notice charges #########################################
## 3. WALES DATA IMPORT ########################################################
################################################################################

library(tidyr)
library(dplyr)
## 0. MASTER TABLE SETUP #######################################################
original.data <- data.frame(country = character(),
                            year = integer(),
                            auth.type = character(),
                            auth.name = character(),
                            income.on = integer(),
                            income.off = integer(),
                            income.pcn = integer(),
                            income.cong.ch = integer(),
                            income.total = integer(),
                            expend.on = integer(),
                            expend.off = integer(),
                            expend.cong.ch = integer(),
                            expend.total = integer(),
                            surplus.budget = integer(),
                            transport.total = integer(),
                            wpl.logical = logical())
                            

## 1. ENGLAND DATA IMPORT ######################################################
## 1.1 ENGLAND outturn data ####################################################
## 1.2 ENGLAND budget data #####################################################
## 2. SCOTLAND DATA IMPORT #####################################################
## 2.1 SCOTLAND incomes and expenditures #######################################




## 2.2 SCOTLAND penalty notice charges #########################################
## 3. WALES DATA IMPORT ########################################################
# read all expenditure data, remove extra row and column
wal.expend.total <- read.csv("data/01-raw/orig.wal-exp-17-18.csv")[-1,-1]
# read all income data, remove extra row and column
wal.income.total <- read.csv("data/01-raw/orig.wal-inc-17-18.csv")[-1,-1]
# read all transport total data, remove extra row and column
wal.transport.total <- read.csv("data/01-raw/orig.wal-trans-17-18.csv")[-1,-1]

# reshape and clean function 
FunWalesReshape <- function(DF) {
  var <- deparse(substitute(DF))
  var <- substring(var, 5)
  DF %>% 
    gather(key = year, value = !!var, 2:(ncol(DF))) %>% 
    separate(year, into = c("X", "year"), sep = "(?<=[A-Z])(?=[0-9])", perl = TRUE) %>% 
    separate (year, into = c("year", "XX")) %>% 
    mutate(year = as.integer(year)) %>% 
    select(-X, -XX) %>% 
    rename(auth.name = X.1)
}

# reshape all three dfs
wal.expend.total<- FunWalesReshape(wal.expend.total)
wal.income.total<- FunWalesReshape(wal.income.total)
wal.transport.total<- FunWalesReshape(wal.transport.total)

# join them together. 
wal.expend.total %>% 
  left_join(wal.income.total) %>% 
  left_join(wal.transport.total) -> original.wales

# add Wales specific data
original.wales %>% 
  mutate(country = "Wales",
         auth.type = "LA" ,
         wpl.logical = FALSE)  -> original.wales

# merge with original data
bind_rows(original.data, original.wales) -> original.data 
