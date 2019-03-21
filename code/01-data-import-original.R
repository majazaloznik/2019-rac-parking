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
library(readxl)
library(tabulizer)
options(stringsAsFactors = FALSE)
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
# load metadta
scotland.i.e.17.18 <- readRDS("data/02-interim/scotland.i.e.17.18.rds")

# function for extracting income and expenditure from relevant cells
FunScotlandLACels <- function(file, sheet, expend.total, income.total, auth.cell) {
  auth.name <- colnames(read_excel(file, sheet, auth.cell))
  expend.total <- colnames(read_excel(file, sheet, expend.total))
  income.total <- colnames(read_excel(file, sheet, income.total))
  c(auth.name, expend.total, income.total)
}
 

# Function that loops through all sheets and extracts relevant cell data 
FunScotandLoopIE <- function(row) {
  # get metadata for this year
  year = scotland.i.e.17.18$year[row]
  file.name = scotland.i.e.17.18$file.name[row]
  start.sh =  scotland.i.e.17.18$start.sh[row]
  end.sh =  scotland.i.e.17.18$end.sh[row]
  exp.cell =  scotland.i.e.17.18$exp.cell[row]
  inc.cell =  scotland.i.e.17.18$inc.cell[row]
  auth.cell = scotland.i.e.17.18$auth.cell[row]

  # prepare empty data frame for the data
  df <- data.frame(auth.name = character(),
                   expend.total = character(),
                   income.total = character())
  
  # loop through all the sheets
  for (sheet in start.sh:end.sh){
    x <- FunScotlandLACels(file.name, sheet, exp.cell, inc.cell, auth.cell)
    names(x) <- colnames(df)
    if(year == 2016) 
      x[1] <- gsub("^.+?, |, 2016-17", "", x)
    df <- bind_rows(df, x)
  }
  
  # change values to numeric type and add the year variable
  df %>% 
    mutate(expend.total = as.numeric(expend.total),
           income.total = as.numeric(income.total)) -> df
  df$year <- year
  df
}

# prepare empty data frame for the final data
original.scotland.i.e <- data.frame(auth.name = character(),
                 expend.total = numeric(),
                 income.total = numeric(),
                 year = numeric())

for (row in 2:nrow(scotland.i.e.17.18)){
  x <- FunScotandLoopIE(row)
  original.scotland.i.e <- bind_rows(original.scotland.i.e, x)
}

original.scotland.i.e %>% 
  mutate(country = "Scotland",
         auth.type = "LA" ,
         wpl.logical = FALSE)  -> original.scotland.i.e

# merge with original data
bind_rows(original.data, original.scotland.i.e) -> original.data 


## 2.2 SCOTLAND penalty notice charges #########################################
# load meta data
scotland.pnc.17.18 <- readRDS("data/02-interim/scotland.pnc.17.18.rds")

# extract DPE type table
scotland.pnc.dpe.16 <- extract_tables(here::here(scotland.pnc.17.18$file.name[4]), 
                    pages = scotland.pnc.17.18$dpe.tab[4])

# turn into data.frame, remove header row and add column names
scotland.pnc.dpe.16 <- data.frame(scotland.pnc.dpe.16[[1]])[-1,]
colnames(scotland.pnc.dpe.16) <- c("dpe.now", "dpe.soon", "dpe.not")

# regex the first column to get out year of introduction
scotland.pnc.dpe.16 %>% 
  separate(dpe.now, into = c("dpe.now", "year"), sep = "\\(")  %>% 
  separate(year, into = c("year", "x"), sep = "\\)") %>% 
  separate(dpe.now, into = c("dpe.now", "x"), sep = "\\\r") %>% 
  select( -x) %>% 
  select(year, dpe.now, dpe.soon, dpe.not) -> x

# now gather 

x %>% 
  gather(key = dpe.status, value = la)



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
