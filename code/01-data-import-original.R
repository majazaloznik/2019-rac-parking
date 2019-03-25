################################################################################
## IMPORT OF ORIGINAL DATA - AVAILABLE MARCH 2019 ##############################
################################################################################
## This file extracts and merges all available data for all three countries as
## it stands at the start of the project. 
################################################################################
## DO NOT MODIFY THIS FILE & DO NOT SOURCE THIS FILE
################################################################################
## INPUTS:
## Outputs of 00-data-sources.R (seven .rds files with metadata info and lookup
## tables for names all in data/01-raw)
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
source("code/functions.R")

# import manually input data - metadata and name lookup tables
myfiles <- list.files(path = "data/01-raw", pattern =  "^orig.*\\.rds", full.names = TRUE)
lapply(myfiles, function(x){
  assign(gsub("\\.rds$", "" , basename(x)), readRDS(x), inherits = TRUE, pos = 1)})

## 0. MASTER TABLE SETUP #######################################################
original.data <- data.frame(country = character(),
                            year = integer(),
                            auth.type = character(),
                            auth.name = character(),
                            income.on = integer(),
                            income.off = integer(),
                            income.pcn = integer(),
                            income.tfs = integer(),
                            income.wpl = integer(),
                            income.cong.ch = integer(),
                            income.total = integer(),
                            expend.on = integer(),
                            expend.off = integer(),
                            expend.tfs = integer(),
                            expend.wpl = integer(),
                            expend.cong.ch = integer(),
                            expend.total = integer(),
                            surplus.budget = integer(),
                            transport.total = integer(),
                            dpe.status = character(),
                            dpe.year = integer(),
                            pcn.number = integer())
                            

## 1. ENGLAND DATA IMPORT ######################################################
## 1.1 ENGLAND outturn data ####################################################
# intitalise data.frame
england.outturn <- data.frame()

# # extract on and off street parking expenditures 
for (row in 2:nrow(orig.eng.meta.outturn.17)){
  file = orig.eng.meta.outturn.17$file.name[row]
  first = orig.eng.meta.outturn.17$first[row]
  last = orig.eng.meta.outturn.17$rows[row] + orig.eng.meta.outturn.17$first[row] - 1
  auth.name = orig.eng.meta.outturn.17$la.name[row]
  auth.type = orig.eng.meta.outturn.17$la.type[row]
  e.sh = orig.eng.meta.outturn.17$e.sh[row]
  e.on = orig.eng.meta.outturn.17$e.on[row]
  e.off =  orig.eng.meta.outturn.17$e.off[row]
  e.cc =  orig.eng.meta.outturn.17$e.cc[row]
  i.sh = orig.eng.meta.outturn.17$i.sh[row]
  i.on = orig.eng.meta.outturn.17$i.on[row]
  i.off =  orig.eng.meta.outturn.17$i.off[row]
  i.cc =  orig.eng.meta.outturn.17$i.cc[row]
  pen.sh = orig.eng.meta.outturn.17$pen.sh[row]
  pen.on = orig.eng.meta.outturn.17$pen.on[row]
  year = orig.eng.meta.outturn.17$year[row]
  x <- FunEnglandOutturn(file, first, last, e.sh, e.on, e.off, e.cc,
                         i.sh, i.on, i.off,i.cc, pen.sh, pen.on,
                         auth.name, auth.type, year)
  england.outturn <- bind_rows(england.outturn, x)
}

## 1.1.1. ENGLAND outturn totals ###############################################
# initialise data frame
england.outturn.totals <- data.frame()

# loop through excel files extracting two cells each time
for (row in 2:nrow(orig.eng.meta.outturn.17)){
  file = orig.eng.meta.outturn.17$file.name[row]
  tot.1 = orig.eng.meta.outturn.17$tot.1[row]
  pen.1 = orig.eng.meta.outturn.17$pen.1[row]
  year = orig.eng.meta.outturn.17$year[row]
  x <- FunEnglandOutturnTotals(file, transport.total = tot.1,
                               income.pcn = pen.1, year)
  england.outturn.totals <- bind_rows(england.outturn.totals, x)
}
england.outturn.totals$auth.name <- "England"
england.outturn.totals$auth.type <- "X"

## 1.2 ENGLAND budget data #####################################################
# initialise data frame
england.budget <- data.frame()

# loop through all excel files extracting budget surplus data
for (row in 3:nrow(orig.eng.meta.budget.18)){
  file = orig.eng.meta.budget.18$file.name[row]
  first = orig.eng.meta.budget.18$first[row]
  last = orig.eng.meta.budget.18$rows[row] + orig.eng.meta.budget.18$first[row] - 1
  auth.name = orig.eng.meta.budget.18$la.name[row]
  auth.type = orig.eng.meta.budget.18$la.type[row]
  sheet = 3
  budg.la = orig.eng.meta.budget.18$budg.la[row]
  year = orig.eng.meta.budget.18$year[row]
  x <- FunEnglandBudget(file, first, last, sheet,
                        budg.la,auth.name,
                        auth.type, year)
  england.budget <- bind_rows(england.budget, x)
}


## 2.3. MERGE all england data together ########################################
# remove authorities we're not interested in:
england.outturn %>% 
  filter(auth.type != "O" | 
           grepl("National Park", auth.name) |
           auth.name == "Greater London Authority") -> england.outturn

england.budget %>% 
  filter(auth.type != "O" | 
           grepl("National Park", auth.name) |
           auth.name == "Greater London Authority") -> england.budget

full_join(england.outturn, england.budget) -> england.outturn.and.budget

full_join(england.outturn.and.budget, orig.eng.nott.wpl.17) -> england.outturn.and.budget

bind_rows(original.data, england.outturn.totals) -> original.data 
bind_rows(original.data, england.outturn.and.budget) -> original.data 
original.data$country <- "England"



## 2. SCOTLAND DATA IMPORT #####################################################
## 2.1 SCOTLAND incomes and expenditures #######################################
# prepare empty data frame for the final data
original.scotland.i.e <- data.frame()

# loop through each excel file running FunScotlandLoopIE which loops
# through each sheet and extracts income and expenditure data
for (row in 2:nrow(orig.sco.meta.i.e.17)){
  # get metadata for this year
  year = orig.sco.meta.i.e.17$year[row]
  file.name = orig.sco.meta.i.e.17$file.name[row]
  start.sh =  orig.sco.meta.i.e.17$start.sh[row]
  end.sh =  orig.sco.meta.i.e.17$end.sh[row]
  exp.cell =  orig.sco.meta.i.e.17$exp.cell[row]
  inc.cell =  orig.sco.meta.i.e.17$inc.cell[row]
  auth.cell = orig.sco.meta.i.e.17$auth.cell[row]
  x <- FunScotlandLoopIE(row, year, file.name, 
                         start.sh, end.sh, 
                         exp.cell, inc.cell,  auth.cell)
  original.scotland.i.e <- bind_rows(original.scotland.i.e, x)
}

## 2.2 SCOTLAND PCN data from pdf ##############################################

## 2.2.1 Scotland DPE table ####################################################
# extract DPE type table for 2016/17
scotland.dpe.16 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[4]), 
                    pages = orig.sco.meta.pdf.17$dpe.tab[4])

# extract DPE type table for 2017/18
scotland.dpe.17 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[5]), 
                                      pages = orig.sco.meta.pdf.17$dpe.tab[5])

# clean DPE type table for 2016/17 # don't worry about the warnings
scotland.dpe.16 <- FunScotlandDPE(scotland.dpe.16, 2016)

# clean DPE type table for 2017/18 # don't worry about the warnings
scotland.dpe.17 <- FunScotlandDPE(scotland.dpe.17, 2017)

## 2.2.2 Scotland PCN table ###################################################

# extract PCN type table for 14/15, 15/16, 16/17 directly into a data.frame
scotland.pcn.14.15.16 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[4]), 
                                      pages = orig.sco.meta.pdf.17$pcn.tab[4],
                     output = "data.frame")[[1]]

# extract PCN type table for 14/15, 15/16, 16/17 directly into a data.frame
scotland.pcn.17 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[5]), 
                                        pages = orig.sco.meta.pdf.17$pcn.tab[5],
                                        output = "data.frame")[[1]]

# clean PCN tables for 14/15, 15/16, 16/17
scotland.pcn.14 <- FunScotlandPCN(scotland.pcn.14.15.16, 2014)
scotland.pcn.15 <- FunScotlandPCN(scotland.pcn.14.15.16, 2015)
scotland.pcn.16 <- FunScotlandPCN(scotland.pcn.14.15.16, 2016)

# clean PCN tables for 17/18
scotland.pcn.17 <- FunScotlandPCN(scotland.pcn.17, 2017)

## 2.2.3 Scotland PCN income table #############################################

# extract PCN type table for 16/17 directly into a data.frame
scotland.tfs.i.e.16 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[4]), 
                                        pages = orig.sco.meta.pdf.17$e.i.tab[4],
                                        output = "data.frame")[[1]]

# extract PCN type table for 17/18 directly into a data.frame
scotland.tfs.i.e.17 <- extract_tables(here::here(orig.sco.meta.pdf.17$file.name[5]), 
                                      pages = orig.sco.meta.pdf.17$e.i.tab[5],
                                      output = "data.frame")[[1]]


# clean TFS income expenditure tables for 16/17
scotland.tfs.i.e.16 <- FunScotlandTFSIE(scotland.tfs.i.e.16, 2016)

# clean TFS income expenditure tables for 17/18
scotland.tfs.i.e.17 <- FunScotlandTFSIE(scotland.tfs.i.e.17, 2017)

## 2.3. Merge all Scotland data together #######################################

# first merge by year
scotland.pdf.14 <- scotland.pcn.14
scotland.pdf.15 <- scotland.pcn.15
scotland.pdf.16 <- full_join(full_join(scotland.dpe.16,
                                       scotland.pcn.16,by = c("auth.name", "year")),
                             scotland.tfs.i.e.16,  by = c("auth.name", "year"))

scotland.pdf.17 <- full_join(full_join(scotland.dpe.17,
                                       scotland.pcn.17,by = c("auth.name", "year")),
                             scotland.tfs.i.e.17,  by = c("auth.name", "year"))

# now row bind all pdf data together
original.scotland.pdf <- bind_rows(scotland.pdf.14,
                                   scotland.pdf.15,
                                   scotland.pdf.16,
                                   scotland.pdf.17)


# now merge income exp data from the Excel files with the pdf data

original.scotland <- full_join(original.scotland.i.e, original.scotland.pdf)

original.scotland %>% 
  mutate(country = "Scotland",
         auth.type = "LA" )  -> original.scotland

# merge with original data
bind_rows(original.data, original.scotland) -> original.data 

## 3. WALES DATA IMPORT ########################################################
# read all expenditure data, remove extra row and column
wal.expend.total <- read.csv("data/01-raw/orig.wal-exp-17-18.csv")[-1,-1]
# read all income data, remove extra row and column
wal.income.total <- read.csv("data/01-raw/orig.wal-inc-17-18.csv")[-1,-1]
# read all transport total data, remove extra row and column
wal.transport.total <- read.csv("data/01-raw/orig.wal-trans-17-18.csv")[-1,-1]

# reshape all three dfs
wal.expend.total<- FunWalesReshape(wal.expend.total)
wal.income.total<- FunWalesReshape(wal.income.total)
wal.transport.total<- FunWalesReshape(wal.transport.total)

# join them together. 
wal.expend.total %>% 
  left_join(wal.income.total) %>% 
  left_join(wal.transport.total) %>% 
  mutate(income.total = -income.total) -> original.wales

# add Wales specific data
original.wales %>% 
  mutate(country = "Wales",
         auth.type = "LA")  -> original.wales

# merge with original data
bind_rows(original.data, original.wales) -> original.data 

write.csv(original.data, "data/02-interim/original.data.csv")
saveRDS(original.data, "data/02-interim/original.data.rds")



rm(list=setdiff(ls(), "original.data"))