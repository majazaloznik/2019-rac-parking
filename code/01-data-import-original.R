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
source("code/functions.R")

## 0. MASTER TABLE SETUP #######################################################
original.data <- data.frame(country = character(),
                            year = integer(),
                            auth.type = character(),
                            auth.name = character(),
                            income.on = integer(),
                            income.off = integer(),
                            income.pcn = integer(),
                            income.tfs = integer(),
                            income.cong.ch = integer(),
                            income.total = integer(),
                            expend.on = integer(),
                            expend.off = integer(),
                            expend.tfs = integer(),
                            expend.cong.ch = integer(),
                            expend.total = integer(),
                            surplus.budget = integer(),
                            transport.total = integer(),
                            wpl.logical = logical(),
                            dpe.status = character(),
                            dpe.year = integer(),
                            pcn.number = integer(),
                            pcn.total = integer())
                            

## 1. ENGLAND DATA IMPORT ######################################################
## 1.1 ENGLAND outturn data ####################################################
# load metadta
england.outturn.17.18 <- readRDS("data/02-interim/england.outturn.17.18.rds")

england.outturn <- data.frame()
# # extract on and off street parking expenditures 
for (row in 2:nrow(england.outturn.17.18)){
  file = england.outturn.17.18$file.name[row]
  first = england.outturn.17.18$first[row]
  last = england.outturn.17.18$rows[row] + england.outturn.17.18$first[row] - 1
  auth.name = england.outturn.17.18$la.name[row]
  auth.type = england.outturn.17.18$la.type[row]
  e.sh = england.outturn.17.18$e.sh[row]
  e.on = england.outturn.17.18$e.on[row]
  e.off =  england.outturn.17.18$e.off[row]
  e.cc =  england.outturn.17.18$e.cc[row]
  i.sh = england.outturn.17.18$i.sh[row]
  i.on = england.outturn.17.18$i.on[row]
  i.off =  england.outturn.17.18$i.off[row]
  i.cc =  england.outturn.17.18$i.cc[row]
  pen.sh = england.outturn.17.18$pen.sh[row]
  pen.on = england.outturn.17.18$pen.on[row]
  year = england.outturn.17.18$year[row]
  country = "England"
  x <- FunEnglandOutturn(file, first, last, e.sh, e.on, e.off, e.cc,
                         i.sh, i.on, i.off,i.cc, pen.sh, pen.on,
                         auth.name, auth.type, year,
                         country)
  england.outturn <- bind_rows(england.outturn, x)
}

# remove authorities we're not interested in:
england.outturn %>% 
  filter(auth.type != "O" | 
           grepl("National Park", auth.name) |
           auth.name == "Greater London Authority") -> england.outturn

## 1.1.1. ENGLAND outturn totals ###############################################
# initialise data frame
england.outturn.totals <- data.frame()

# loop through excel files extracting two cells each time
for (row in 2:nrow(england.outturn.17.18)){
  file = england.outturn.17.18$file.name[row]
  tot.1 = england.outturn.17.18$tot.1[row]
  pen.1 = england.outturn.17.18$pen.1[row]
  year = england.outturn.17.18$year[row]
  x <- FunEnglandOutturnTotals(file, transport.total = tot.1,
                               pcn.total = pen.1, year)
  england.outturn.totals <- bind_rows(england.outturn.totals, x)
}
england.outturn.totals$auth.name <- "England"
england.outturn.totals$country <- "England"
england.outturn.totals$auth.type <- "X"

## 1.2 ENGLAND budget data #####################################################





bind_rows(original.data, england.outturn.totals) -> original.data 
bind_rows(original.data, england.outturn) -> original.data 



## 2. SCOTLAND DATA IMPORT #####################################################
## 2.1 SCOTLAND incomes and expenditures #######################################
# load metadta
scotland.i.e.17.18 <- readRDS("data/02-interim/scotland.i.e.17.18.rds")

# prepare empty data frame for the final data
original.scotland.i.e <- data.frame(auth.name = character(),
                 expend.total = numeric(),
                 income.total = numeric(),
                 year = numeric())

# loop through each excel file running FunScotandLoopIE which loops
# through each sheet and extracts income and expenditure data
for (row in 2:nrow(scotland.i.e.17.18)){
  # get metadata for this year
  year = scotland.i.e.17.18$year[row]
  file.name = scotland.i.e.17.18$file.name[row]
  start.sh =  scotland.i.e.17.18$start.sh[row]
  end.sh =  scotland.i.e.17.18$end.sh[row]
  exp.cell =  scotland.i.e.17.18$exp.cell[row]
  inc.cell =  scotland.i.e.17.18$inc.cell[row]
  auth.cell = scotland.i.e.17.18$auth.cell[row]
  x <- FunScotandLoopIE(row)
  original.scotland.i.e <- bind_rows(original.scotland.i.e, x)
}

## 2.2 SCOTLAND PCN data from pdf ##############################################
# load meta data
scotland.pdf.17.18 <- readRDS("data/02-interim/scotland.pnc.17.18.rds")

## 2.2.1 Scotland DPE table ####################################################
# extract DPE type table for 2016/17
scotland.dpe.16 <- extract_tables(here::here(scotland.pdf.17.18$file.name[4]), 
                    pages = scotland.pdf.17.18$dpe.tab[4])

# extract DPE type table for 2017/18
scotland.dpe.17 <- extract_tables(here::here(scotland.pdf.17.18$file.name[5]), 
                                      pages = scotland.pdf.17.18$dpe.tab[5])

# clean DPE type table for 2016/17 # don't worry about the warnings
scotland.dpe.16 <- FunScotlandDPE(scotland.dpe.16, 2016)

# clean DPE type table for 2017/18 # don't worry about the warnings
scotland.dpe.17 <- FunScotlandDPE(scotland.dpe.17, 2017)

## 2.2.2 Scotland PNC table ###################################################

# extract PCN type table for 14/15, 15/16, 16/17 directly into a data.frame
scotland.pcn.14.15.16 <- extract_tables(here::here(scotland.pdf.17.18$file.name[4]), 
                                      pages = scotland.pdf.17.18$pcn.tab[4],
                     output = "data.frame")[[1]]

# extract PCN type table for 14/15, 15/16, 16/17 directly into a data.frame
scotland.pcn.17 <- extract_tables(here::here(scotland.pdf.17.18$file.name[5]), 
                                        pages = scotland.pdf.17.18$pcn.tab[5],
                                        output = "data.frame")[[1]]

# clean PCN tables for 14/15, 15/16, 16/17
scotland.pcn.14 <- FunScotlandPCN(scotland.pcn.14.15.16, 2014)
scotland.pcn.15 <- FunScotlandPCN(scotland.pcn.14.15.16, 2015)
scotland.pcn.16 <- FunScotlandPCN(scotland.pcn.14.15.16, 2016)

# clean PCN tables for 17/18
scotland.pcn.17 <- FunScotlandPCN(scotland.pcn.17, 2017)

## 2.2.3 Scotland PNC income table #############################################

# extract PCN type table for 16/17 directly into a data.frame
scotland.tfs.i.e.16 <- extract_tables(here::here(scotland.pdf.17.18$file.name[4]), 
                                        pages = scotland.pdf.17.18$e.i.tab[4],
                                        output = "data.frame")[[1]]

# extract PCN type table for 17/18 directly into a data.frame
scotland.tfs.i.e.17 <- extract_tables(here::here(scotland.pdf.17.18$file.name[5]), 
                                      pages = scotland.pdf.17.18$e.i.tab[5],
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
         auth.type = "LA" ,
         wpl.logical = FALSE)  -> original.scotland

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
         auth.type = "LA" ,
         wpl.logical = FALSE)  -> original.wales

# merge with original data
bind_rows(original.data, original.wales) -> original.data 

write.csv(original.data, "data/03-processed/original.data.csv")
rm(list=setdiff(ls(), "original.data"))