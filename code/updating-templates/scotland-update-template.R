################################################################################
## SCOTLAND UPDATE TEMPLATE ####################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for Scotland for any year available in the file         ##
################################################################################
## Instructions (detailed instructions are in /docs/technical):
## 1. Manual input: input (update) the current year and the three .csv filenames 
##      you have saved into /data/01-raw
## 2. Run through the rest of the script that imports the data, cleans it,  
##      creates an .Rmd file and produces the compiled .pdf report.
## 3. Optional: you can now modify the .Rmd file in /code/report-rmds and 
##      compile the .pdf again using the last chunk of code in this script.
################################################################################
################################################################################

################################################################################
## MANUAL DATA INPUT ###########################################################
################################################################################
current.year <- 2016

## after dowloading the Wales files into the data/01-raw folder, enter their
## correct filenames here:
# wal.income.file <-"orig.wal.inc.17.csv"
# wal.expenditure.file <-"orig.wal.exp.17.csv"
# wal.transport.file <-"orig.wal.trans.17.csv"

## replace with date of access to data:
new.date.accessed <- "11.03.2019"

################################################################################
################################################################################
##                                                                            ##
##   THE REST OF THIS SCRIPT IS TO BE RUN ONLY -- NO MODIFICATIONS!           ##
##                                                                            ##
################################################################################
################################################################################
################################################################################
## LOAD PACKAGES AND DATA ######################################################
################################################################################
source("code/functions.R")
library(tidyr)
library(dplyr)
library(tibble)
library(RefManageR)
# load existing master file
master <- readRDS("data/03-processed/master.rds")
bib.master <- readRDS("data/03-processed/bib.master.rds")

################################################################################
## AUTOMATIC DATA IMPORT AND CLEANING 
################################################################################
path <- "data/01-raw/"
# # read all expenditure data, remove extra row and column
# wal.income.total <- read.csv(paste0(path, wal.income.file))[-1,-1]
# # read all income data, remove extra row and column
# wal.expend.total <- read.csv(paste0(path, wal.expenditure.file))[-1,-1]
# # read all transport total data, remove extra row and column
# wal.transport.total <- read.csv(paste0(path, wal.transport.file))[-1,-1]
# 
# # reshape all three dfs - you can ignore the warnigns here!
# wal.expend.total<- FunWalesReshape(wal.expend.total)
# wal.income.total<- FunWalesReshape(wal.income.total)
# wal.transport.total<- FunWalesReshape(wal.transport.total)
# 
# # join them together and calculate surplus
# wal.expend.total %>% 
#   left_join(wal.income.total) %>% 
#   left_join(wal.transport.total) %>% 
#   mutate(income.total = -income.total,
#          surplus.total = income.total - expend.total) %>% 
#   filter(year == current.year) -> update
# 
# # add Wales specific data
# update %>% 
#   mutate(country = "Wales",
#          auth.type = "LA")  -> update
# 
# # double check the update is OK:
# if (nrow(update) != 22) {
#   paste("Something is wrong. The update should have 22 rows, but it has",
#         nrow(update), "instead.")} else {
#           "Everything checks out, the update has 22 rows"}

################################################################################
## Add (or overwrite) new rows to master #######################################
################################################################################

# # add update for Wales - if that year already exists, it will be overwritten!!!
# master %>% 
#   anti_join(update, by = c("country", "auth.name", "year")) %>% 
#   bind_rows(update) -> master

# add new date.accessed to bibliography master
bib.master %>% 
  mutate(urldate = ifelse(country == "Scotland", new.date.accessed,
                                urldate)) -> bib.master

# # save updated datafile to master
# saveRDS(master, "data/03-processed/master.rds")

# save updated datafile to master
saveRDS(bib.master, "data/03-processed/bib.master.rds")

# new report name
report.name <- paste0("scotland-report-", current.year, "-",
                      current.year - 1999)

# create a copy of the wales report template
file.copy("code/report-templates/scotland-report-template.Rmd",
          paste0("code/report-rmds/", report.name, ".Rmd"),
          overwrite = TRUE)

# select a bibliography for the wscotland report - only rows needed
bib.master %>% 
  filter(year > current.year - 5, !type %in% c("budget")) %>% 
  mutate(refs = paste0("@", key)) %>% 
  column_to_rownames("key") %>% 
  select(-type) -> bib.master

# create bib file
bib.master %>% 
  as.BibEntry() %>% 
  WriteBib(file = "code/report-rmds/scotland.bib", 
           biblatex = FALSE, verbose = FALSE)

# also save the data.frame
saveRDS(bib.master, paste0("data/03-processed/", report.name, ".rds"))

################################################################################
## COMPILE REPORT -  THIS IS THE ONLY PART OF THE SCRIPT THAT CAN BE RE-RUN   ##
################################################################################
# # compile the report - you can repeat this as many times as you like after 
# # updating the .Rmd file 
# rmarkdown::render(paste0("code/report-rmds/", report.name, ".Rmd"),
#                   output_file = paste0(report.name, ".pdf"),
#                   output_dir = "outputs/reports",
#                   params = list(current.year = current.year))
# 
# # remove empty folder that the compilation creates
# unlink(paste0("outputs/reports/", report.name, "_files"), recursive=TRUE)
# 
# # the report are saved to /outputs/reports/
################################################################################
################################################################################