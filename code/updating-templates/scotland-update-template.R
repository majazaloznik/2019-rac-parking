################################################################################
## SCOTLAND UPDATE TEMPLATE ####################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for Scotland for any year available in the file         ##
################################################################################
## Instructions (detailed instructions are in /docs/technical):
## 1. Manual input: input (update) the current year and the required metadata
##      for all the files you've downloaded into data/01-raw
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

# number of decimal places in text and tables:
dp.text <- 1
dp.tables <- 2

## after dowloading the Scotland files into the data/01-raw folder, enter their
## metadata here:

# Scottish Local Government Finance Statistics (Income and Expenditure data)####
################################################################################
# title as it will appear in the references:
sco.i.e.title <- "Scottish Local Government Finance Statistics 2016-17 Annex A by LA"

# url of the file:
sco.i.e.url <- "https://www2.gov.scot/Resource/0053/00536018.xlsx"

# year published, as it will appear in the references:
sco.i.e.year.published <- 2018

## replace with date of access to data:
sco.i.e.date.accessed <- "13.03.2019"

# path and name of file where you have saved it:
sco.i.e.file <- "data/01-raw/orig.sco-16-17.xlsx"

# which sheet has the first LA (Aberdeen city) on it?
sco.i.e.start.sh <- 2

# which sheet has the last LA (West Lothian) on it?
sco.i.e.end.sh <- 33

# which cell has the Parking "Gross Expenditure on a Funding Basis"?
sco.i.e.exp.cell <- "B41"

# which cell has the Parking "Gross Income on a Funding Basis"?
sco.i.e.inc.cell <- "C41"

# which cell has the Total Roads and Transport "Gross Expenditure on a Funding Basis"?
sco.i.e.transp.cell <- "B34"

# On individual LA sheets, which cell has the name of the LA?
sco.i.e.auth.cell <- "A1"

# Aberdeen city - separate data source if available? ###########################
################################################################################
# title as it will appear in the references:
sco.aberdeen.title <- ""

# url of the file:
sco.aberdeen.url <- ""

# year published, as it will appear in the references:
sco.aberdeen.year.published <- NA

## replace with date of access to data:
sco.aberdeen.date.accessed <- ""

# How much parking Income did Aberdedn City report this year??
sco.aberdeen.income.total <- 0

# How much parking Expenditure did Aberdedn City report this year??
sco.aberdeen.expend.total <- 0

# How much total transport expenditure did Aberdedn City report this year??
sco.aberdeen.transport.total <- 0



# Transport Scotland (PCN) data  ###############################################
################################################################################
# title as it will appear in the references:
sco.pdf.title <- "Decriminalised Parking Enforcement: Local Authorites’ Income
and Expenditure: 2016 to 2017"

# url of the file:
sco.pdf.url <- "https://www.transport.gov.scot/publication/
decriminalised-parking-enforcement-local-authorities-income-and-expenditure-2016-to-2017/"

# year published, as it will appear in the references:
sco.pdf.year.published <- 2017

## replace with date of access to data:
sco.pdf.date.accessed <- "13.03.2019"

# path and name of file where you have saved it:
sco.pdf.file <- "data/01-raw/orig.sco-16-17-pcn.pdf"

# which page in the report is the DPE table on?
sco.pdf.dpe.tab <- 4

# which page in the report is the number of PCNs table on?
sco.pdf.pcn.tab <- 5

# which page in the report is the income/expenditure table on?
sco.pdf.i.e.tab <- 6


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
library(readxl)
library(tabulizer)
options(stringsAsFactors = FALSE)
# load existing master file, bib.master and name lookup table
master <- readRDS("data/03-processed/master.rds")
bib.master <- readRDS("data/03-processed/bib.master.rds")
orig.sco.name.lookup <- readRDS("data/01-raw/orig.sco.name.lookup.rds")

# new report name
report.name <- paste0("scotland-report-", current.year, "-",
                      current.year - 1999)

################################################################################
## AUTOMATIC DATA IMPORT AND CLEANING
################################################################################
# # loop through each sheet of the excel file to extract the income/expenditure data
# FunScotlandLoopIE(year = current.year,
#                   file.name = sco.i.e.file,
#                   start.sh =  sco.i.e.start.sh,
#                   end.sh =  sco.i.e.end.sh,
#                   exp.cell =  sco.i.e.exp.cell,
#                   inc.cell =  sco.i.e.inc.cell,
#                   transp.cell = sco.i.e.transp.cell,
#                   auth.cell = sco.i.e.auth.cell) -> scotland.i.e.
#
# # extract the dpe table from the pdf
# scotland.dpe <- extract_tables(sco.pdf.file, pages = sco.pdf.dpe.tab)
#
# # clean DPE type table
# scotland.dpe <- FunScotlandDPE(scotland.dpe, current.year)
#
# # extract PCN type table
# scotland.pcn <- extract_tables(sco.pdf.file,
#                                   pages = sco.pdf.pcn.tab,
#                                   output = "data.frame")[[1]]
#
# # clean PCN table
# scotland.pcn <- FunScotlandPCN(scotland.pcn, current.year)
#
# # extract TfS i.e. type table directly into a data.frame
# scotland.tfs.i.e <- extract_tables(sco.pdf.file,
#                                       pages = sco.pdf.i.e.tab,
#                                       output = "data.frame")[[1]]
#
# # clean TFS income expenditure table
# scotland.tfs.i.e <- FunScotlandTFSIE(scotland.tfs.i.e, current.year)
#
# # join all 3 tables from the pdf
# scotland.pdf <- full_join(full_join(scotland.dpe,
#                                     scotland.pcn,by = c("auth.name", "year")),
#                           scotland.tfs.i.e,  by = c("auth.name", "year"))
#
# # now merge income exp data from the Excel files with the pdf data
# update <- full_join(scotland.i.e., scotland.pdf)
#
# # add Scotland data and calculate surplus
# update %>%
#   mutate(country = "Scotland",
#          auth.type = "LA",
#          surplus.total = income.total - expend.total) -> master.update
#
# # double check the update is OK:
# if (nrow(master.update) != 32) {
#   paste("Something is wrong. The update should have 32 rows, but it has",
#         nrow(master.update), "instead.")} else {
#           "Everything checks out, the update has 32 rows"}

################################################################################
## Add (or overwrite) new rows to master #######################################
################################################################################

# add update for Wales - if that year already exists, it will be overwritten!!!
if (exists("master.update")){
  master %>%
    anti_join(master.update, by = c("country", "auth.name", "year")) %>%
    bind_rows(master.update) -> master}


# save updated datafile to master
saveRDS(master, "data/03-processed/master.rds")

################################################################################
# add new files to bibliography master #########################################
################################################################################
# add new rows to bibliography #################################################
# add i.e. source:
sco.i.e.bib <- data.frame(fiscyear = current.year,
                 url = sco.i.e.url,
                 country = "Scotland",
                 content = "i.e",
                 bibtype = "misc",
                 year = sco.i.e.year.published,
                 author = "{Scottish Government}",
                 urldate = sco.i.e.date.accessed,
                 title = sco.i.e.title,
                 key = paste0("Scotland.i.e.", current.year))

# add it to bib.master (overwriting if already exists)
bib.master %>%
  anti_join(sco.i.e.bib, by = c("fiscyear", "country", "content")) %>%
  bind_rows(sco.i.e.bib) -> bib.master


# add pdf source:
sco.pdf.bib <- data.frame(fiscyear = current.year,
                          url = sco.pdf.url,
                          country = "Scotland",
                          content = "pcn",
                          bibtype = "misc",
                          year = sco.pdf.year.published,
                          author = "{Transport Scotland}",
                          urldate = sco.pdf.date.accessed,
                          title = sco.pdf.title,
                          key = paste0("Scotland.pdf.", current.year))

# add it to bib.master (overwriting if already exists)
bib.master %>%
  anti_join(sco.pdf.bib, by = c("fiscyear", "country", "content")) %>%
  bind_rows(sco.pdf.bib) -> bib.master

# add aberdeen data source:
sco.aberdeen.bib <- data.frame(fiscyear = current.year,
                          url = sco.aberdeen.url,
                          country = "Scotland",
                          content = "aberdeen",
                          bibtype = "misc",
                          year = sco.aberdeen.year.published,
                          author = "Transport Scotland",
                          urldate = sco.aberdeen.date.accessed,
                          title = sco.aberdeen.title,
                          refs = paste0("@Scotland.pdf.", current.year))


# if exists add it to bib.master (overwriting if already exists)
if(!is.na(sco.aberdeen.year.published)){
  bib.master %>%
    anti_join(sco.aberdeen.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(sco.aberdeen.bib) -> bib.master}

# update RPI data acces date and year of publication
bib.master %>%
  mutate(urldate = ifelse(content == "rpi",
                          as.character(format(Sys.Date(), "%d.%m.%Y")), urldate),
         year = ifelse(content == "rpi",
                       as.numeric(format(Sys.Date(), "%Y")), year)) ->
  bib.master

# save updated datafile to master
saveRDS(bib.master, "data/03-processed/bib.master.rds")

# select scotland only bibliograpy #############################################
# select a bibliography for the scotland report - only the rows needed
bib.master %>%
  filter(fiscyear > current.year - 5, !content %in% c("budget")) %>%
  mutate(refs = paste0("@", key)) %>%
  column_to_rownames("key") -> bib.scotland

# create bib file
bib.scotland %>%
  as.BibEntry() %>%
  WriteBib(file = "code/report-rmds/scotland.bib",
           biblatex = FALSE, verbose = FALSE)

# also save the data.frame
saveRDS(bib.scotland, paste0("data/03-processed/", report.name, "-bib.rds"))

# create a copy of the scotland report template
file.copy("code/report-templates/scotland-report-template.Rmd",
          paste0("code/report-rmds/", report.name, ".Rmd"),
          overwrite = TRUE)

################################################################################
## COMPILE REPORT -  THIS IS THE ONLY PART OF THE SCRIPT THAT CAN BE RE-RUN   ##
################################################################################
# compile the report - you can repeat this as many times as you like after
# updating the .Rmd file
rmarkdown::render(paste0("code/report-rmds/", report.name, ".Rmd"),
                  output_file = paste0(report.name, ".pdf"),
                  output_dir = "outputs/reports",
                  params = list(current.year = current.year,
                                dp.text = dp.text,
                                dp.tables = dp.tables))

# remove empty folder that the compilation creates
unlink(paste0("outputs/reports/", report.name, "_files"), recursive=TRUE)

# remove log file (comment this out if there are issues and look at the log
# file for clues?
suppressWarnings(file.remove(paste0("code/report-rmds/", report.name, ".log")))

# # the report are saved to /outputs/reports/
################################################################################
################################################################################

