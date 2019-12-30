################################################################################
## SCOTLAND UPDATE TEMPLATE ####################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for Scotland for any year available in the file         ##
################################################################################
## Instructions (detailed instructions are in /docs/technical):
## 1. mandatory input: input (update) the current year and whether or not you
##      are producing a new report, or recompiling an exisitng one.
## 2. If you are producing a new report, add input the required metadata
##      for all the files you've downloaded into data/01-raw. 
## 3. Run through the rest of the script that imports the data, cleans it,  
##      creates an .Rmd file and produces the compiled .pdf report.
################################################################################
################################################################################
##  MANDATORY INPUTS                                                          ##
################################################################################

# which fiscal year do you want to produce a report for?
# NB: the current year is the year in which the fiscal year starts
current.year <- 2017

# if you want to produce a report based on current data - but for a previous year
# set add.new.data to FALSE. Run the rest of the script.
# If you want to add new data from GS, TS and Aberdeen council, then change to 
# TRUE and proceed through the script. Alyways make sure the data you are entering 
# matches the current.year variable. 
add.new.data <- FALSE

# If you have already produced an .Rmd file by running this script, and have 
# made changes to the .Rmd file and just want to recompile it switch to TRUE.
# If you want to produce a fresh copy of the template for this year switch
# to FALSE
recompile.rmd <- FALSE

# number of decimal places in text and tables:
dp.text <- 1
dp.tables <- 2



################################################################################
## MANUAL DATA INPUT ###########################################################
################################################################################
if (add.new.data){
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
  
  # which cell has the Total Roads and Transport "Net revenue Expenditure on a funding basis"?
  sco.i.e.transp.cell <- "D34"
  
  
  # Aberdeen city - separate data source if available? ###########################
  ################################################################################
  # title as it will appear in the references:
  sco.aberdeen.title <- "{Aberdeen City Council Annual Accounts 2016-17}"
  
  # url of the file:
  sco.aberdeen.url <- "https://www.aberdeencity.gov.uk/media/5702"
  
  # year published, as it will appear in the references:
  sco.aberdeen.year.published <- 2017
  
  ## replace with date of access to data:
  sco.aberdeen.date.accessed <- "4.4.2019"
  
  # How much parking Income did Aberdeen City report this year??
  sco.aberdeen.income.total <- 8040
  
  # How much parking Expenditure did Aberdeen City report this year??
  sco.aberdeen.expend.total <- 4821
  
  
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
} 

################################################################################
################################################################################
##                                                                            ##
##     THE REST OF THIS SCRIPT IS TO BE RUN ONLY -- NO MODIFICATIONS!         ##
##                                                                            ##
################################################################################
################################################################################
################################################################################
## LOAD PACKAGES AND DATA ######################################################
################################################################################
source("code/do-not-touch-scripts/functions.R")
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

if (add.new.data){  
  ################################################################################
  ## AUTOMATIC DATA IMPORT AND CLEANING
  ################################################################################
  # loop through each sheet of the excel file to extract the income/expenditure data
  FunScotlandLoopIE(year = current.year,
                    file.name = sco.i.e.file,
                    start.sh =  sco.i.e.start.sh,
                    end.sh =  sco.i.e.end.sh,
                    exp.cell =  sco.i.e.exp.cell,
                    inc.cell =  sco.i.e.inc.cell,
                    transp.cell = sco.i.e.transp.cell) -> scotland.i.e.
  
  aberdeen <- data.frame(auth.name = "Aberdeen City",
                         year = current.year,
                         income.total = sco.aberdeen.income.total,
                         expend.total =  sco.aberdeen.expend.total)
  
  # slot in manual aberdeen city data
  scotland.i.e. %>% 
    filter(auth.name == "Aberdeen City") %>% 
    select(auth.name, transport.total, year) %>% 
    full_join(aberdeen) -> full.aberdeen
  
  # merge back with scotland i.e.
  scotland.i.e. %>% 
    filter(auth.name != "Aberdeen City") %>%
    bind_rows(full.aberdeen) -> scotland.i.e.
  
  # extract the dpe table from the pdf
  scotland.dpe <- extract_tables(sco.pdf.file, pages = sco.pdf.dpe.tab)
  
  # clean DPE type table
  scotland.dpe <- FunScotlandDPE(scotland.dpe, current.year)
  
  # extract PCN type table
  scotland.pcn <- extract_tables(sco.pdf.file,
                                 pages = sco.pdf.pcn.tab,
                                 output = "data.frame")[[1]]
  
  # clean PCN table
  scotland.pcn <- FunScotlandPCN(scotland.pcn, current.year)
  
  # extract TfS i.e. type table directly into a data.frame
  scotland.tfs.i.e <- extract_tables(sco.pdf.file,
                                     pages = sco.pdf.i.e.tab,
                                     output = "data.frame")[[1]]
  
  # clean TFS income expenditure table
  scotland.tfs.i.e <- FunScotlandTFSIE(scotland.tfs.i.e, current.year)
  
  # join all 3 tables from the pdf
  scotland.pdf <- full_join(full_join(scotland.dpe,
                                      scotland.pcn,by = c("auth.name", "year")),
                            scotland.tfs.i.e,  by = c("auth.name", "year"))
  
  # now merge income exp data from the Excel files with the pdf data
  update <- full_join(scotland.i.e., scotland.pdf)
  
  # add Scotland data and calculate surplus
  update %>%
    mutate(country = "Scotland",
           auth.type = "LA",
           surplus.total = income.total - expend.total) -> master.update
  
  # double check the update is OK:
  if (nrow(master.update) != 32) {
    paste("Something is wrong. The update should have 32 rows, but it has",
          nrow(master.update), "instead.")} else {
            "Everything checks out, the update has 32 rows"}
  
  ################################################################################
  ## Add (or overwrite) new rows to master #######################################
  ################################################################################
  
  # add update - if that year already exists, it will be overwritten!!!
  if (exists("master.update")){
    master %>%
      anti_join(master.update, by = c("country", "auth.name", "year")) %>%
      bind_rows(master.update) -> master}
  
  
  # save updated datafile to master
  saveRDS(master, "data/03-processed/master.rds")
  write.csv(master, "outputs/csv-tables/master.csv")
  
  ## IMPORT AND CLEAN RPI DATA ###################################################
  # if RPI file doesn't exist, or if it doesn't have today's date, download it again. 
  if (!file.exists("data/01-raw/rpi.csv") | 
      format(file.mtime("data/01-raw/rpi.csv"), "%d.%m.%Y") != 
      format(Sys.Date(), "%d.%m.%Y")) {
    url <- paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vTisg2eXAykXY-",
                  "jcDJRXBf7LlL8IBFRmwBgJGF6-kcFVTlx96kAurVWCohG1ryXMvtvD1dNvQ6otS2R",
                  "/pub?gid=543857295&single=true&output=csv")
    
    download.file(url, destfile = "data/01-raw/rpi.csv", method="curl")
  }
  
  # update RPI data acces date and year of publication in the bibliography
  bib.master %>%
    mutate(urldate = ifelse(content == "rpi",
                            as.character(format(Sys.Date(), "%d.%m.%Y")), urldate),
           year = ifelse(content == "rpi",
                         as.numeric(format(Sys.Date(), "%Y")), year)) ->
    bib.master
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
  
  # add aberdeen data source:
  sco.aberdeen.bib <- data.frame(fiscyear = current.year,
                                 url = sco.aberdeen.url,
                                 country = "Scotland",
                                 content = "i.e.",
                                 bibtype = "misc",
                                 year = sco.aberdeen.year.published,
                                 author = "{Aberdeen City Council}",
                                 urldate = sco.aberdeen.date.accessed,
                                 title = sco.aberdeen.title,
                                 key = paste0("Scotland.abd.", current.year))
  
  
  # if exists add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(sco.aberdeen.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(sco.aberdeen.bib) -> bib.master
  
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
  

  
  # save updated datafile to master
  saveRDS(bib.master, "data/03-processed/bib.master.rds")
}

  # select scotland only bibliograpy #############################################
  # select a bibliography for the scotland report - only the rows needed
  bib.master %>%
    filter(fiscyear > current.year - 5, !content %in% c("budget", "wpl")) %>%
   group_by(country) %>%
   filter(country %in% c("Wales", "Scotland") | country == "England" & fiscyear == max(fiscyear)) %>%
    mutate(refs = paste0("@", key)) %>%
    column_to_rownames("key") -> bib.scotland
  
  # create bib file
  bib.scotland %>%
    as.BibEntry() %>%
    WriteBib(file = "code/report-rmds/scotland.bib",
             biblatex = FALSE, verbose = FALSE)
  
  # also save the data.frame
   saveRDS(bib.scotland, paste0("data/03-processed/", report.name, "-bib.rds"))



################################################################################
## COMPILE REPORT 
################################################################################
# check if master data is available for current year?

if(nrow(filter(master, country == "Scotland", year == current.year)) == 0) {
  warning("There are no records for the year ", current.year) } else {
    if(nrow(filter(master, country == "Scotland", year == current.year)) !=32) {
      warning("Something has gone wrong. There should be 32 rows for ", 
             current.year, " but there are not. I suggest you revert to a ",
             "previous version of the repository and try again.")} else {
               
               
               if(!recompile.rmd){
                 # create a fresh copy of the scotland report template
                 file.copy("code/report-templates/scotland-report-template.Rmd",
                           paste0("code/report-rmds/", report.name, ".Rmd"),
                           overwrite = TRUE)}
               
               
               # compile the report (but check if file exists first)
               if(recompile.rmd & !file.exists(paste0("code/report-rmds/", 
                                                     report.name, ".Rmd"))){
                 warning("The Rmd file does not exist. Rerun this script with ",
                       "recompile.rmd swithced to FALSE.")} else { 
                         
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
                       }
             }
  }
# # the report are saved to /outputs/reports/
################################################################################
################################################################################