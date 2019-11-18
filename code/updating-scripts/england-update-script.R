################################################################################
## ENGLAND  UPDATE TEMPLATE ####################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for England for any year available in the file          ##
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
# If you want to add new data for the current fiscal year then change to 
# TRUE and proceed through the script. Alyways make sure the data you are entering 
# matches the current.year variable above
add.new.data <- FALSE


# If you have already produced an .Rmd file by running this script, and have 
# made changes to the .Rmd file and just want to recompile it switch to TRUE.
# If you want to produce a fresh copy of the template for this year switch
# to FALSE
recompile.rmd <- FALSE

# number of decimal places in text and tables:
dp.text <- 1
dp.tables <- 1


################################################################################
# ## MANUAL DATA INPUT #########################################################
# ##############################################################################
if (add.new.data){
  ## after dowloading the three England files into the data/01-raw folder, 
  ## enter their metadata here:

  #############################################################################
  # England outturn data (Income and Expenditure data)                        #
  #############################################################################
  # title as it will appear in the references:
  eng.i.e.title <- "Local authority revenue expenditure and financing England: 2017-18, individual local authority data - outturn"

  # url of the file:
  eng.i.e.url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/659775/RO2_2016-17_data_by_LA.xlsx"

  # year published, as it will appear in the references:
  eng.i.e.year.published <- 2018

  ## replace with date of access to data:
  eng.i.e.date.accessed <- "07.10.2019"

  # path and name of file where you have saved it:
  eng.i.e.file <- "data/01-raw/orig.eng-17-18.xlsx"

  # on which sheet is the LA data on?
  eng.i.e.sh <- 3
  
  # on the LA sheet, which row is the first LA in?
  eng.i.e.first <- 8
  
  # on the LA sheet, which row is the last row of the table?
  eng.i.e.last <- 452
  
  # on the LA sheet, which column has the LA names
  eng.i.e.la.name <- "C"
  
   # on the LA sheet, which column has the LA types or classes
  eng.i.e.la.type <- "E"
  
  # on the LA sheet, which column is the ON-street parking Total Expenditure in?
  eng.e.on <- "CU"
    
  # on the LA sheet, which column is the OFF-street parking Total Expenditure in?
  eng.e.off <- "DB"
  
  # on the LA sheet, which column is the congestion charge Total Expenditure in?
  eng.e.cc <- "BS"
  
  # on the LA sheet, which column is the ON-street parking Total Income in?
  eng.i.on <- "CX"
  
  # on the LA sheet, which column is the OFF-street parking Total Income in?
  eng.i.off <- "DE"
  
  # on the LA sheet, which column is the congestion charge Total Income in?
  eng.i.cc <- "BV"
  
  # on the LA sheet, which column is the On-street parking: Penalty Charge Notice 
  # income included in line 61? ?
  eng.pen.on <- "FX"
  
  # on the SUMMARY sheet, which cell is the total On-street parking: Penalty 
  # Charge Notice income included in line 61?
  eng.pen.1 <- "H67"
  
  # on the SUMMARY sheet, which cell is the Total Expenditure for 
  # On-street parking: Penalty Charge Notice income included in line 61?
  eng.pen.1 <- "H67"

  # on the SUMMARY sheet, which cell is the Net Current Expenditure for 
  # TOTAL HIGHWAYS AND TRANSPORT SERVICES?
  eng.tot.1 <- "K48"

  #############################################################################
  # England budget data - for the next fiscal year                            #
  #############################################################################
  # title as it will appear in the references:
  eng.budg.title <- "Local authority revenue expenditure and financing England: 2018 to 2019 budget (Revenue Account budget)"
  
  # url of the file:
  eng.budg.url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/720343/RA_2018-19_data_by_LA.xlsx"
  
  # year published, as it will appear in the references:
  eng.budg.year.published <- 2018
  
  ## replace with date of access to data:
  eng.budg.date.accessed <- "07.10.2019"
  
  # path and name of file where you have saved it:
  eng.budg.file <- "data/01-raw/orig.eng-18-19-budget.xlsx"
  
  # on which sheet is the LA data on?
  eng.budg.sh <- 3
  
  # on the LA sheet, which row is the first LA in?
  eng.budg.first <- 8
  
  # on the LA sheet, which row is the last row of the table?
  eng.budg.last <- 450
  
  # on the LA sheet, which column has the LA names?
  eng.budg.la.name <- "C"
  
  # on the LA sheet, which column has the LA types or classes?
  eng.budg.la.type <- "E"
  
  # on the LA sheet, which column has the parking services column?
  eng.budg.la <- "V"
  
  # on the summary sheet, which cell has the TOTAL HIGHWAYS AND TRANSPORT SERVICES 
  # (total of lines 210 to 280) budgeted net current expenditure
  eng.budg.trans <- "E41"
  
  # on the summary sheet, which cell has the CONGESTION CHARGE 
  # budgeted net current expenditure
  eng.budg.cc <- "E31"
  
  
  #############################################################################
  # Nottingham WPL data                                                   #####
  #############################################################################
  # title as it will appear in the references:
  eng.nhm.title <- "{Statement of Accounts}"

  # url of the file:
  eng.nhm.url <- "https://www.nottinghaminsight.org.uk/d/aAXEduG_"

  # year published, as it will appear in the references:
  eng.nhm.year.published <- 2018

  ## replace with date of access to data:
  eng.nhm.date.accessed <- "4.10.2019"

  # page on which the table "Road Charging Schemes under the Transport Act 2000"
  # can be found 
  eng.nhm <- 86
  
  # income from WPL as shown in this table (in thousands, not in millions!):
  eng.nhm.income.wpl <- 9178
  
  # expenditures for WPL as shown in this table (in thousands, not in millions!): 
  eng.nhm.expend.wpl <- 219
  
  # that's all the manual entry done!
}


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
orig.eng.name.lookup <- readRDS("data/01-raw/orig.eng.name.lookup.rds")

# new report name
report.name <- paste0("england-report-", current.year, "-",
                      current.year - 1999)

if (add.new.data){  
  ################################################################################
  ## AUTOMATIC DATA IMPORT AND CLEANING
  ################################################################################
  # go through excel file to extract the income/expenditure data for LAs
  FunEnglandOutturn(file = eng.i.e.file, 
                    first = eng.i.e.first, 
                    last = eng.i.e.last,
                    e.sh = eng.i.e.sh, 
                    e.on = eng.e.on, 
                    e.off = eng.e.off, 
                    e.cc = eng.e.cc,
                    i.sh = eng.i.e.sh, 
                    i.on = eng.i.on,
                    i.off = eng.i.off,
                    i.cc = eng.i.cc, 
                    pen.sh = eng.i.e.sh, 
                    pen.on = eng.pen.on,
                    auth.name = eng.i.e.la.name, 
                    auth.type = eng.i.e.la.type, 
                    year = current.year)  -> england.i.e
  
  # go throught excel outturn file and extract totals 
  FunEnglandOutturnTotals(file = eng.i.e.file, 
                          transport.total = eng.tot.1,
                          income.pcn = eng.pen.1, 
                          year = current.year) %>% 
    bind_rows() -> england.i.e.tot
  england.i.e.tot$auth.name <- "England"
  england.i.e.tot$auth.type <- "X"
  
  bind_rows( england.i.e,  england.i.e.tot) -> england.i.e

  # go throught excel budget files and extract LA data
  FunEnglandBudget(file = eng.budg.file, 
                   first = eng.budg.first, 
                   last = eng.budg.last, 
                   sheet = eng.budg.sh,
                   budg.la = eng.budg.la,
                   auth.name = eng.budg.la.name,
                   auth.type = eng.budg.la.type, 
                   year = current.year) -> england.budget
  

  # go throught excel budget file and extract totals 
  FunEnglandBudgetTransport(file = eng.budg.file, 
                            budg.trans = eng.budg.trans,
                            year = current.year) %>% 
    bind_rows() -> england.budget.tot
  england.budget.tot$auth.name <- "England"
  england.budget.tot$auth.type <- "X"

  bind_rows(england.budget,  england.budget.tot) -> england.budget
  
  # add nottingham wpl data
  data.frame(year = current.year,
        auth.name = "Nottingham",
        income.wpl = eng.nhm.income.wpl,
        expend.wpl = eng.nhm.expend.wpl) -> nhm
    
  england.i.e %>% 
    filter(auth.type != "O" | 
             grepl("National Park", auth.name) |
             auth.name == "Greater London Authority") -> england.i.e
  
  england.budget %>% 
    filter(auth.type != "O" | 
             grepl("National Park", auth.name) |
             auth.name == "Greater London Authority") -> england.budget
  
  # now merge income exp data from the Excel files with the pdf data
  update <- full_join(england.i.e, england.budget) 
  update <- full_join(update, nhm)
  # add Scotland data and calculate surplus
  update %>%
    mutate(country = "England") -> master.update
  
  # double check the update is OK:
  master.update %>% 
    filter(!auth.type %in% c("O", "X", "GLA")) %>% 
    nrow() -> nrows
  
  if (nrows != 353) {
    paste("Something is wrong. The update should have data for 353 LAs, but it has",
          nrows, "instead.")} else {
            "Everything checks out, the update has 353 LA rows"}

###############################################################################
# Add (or overwrite) new rows to master #######################################
###############################################################################

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
  eng.i.e.bib <- data.frame(fiscyear = current.year,
                            url = eng.i.e.url,
                            country = "England",
                            content = "i.e",
                            bibtype = "misc",
                            year = eng.i.e.year.published,
                            author = "{Office for National Statistics}",
                            urldate = eng.i.e.date.accessed,
                            title = eng.i.e.title,
                            key = paste0("England.i.e.", current.year))

  # add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(eng.i.e.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(eng.i.e.bib) -> bib.master

  # add budget. source:
  eng.budg.bib <- data.frame(fiscyear = current.year,
                            url = eng.budg.url,
                            country = "England",
                            content = "budget",
                            bibtype = "misc",
                            year = eng.budg.year.published,
                            author = "{Office for National Statistics}",
                            urldate = eng.budg.date.accessed,
                            title = eng.budg.title,
                            key = paste0("England.budg.", current.year))
  
  # add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(eng.budg.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(eng.budg.bib) -> bib.master
  
  # # add pnottingham df source:
  eng.nhm.bib <- data.frame(fiscyear = current.year,
                            url = eng.nhm.url,
                            country = "England",
                            content = "wpl",
                            bibtype = "misc",
                            year = eng.nhm.year.published,
                            author = "{Nottingham City Council}",
                            urldate = eng.nhm.date.accessed,
                            title = eng.nhm.title,
                            key = paste0("England.wpl.", current.year))

  # add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(eng.nhm.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(eng.nhm.bib) -> bib.master

  # save updated datafile to master
  saveRDS(bib.master, "data/03-processed/bib.master.rds")
} 

# select england only bibliograpy #############################################
# select a bibliography for the england report - only the rows needed
bib.master %>%
  filter(fiscyear > current.year - 5, country  == "England" | country == "GB") %>%
  mutate(refs = paste0("@", key)) %>%
  column_to_rownames("key") -> bib.england

# create bib file
bib.england %>%
  as.BibEntry() %>%
  WriteBib(file = "code/report-rmds/england.bib",
           biblatex = FALSE, verbose = FALSE)

# also save the data.frame
saveRDS(bib.england, paste0("data/03-processed/", report.name, "-bib.rds"))


################################################################################
## COMPILE REPORT 
################################################################################
# check if master data is available for current year?

if(nrow(filter(master, country == "England", year == current.year)) == 0) {
  warning("There are no records for the year ", current.year) } else {
    if(nrow(filter(master, country == "England", year == current.year)) !=364) {
      warning("Something has gone wrong. There should be 364 rows for ", 
             current.year, " but there are not. I suggest you revert to a ",
             "previous version of the repository and try again.")} else {
               
               
               if(!recompile.rmd){
                 # create a fresh copy from the england report template
                 file.copy("code/report-templates/england-report-template.Rmd",
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