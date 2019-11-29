################################################################################
## MANUAL DATA ENTRY FOR ORIGINAL DATA IMPORT ##################################
################################################################################
## This file contains manually entered info on the main data sources for the 
## RAC parking reports, and the manually entered lookup tables for mistyped or
## inconsistent LA names for England and Scotland 
################################################################################
## Original data entry March 2019. 
## DO NOT MODIFY THIS FILE & DO NOT SOURCE THIS FILE
## This file contains data on where relevant ranges can be found in the original
## data sources. The resulting data.frames are saved as tables into 
## /data/01-raw/. They are then used 
## * in the documentation and journals
## * in the report compilation. 
## For descsriptions of variables, see them printed in the appendix of the 
## journal in docs/journal/journal.pdf
## This file also contains manually entered naming lookup tables, also saved as 
## data frames in /data/01-raw/
################################################################################
## 1. EXISTING REPORTS #########################################################
## 2. ENGLAND DATA SOURCES #####################################################
## 2.1 ENGLAND outturn data ####################################################
## 2.2 ENGLAND budget data #####################################################
## 2.3 Nottingham WPL data #####################################################
## 2.3.1. manual input of original WPL data for Nottingham #####################
## 3. SCOTLAND DATA SOURCES ####################################################
## 3.1 SCOTLAND incomes and expenditures #######################################
## 3.2 SCOTLAND penalty notice charges #########################################
## 4. WALES DATA SOURCES #######################################################
## 5. NAME LOOKUP TABLE FOR LAs ################################################
################################################################################
library(dplyr)
library(tibble)
library(curl)
options(stringsAsFactors = FALSE)
source("code/functions.R")

## 1. EXISTING REPORTS #########################################################
country <- c(rep("England", 8), rep("Scotland", 8),rep("Wales", 8))

year <- rep(c("2010/11", "2011/12","2012/13", "2013/14", "2014/15", "2015/16", 
              "2016/17", "2017/18"),3)

htmls <- c("\\href{https://www.racfoundation.org/media-centre/councils-make-500-million-pounds-from-parking}{link}",
           "\\href{https://www.racfoundation.org/media-centre/councils-make-record-profit-parking-2012}{link}",
           "\\href{https://www.racfoundation.org/media-centre/council-parking-profits-surge-new-high}{link}",
           "\\href{https://www.racfoundation.org/media-centre/english-council-parking-profits-new-high-2013-2014-press-release}{link}",
           "\\href{https://www.racfoundation.org/research/mobility/council-parking-revenue-in-england-2014-15-david-leibling}{link}*",
           "\\href{https://www.racfoundation.org/media-centre/parking-profits-break-three-quarters-of-a-billion}{link}",
           "\\href{https://www.racfoundation.org/media-centre/english-council-parking-profits-up-ten-percent-2016-17}{link}",
           "\\href{https://www.racfoundation.org/media-centre/english-council-parking-profits-rise-again}{link}",
           "",
           "\\href{https://www.racfoundation.org/media-centre/parking-profits-in-scotland-revealed}{link}",
           "",
           "\\href{https://www.racfoundation.org/media-centre/scottish-council-parking-accounts-2013-14-press-release}{link}",
           "\\href{https://www.racfoundation.org/media-centre/scottish-council-parking-profit-up-again-2014-15}{link}",
           "\\href{https://www.racfoundation.org/media-centre/scottish-councils-parking-profits-top-40m-2015-16}{link}",
           "\\href{https://www.racfoundation.org/research/mobility/council-parking-revenue-in-scotland-2016-17}{link}",
           "",
           "",
           "",
           "\\href{https://www.racfoundation.org/media-centre/welsh-councils-parking-accounts-2012-13-press-release}{link}",
           "",
           "",
           "\\href{https://www.racfoundation.org/media-centre/welsh-council-parking-profits-up-for-third-year}{link}",
           "\\href{https://www.racfoundation.org/media-centre/welsh-councils-parking-profits-inch-upwards-2016-2017}{link}",
           "\\href{https://www.racfoundation.org/media-centre/welsh-council-parking-profits-up-five-years-running}{link}")

pdfs <- c("",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/local_authority_parking_finances_report_david_leibling_010813.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/council_parking_accounts_2012-13_report_david_leibling.pdf}{link}",
          "\\href{https://www.racfoundation.org/assets/rac_foundation/content/downloadables/Local_Authority_Parking_Finances_England_2014_David_Leibling_December_2013-14.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_authority_parking_finances_England_2014-15_report_David_Leibling_December_2015.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_Authority_Finances_2015-16_Leibling_Nov_2016_final.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/Local_Authority_Parking_Finances_England_2016-17_Leibling_Final.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/Local_Authority_Parking_Finances_England_2017-18-Leibling_Final.pdf}{link}",
          
          "",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/local_authority_parking_finances_scotland.pdf}{link}",
          "",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_authority_parking_finances_Scotland_2013-14_report_David_Leibling_May_2015.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/local_authority_parking_finances_Scotland_2014-15_leibling_March_2016.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_authority_parking_finances_Scotland_2015-16.pdf}{link}",
          "\\href{https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwiDx9iWp4zhAhXhDWMBHf6ZAbYQFjAAegQIBBAC&url=https%3A%2F%2Fwww.racfoundation.org%2Fwp-content%2Fuploads%2FLocal_authority_parking_finances_Scotland_2016-17_Leibling.pdf&usg=AOvVaw2fU3JdI5G6nJh9LK4yu09y}{link}",
          "",
          
          "",
          "",
          "\\href{https://www.racfoundation.org/assets/rac_foundation/content/downloadables/local_parking_finances_wales_2012-13_leibling.pdf}{link}",
          "",
          "",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_authority_parking_finances_Wales-2015-16_Leibling.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_authority_parking_finances_Wales_2016-17_final_report.pdf}{link}",
          "\\href{https://www.racfoundation.org/wp-content/uploads/Local_authority_parking_finances_Wales_2017-18_final.pdf}{link}")

tab <- c("\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/council-parking-revenue-outturn-summary-10-11.pdf}{link}",
         "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/parking_operations_revenue_outturn_for_all_english_councils_2011-12_data_table.pdf}{link}",
         "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/council_parking_accounts_2012-13_tables.pdf}{link}",
         "<-",
         "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_Authority_Parking_Operations_Revenue_Outturn_for_England_2014-15_listed_by_size_of_surplus_and_alphabetic.pdf}{link}",
         "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_Authority_Parking_Operations_Revenue_Outturn_for_England_2015-16_listed_by_size_of_surplus_and_alphabetically.pdf}{link}",
         "\\href{https://www.racfoundation.org/wp-content/uploads/2017/11/Local_Authority_Parking_Operations_Revenue_Outturn_for_England_2016-17_listed_by_size_of_surplus_and_alphabetically.pdf}{link}",
         "\\href{https://www.racfoundation.org/wp-content/uploads/English-council-parking-surpluses-2017-18-all-councils-ranked-A-Z.pdf}{link}",
         rep("",16))


window <- c("", "2(4)", "3(5)", "4", "4", "4", "4", "4",
            "", "1", "", "3", "4", "4", "", "",
            "", "", "5", "", "", "4", "4", "4")

reports <- data.frame(country, year,htmls, pdfs, tab, window)

## commented out to stop overwriting!
saveRDS(reports, "data/01-raw/orig.reports.rds")

## 2. ENGLAND DATA SOURCES #####################################################
## 2.1 ENGLAND outturn data ####################################################

fisc.year <- c("2008/09", "2009/10", "2010/11", "2011/12","2012/13", "2013/14", 
          "2014/15", "2015/16", "2016/17", "2017/18")

year <- as.numeric(substr(fisc.year, 1, 4))

link <- c("\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/397002/Revenue_Outturn__RO2__data_2008-09_by_LA_-_Revised_17-Nov-2011.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/387065/RO2.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/16448/Revenue_Outturn__RO2__data_2010-11_by_LA_-_27-Nov-2012-v2.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/262061/Revenue_Outturn__RO2__data_2011-12_by_LA_-_Revised_28-Nov-2013.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/282558/Revenue_Outturn__RO2__data_2012-13_by_LA__Revised__-_18-Feb-2014.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/381066/Revenue_Outturn__RO2__data_2013-14_by_LA.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/497099/Revenue_Outturn__RO2__data_2014-15_by_LA_-_02-Feb-2016.xls}{xls}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/659790/RO2_2015-16_data_by_LA_-_Revision.xlsx}{xlsx}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/659775/RO2_2016-17_data_by_LA.xlsx}{xlsx}",
          "\\href{https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/757366/RO2_2017-18_data_by_LA.xlsx}{xlsx}")

file.name <- c(  "data/01-raw/orig.eng-08-09.xls",         
                 "data/01-raw/orig.eng-09-10.xls",          
                 "data/01-raw/orig.eng-10-11.xls",         
                 "data/01-raw/orig.eng-11-12.xls",          
                 "data/01-raw/orig.eng-12-13.xls",       
                 "data/01-raw/orig.eng-13-14.xls",       
                 "data/01-raw/orig.eng-14-15.xls",        
                 "data/01-raw/orig.eng-15-16.xlsx",         
                 "data/01-raw/orig.eng-16-17.xlsx",    
                 "data/01-raw/orig.eng-17-18.xlsx")
  
  
rows <- c(405,370, 371, 371, 371, 371, 371, 444, 446, 445)

las <- c( 388, 353, 353, 353, 353, 353, 353, 353, 353, 353)

first <- c(0, 12, 12, 12, 12, 12, 12, 8, 8, 8)

la.name <- c(rep("B", 6), rep("C", 4))

la.type <- c(rep("D", 6), rep("E", 4))

la.code <- rep("A", 10)

e.sh <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3) 

e.on <- c("CU", "BH", "BH", "BH", "BH", "BH", "BE", "CU", "CU", "CU")

e.off <- c("DB", "BL", "BL", "BL", "BL", "BL", "BI", "DB", "DB", "DB")

e.cc <- c("BZ", "AV", "AV", "AV", "AV", "AV", "AS", "BS", "BS", "BS")

i.sh <- c(3, 4, 4, 4, 4, 4, 4, 3, 3, 3) 

i.on <- c("CX", "BH", "BH", "BH", "BH", "BH", "BE", "CX", "CX", "CX")

i.off <- c("DE", "BL", "BL", "BL", "BL", "BL", "BI", "DE", "DE", "DE")

i.cc <- c("CC", "AV", "AV", "AV", "AV", "AV", "AS", "BV", "BV", "BV")

pen.sh <- c(3, 4, 4, 4, 4, 4, 4, 3, 3, 3) 

pen.on <- c("HD", "DS", "DS", "DS", "DW", "DW", "DT", "FX", "FX", "FX")

pen.1 <- c("G64", "G64", "G64", "G64", "G65", "G65", "I68", "H67", "H67", "H67")

tot.1 <- c("J45", "J45", "J45", "J45", "J46", "J46", "L49", "K48", "K48", "K48")

england.outturn.17.18 <- data.frame(fisc.year, year,link, rows, las, first,
                              e.sh, e.on, e.off, la.name, la.type, la.code,
                              i.sh, i.on, i.off, e.cc, i.cc,
                              pen.sh,pen.on, pen.1, tot.1, file.name)

## commented out to stop overwriting!
saveRDS(england.outturn.17.18, "data/01-raw/orig.eng.meta.outturn.17.rds")

## 2.2 ENGLAND budget data #####################################################


fisc.year <- c("2008/09", "2009/10", "2010/11", "2011/12","2012/13", "2013/14", 
          "2014/15", "2015/16", "2016/17", "2017/18", "2018/19")

year <- as.numeric(substr(fisc.year, 1, 4))

file.name <- c("", "",
               "data/01-raw/orig.eng-10-11-budget.xls",
               "data/01-raw/orig.eng-11-12-budget.xls",   
               "data/01-raw/orig.eng-12-13-budget.xls",   
               "data/01-raw/orig.eng-13-14-budget.xls", 
               "data/01-raw/orig.eng-14-15-budget.xls", 
               "data/01-raw/orig.eng-15-16-budget.xls", 
               "data/01-raw/orig.eng-16-17-budget.xlsx",
               "data/01-raw/orig.eng-17-18-budget.xlsx",
               "data/01-raw/orig.eng-18-19-budget.xlsx")
  
  
link <- c("", 
          "", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/7367/1934015.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/7369/2179334.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/78647/RA_2012-13_data_by_LA_-_Nat_Stats_Release_-_Revised_31-Jul-2013.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/225875/RA_2013-14_data_by_LA_-_Nat_Stats_Release_-_31-Jul-2013.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/365591/RA_2014-15_data_by_LA_-_Nat_Stats_Release_-_Revised_22-Oct-2014.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/444910/RA_2015-16_data_by_LA_-_Nat_Stats_Release_-_15-Jul-2015.xls}{xls}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/532962/RA_2016-17_data_by_LA.xlsx}{xlsx}", 
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/623097/RA_2017-18_data_by_LA.xlsx}{xlsx}",
          "\\href{https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/720343/RA_2018-19_data_by_LA.xlsx}{xlsx}")

la.name <- c(rep("B", 9), rep("C", 2))

la.type <- c(rep("D", 9), rep("E", 2))

la.code <- rep("A", 11)

rows <- c(0, 0, 443, 444, 444, 444, 444, 444, 443, 446, 443)

las <- c( 0, 0,353, 353,353, 353, 353, 353, 353, 353, 353)

first <- c(0, 0, 10, 10, 10, 10, 9, 9, 8, 8, 8)

budg.trans <- c("-", "-", "E32", "E33", "E34", "E34", "F40", "F40", "E41", "E41", "E41") 

budg.la <- c("-", "-", "U", "V", "V", "V", "U", "U", "U",  "V", "V") 

budg.cong.ch <- c("-", "-", "E24", "E25", "E25", "E25", "F31", "F30", "E31", "E31", "E31") 

england.budget.18.19 <- data.frame(fisc.year, year, file.name, link, rows, first, las,
                             budg.trans, budg.la, budg.cong.ch, la.name, la.type, la.code)

## commented out to stop overwriting!
saveRDS(england.budget.18.19, "data/01-raw/orig.eng.meta.budget.18.rds")


## 2.3 Nottingham WPL data #####################################################


year <- c("2014/15", "2015/16", "2016/17", "2017/18")


link <- c("\\href{https://www.nottinghaminsight.org.uk/d/aAXEaM3}{link}", 
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXD6dh}{link}",
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXEbNy}{link}", 
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXEduG_}{link}")
          
page <- c(69,79, 84, 86) 
nottingham.wpl <- data.frame(year,link, page)

## commented out to stop overwriting!
saveRDS(nottingham.wpl, "data/01-raw/orig.eng.meta.nott.wpl.17.rds")

## 2.3.1. manual input of original WPL data for Nottingham #####################

# initialise empty data frame
wpl <- data.frame()

# manual entry 2017
year <- 2017
income.wpl <- 9178
expend.wpl <- 219
wpl.row <- c(year, income.wpl, expend.wpl)
names(wpl.row) <- c("year", "income.wpl", "expend.wpl")

wpl <- bind_rows(wpl, wpl.row)

year <- 2016
income.wpl <- 9422
expend.wpl <- 588
wpl.row <- c(year, income.wpl, expend.wpl)
names(wpl.row) <- c("year", "income.wpl", "expend.wpl")

wpl <- bind_rows(wpl, wpl.row)

year <- 2015
income.wpl <- 9336
expend.wpl <- 713
wpl.row <- c(year, income.wpl, expend.wpl)
names(wpl.row) <- c("year", "income.wpl", "expend.wpl")

wpl <- bind_rows(wpl, wpl.row)

year <- 2014
income.wpl <- 9089
expend.wpl <- 837
wpl.row <- c(year, income.wpl, expend.wpl)
names(wpl.row) <- c("year", "income.wpl", "expend.wpl")

wpl <- bind_rows(wpl, wpl.row)

wpl$auth.name <- "Nottingham"

saveRDS(wpl, "data/01-raw/orig.eng.nott.wpl.17.rds")


## 3. SCOTLAND DATA SOURCES ####################################################
## 3.1 SCOTLAND incomes and expenditures #######################################


fisc.year <- c( "2011/12","2012/13", "2013/14", "2014/15", "2015/16", "2016/17", 
                "2017/18")

year <- as.numeric(substr(fisc.year, 1, 4))

report <- c("yes", "-", "yes", "yes", "yes", "-", "-")

file.name <- c("", 
               "data/01-raw/orig.sco-12-13.xlsx",
               "data/01-raw/orig.sco-13-14.xlsx",
               "data/01-raw/orig.sco-14-15.xlsx",
               "data/01-raw/orig.sco-15-16.xlsx",
               "data/01-raw/orig.sco-16-17.xlsx",
               "data/01-raw/orig.sco-17-18.xlsx")

link <- c("--", 
          "\\href{https://www2.gov.scot/Resource/0044/00446440.xlsx}{xlsx}", 
          "\\href{https://www2.gov.scot/Resource/0047/00475683.xlsx}{xlsx}",
          "\\href{https://www2.gov.scot/Resource/0049/00494926.xlsx}{xlsx}",
          "\\href{https://www2.gov.scot/Resource/0051/00515383.xlsx}{xlsx}",
          "\\href{https://www2.gov.scot/Resource/0053/00536018.xlsx}{xlsx}",
          "\\href{https://www2.gov.scot/Resource/0054/00546675.xlsx}{xlsx}")

start.sh <- c(NA,2,2,2,2,2,2)

end.sh<- c(NA,33,33,33,33,33,33)

exp.cell <- c("-", "F57", "F40", "E40", "B41", "B41", "B41")

inc.cell <- c("-", "G57", "G40", "F40", "C41", "C41", "C41")

t.exp.cell <- c("-", "H50", "H33", "G33", "D34", "D34", "D34") #(net expenditure)

# t.exp.cell <- c("-", "F50", "F33", "E33", "B34", "B34", "U13") # gross revenue expenditure

auth.cell <- c("-", "A2", "A2", "A2", "A2", "A1", "A1")
scotland.i.e. <- data.frame(fisc.year,year,report, file.name, link, 
                            start.sh, end.sh, exp.cell,
                            inc.cell, t.exp.cell, 
                            auth.cell)
# save
saveRDS(scotland.i.e., "data/01-raw/orig.sco.meta.i.e.rds") #

## 3.1.5  Aberdeen incomes and expenditures MANUAL #############################

aberdeen.12 <- data.frame(auth.name = "Aberdeen City",
                          year = 2012,
                          income.total = 8074,
                          expend.total = 3716 )   


aberdeen.13 <- data.frame(auth.name = "Aberdeen City",
                          year = 2013,
                          income.total = 9200,
                          expend.total = 4315)   

aberdeen.14 <- data.frame(auth.name = "Aberdeen City",
                          year = 2014,
                          income.total = 8730, 
                          expend.total =  4221) 

aberdeen.15 <- data.frame(auth.name = "Aberdeen City",
                          year = 2015,
                          income.total = 8444 , # 8483
                          expend.total =  4877 )   # 5156

aberdeen.16 <- data.frame(auth.name = "Aberdeen City",
                          year = 2016,
                          income.total = 8040,
                          expend.total =  4821) 


aberdeen.17 <- data.frame(auth.name = "Aberdeen City",
                          year = 2017,
                          income.total = 8397,
                          expend.total =  5075)

bind_rows(aberdeen.12,
          aberdeen.13,
          aberdeen.14,
          aberdeen.15,
          aberdeen.16,
          aberdeen.17) -> aberdeen

aberdeen %>%  
  mutate(surplus.total = income.total - expend.total) -> aberdeen

# save
saveRDS(aberdeen, "data/01-raw/orig.sco.aberdeen.rds")


## 3.2 SCOTLAND penalty notice charges #########################################

fisc.year <- c( "2011/12","2012/13", "2013/14/15/16", "2016/17", "2017/18")

year <- as.numeric(substr(fisc.year, 1, 4))

file.type <- c("--","--", "scan", "pdf", "pdf")

file.name <- c("--","--", "data/01-raw/orig.sco-13-14-15-16-pcn.pdf", "data/01-raw/orig.sco-16-17-pcn.pdf", 
               "data/01-raw/orig.sco-17-18-pcn.pdf")

link <- c("--", 
          "--",
          "\\href{http://www.parliament.scot/S5_Rural/Meeting%20Papers/20161221_REC_Committee_Public_Paper.pdf}{pdf}",
          "\\href{https://www.transport.gov.scot/publication/decriminalised-parking-enforcement-local-authorities-income-and-expenditure-2016-to-2017/}{pdf}",
          "\\href{https://www.transport.gov.scot/media/43636/decriminalised-parking-enforcement-income-expenditure-annual-report-2017-18.pdf}{pdf}")

dpe.tab <- c(NA, NA, NA, 4, 4)

pcn.tab <- c(NA, NA, NA, 5, 5)

e.i.tab <- c(NA, NA, NA, 6, 6)

scotland.pdf <- data.frame(fisc.year, year,file.name, file.type, link, dpe.tab,pcn.tab, e.i.tab)

## save
saveRDS(scotland.pdf, "data/01-raw/orig.sco.meta.pdf.rds")

## 3.3. manual input of PCN data for 2013-2015
sco.pcn.number.13 <- c("Aberdeen City"       = 47320,
                       "City of Edinburgh"   = 181756,
                       "Dundee City"         = 23180,
                       "East Ayrshire"       = 7597,
                       "East Renfrewshire"   = 4762,
                       "Fife"                = 16639,
                       "Glasgow City"        = 118245,
                       "Perth and Kinross"   = 11881,
                       "Renfrewshire"        = 10280,
                       "South Ayrshire"       = 7187,
                       "South Lanarkshire"   = 18787)

sco.income.pcn.13 <- c("Aberdeen City"         = 1837017,
                         "City of Edinburgh"   = 5261836,
                         "Dundee City"         = 530039,
                         "East Ayrshire"       = 333563,
                         "East Renfrewshire"   = 134776,
                         "Fife"                = 425720,
                         "Glasgow City"        = 4329842,
                         "Perth and Kinross"   = 444000,
                         "Renfrewshire"        = 303585,
                         "South Ayrshire"       = 246423,
                         "South Lanarkshire"   = 615276)

sco.income.pcn.14  <- c("Aberdeen City"         = 1640090,
                          "Argyll and Bute"     = 232711,
                          "City of Edinburgh"   = 5127332,
                          "Dundee City"         = 749850,
                          "East Ayrshire"       = 250662,
                          "East Renfrewshire"   = 209403,
                          "Fife"                = 585378,
                          "Glasgow City"        = 4345454,
                          "Inverclyde"          = 132570,
                          "Perth and Kinross"   = 346217,
                          "Renfrewshire"        = 232783,
                          "South Ayrshire"       = 255971,
                          "South Lanarkshire"   = 728302)


sco.income.pcn.15 <- c("Aberdeen City"         = 1478540,
                         "Argyll and Bute"     = 267837,
                         "City of Edinburgh"   = 5232993,
                         "Dundee City"         = 1112248,
                         "East Ayrshire"       = 204645,
                         "East Renfrewshire"   = 208460,
                         "Fife"                = 499218,
                         "Glasgow City"        = 3985927,
                         "Inverclyde"          = 277598,
                         "Perth and Kinross"   = 296377,
                         "Renfrewshire"        = 217620,
                         "South Ayrshire"       = 218998,
                         "South Lanarkshire"   = 746794)

sco.income.tfs.13 <- c("Aberdeen City"         = 9166031,
                         "City of Edinburgh"   = 22402125,
                         "Dundee City"         = 3605024,
                         "East Ayrshire"       = 1241587,
                         "East Renfrewshire"   = 134776,
                         "Fife"                = 2903581,
                         "Glasgow City"        = 4758344,
                         "Perth and Kinross"   = 2901000,
                         "Renfrewshire"        = 1063338,
                         "South Ayrshire"       = 826046,
                         "South Lanarkshire"   = 2136272)

sco.income.tfs.14 <- c("Aberdeen City"         = 8516749,
                         "Argyll and Bute"     = 1004377,
                         "City of Edinburgh"   = 24205605,
                         "Dundee City"         = 3875694,
                         "East Ayrshire"       = 1088732,
                         "East Renfrewshire"   = 209403,
                         "Fife"                = 3428805,
                         "Glasgow City"        = 4719294,
                         "Inverclyde"          = 157384,
                         "Perth and Kinross"   = 2915788,
                         "Renfrewshire"        = 1063219,
                         "South Ayrshire"       = 774956,
                         "South Lanarkshire"   = 2280904)

sco.income.tfs.15 <- c("Aberdeen City"         = 8534428,
                         "Argyll and Bute"     = 1089262,
                         "City of Edinburgh"   = 26033092,
                         "Dundee City"         = 4485080,
                         "East Ayrshire"       = 1121401,
                         "East Renfrewshire"   = 208460,
                         "Fife"                = 2908587,
                         "Glasgow City"        = 4316282,
                         "Inverclyde"          = 328341,
                         "Perth and Kinross"   = 2855525,
                         "Renfrewshire"        = 1164513,
                         "South Ayrshire"       = 790756,
                         "South Lanarkshire"   = 2462126)

# merge together all three tables for 2013
data.frame(income.pcn = sco.income.pcn.13, 
           income.tfs = sco.income.tfs.13,
           pcn.number = sco.pcn.number.13) %>% 
  rownames_to_column("auth.name") %>% 
  mutate_at(2:4, function(x) x/1000) %>% 
  mutate(year = 2013) -> scotland.pdf.13

# merge together the two tables for 2014
data.frame(income.pcn = sco.income.pcn.14, 
           income.tfs = sco.income.tfs.14) %>% 
  rownames_to_column("auth.name") %>% 
  mutate_at(2:3, function(x) x/1000) %>% 
  mutate(year = 2014) -> scotland.pdf.14

# merge together the two tables for 2015
data.frame(income.pcn = sco.income.pcn.15, 
           income.tfs = sco.income.tfs.15) %>% 
  rownames_to_column("auth.name") %>% 
  mutate_at(2:3, function(x) x/1000) %>% 
  mutate(year = 2015) -> scotland.pdf.15

saveRDS(scotland.pdf.13, "data/01-raw/orig.sco.pdf.13.rds")
saveRDS(scotland.pdf.14, "data/01-raw/orig.sco.pdf.14.rds")
saveRDS(scotland.pdf.15, "data/01-raw/orig.sco.pdf.15.rds")

## 4. WALES DATA SOURCES #######################################################
# missing atm, only need to add the links, since the files are all simple

## 5. NAME LOOKUP TABLE FOR LAs ################################################
# lookup  to manually fix LA names that are inconsistent...
england.name.lookup <- c("City of Nottingham" = "Nottingham",
                         "Nottingham City" = "Nottingham",
                         "Middlesbrough" =  "Middlesborough", 
                         "County Durham" =  "Durham",
                         "Hyndburn B C" = "Hyndburn",
                         "Kingston-upon-Hull" = "Kingston upon Hull",
                         "Kingston Upon Thames" =  "Kingston upon Thames",
                         "Lincoln City" =  "Lincoln",
                         "MaIdon" = "Maldon",
                         "The Medway Towns"= "Medway",
                         "Medway Towns" = "Medway",
                         "Newcastle" = "Newcastle upon Tyne",
                         "Norwich City" = "Norwich",
                         "Reigate & Banstead" = "Reigate and Banstead",
                         "South Buckinghamshire" = "South Bucks",
                         "South Downs National Park" = "South Downs National Park Authority",
                         "Telford & the Wrekin" = "Telford and Wrekin",
                         "Telford and the Wrekin" = "Telford and Wrekin",
                         "Folkestone and Hythe" = "Shepway")

saveRDS(england.name.lookup, "data/01-raw/orig.eng.name.lookup.rds")

scotland.name.lookup  <- c("Argyll  Bute" = "Argyll and Bute",
                           "Argyll & Bute" = "Argyll and Bute",
                           "Perth & Kinross" = "Perth and Kinross",
                           "Perth  Kinross" = "Perth and Kinross",
                           "Edinburgh, City of" = "Edinburgh City",
                           "Edinburgh, city of" = "Edinburgh City",
                           "City of Edinburgh" = "Edinburgh City",
                           "Dundee" =  "Dundee City",
                           "Glasgow" = "Glasgow City",
                           "East" =  "East Dunbartonshire",
                           "Comhairle nan Eilean Siar"  = "Eilean Siar",
                           "Western Isles" = "Eilean Siar",
                           "Na h-Eileanan Siar" = "Eilean Siar",
                           "Helensburgh and Lomond" = "Argyll and Bute",
                           "Dumfries & Galloway" = "Dumfries and Galloway",
                           "North Ayrshire mainland" = "North Ayrshire",
                           "Ross and Cromarty" = "Highland",
                           "Caithness and Sutherland" = "Highland",
                           "Inverness and Nairn" = "Highland",
                           "Badenoch and Strathspey" = "Highland",
                           "West Moray" = "Moray",
                           "North East Moray" = "Moray", 
                           "Arran and Cumbrae" = "Argyll and Bute",
                           "Argyll and Bute Islands" = "Argyll and Bute",
                           "Argyll and Bute Mainland" = "Argyll and Bute",
                           "Lochaber" = "Highland",
                           "Skye and Lochalsh" = "Highland")
                                                                 
saveRDS(scotland.name.lookup, "data/01-raw/orig.sco.name.lookup.rds")

## 5. ORIGINAL BIBLIOGRAPHY ####################################################
# start the bib.table
# map first
bib.master <- data.frame(country = "GB",
                         fiscyear = 2018, 
                         year = 2017,
                         author = "{Office for National Statistics}", 
                         title = "{Local Administrative Units Level 1 (January 2018) Super Generalised Clipped Boundaries in United Kingdom}", 
                         url ="http://geoportal1-ons.opendata.arcgis.com/datasets/3dc07a60f46b4e01ab0ec8ba71c7a879_3.zip", 
                         urldate = "10.03.2019",
                         bibtype = "misc", 
                         content = "map")

# RPI data
data.frame(country = "GB",
           fiscyear = 2018, 
           year = 2019,
           author = "{Office for National Statistics}", 
           title = "{Inflation and price indices}", 
           url ="https://www.ons.gov.uk/economy/inflationandpriceindices", 
           urldate = "3.4.2019",
           bibtype = "misc", 
           content = "rpi") %>% 
  bind_rows(bib.master) -> bib.master

# add scottish income and expenditure data 
scotland.i.e. %>% 
               select(fiscyear = year, url = link) %>% 
               filter(fiscyear > 2011) %>% 
              mutate(url = gsub("[\\{\\}]", "", 
                                 regmatches(url, regexpr("\\{.*?\\}", url)))) %>% 
  mutate(country ="Scotland",
         title = paste("{Scottish Local Government Finance Statistics",
                       FunFisc(c.y = fiscyear), "Annex A by LA}"),
         urldate = "10.03.2019",
         bibtype = "misc",
         content = "i.e", 
         year = fiscyear + 2, 
         author = "{Scottish Government}") %>% 
  bind_rows(bib.master) -> bib.master

# add scotland pcn data. 
scotland.pdf %>% 
  filter(year >2012) %>% 
  select(fiscyear = year, url = link) %>% 
  mutate(url = gsub("[\\{\\}]", "", 
                     regmatches(url, regexpr("\\{.*?\\}", url))),
         country = "Scotland",
         content = "pcn",
         bibtype = "misc",
         year = c(2016, 2017, 2018),
         author = "{Transport Scotland}",
         urldate = "10.03.2019",
         title = paste("{Decriminalised Parking Enforcement: Local Authorites'",
                       "Income and Expenditure:", fiscyear, "to", 
                       ifelse(row_number() == n(), fiscyear + 1, lead(fiscyear)), "}")) %>% 
  bind_rows(bib.master) -> bib.master
         
# add Aberdeen data

bib.master %>% 
  bind_rows(data.frame(country = "Scotland",
                       author = "{Aberdeen City Council}", 
                       bibtype = "misc", 
                       content = "i.e.",
                       fiscyear = 2016,
                       year = 2017,
                       title = "{Aberdeen City Council Annual Accounts 2016-17}",
                       url = "https://www.aberdeencity.gov.uk/media/5702",
                       urldate = "4.4.2019")) -> bib.master

bib.master %>% 
  bind_rows(data.frame(country = "Scotland",
                       author = "{Aberdeen City Council}", 
                       bibtype = "misc", 
                       content = "i.e.",
                       fiscyear = 2015,
                       year = 2016,
                       title = "{Aberdeen City Council Annual Accounts 2015-16}",
                       url = "https://www.aberdeencity.gov.uk/media/3101",
                       urldate = "4.4.2019")) -> bib.master

bib.master %>% 
  bind_rows(data.frame(country = "Scotland",
                       author = "{Aberdeen City Council}", 
                       bibtype = "misc", 
                       content = "i.e.",
                       fiscyear = 2014,
                       year = 2015,
                       title = "{Aberdeen City Council Annual Accounts 2014-15}",
                       url = "https://www.aberdeencity.gov.uk/media/3111",
                       urldate = "4.4.2019")) -> bib.master

bib.master %>% 
  bind_rows(data.frame(country = "Scotland",
                       author = "{Aberdeen City Council}", 
                       bibtype = "misc", 
                       content = "i.e.",
                       fiscyear = 2013,
                       year = 2014,
                       title = "{Aberdeen City Council Annual Accounts 2013-14}",
                       url = "https://www.aberdeencity.gov.uk/media/3114",
                       urldate = "4.4.2019")) -> bib.master


bib.master %>% 
  bind_rows(data.frame(country = "Scotland",
                       author = "{Aberdeen City Council}", 
                       bibtype = "misc", 
                       content = "i.e.",
                       fiscyear = 2012,
                       year = 2013,
                       title = "{Aberdeen City Council Annual Accounts 2012-13}",
                       url = "https://www.aberdeencity.gov.uk/media/3135",
                       urldate = "4.4.2019")) -> bib.master


# add wales data

bind_rows(bib.master,
data.frame(country = "Wales",
           content = "i.e",
           bibtype = "misc",
           author =  "{Welsh Government}",
           title = "{Revenue outturn expenditure: roads and transport (Table LGFS0009)}",
           url = "https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Transport/RoadsAndTransportRevenueExpenditure-by-authority",
           year = 2018,
           fiscyear = 2017, 
           urldate = "10.03.2019")) -> bib.master

# add England i.e. data
england.outturn.17.18 %>% 
  select(fiscyear = year, url = link) %>% 
  mutate(url = gsub("[\\{\\}]", "", 
                     regmatches(url, regexpr("\\{.*?\\}", url))),
               country = "England",
               content = "i.e",
               bibtype = "misc",
               year = c(2010:2017, 2017, 2019),
                 author = "{UK Government}",
         urldate = "10.03.2019",
         title = paste0("{Local authority revenue expenditure and financing England: ", fiscyear,
         " to ", fiscyear + 1, ", individual local authority data - outturn}")) %>% 
  bind_rows(bib.master) -> bib.master

           
# add England budget data
england.budget.18.19 %>% 
  select(fiscyear = year, url = link) %>% 
  mutate(url = ifelse(is.na(url), NA, gsub("[\\{\\}]", "", 
                     regmatches(url, regexpr("\\{.*?\\}", url)))),
         country = "England",
         content = "budget",
         bibtype = "misc",
         year  = c(NA, NA, 2011, 2011, 2012:2018),
         author = "{UK Government}",
         urldate = "10-03-2019",
         title = paste0("{Local authority revenue expenditure and financing England: ", fiscyear,
                       " to ", fiscyear + 1, ", budget (Revenue Account budget)}")) %>% 
  bind_rows(bib.master) -> bib.master


# add Nottingham WPL data
# assuming it is published in the year of end of the fiscal year
nottingham.wpl %>% 
  select(fiscyear = year, url = link) %>% 
  mutate(url = ifelse(is.na(url), NA, gsub("[\\{\\}]", "", 
                                           regmatches(url, regexpr("\\{.*?\\}", url)))),
         country = "England",
         content = "wpl",
         bibtype = "misc",
         year = as.numeric(substr(fiscyear, 1, 4)) + 1,
         fiscyear = as.numeric(substr(fiscyear, 1, 4)),
         author = "{Nottingham City Council}",
         urldate = "10-03-2019",
         title = paste0("{Statement of Accounts ", fiscyear, "}")) %>% 
  bind_rows(bib.master) -> bib.master

bib.master %>% 
  mutate(key = paste0(country,".", content, ".", fiscyear)) -> bib.master

saveRDS(bib.master, "data/01-raw/orig.bib.master.rds")
saveRDS(bib.master, "data/03-processed/bib.master.rds")

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

