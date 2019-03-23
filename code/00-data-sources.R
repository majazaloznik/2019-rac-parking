################################################################################
## DESCRIPTION OF DATA SOURCES -- MANUAL DATA ENTRY ############################
################################################################################
## This file contains manually entered info on the main data sources for the 
## RAC parking reports. 
################################################################################
## Original data entry March 2019. 
## DO NOT MODIFY THIS FILE & DO NOT SOURCE THIS FILE
## This file contains data on where relevant ranges can be found in the original
## data sources. The resulting data.frames are saved as tables into 
## /data/02-interim/. They are then used 
## * in the documentation and journals
## * in the report compilation. 
## For descsriptions of variables, see them printed in the appendix of the 
## journal in docs/journal/journal.pdf
################################################################################
## 1. EXISTING REPORTS #########################################################
## 2. ENGLAND DATA SOURCES #####################################################
## 2.1 ENGLAND outturn data ####################################################
## 2.2 ENGLAND budget data #####################################################
## 3. SCOTLAND DATA SOURCES ####################################################
## 3.1 SCOTLAND incomes and expenditures #######################################
## 3.2 SCOTLAND penalty notice charges #########################################
## 4. WALES DATA SOURCES #######################################################
################################################################################


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
# saveRDS(reports, "data/02-interim/reports.rds")

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

la.name <- c(rep("B", 7), rep("C", 3))

la.type <- c(rep("D", 7), rep("E", 3))

e.sh <- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3) 

e.on <- c("CU", "BH", "BH", "BH", "BH", "BH", "BE", "CU", "CU", "CU")

e.off <- c("DB", "BL", "BL", "BL", "BL", "BL", "BI", "DB", "DB", "DB")

e.cc <- c("BZ", "AV", "AV", "AV", "AV", "AV", "AS", "BS", "BS", "BS")

i.sh <- c(3, 4, 4, 4, 4, 4, 4, 3, 3, 3) 

i.on <- c("CX", "BH", "BH", "BH", "BH", "BH", "BE", "CX", "CX", "CX")

i.off <- c("DE", "BL", "BL", "BL", "BL", "BL", "BI", "DE", "DE", "DE")

i.cc <- c("CC", "AV", "AV", "AV", "AV", "AV", "AS", "BV", "BV", "BV")

pen.sh <- c(NA, NA, NA, NA, NA, NA, NA, 3, 3, 3) 

pen.on <- c("--", "--", "--", "--", "--", "--", "--", "FX", "FX", "FX")

pen.1 <- c("G64", "G64", "G64", "G64", "G65", "G65", "I68", "H67", "H67", "H67")

tot.1 <- c("J45", "J45", "J45", "J45", "J46", "J46", "L49", "K48", "K48", "K48")

england.outturn <- data.frame(fisc.year, year,link, rows, las, first,
                              e.sh, e.on, e.off, la.name, la.type,
                              i.sh, i.on, i.off, 
                              pen.sh,pen.on, pen.1, tot.1, file.name)

## commented out to stop overwriting!
# saveRDS(england.outturn, "data/02-interim/england.outturn.17.18.rds")

## 2.2 ENGLAND budget data #####################################################


fisc.year <- c("2008/09", "2009/10", "2010/11", "2011/12","2012/13", "2013/14", 
          "2014/15", "2015/16", "2016/17", "2017/18", "2018/19")

year <- as.numeric(substr(fisc.year, 1, 4))

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

rows <- c(0, 0, 444, 444, 444, 444, 444, 444, 443, 446, 443)

las <- c( 0, 0,353, 353,353, 353, 353, 353, 353, 353, 353)

first <- c(0, 0, 10, 10, 10, 10, 9, 9, 8, 8, 8)

budg.tot <- c("-", "-", "E27", "E28", "E28", "E28", "F34", "F34", "E35", "E35", "E35") 

budg.la <- c("-", "-", "U", "V", "V", "V", "U", "U", "U",  "V", "V") 

england.budget <- data.frame(fisc.year, year,link, rows, first, las,
                             budg.tot, budg.la)

## commented out to stop overwriting!
# saveRDS(england.budget, "data/02-interim/england.budget.18.19.rds")


## 2.3 Nottingham WPL data #####################################################


year <- c("2014/15", "2015/16", "2016/17", "2017/18")


link <- c("\\href{https://www.nottinghaminsight.org.uk/d/aAXEaM3}{link}", 
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXD6dh}{link}",
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXEbNy}{link}", 
          "\\href{https://www.nottinghaminsight.org.uk/d/aAXEduG_}{link}")
          
page <- c(69,79, 84, 86) 
nottingham.wpl <- data.frame(year,link, page)

## commented out to stop overwriting!
# saveRDS(nottingham.wpl, "data/02-interim/nottingham.wpl.17.18.rds")


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

start.sh <- c(NA,2,2,2,2,2,3)

end.sh<- c(NA,33,33,33,33,33,34)

exp.cell <- c("-", "F57", "F40", "E40", "B41", "B41", "L13")

inc.cell <- c("-", "G57", "G40", "F40", "C41", "C41", "L23")

t.exp.cell <- c("-", "F50", "F33", "E33", "B34", "B34", "U13")

t.inc.cell <- c("-", "G50", "G33", "F33", "C34", "C34", "U23")

auth.cell <- c("-", "A2", "A2", "A2", "A2", "A1", "E2")
scotland.i.e. <- data_frame(fisc.year,year,report, file.name, link, 
                            start.sh, end.sh, exp.cell,
                            inc.cell, t.exp.cell, t.inc.cell,
                            auth.cell)


## commented out to stop overwriting!
# saveRDS(scotland.i.e., "data/02-interim/scotland.i.e.17.18.rds")

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

scotland.pnc <- data.frame(fisc.year, year,file.name, file.type, link, dpe.tab,pcn.tab, e.i.tab)

## commented out to stop overwriting!
# saveRDS(scotland.pnc, "data/02-interim/scotland.pnc.17.18.rds")
