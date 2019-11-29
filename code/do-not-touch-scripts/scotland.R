################################################################################
##           CODE REQUIRED FOR COMPILATION OF SCOTLAND REPORT                 ##
##                                                                            ##
##          This script is sourced  by the Scotland .Rmd file.                ##
##                                                                            ##
##              You do not need to run this directly ever.                    ##
##                                                                            ##
##                 Also, DO NOT MAKE ANY CHANGES HERE.                        ##
##                                                                            ##
################################################################################

## preliminaries ###############################################################
current.year <- params$current.year
# knitr options
knitr::opts_chunk$set(warning=FALSE, message=FALSE, echo = FALSE)
knitr::opts_chunk$set(fig.pos = 'H')

# load packages
suppressWarnings(suppressMessages(library(kableExtra)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(tibble)))
suppressWarnings(suppressMessages(library(showtext)))
suppressWarnings(suppressMessages(library(sf)))
suppressWarnings(suppressMessages(library(viridis)))
suppressWarnings(suppressMessages(library(classInt)))
suppressWarnings(suppressMessages(library(colorspace)))
options(knitr.kable.NA = '')
options(stringsAsFactors = FALSE)

# add font for plots
font_add(family = "meri", regular = 
           here::here("data/01-raw/fonts/merriweather.google.ttf"))
showtext_auto()

# source functions
source(here::here("code/do-not-touch-scripts/functions.R"))

# load data
master <- readRDS(here::here("data/03-processed/master.rds"))
orig.sco.name.lookup <- readRDS(here::here("data/01-raw/orig.sco.name.lookup.rds"))
uc <- st_read(here::here("data/01-raw/maps/Local_Administrative_Units_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp"), quiet = TRUE)
rpi <- read.csv(here::here("data/01-raw/rpi.csv"))

# load bibliography from report.name
report.name <- paste0("scotland-report-", current.year, "-",
                      current.year - 1999)
bib <- readRDS(here::here(paste0("data/03-processed/", report.name, "-bib.rds")))

# pass parameters for decimal points in text and tables
dp.text <-  params$dp.text
dp.tables <-  params$dp.tables

# create folder for csv tables if it does not exist already
suppressWarnings(dir.create(here::here(paste0("outputs/csv-tables/scotland-", FunFisc())), 
                            showWarnings =TRUE))

## data preparation ############################################################
# extract relevant country subset of data for all years
master %>% 
  filter(country == "Scotland") %>% 
  filter(year <= current.year) -> data.full

# only for the last four years
data.full %>% 
  filter(year >= current.year -4 & year <= current.year) -> data

# data for current year only
data %>% 
  filter(year == current.year) -> data.current


## extract some useful variables like bib references and column headers ########
# current main I.E. reference
bib %>% 
  filter(country == "Scotland", fiscyear == current.year, content == "i.e") %>% 
  pull(refs) -> sco.bib.main.i.e

# current main pcn reference
bib %>% 
  filter(country == "Scotland", fiscyear == current.year, content == "pcn") %>% 
  pull(refs) -> sco.bib.main.pcn

# original scanned pcn reference
bib %>% 
  filter(country == "Scotland", fiscyear == 2013, content == "pcn") %>% 
  pull(refs) -> sco.bib.old.pcn

# map reference
bib %>% 
  filter( content == "map") %>% 
  pull(refs) -> sco.bib.map

# rpi reference
bib %>% 
  filter( content == "rpi") %>% 
  pull(refs) -> sco.bib.rpi

# column headers 
yearz <- paste0("\\multirow{1}{*}[0pt]{",(current.year-4):(current.year), "-",
                (current.year-4-1999):(current.year-1999),"}")

################################################################################
## cleaning up data for various tables and charts ##############################
################################################################################

## INTRODUCTION ################################################################
# clean up DPE status table
data.current %>% 
  select(auth.name, dpe.status, dpe.year) %>% 
  arrange(dpe.status) %>% 
  group_by(dpe.status) %>% 
  mutate(id =  1:n())  %>% 
  ungroup() %>% 
  complete(dpe.status, nesting( id)) %>% 
  select(auth.name, dpe.status, dpe.year) %>% 
  gather(variable, value, -(dpe.status)) %>% 
  unite(temp, dpe.status, variable)  %>% 
  group_by(temp) %>% 
  mutate(id = 1:n()) %>% 
  spread(temp, value) %>% 
  select(-dpe.next_dpe.year, -dpe.not_dpe.year) %>% 
  unite(dpe.now, dpe.now_auth.name, dpe.now_dpe.year, sep = " (") %>% 
  mutate(dpe.now = paste0(dpe.now, ")")) %>% 
  rename(dpe.next = dpe.next_auth.name , dpe.not = dpe.not_auth.name) %>% 
  select(dpe.now, dpe.next, dpe.not) -> sco.dpe 

# save csv table 1
write.csv(sco.dpe, here::here(paste0("outputs/csv-tables/scotland-",
                                     FunFisc(), "/scotland-", 
                                     FunFisc(), "-table-01.csv")),
          row.names = FALSE)

# prepare data for DPE map
uc %>% 
  filter(grepl("^S", lau118cd)) %>% 
  mutate(lau118nm = recode(lau118nm, !!!orig.sco.name.lookup)) %>% 
  left_join(select(data.current, auth.name, dpe.status), 
            by = c("lau118nm"= "auth.name")) -> sco.map


## SUMMARY #####################################################################
# clean data for Summary table
data %>% 
  select(year, auth.type, income.total, expend.total, 
         surplus.total, transport.total) %>% 
  group_by(year, auth.type) %>% 
  summarise_at(vars(income.total:transport.total), sum) %>% 
  select(-auth.type) %>% 
  summarise_at(vars(income.total:transport.total), list(~sum(.,na.rm=TRUE))) %>% 
  mutate_at(vars(income.total:transport.total), list(~./1000))  %>% 
  mutate(surplus.of.transport = 100 * surplus.total/transport.total) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  filter(rowname != "year") %>%
  mutate(change = FunPercent(V5, V4) - 100)  %>% 
  rename(!!as.name(current.year -4) := V1,
         !!as.name(current.year -3) := V2,
         !!as.name(current.year -2) := V3,
         !!as.name(current.year -1) := V4,
         !!as.name(current.year ) := V5) -> sco.summary

# save csv table 2
write.csv(sco.summary, here::here(paste0("outputs/csv-tables/scotland-",
                                         FunFisc(), "/scotland-", 
                                         FunFisc(), "-table-02.csv")),
          row.names = FALSE)

# extract total England transport-surplus ratio (single number used in text) 
master %>% 
  filter(country == "England") %>% 
  filter(year == current.year) %>% 
  summarise(transport = sum(transport.total, na.rm = TRUE),
            surplus = sum(surplus.total, na.rm = TRUE)) %>% 
  mutate(proportion = 100*surplus/transport) %>% 
  pull(proportion)  -> eng.trans


# prepare summary data for tabulation
sco.summary %>% 
  mutate(change = ifelse(rowname == "surplus.of.transport", NA, 
                         paste(FunDec(change, dp.tables), "\\%"))) %>% 
  mutate(rowname = c("Income", "Expenditure", "Surplus", "Net expenditure", 
                     "Parking surplus as percentage of net transport expenditure"))  %>% 
  mutate_at(vars(-rowname, -change), function(x) FunDec(x, dp.tables)) %>% 
  mutate_at(vars(-rowname, -change), function(x) ifelse(.$rowname == "Parking surplus 
                                            as percentage of net transport 
                                            expenditure", paste0(x, " \\%"), x)) %>%
  mutate(collapsed = c(rep("Parking", 3), "Total transport", "")) %>% 
  select(collapsed, rowname:change) -> sco.summary.formatted

# prepare summary trend data for chart
data.full  %>% 
  select(year, income.total, expend.total, surplus.total) %>% 
  group_by(year) %>% 
  summarise_at(vars(income.total:surplus.total), list(~sum(.,na.rm=TRUE))) %>% 
  mutate_at(vars(income.total:surplus.total), list(~./1000)) -> sco.plot

# comparing summaries for all three countries
# get all the data summarised for all 3 countries, separating out London
master %>% 
  mutate(country = recode(auth.type, "X" = "X", "L" = "London",
                          .default = country),
         country = recode(country, "England" =  "England without London",
                          .default = country)) %>% 
  group_by(country) %>% 
  filter(country != "X") %>% 
  group_by(country, year) %>% 
  select(country, year, income.total, expend.total, surplus.total) %>% 
  summarise(income = sum(income.total, na.rm = TRUE),
            expend = sum(expend.total, na.rm = TRUE),
            surplus = sum(surplus.total, na.rm = TRUE)) %>% 
  bind_rows(group_by(., year) %>% 
              mutate(n = n()) %>% 
              filter(n == 4) %>% 
              summarise_at(vars(income:surplus), sum) %>% 
              mutate(country = "Great Britain")) -> sub.gb.years


# extract most recent year, calculate proportion and reshape
sub.gb.years %>% 
  filter(income > 0) %>% 
  filter(year == max(year))  %>% 
  mutate(prop.of.income = surplus/income,
         year = paste0("(", year, "-", year-1999, ")"),
         surplus = surplus/1000, 
         income = income/1000, 
         expend = expend/1000, 
         prop.of.income = prop.of.income*100) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column("var") %>% 
  setNames(.[1,]) %>% 
  filter(country != "country") %>% 
  mutate(country = c("Fiscal year", "Parking income", "Parking expenditure", 
                     "Surplus", "Surplus as proportion of income")) -> sum.gb

# save csv table 3
write.csv(sum.gb, here::here(paste0("outputs/csv-tables/scotland-",
                                    FunFisc(), "/scotland-", 
                                    FunFisc(), "-table-03.csv")),
          row.names = FALSE)

# same as before, but add % signs to bottom row for tabulation
sub.gb.years %>% 
  filter(income > 0) %>% 
  filter(year == max(year))  %>% 
  mutate(prop.of.income = surplus/income,
         year = paste0("(", year, "-", year-1999, ")"),
         surplus = FunDec(surplus/1000, dp.tables), 
         income = FunDec(income/1000, dp.tables), 
         expend = FunDec(expend/1000, dp.tables), 
         prop.of.income = paste0(FunDec(prop.of.income*100, dp.tables),"\\%")) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column("var") %>% 
  setNames(.[1,]) %>% 
  filter(country != "country") %>% 
  mutate(country = c("Fiscal year", "Parking income", "Parking expenditure", 
                     "Surplus", "Surplus as proportion of income")) -> 
  sum.gb.formatted


# table annual and average changes ############################################
# extract current year, 1 year back,and 4 years back from to most recent year
sub.gb.years %>% 
  filter(income != 0) %>% 
  group_by(country) %>% 
  mutate(most.recent = max(year)) %>% 
  filter(year == most.recent | year == most.recent - 1 |
           year == most.recent-4) -> sub.gb.ref

# calculate change over 4 years as well as last year. 
sub.gb.ref  %>% 
  mutate(year = paste0("m",abs(year - most.recent))) %>% 
  gather(var, value, 3:5) %>% 
  unite(temp, year, var) %>% 
  spread(temp, value) %>% 
  mutate(income.change.4 = FunPercent(m0_income , m4_income, 1), 
         expend.change.4 = FunPercent(m0_expend , m4_expend, 1),
         surplus.change.4 = FunPercent(m0_surplus,  m4_surplus, 1)) %>% 
  mutate_at(vars(ends_with(".4")), function(x) 100*(x ^ 0.25 - 1)) %>% 
  mutate(income.change = FunPercent(m0_income , m1_income) - 100, 
         expend.change  = FunPercent(m0_expend , m1_expend) - 100,
         surplus.change  = FunPercent(m0_surplus,  m1_surplus) - 100,
         most.recent = paste0("(", most.recent, "-", most.recent-1999, ")")) %>% 
  select(country, most.recent, income.change.4, income.change,
         expend.change.4, expend.change, 
         surplus.change.4, surplus.change) -> sum.gb.change
# prepare for tabulation
sum.gb.change %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column("var") %>% 
  setNames(.[1,]) %>% 
  filter(country != "country") %>% 
  select(country, "England without London", "London", 
         "Scotland", "Wales", "Great Britain") %>% 
  mutate(country = c( "Most recent year available", 
                      "Average annual change in income", 
                      "Change in income since previous year", 
                      "Average annual change in expenditure", 
                      "Change in expenditure since previous year", 
                      "Average annual change in surplus",
                      "Change in surplus since previous year")) -> 
  sum.gb.change.tab

# save csv table 4
write.csv(sum.gb.change.tab, here::here(paste0("outputs/csv-tables/scotland-",
                                               FunFisc(), "/scotland-",
                                               FunFisc(), "-table-04.csv")),
          row.names = FALSE)


# prepare for tabulation with formatting
sum.gb.change %>% 
  ungroup() %>% 
  mutate_at(vars(-country, -most.recent), 
            list(~ paste0(FunDec(., dp.tables), " \\%"))) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column("var") %>% 
  setNames(.[1,]) %>% 
  filter(country != "country") %>% 
  select(country, "England without London", "London", 
         "Scotland", "Wales", "Great Britain") %>% 
  mutate(country = c( "Most recent year available", 
                      "Average annual change in income", 
                      "Change in income since previous year", 
                      "Average annual change in expenditure", 
                      "Change in expenditure since previous year", 
                      "Average annual change in surplus",
                      "Change in surplus since previous year")) ->  
  sum.gb.change.tab.formatted

# some values for text
sco.ann <- as.numeric(sum.gb.change.tab$Scotland[7])

sco.4av <- as.numeric(sum.gb.change.tab$Scotland[6])

gb.ann <- as.numeric(sum.gb.change.tab$`Great Britain`[7])
gb.4av <- as.numeric(sum.gb.change.tab$`Great Britain`[6])

# gb most recent year
sub.gb.ref %>% 
  ungroup() %>% 
  filter(country == "Great Britain") %>% 
  select(most.recent) %>% max() -> gb.mr.year

# RPI calculation 
rpi.annual.gb <- FunRpi(gb.mr.year, n = 4)



## INCOME ######################################################################
# clean up income data, add totals row and change variable
data %>% 
  select(auth.name, year, income.total) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(auth.name != "Scotland") %>% 
  bind_rows(group_by(. ,auth.name) %>%
              ungroup() %>% 
              summarise_at(vars(-auth.name), sum) %>%
              mutate(auth.name='Total')) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4))) -> sco.income


# save csv table 5
write.csv(sco.income, here::here(paste0("outputs/csv-tables/scotland-",
                                         FunFisc(), "/scotland-", 
                                         FunFisc(), "-table-05.csv")),
           row.names = FALSE)

# format table for kable
sco.income %>% 
  mutate(change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(sco.income$change, 
                                                c(sco.income$change, 
                                                  sco.income$change.4),
                                                dir = 1, factor = 1.2)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                            background = 
                              FunDivergePalette(sco.income$change.4, 
                                                c(sco.income$change, 
                                                  sco.income$change.4),
                                                dir = 1, factor = 1.2)[[3]])) ->
  sco.income.formatted

# no income councils
sco.no.income <- nrow(filter(sco.income, !!as.name(current.year) == 0))

# split into increase/decrease and NA

sco.income %>% 
  filter(auth.name != "Total") %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                             ifelse(!!as.name(current.year) == 
                                      !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> income.bin

# select only valid
sco.income %>% 
  filter(auth.name != "Total") %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> sco.income.valid

# get top three LAs and proportion they command
sco.income.valid %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  group_by(row_number() == 1, row_number() == 2, 
           row_number() == 3, row_number() > 3) %>% 
  mutate(sum = sum(!!as.name(current.year))) %>% 
  ungroup() %>% 
  filter(row_number() <= 4) %>% 
  mutate(auth.name = ifelse(row_number() == 4, "Other", auth.name)) %>% 
  select(auth.name, sum)  %>% 
  mutate(total = sum(sum)) %>% 
  group_by(auth.name == "Other") %>% 
  mutate(proportion = 100*sum(sum)/total) %>% 
  ungroup() %>% 
  select(auth.name, proportion) %>% 
  deframe() -> sco.income.top3

# get top three relevant income changes
sco.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> sco.income.change.top3

# find excluded rows in top of the table 
sco.income.valid %>% 
  filter(rowname <= max(sco.income.change.top3$rowname)) %>% 
  anti_join(sco.income.change.top3) -> sco.income.excluded.top

# get bottom two relevant income changes
sco.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> sco.income.change.bottom2

# find excluded rows in bottom of the table 
sco.income.valid %>% 
  filter(rowname > min(sco.income.change.bottom2$rowname)) %>% 
  anti_join(sco.income.change.bottom2) -> sco.income.excluded.bottom

## PCN #########################################################################
# extract data for PCN numbers for each year, and the PCN income for the most 
# recent year
data %>% 
  filter(year >= max(current.year-4, 2013)) %>% 
  select(auth.name, year, pcn.number) %>% 
  spread(key = year, value = pcn.number) %>% 
  mutate_at(vars(contains("2013")), function(x) x*1000) %>% 
  full_join(data %>% 
              select(auth.name, year, income.pcn) %>% 
              filter(year == current.year)) %>% 
  bind_rows(group_by(. ,auth.name) %>%
              ungroup() %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total'))  %>% 
  mutate(income.per.pcn = 1000* income.pcn/!!as.name(current.year)) %>% 
  select(-year, -income.pcn) %>% 
  filter(!is.na(income.per.pcn)) -> sco.pcn.numbers

# save csv table 6
write.csv(sco.pcn.numbers, here::here(paste0("outputs/csv-tables/scotland-",
                                             FunFisc(), "/scotland-", 
                                             FunFisc(), "-table-06.csv")),
          row.names = FALSE)

sco.pcn.numbers %>% 
  mutate(income.per.pcn = ifelse(is.na(income.per.pcn), NA, 
                                 paste0("£", FunDec(income.per.pcn, dp.tables)))) ->
  sco.pcn.numbers.formatted

# get correct number of headers for the table
x <- ncol(sco.pcn.numbers) - 3
yearz.pcn <- paste0("\\multirow{1}{*}[0pt]{",(current.year-x):(current.year), 
                    "-", (current.year-x-1999):(current.year-1999),"}")

# get average anual increase over the years in the table
sco.pcn.numbers %>% 
  filter(auth.name == "Total") %>% 
  select(-auth.name, -income.per.pcn) %>% 
  mutate(annual = (.[[ncol(.)]]/ .[[1]] )^(1/(ncol(.)-1))) %>% 
  pull(annual)*100 -100 -> sco.pcn.numbers.annual

# get £ per PCN for total Scotland
sco.pcn.tot.average <- filter(sco.pcn.numbers, auth.name == "Total") %>% 
  pull(income.per.pcn)

# calculate proportion of PCN in income for all LA/year combinations
# and also for each year's total. 
data %>% 
  select(country, auth.name, year, income.pcn, income.total) %>% 
  filter(auth.name != "Scotland"& year > 2012) %>% 
  arrange(year, desc(income.total)) %>%
  bind_rows(master %>% 
              select(country, auth.type, year, income.pcn, income.total) %>% 
              filter(auth.type != "X",  year > 2012 , year <= current.year,
                     country != "Wales") %>% 
              mutate(country = recode(auth.type, "L" = "London",
                                      .default = country),
                     country = recode(country, 
                                      "England" =  "England without London",
                                      .default = country)) %>% 
              group_by(country, year) %>% 
              summarise_at(vars(income.pcn, income.total), 
                           list(~sum(., na.rm = TRUE))) %>% 
              mutate(auth.name = country) %>% 
              ungroup() %>% 
              mutate(country = ifelse(country == "Scotland", 
                                      "Scotlan", country))) %>%  
  mutate(pcn.prop = income.pcn/income.total*100) %>% 
  select(country, auth.name, year, pcn.prop) %>% 
  mutate(pcn.prop = ifelse(is.nan(pcn.prop), NA, 
                           ifelse(is.infinite(pcn.prop), NA, pcn.prop))) %>% 
  spread(key = year, value = pcn.prop) %>% 
  arrange(desc(country), auth.name) %>% 
  select(-country)  %>% 
  select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% 
  mutate(auth.name = ifelse(auth.name == "Scotland", 
                            "Scottish DPE authorties", auth.name)) %>% 
  filter(rowSums(is.na(.[,2:5]))!=4)  -> sco.pcn.prop

# save csv table 7
write.csv(sco.pcn.prop, here::here(paste0("outputs/csv-tables/scotland-",
                                          FunFisc(), "/scotland-", 
                                          FunFisc(), "-table-07.csv")),
          row.names = FALSE)


sco.pcn.prop %>%
  mutate_at(vars(-auth.name), 
            function(x) {cell_spec(ifelse(is.na(x), "", 
                                          paste(FunDec(x, dp.tables), "%")), "latex", 
                                   background = 
                                     spec_color(log(x), 
                                                begin = 0.3,
                                                end = 0.9, 
                                                option = "D",
                                                na_color = "#FFFFFF"))}) ->
  sco.pcn.prop.formatted

## EXPENDITURE #################################################################
# get scotland totals for expenditure for each year in data,
# caluclate change on previous year and proportion of income
data %>% 
  select(auth.name, year, expend.total) %>% 
  filter(auth.name != "Scotland") %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(data %>% 
              filter(auth.name != "Scotland") %>% 
              select(auth.name, year, income.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(.[[6]])) %>% 
  bind_rows(group_by(. ,auth.name) %>%
              ungroup() %>% 
              summarise_at(vars(-auth.name), sum) %>%
              mutate(auth.name='Total')) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1),
         prop.income = FunPercent(!!as.name(current.year),income.total)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4)),
         prop.income = ifelse(is.nan(prop.income), NA, 
                           ifelse(is.infinite(prop.income), NA, prop.income))) %>% 
  select(-income.total) -> sco.expend


# save csv table 8
write.csv(sco.expend, here::here(paste0("outputs/csv-tables/scotland-",
                                        FunFisc(), "/scotland-", 
                                        FunFisc(), "-table-08.csv")),
          row.names = FALSE)

# format cells for tabulations
sco.expend %>% 
  mutate(prop.income = ifelse(is.na(prop.income), NA, 
                              paste(FunDec(prop.income, dp.tables), "\\%"))) %>% 
  mutate(change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(sco.expend$change,
                                                c(sco.expend$change,
                                                  sco.expend$change.4),dir = -1,
                                                factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(sco.expend$change.4,
                                                  c(sco.expend$change,
                                                    sco.expend$change.4),dir = -1,
                                                  factor = 1)[[3]])) ->
  sco.expend.formatted


# calculate change and prop of income for Scotland total row. 
sco.expend %>% 
  filter(auth.name == "Total") %>% 
  mutate(current.change = (.[[6]]-.[[5]])/1000,
         last.change = (.[[5]]-.[[4]])/1000) %>% 
  select(change, current.change, last.change) -> sco.expend.t

# count councils with +/- change
sco.expend %>% 
  filter(!grepl("Total", auth.name)) %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                             ifelse(!!as.name(current.year) == 
                                      !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> expend.bin


# select only valid
sco.expend %>% 
  filter(auth.name != "Total") %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> sco.expend.valid

# get top three LAs and proportion they command
sco.expend.valid %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  group_by(row_number() == 1, row_number() == 2, 
           row_number() == 3, row_number() > 3) %>% 
  mutate(sum = sum(!!as.name(current.year))) %>% 
  ungroup() %>% 
  filter(row_number() <= 4) %>% 
  mutate(auth.name = ifelse(row_number() == 4, "Other", auth.name)) %>% 
  select(auth.name, sum)  %>% 
  mutate(total = sum(sum)) %>% 
  group_by(auth.name == "Other") %>% 
  mutate(proportion = 100*sum(sum)/total) %>% 
  ungroup() %>% 
  select(auth.name, proportion) %>% 
  deframe() -> sco.expend.top3

# get top three relevant expenditure changes
sco.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> sco.expend.change.top3

# find excluded rows in top of the table 
sco.expend.valid %>% 
  filter(rowname <= max(sco.expend.change.top3$rowname)) %>% 
  anti_join(sco.expend.change.top3) -> sco.expend.excluded.top

# get bottom two relevant surplus changes
sco.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> sco.expend.change.bottom2

# find excluded rows in bottom of the table 
sco.expend.valid %>% 
  filter(rowname > min(sco.expend.change.bottom2$rowname)) %>% 
  anti_join(sco.expend.change.bottom2) -> sco.expend.excluded.bottom

# calculate the proportion of the expenditure by Glasgow and Edinburgh
sco.expend %>% 
  select(auth.name, prop.income) %>% 
  filter(auth.name %in% c("Total", "Glasgow City", "Edinburgh City")) %>% 
  deframe() -> sco.expend.p

# calculate proportion of expenditure in income for all LA/year combinations
# and also for each year's total. 

data %>% 
  select(auth.name, year, expend.total, income.total) %>% 
  filter(auth.name != "Scotland") %>% 
  bind_rows(group_by(., year) %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total'))  %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  spread(key = year, value = expend.prop) %>% 
  full_join(data %>% 
              filter(auth.name != "Scotland"& year == current.year) %>% 
              select(auth.name, year, expend.total)) %>% 
  arrange(desc(expend.total)) %>% 
  select(-year, -expend.total) -> sco.expend.of.income

# save csv table 9
write.csv(sco.expend.of.income, here::here(paste0("outputs/csv-tables/scotland-",
                                                  FunFisc(), "/scotland-", 
                                                  FunFisc(), "-table-09.csv")),
          row.names = FALSE)

# format for tabulation
sco.expend.of.income %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  mutate_at(vars(-auth.name), function(x) { 
    cell_spec(ifelse(is.na(x), "", paste(FunDec(x, dp.tables), "%")), "latex", 
              background  = spec_color(1/x, begin = 0.3,
                                       end = 0.9, option = "D", 
                                       na_color = "#FFFFFF"))}) ->
  sco.expend.of.income.formatted



## SURPLUS #####################################################################
# clean up surplus data
data %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  full_join(data %>% 
              select(auth.name, year, transport.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(.[[6]])) -> sco.surplus

# get numbers of +/-/0 surplus change
sco.surplus %>% 
  filter(auth.name != "Total") %>% 
  mutate(sign = ifelse(!!as.name(current.year) > 0, "poz",
                       ifelse(!!as.name(current.year) == 0, "zero", "neg"))) %>% 
  group_by(sign) %>% 
  summarise(n = n()) %>% 
  deframe() -> surplus.bin

# create totals row for surpluses and deficits. 
data %>% 
  select(auth.name, year, surplus.total, transport.total) %>% 
  mutate(poz.neg = ifelse(surplus.total >= 0, "poz", "neg")) %>% 
  group_by(year, poz.neg) %>%
  summarise_at(vars(-auth.name), sum) %>% 
  full_join(expand.grid(year = (current.year - 4):current.year,
                        poz.neg = c("poz", "neg", NA)) %>% 
              mutate(poz.neg = as.character(poz.neg))) %>% 
  gather(variable, value, -c(year, poz.neg)) %>%
  unite(temp, year, variable) %>%
  spread(temp, value) %>% 
  filter(!is.na(poz.neg)) %>% 
  select(poz.neg, contains("surplus"), paste0(current.year, 
                                              "_transport.total"))  %>% 
  mutate(poz.neg = c("Total deficit", "Total surplus")) %>% 
  rename_all(~ c("auth.name", (current.year-4):(current.year), 
                 "transport.total")) %>% 
  bind_rows(group_by(., auth.name) %>% 
              ungroup() %>% 
              summarise_at(vars(-auth.name), sum) %>%
              mutate(auth.name = c("Total"))) -> sco.surplus.totals

# bind both tables together
bind_rows(sco.surplus, sco.surplus.totals) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         prop.transp = 100*.[[6]]/.[[7]]) %>% 
  mutate(change = ifelse(abs(sign(!!as.name(current.year)) -
                               sign(!!as.name(current.year-1))) == 2, NA, 
                         change)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         prop.transp = ifelse(is.nan(prop.transp), NA, 
                              ifelse(is.infinite(prop.transp), NA, 
                                     prop.transp))) %>% 
  select(-transport.total) -> sco.surplus.totals.table

# save csv table 10
write.csv(sco.surplus.totals.table, here::here(
  paste0("outputs/csv-tables/scotland-", FunFisc(), "/scotland-", 
         FunFisc(), "-table-10.csv")), row.names = FALSE)

# format for tabulation
sco.surplus.totals.table %>% 
  mutate(change = ifelse(is.na(change), NA, 
                         paste(FunDec(change, dp.tables), "%")),
         prop.transp = ifelse(is.na(prop.transp), NA, 
                              paste(FunDec(prop.transp, dp.tables), "\\%"))) %>% 
  mutate(change =cell_spec(change, "latex", 
                           italic = ifelse(is.na(.[[7]]), FALSE,
                                           ifelse(.[[6]] < 0 , TRUE, FALSE)))) %>% 
  mutate(change = ifelse(change == "NA", "", change)) -> 
  sco.surplus.totals.table.formatted


# get top three LAs and proportion they command
sco.surplus %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(!!as.name(current.year) >= 0) %>% 
  group_by(row_number() == 1, row_number() == 2, 
           row_number() == 3, row_number() > 3) %>% 
  mutate(sum = sum(!!as.name(current.year))) %>% 
  ungroup() %>% 
  filter(row_number() <= 4) %>% 
  mutate(auth.name = ifelse(row_number() == 4, "Other", auth.name)) %>% 
  select(auth.name, sum)  %>% 
  mutate(total = sum(sum)) %>% 
  group_by(auth.name == "Other") %>% 
  mutate(proportion = 100*sum(sum)/total) %>% 
  ungroup() %>% 
  select(auth.name, proportion) %>% 
  deframe() -> sco.surplus.top3

# surplus.councils (pozitive)
sco.poz.surplus <- nrow(filter(sco.surplus, !!as.name(current.year) >= 0))

# extract "surplus" table where only valid changes are 
sco.surplus %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) >= 0,
         !!as.name(current.year - 1) >= 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> sco.surplus.valid

# get top three relevant surplus changes
sco.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> sco.surplus.change.top3

# find excluded rows in top of the table 
sco.surplus.valid %>% 
  filter(rowname <= max(sco.surplus.change.top3$rowname)) %>% 
  anti_join(sco.surplus.change.top3) -> sco.surplus.excluded.top

# get bottom two relevant surplus changes
sco.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> sco.surplus.change.bottom2

# find excluded rows in bottom of the table 
sco.surplus.valid %>% 
  filter(rowname > min(sco.surplus.change.bottom2$rowname)) %>% 
  anti_join(sco.surplus.change.bottom2) -> sco.surplus.excluded.bottom

##

# extract "deficit" table where only valid changes are 
sco.surplus %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) < 0,
         !!as.name(current.year - 1) < 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> sco.deficit.valid

# get top three relevant surplus changes
sco.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> sco.deficit.change.top3

# find excluded rows in top of the table 
sco.deficit.valid %>% 
  filter(rowname <= max(sco.deficit.change.top3$rowname)) %>% 
  anti_join(sco.deficit.change.top3) -> sco.deficit.excluded.top

# get bottom two relevant surplus changes
sco.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> sco.deficit.change.bottom2

# find excluded rows in bottom of the table 
sco.deficit.valid %>% 
  filter(rowname > min(sco.deficit.change.bottom2$rowname)) %>% 
  anti_join(sco.deficit.change.bottom2) -> sco.deficit.excluded.bottom

## COMPARE TWO DATA SOURCES ###################################################
# clean up data
data %>% 
  filter(year == current.year) %>% 
  select(auth.name, income.total, expend.total, surplus.total,
         income.tfs, expend.tfs) %>% 
  mutate(surplus.tfs = income.tfs - expend.tfs,
         income.diff = (income.total - income.tfs),
         expend.diff = (expend.total - expend.tfs),
         surplus.diff = (surplus.total - surplus.tfs)) -> sco.compare.tab

# get top 2 and bottow 2 income differences
sco.compare.tab %>% 
  arrange(desc(income.diff)) %>% 
  select(auth.name, income.diff, expend.diff, surplus.diff) %>% 
  filter(!is.na(income.diff)) %>% 
  filter(row_number() < 3 | row_number() > n()-2) -> sco.compare.income.ht


# add row with totals
sco.compare.tab %>% 
  bind_rows(ungroup(.) %>% 
              summarise_at(vars(income.total:surplus.diff), 
                           list(~sum(., na.rm = TRUE))) %>% 
              mutate(auth.name = "Total")) -> sco.compare.tab


# save csv table 11
write.csv(sco.compare.tab, here::here(paste0("outputs/csv-tables/scotland-",
                                             FunFisc(), "/scotland-", 
                                             FunFisc(), "-table-11.csv")),
          row.names = FALSE)

