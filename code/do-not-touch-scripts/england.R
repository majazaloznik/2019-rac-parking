################################################################################
##           CODE REQUIRED FOR COMPILATION OF ENGLAND  REPORT                 ##
##                                                                            ##
##          This script is sourced  by the Scotland .Rmd file.                ##
##                                                                            ##
##              You do not need to run this directly ever.                    ##
##                                                                            ##
##                 Also, DO NOT MAKE ANY CHANGES HERE.                        ##
##                                                                            ##
################################################################################

## preliminaries ###############################################################
current.year <-   params$current.year
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
suppressWarnings(suppressMessages(library(english)))
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
orig.eng.name.lookup <- readRDS(here::here("data/01-raw/orig.eng.name.lookup.rds"))
uc <- st_read(here::here(paste0("data/01-raw/maps/Local_Administrative_Units_",
                                "Level_1_January_2018_Ultra_Generalised_Clipped_",
                                "Boundaries_in_United_Kingdom.shp")), quiet = TRUE)
rpi <- read.csv(here::here("data/01-raw/rpi.csv"))

# load bibliography from report.name
report.name <- paste0("england-report-", current.year, "-",
                      current.year - 1999)
bib <- readRDS(here::here(paste0("data/03-processed/", report.name, "-bib.rds")))

# pass parameters for decimal points in text and tables
dp.text <-   params$dp.text
dp.tables <-  params$dp.tables

# create folder for csv tables if it does not exist already
suppressWarnings(dir.create(here::here(paste0("outputs/csv-tables/england-", 
                                              FunFisc())), showWarnings =TRUE))

## data preparation ############################################################
# extract relevant country subset of data for all years
# and calculate on and off surplus
master %>% 
  filter(country == "England") %>% 
  mutate(surplus.on = income.on - expend.on,
         surplus.off = income.off - expend.off,
         income.fnp = income.on - income.pcn,
         surplus.budget = -surplus.budget,
         surplus.cong.ch = income.cong.ch - expend.cong.ch,
         budg.cong.ch = -budg.cong.ch) %>% 
  filter(year <= current.year + 1) -> data.full

# only for the last four years and next year (for budget)
data.full %>% 
  filter(year >= current.year -4 & year <= current.year + 1) -> data

# data for current year only
# but make sure the misnamed/merged LAs aren't there though
data %>% 
  filter(year == current.year, 
         !is.na(income.on)) -> data.current

# data for 353 LAs only for all available years
data.full %>% 
  filter(!auth.type %in% c("X", "GLA", "O")) -> la.data.all

# data for 353 LAs only for last 5 years
data %>% 
  filter(!auth.type %in% c("X", "GLA", "O")) -> la.data

data.current %>% 
  filter(!auth.type %in% c("X", "GLA", "O")) -> la.data.current

# number of councils 
la.data.current %>% 
  nrow() -> no.councils
# extract some useful variables like bib references and column headers ########
# current main I.E. reference
bib %>% 
  filter(country == "England", fiscyear == current.year, content == "i.e") %>% 
  pull(refs) -> eng.bib.main.i.e

# current  budget reference
bib %>% 
  filter(country == "England", fiscyear == current.year, content == "budget") %>% 
  pull(refs) -> eng.bib.main.budget

# next  budget reference
bib %>% 
  filter(country == "England", fiscyear == current.year + 1, 
         content == "budget") %>% 
  pull(refs) -> eng.bib.next.budget

# all except current main I.E. referece
bib %>% 
  filter(country == "England", fiscyear < current.year , 
         content == "i.e") %>% 
  pull(refs) -> eng.bib.previous.i.e

# all except next budget referece
bib %>% 
  filter(country == "England", fiscyear < current.year + 1, 
         content == "budget") %>% 
  pull(refs) -> eng.bib.previous.budget

# map reference
bib %>% 
  filter( content == "map") %>% 
  pull(refs) -> sco.bib.map

# rpi reference
bib %>% 
  filter( content == "rpi") %>% 
  pull(refs) -> eng.bib.rpi

# column headers 
yearz <- paste0("\\multirow{1}{*}[0pt]{",(current.year-4):(current.year), "-",
                (current.year-4-1999):(current.year-1999),"}")

# Introduction ################################################################
# number of national parks
data.current %>% 
  filter(auth.type == "O") %>% 
  nrow -> n.nat.parks

# total for national parks
data.current %>% 
  filter(auth.type == "O") %>% 
  select(surplus.total) %>% 
  summarise(surplus.total = sum(surplus.total)) -> surplus.nat.parks

# total for Nottingham wpl
data.current %>% 
  filter(auth.name == "Nottingham") %>% 
  mutate(surplus.wpl = income.wpl-expend.wpl) %>% 
  pull(surplus.wpl) -> surplus.wpl

# Summary England data ########################################################

# all england transport data
data %>% 
  filter(auth.type == "X") %>% 
  filter(year %in% c( (current.year - 4): current.year)) %>% 
  select(year, transport.total) -> summary.eng.transp

# all i.e. data for LAs only. no Nat parks, but including WPL?
# add transport data and compute proportion from parking
la.data %>% 
  filter(year %in% c( (current.year - 4): current.year)) %>% 
  group_by(year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(year,
         income.fnp,
         income.pcn,
         income.on,
         expend.on,
         surplus.on,
         income.off,
         expend.off,
         surplus.off,
         income.total,
         expend.total,
         surplus.total) %>% 
  left_join(summary.eng.transp) %>% 
  mutate_at(vars(income.fnp:transport.total), function(x) x/1000) %>% 
  mutate(prop.net.expen = 100* surplus.total/transport.total) -> summary.i.e

# budget data for current and next year
la.data %>%
  select(year, surplus.budget) %>% 
  group_by(year) %>% 
  summarise(surplus.total =  sum(surplus.budget)) %>% 
  filter(year %in% c(current.year, current.year +1)) -> budget.surplus

# budgeted total for current and next year 
data %>%
  filter(auth.type == "X", year %in% c(current.year, current.year +1)) %>% 
  select(year, transport.total = budg.trans) -> budget.transport

# merge budgeted totals
full_join(budget.surplus, budget.transport) %>% 
  mutate_at(vars(-year), function(x) x/1000) %>% 
  mutate(prop.net.expen = surplus.total/transport.total * 100) %>% 
  mutate(year = paste0(year, "B")) -> summary.budget
  
bind_rows(mutate_at(summary.i.e, vars(year), as.character), 
          summary.budget) -> summary

# transpose table and add change variable and budget lines
summary %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  mutate(order = row_number()) %>% 
  group_by(variable) %>% 
  mutate(order = first(order)) %>% 
  spread(rowname, value) %>% 
  arrange(order) %>% 
  filter(variable != "year") %>% 
  mutate_at(vars(-variable), as.numeric) %>% 
  select(-order) %>% 
  ungroup() %>% 
  mutate(change = ifelse(variable == "prop.net.expen", NA,  
                         100*(`5` / `4` - 1))) %>% 
  rename(!!as.character(current.year - 4) := `1`,
         !!as.character(current.year - 3) := `2`,
         !!as.character(current.year - 2) := `3`,
         !!as.character(current.year - 1) := `4`,
         !!as.character(current.year ) := `5`,
         !!paste0(as.character(current.year) , " budget"):= `6`,
         !!paste0(as.character(current.year + 1) , " budget"):= `7`) %>% 
  mutate(variable = 
           c("Fees \\& permits", "Penalties", "Total Income",
             "Expenditure", "Surplus", "Total Income", "Expenditure",
             "Surplus",  "Total Income", "Expenditure", "Surplus", 
             "Net Expenditure", 
             "Parking surplus as percentage of net transport expenditure"))  %>% 
  mutate(collapsed = c(rep("On-street", 5), 
                       rep("Off-street", 3),
                       rep("All parking", 3),
                       "All England transport", "")) %>% 
  select(collapsed, variable:change) -> summary.prep

# remove first two rows, decided we don't need them anymore (26.11.2019)
summary.prep %>% 
  filter(!variable %in% c("Fees \\& permits", "Penalties")) -> summary.prep


# # save csv table 1
write.csv(summary.prep, here::here(paste0("outputs/csv-tables/england-",
                                    FunFisc(), "/england-",
                                    FunFisc(), "-table-01.csv")),
          row.names = FALSE)


# prepare data for tabulation. 
summary.prep %>% 
  mutate(change = ifelse(is.na(change), NA, FunDec(change,1))) %>% 
  mutate_at(vars(-variable, -change, - collapsed), function(x) 
    ifelse(.$variable == "Parking surplus as percentage of net transport expenditure",
           FunDec(x, 1),
           ifelse(is.na(x), NA, 
                  formatC(x, format = "f", 
                          digits = 0, big.mark = ",")))) %>% 
  mutate(change = ifelse(variable == "Parking surplus as percentage of net transport expenditure", NA, 
                         paste(change, "\\%"))) %>% 
  mutate_at(vars(-variable, -change, -collapsed), function(x) ifelse(
    .$variable == "Parking surplus as percentage of net transport expenditure", 
    paste0(x, " \\%"), x)) %>% 
  mutate_at(vars(-collapsed), function(x)  
    ifelse(.$collapsed == "All parking" & .$variable == "Surplus",  
           paste0("\\textbf{", x, "}"), x)) %>% 
  select(collapsed, variable:change) -> eng.summary.formatted


# prepare data for london/rest of england summary table. LAs only
la.data.current %>% 
  mutate(london = ifelse(auth.type == "L", "london", "rest")) %>% 
  group_by(london) %>% 
  summarise_at(vars(income.on, income.off, income.total,
                    expend.on, expend.off, expend.total,
                    surplus.on, surplus.off, surplus.total), 
               list(~sum(., na.rm = TRUE))) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  mutate(order = row_number()) %>% 
  group_by(variable) %>% 
  mutate(order = first(order)) %>% 
  spread(rowname, value) %>% 
  arrange(order)  %>% 
  filter(variable != "london") %>% 
  mutate_at(vars(-variable), as.numeric) %>% 
  select(-order) %>% 
  ungroup()  %>% 
  rename(london = `1`, rest = `2`) %>% 
  mutate(total = london + rest,
         prop = london/total*100, 
         prop = paste(FunDec(prop, dp.tables), "\\%") ) -> summary.london.prep.num

summary.london.prep.num %>% 
  mutate_at(vars(-variable, -prop), function(x) 
    formatC(x/1000, format = "f", digits = 0, big.mark = ",")) %>% 
  mutate(variable = c(rep(c("On-street", "Off-street", "Total"), 3))) %>% 
  mutate(collapsed = c(rep("Income", 3), 
                       rep("Expenditure", 3),
                       rep("Surplus", 3)))   %>% 
  select(collapsed, variable:prop)-> summary.london.prep


# # save csv table 2
write.csv(summary.london.prep, here::here(paste0("outputs/csv-tables/england-",
                                                 FunFisc(), "/england-",
                                                 FunFisc(), "-table-02.csv")),
          row.names = FALSE)

# prepare data for tabulation. 
summary.london.prep%>% 
  mutate_at(vars(-collapsed), function(x)  ifelse(.$variable == "Total",
                                                  paste0("\\textbf{", x, "}"), x)) -> eng.summary.london.formatted
# prepare data for london/rest of england summary table including penalty and fee charges
# this gets used in the text, not table
la.data %>% 
  filter(year %in% c(current.year, current.year -1)) %>% 
  mutate(london = ifelse(auth.type == "L", "london", "rest")) %>% 
  group_by(london, year) %>% 
  summarise_at(vars(income.pcn, income.fnp,income.on, income.off, income.total,
                    expend.on, expend.off, expend.total,
                    surplus.on, surplus.off, surplus.total), 
               list(~sum(., na.rm = TRUE))) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  mutate(order = row_number()) %>% 
  group_by(variable) %>% 
  mutate(order = first(order)) %>% 
  spread(rowname, value) %>% 
  arrange(order)  %>% 
  filter(variable != "london", variable != "year") %>% 
  mutate_at(vars(-variable,), as.numeric) %>% 
  select(-order) %>% 
  ungroup()  %>% 
  rename(london.last = `1`, london.this = `2`,
         rest.last = `3`, rest.this = `4`) %>% 
  mutate(prop.london.ch = london.this/london.last*100-100, 
         prop.rest.ch = rest.this/rest.last*100-100) -> summary.london.prep.plus


# easy access for variables in text
summary.london.prep.num

lnd.prop.off <- 100*as.numeric(summary.london.prep.num$london[2])/
  as.numeric(summary.london.prep.num$london[3])

rest.prop.off <- 100*as.numeric(summary.london.prep.num$rest[2])/
  as.numeric(summary.london.prep.num$rest[3])

lnd.surplus <- as.numeric(summary.london.prep.num$london[9])/1000

lnd.prop.surplus <- 100*as.numeric(summary.london.prep.num$london[9])/
  as.numeric(summary.london.prep.num$total[9])

# easy access vars for the text
inc.ch <- 100*(summary[5,10]/summary[4,10] -1)
inc.on.ch <- 100*(summary[5,4]/summary[4,4] -1)
inc.off.ch <- 100*(summary[5,7]/summary[4,7] -1)

exp.ch <- 100*(summary[5,11]/summary[4,11] -1)
exp.on.ch <- 100*(summary[5,5]/summary[4,5] -1)
exp.off.ch <- 100*(summary[5,8]/summary[4,8] -1)

sur.ch <-  100*(summary[5,12]/summary[4,12] -1)
sur.ch4 <- 100*(summary[5,12]/summary[1,12] -1)

sur.budg.diff <- 100*(summary$surplus.total[5]/summary$surplus.total[6]-1)
sur.budg.inc <- 100*(summary$surplus.total[7]/summary$surplus.total[5]-1)

sur.hyp <- (sur.budg.diff/100+1) * summary$surplus.total[7]

tran.ch <-100*(summary[5,13]/summary[4,13] -1)

summary$prop.net.expen[5]

## comparison for all of GB ###################################################
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

# # save csv table 3
write.csv(sum.gb, here::here(paste0("outputs/csv-tables/england-",
                                          FunFisc(), "/england-",
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
# extract current year, 1 year back,and 4 years back from most recent year
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
  mutate(income.change.4 = 100*(m0_income / m4_income - 1), 
         expend.change.4 = 100*(m0_expend / m4_expend - 1),
         surplus.change.4 = 100*(m0_surplus / m4_surplus - 1)) %>% 
  mutate_at(vars(ends_with(".4")), function(x) 100*(x/100 + 1) ^ 
              ( 1 / (4)) - 100) %>% 
  mutate(income.change = 100*(m0_income/ m1_income - 1), 
         expend.change = 100*(m0_expend/ m1_expend - 1), 
         surplus.change = 100*(m0_surplus/ m1_surplus - 1),
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

# # save csv table 4
write.csv(sum.gb.change.tab, here::here(paste0("outputs/csv-tables/england-",
                                    FunFisc(), "/england-",
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
lond.ann <- as.numeric(sum.gb.change.tab$London[7])
rest.ann <- as.numeric(sum.gb.change.tab$`England without London`[7])

lond.4av <- as.numeric(sum.gb.change.tab$London[6])
rest.4av <- as.numeric(sum.gb.change.tab$`England without London`[6])

gb.ann <- as.numeric(sum.gb.change.tab$`Great Britain`[7])
gb.4av <- as.numeric(sum.gb.change.tab$`Great Britain`[6])

# gb most recent year
sub.gb.ref %>% 
  ungroup() %>% 
  filter(country == "Great Britain") %>% 
  select(most.recent) %>% max() -> gb.mr.year

# RPI calculation 
rpi.annual.gb <- FunRpi(gb.mr.year, n = 4)


# prepare data for trends plot ################################################
# prepare summary trend data for chart
la.data.all %>% 
  select(year, income.total, expend.total, surplus.total, surplus.budget) %>% 
  group_by(year) %>% 
  summarise_at(vars(income.total:surplus.budget), list(~sum(.,na.rm=TRUE))) %>% 
  mutate_at(vars(income.total:surplus.budget), list(~./1000)) %>% 
  mutate_all(function(x) ifelse(x == 0, NA, x)) -> eng.plot


###############################################################################
##                                                                          ###
## INCOME #####################################################################
##                                                                          ###
###############################################################################

# clean up income data, add totals row and change variable
# create three totals row for income data
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(income.total = income.total /1000) %>% 
  select(auth.name, year, income.total, auth.type) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  mutate(auth.name = ifelse(auth.type == "L", "Total for London", "Total for rest of England")) %>% 
  group_by(auth.name) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
bind_rows(group_by(. ,auth.name) %>%
              ungroup() %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total for all of England')) -> income.totals

# only london income
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(income.total = income.total /1000) %>% 
  select(auth.name, year, income.total, auth.type) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(auth.type == "L") %>% 
  select(-auth.type) %>% 
  bind_rows(income.totals) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                               sto = 1) ^ 0.25 - 1)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4))) -> 
  eng.income.london

# # save csv table 5
write.csv(  eng.income.london, here::here(paste0("outputs/csv-tables/england-",
                                                 FunFisc(), "/england-",
                                                 FunFisc(), "-table-05.csv")),
            row.names = FALSE)


# only non-london income
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(income.total = income.total /1000) %>% 
  select(auth.name, year, income.total, auth.type) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(auth.type != "L") %>% 
  select(-auth.type) %>% 
  bind_rows(income.totals) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4))) -> eng.income.rest

# # save csv table 6
write.csv(eng.income.rest, here::here(paste0("outputs/csv-tables/england-",
                                                 FunFisc(), "/england-",
                                                 FunFisc(), "-table-06.csv")),
            row.names = FALSE)


# all councils income for appendix
la.data %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, income.total, auth.type) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  bind_rows(income.totals) %>% 
  select(-auth.type) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1)) %>%  
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4))) -> eng.income

# # save csv table 14 in Appendix
write.csv(eng.income, here::here(paste0("outputs/csv-tables/england-",
                                             FunFisc(), "/england-",
                                             FunFisc(), "-table-ap-14.csv")),
          row.names = FALSE)



# format table for london
eng.income.london %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name),
         change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.income.london$change, 
                                                c(eng.income.london$change, 
                                                  eng.income.london$change.4),
                                                dir = 1, factor = 1.2)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.income.london$change.4, 
                                                  c(eng.income.london$change, 
                                                    eng.income.london$change.4),
                                                  dir = 1, factor = 1.2)[[3]])) %>% 
  as_tibble() ->  eng.income.london.formatted



# format table for kable rest of england
eng.income.rest %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name),
         change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.income.rest$change, 
                                                c(eng.income.rest$change, 
                                                  eng.income.rest$change.4),
                                                dir = 1, factor = 1.2)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.income.rest$change.4, 
                                                  c(eng.income.rest$change, 
                                                    eng.income.rest$change.4),
                                                  dir = 1, factor = 1.2)[[3]])) %>% 
  as_tibble() ->
  eng.income.rest.formatted

# format table for kable rest of england
eng.income %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name),
         change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.income.rest$change, 
                                                c(eng.income.rest$change, 
                                                  eng.income.rest$change.4),
                                                dir = 1, factor = 1.2)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.income.rest$change.4, 
                                                  c(eng.income.rest$change, 
                                                    eng.income.rest$change.4),
                                                  dir = 1, factor = 1.2)[[3]])) %>% 
  as_tibble() ->
  eng.income.formatted


# no income councils
eng.income %>% 
  filter(!grepl("Total", auth.name)) -> e.g.icnome.353

eng.no.income <- nrow(filter(e.g.icnome.353, !!as.name(current.year) == 0))

# split into increase/decrease and NA
e.g.icnome.353 %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                         ifelse(!!as.name(current.year) == 
                                  !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> income.bin

# select only valid
e.g.icnome.353 %>% 
  filter(auth.name != "Total") %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> eng.income.valid

# get top three LAs and proportion they command
eng.income.valid %>% 
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
  deframe() -> eng.income.top3

# get top three relevant income changes
eng.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> eng.income.change.top3

# find excluded rows in top of the table 
eng.income.valid %>% 
  filter(rowname <= max(eng.income.change.top3$rowname)) %>% 
  anti_join(eng.income.change.top3) -> eng.income.excluded.top

# get bottom two relevant income changes
eng.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> eng.income.change.bottom2

# find excluded rows in bottom of the table 
eng.income.valid %>% 
  filter(rowname > min(eng.income.change.bottom2$rowname)) %>% 
  anti_join(eng.income.change.bottom2) -> eng.income.excluded.bottom


## income text numbers  ########################################################
inc.tot <- summary$income.total[5]

inc.fnp <- summary$income.fnp[5]
inc.fnp.ch <- 100*(summary$income.fnp[5]/summary$income.fnp[4]-1)
inc.pcn <- summary$income.pcn[5]
inc.pcn.ch <- 100*(summary$income.pcn[5]/summary$income.pcn[4]-1)
inc.off <- summary$income.off[5]


# income for london
summary.london.prep.plus %>% 
  filter(variable == "income.pcn") %>% 
  pull(london.this)/1000 -> inc.london.pcn 

summary.london.prep.plus %>% 
  filter(variable == "income.pcn") %>% 
  mutate(ch = london.this/london.last) %>% 
  pull(ch)*100-100 -> inc.london.pcn.ch 


summary.london.prep.plus %>% 
  filter(variable == "income.total") %>% 
  pull(london.this)/1000 -> inc.london.tot

summary.london.prep.plus %>% 
  filter(variable == "income.total") %>% 
  mutate(ch = london.this/london.last) %>% 
  pull(ch)*100-100 -> inc.london.tot.ch 
  
# income for rest of england

summary.london.prep.plus %>% 
  filter(variable == "income.pcn") %>% 
  pull(rest.this)/1000 -> inc.rest.pcn 

summary.london.prep.plus %>% 
  filter(variable == "income.pcn") %>% 
  mutate(ch = rest.this/rest.last) %>% 
  pull(ch)*100-100 -> inc.rest.pcn.ch 


summary.london.prep.plus %>% 
  filter(variable == "income.total") %>% 
  pull(rest.this)/1000 -> inc.rest.tot

summary.london.prep.plus %>% 
  filter(variable == "income.total") %>% 
  mutate(ch = rest.this/rest.last) %>% 
  pull(ch)*100-100 -> inc.rest.tot.ch 



###############################################################################
##                                                                          ###
## EXPENDITURE ################################################################
##                                                                          ###
###############################################################################

# get england totals for expenditure for each year in data,
# create three totals row for expenditure data
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(expend.total = expend.total /1000) %>% 
  select(auth.name, auth.type, year, expend.total) %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(la.data %>% 
              select(auth.name, year, income.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  mutate(auth.name = ifelse(auth.type == "L", "Total for London", "Total for rest of England")) %>% 
  group_by(auth.name) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  bind_rows(group_by(. ,auth.name) %>%
              ungroup() %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total for all of England')) ->  expend.totals


# only london expenditure
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(expend.total = expend.total /1000) %>% 
  select(auth.name, year, expend.total, auth.type) %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(la.data %>% 
              select(auth.name, year, income.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(auth.type == "L") %>% 
  select(-auth.type) %>% 
  bind_rows(expend.totals)  %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1),
         prop.income = 100 *!!as.name(current.year)/income.total*1000) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4)),
         prop.income = ifelse(is.nan(prop.income), NA, 
                              ifelse(is.infinite(prop.income), NA, prop.income))) %>% 
  select(-income.total) %>% 
  as_tibble() -> eng.expend.london

# # save csv table 7
write.csv(eng.expend.london, here::here(paste0("outputs/csv-tables/england-",
                                        FunFisc(), "/england-",
                                        FunFisc(), "-table-07.csv")),
          row.names = FALSE)


# rest of england expenditure, with totals
la.data %>% 
  filter(year <= current.year) %>% 
  mutate(expend.total = expend.total /1000) %>% 
  select(auth.name, year, expend.total, auth.type) %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(la.data %>% 
              select(auth.name, year, income.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(auth.type != "L") %>% 
  select(-auth.type) %>% 
  bind_rows(expend.totals)  %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1),
         prop.income = 100 *!!as.name(current.year)/income.total*1000) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4)),
         prop.income = ifelse(is.nan(prop.income), NA, 
                              ifelse(is.infinite(prop.income), NA, prop.income))) %>% 
  select(-income.total) -> eng.expend.rest


# # save csv table 8
write.csv(eng.expend.rest, here::here(paste0("outputs/csv-tables/england-",
                                               FunFisc(), "/england-",
                                               FunFisc(), "-table-08.csv")),
          row.names = FALSE)


# all england LAs expenditure, with totals
la.data %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, expend.total, auth.type) %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(la.data %>% 
              select(auth.name, year, income.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  select(-auth.type) %>% 
  bind_rows(expend.totals)  %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         change.4 = 100*(FunPercent(!!as.name(current.year),!!as.name(current.year -4), 
                                    sto = 1) ^ 0.25 - 1),
         prop.income = 100 *!!as.name(current.year)/income.total) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4)),
         prop.income = ifelse(is.nan(prop.income), NA, 
                              ifelse(is.infinite(prop.income), NA, prop.income))) %>% 
  select(-income.total) -> eng.expend


# # save csv table 15 - appendix
write.csv(eng.expend, here::here(paste0("outputs/csv-tables/england-",
                                             FunFisc(), "/england-",
                                             FunFisc(), "-table-ap-15.csv")),
          row.names = FALSE)

# format cells for tabulations for london
eng.expend.london %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate(prop.income = ifelse(is.na(prop.income), NA, 
                              paste0(FunDec(prop.income, dp.tables), "~\\%"))) %>% 
  mutate(change = ifelse(is.na(change), "", paste0(FunDec(change, dp.tables), " %")),
         change.4 = ifelse(is.na(change.4), "", paste0(FunDec(change.4, dp.tables), " %"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.expend.london$change,
                                                c(eng.expend$change,
                                                  eng.expend$change.4),dir = -1,
                                                factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.expend.london$change.4,
                                                  c(eng.expend$change,
                                                    eng.expend$change.4),dir = -1,
                                                  factor = 1)[[3]])) ->
  eng.expend.london.formatted


# format cells for tabulations for rest of england
eng.expend.rest %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate(prop.income = ifelse(is.na(prop.income), NA, 
                              paste0(FunDec(prop.income, dp.tables), "~\\%"))) %>% 
  mutate(change = ifelse(is.na(change), "", paste0(FunDec(change, dp.tables), " %")),
         change.4 = ifelse(is.na(change.4), "", paste0(FunDec(change.4, dp.tables), " %"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.expend.rest$change,
                                                c(eng.expend$change,
                                                  eng.expend$change.4),dir = -1,
                                                factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.expend.rest$change.4,
                                                  c(eng.expend$change,
                                                    eng.expend$change.4),dir = -1,
                                                  factor = 1)[[3]])) ->
  eng.expend.rest.formatted


# format cells for tabulations for all of england for the appendix
eng.expend %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate(prop.income = ifelse(is.na(prop.income), NA, 
                              paste0(FunDec(prop.income, dp.tables), "~\\%"))) %>% 
  mutate(change = ifelse(is.na(change), "", paste0(FunDec(change, dp.tables), " %")),
         change.4 = ifelse(is.na(change.4), "", paste0(FunDec(change.4, dp.tables), " %"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(eng.expend$change,
                                                c(eng.expend$change,
                                                  eng.expend$change.4),dir = -1,
                                                factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                              background = 
                                FunDivergePalette(eng.expend$change.4,
                                                  c(eng.expend$change,
                                                    eng.expend$change.4),dir = -1,
                                                  factor = 1)[[3]])) ->
  eng.expend.formatted


# calculate change and prop of income for ENglandtotal row. 
eng.expend %>% 
  filter(auth.name == "Total for all of England") %>% 
  mutate(current.change = (.[[6]]-.[[5]]),
         last.change = (.[[5]]-.[[4]])) %>% 
  select(change, current.change, last.change) -> eng.expend.t

# count councils with +/- change
eng.expend %>% 
  filter(!grepl("Total", auth.name)) %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                             ifelse(!!as.name(current.year) == 
                                      !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> expend.bin


# select only valid
eng.expend %>% 
  filter(!grepl("Total", auth.name)) %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> eng.expend.valid

# get top three LAs and proportion they command
eng.expend.valid %>% 
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
  deframe() -> eng.expend.top3

# get top three relevant expenditure changes
eng.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> eng.expend.change.top3

# find excluded rows in top of the table 
eng.expend.valid %>% 
  filter(rowname <= max(eng.expend.change.top3$rowname)) %>% 
  anti_join(eng.expend.change.top3) -> eng.expend.excluded.top

# get bottom two relevant surplus changes
eng.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> eng.expend.change.bottom2

# find excluded rows in bottom of the table 
eng.expend.valid %>% 
  filter(rowname > min(eng.expend.change.bottom2$rowname)) %>% 
  anti_join(eng.expend.change.bottom2) -> eng.expend.excluded.bottom



eng.expend %>% 
  filter(grepl("Total", auth.name)) %>% 
  select(auth.name, prop.income) %>% 
  mutate(prop.income = prop.income) %>% 
  deframe() -> prop.income.summary


## second set of expenditure tables on proportion of income. 
# first get the totals 

la.data %>% 
  select(auth.name, auth.type, year, expend.total, income.total) %>% 
  filter(year <= current.year) %>% 
  mutate(auth.name = ifelse(auth.type == "L", "Total for London", "Total for rest of England")) %>% 
  group_by(auth.name, year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  bind_rows(group_by(. ,auth.name) %>%
              group_by(year) %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total for all of England')) %>% 
  ungroup() %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  mutate(n = row_number()) %>% 
  group_by(auth.name) %>% 
  mutate(n = first(n)) %>% 
  spread(key = year, value = expend.prop) %>% 
  arrange(n) %>% 
  select(-n)  ->  expend.props.totals


# get propo of expenditure in income for london borougns only
la.data %>% 
  select(auth.name, year, expend.total, income.total, auth.type) %>% 
  filter(year <= current.year, auth.type == "L") %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  spread(key = year, value = expend.prop) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  bind_rows(expend.props.totals) -> eng.expend.props.london

# # save csv table 9
write.csv(eng.expend.props.london, here::here(paste0("outputs/csv-tables/england-",
                                        FunFisc(), "/england-",
                                        FunFisc(), "-table-09.csv")),
          row.names = FALSE)

# get propo of expenditure in income for rest of england 
la.data %>% 
  select(auth.name, year, expend.total, income.total, auth.type) %>% 
  filter(year <= current.year, auth.type != "L") %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  spread(key = year, value = expend.prop) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  filter(!is.infinite(!!as.name(current.year))) %>% 
  bind_rows(expend.props.totals) -> eng.expend.props.rest

# # save csv table 10
write.csv(eng.expend.props.rest, here::here(paste0("outputs/csv-tables/england-",
                                                     FunFisc(), "/england-",
                                                     FunFisc(), "-table-10.csv")),
          row.names = FALSE)

# get propo of expenditure in income for all of england 
la.data %>% 
  select(auth.name, year, expend.total, income.total, auth.type) %>% 
  filter(year <= current.year) %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  spread(key = year, value = expend.prop) %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>%
  arrange(desc(!!as.name(current.year))) %>% 
  bind_rows(expend.props.totals) -> eng.expend.props

# # save csv table 16 apendix
write.csv(eng.expend.props, here::here(paste0("outputs/csv-tables/england-",
                                                   FunFisc(), "/england-",
                                                   FunFisc(), "-table-ap-16.csv")),
          row.names = FALSE)

# format for tabulation london only
eng.expend.props.london %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  mutate_at(vars(-auth.name), function(x) { 
    cell_spec(ifelse(is.na(x), "", paste(FunDec(x, dp.tables), "%")), "latex", 
              background  = spec_color(1/x, begin = 0.3,
                                       end = 0.9, option = "D", 
                                       na_color = "#FFFFFF"))}) ->
  eng.expend.props.london.formatted


# format for tabulation rest of england 
eng.expend.props.rest %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  mutate_at(vars(-auth.name), function(x) { 
    cell_spec(ifelse(is.na(x), "", paste(FunDec(x, dp.tables), "%")), "latex", 
              background  = spec_color(1/x, begin = 0.3,
                                       end = 0.9, option = "D", 
                                       na_color = "#FFFFFF"))}) ->
  eng.expend.props.rest.formatted

# format for tabulation all of england
eng.expend.props %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  mutate_at(vars(-auth.name), function(x) { 
    cell_spec(ifelse(is.na(x), "", paste(FunDec(x, dp.tables), "%")), "latex", 
              background  = spec_color(1/x, begin = 0.3,
                                       end = 0.9, option = "D", 
                                       na_color = "#FFFFFF"))}) ->
  eng.expend.props.formatted


## expenditure  text variables #################################################

eng.expend.props %>% 
  filter(auth.name == "Total for all of England") %>% 
  select(!!as.name(current.year)) %>% pull() -> eng.expend.of.income.tot 

eng.expend.props %>% 
  filter(auth.name == "Total for all of England") %>% 
  select(!!as.name(current.year-1)) %>% pull() -> eng.expend.of.income.tot.prev 


exp.tot <- summary$expend.total[5]

# expenditure for london
exp.london.tot.ch <- summary.london.prep.plus$london.this[8]/
  summary.london.prep.plus$london.last[8]*100-100
exp.rest.tot.ch <- summary.london.prep.plus$rest.this[8]/
  summary.london.prep.plus$rest.last[8]*100-100

exp.prop.inc.on <- summary$expend.on[5]/summary$income.on[5]* 100
exp.prop.inc.off <- summary$expend.off[5]/summary$income.off[5]* 100




###############################################################################
##                                                                          ###
## SURPLUS     ################################################################
##                                                                          ###
###############################################################################



## surplus variables in text  #################################################

sur.tot <- summary$surplus.total[5]

# surplus for london and rest
sur.london <- summary.london.prep.plus$london.this[11]/1000
sur.rest <- summary.london.prep.plus$rest.this[11]/1000
sur.london.prop <- sur.london/(sur.london + sur.rest)*100

sur.london.tot.ch <- summary.london.prep.plus$london.this[11]/
  summary.london.prep.plus$london.last[11]*100-100
sur.rest.tot.ch <- summary.london.prep.plus$rest.this[11]/
  summary.london.prep.plus$rest.last[11]*100-100


## SURPLUS ####################################################################
# london surplus table  #######################################################
# clean up surplus data
data %>% 
  filter(auth.type == "L") %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  arrange(desc(.[[6]])) -> london.surplus

# get numbers of +/-/0 surplus change
london.surplus %>% 
  filter(auth.name != "Total") %>% 
  mutate(sign = ifelse(!!as.name(current.year) > 0, "poz",
                       ifelse(!!as.name(current.year) == 0, "zero", "neg"))) %>% 
  group_by(sign) %>% 
  summarise(n = n()) %>% 
  deframe() -> surplus.bin

# create totals row for surpluses and deficits. 
data %>% 
  filter(auth.type == "L") %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, surplus.total) %>% 
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
  select(poz.neg, contains("surplus")) %>% 
  mutate(poz.neg = c("Total deficit", "Total surplus")) %>% 
  rename_all(~c("auth.name", (current.year-4):(current.year))) %>% 
  bind_rows(group_by(., auth.name) %>% 
              ungroup() %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name = c("Total"))) -> london.surplus.totals

# bind both tables together
bind_rows(london.surplus, london.surplus.totals) %>% 
  mutate(change =100*(!!as.name(current.year)/!!as.name(current.year - 1)-1))%>% 
  mutate(change = ifelse(abs(sign(!!as.name(current.year)) -
                               sign(!!as.name(current.year-1))) == 2, NA, 
                         change)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change))) %>% 
  mutate_at(vars(-auth.name, -change), list(~./1000)) -> london.surplus.totals.table

## save csv table 11
write.csv(london.surplus.totals.table, here::here(
  paste0("outputs/csv-tables/england-", FunFisc(), "/england-",
         FunFisc(), "-table-11.csv")), row.names = FALSE)



# format for tabulation
london.surplus.totals.table  %>% 
  mutate(change = ifelse(is.na(change), NA, 
                         paste(FunDec(change, dp.tables), "%"))) %>% 
  mutate(change =cell_spec(change, "latex", 
                           italic = ifelse(is.na(.[[7]]), FALSE,
                                           ifelse(.[[6]] < 0 , TRUE, FALSE)))) %>% 
  mutate(change = ifelse(change == "NA", NA, change)) %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name, -change), list(~ifelse(is.na(.), NA, FunDec(., dp.tables)))) ->
  london.surplus.totals.table.formatted

# format surplus list of london boroughs. 
# top boroug
sur.london.top.la <- london.surplus.totals.table$auth.name[1]
sur.london.top.amount <- london.surplus.totals.table[1,6]
sur.london.top.ch <- london.surplus.totals.table$change[1]

# tob bottom three london #####################################################
# get top three LAs and proportion they command
  london.surplus %>% 
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
  deframe() -> london.surplus.top3

# surplus.councils (pozitive)
london.poz.surplus <- nrow(filter(london.surplus, !!as.name(current.year) >= 0))

# extract "surplus" table where only valid changes are 
london.surplus %>% 
  mutate(change =100*(!!as.name(current.year)/!!as.name(current.year 
                                                        - 1) - 1)) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) >= 0,
         !!as.name(current.year - 1) >= 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> london.surplus.valid

# get top three relevant surplus changes
london.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> london.surplus.change.top3

# find excluded rows in top of the table 
london.surplus.valid %>% 
  filter(rowname <= max(london.surplus.change.top3$rowname)) %>% 
  anti_join(london.surplus.change.top3) -> london.surplus.excluded.top

# get bottom two relevant surplus changes
london.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> london.surplus.change.bottom2

# find excluded rows in bottom of the table 
london.surplus.valid %>% 
  filter(rowname > min(london.surplus.change.bottom2$rowname)) %>% 
  anti_join(london.surplus.change.bottom2) -> london.surplus.excluded.bottom


# extract "deficit" table where only valid changes are 
london.surplus %>% 
  mutate(change =100*(!!as.name(current.year)/!!as.name(current.year 
                                                        - 1) - 1)) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) < 0,
         !!as.name(current.year - 1) < 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> london.deficit.valid

# get top three relevant surplus changes
london.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> london.deficit.change.top3

# find excluded rows in top of the table 
london.deficit.valid %>% 
  filter(rowname <= max(london.deficit.change.top3$rowname)) %>% 
  anti_join(london.deficit.change.top3) -> london.deficit.excluded.top

# get bottom two relevant surplus changes
london.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> london.deficit.change.bottom2

# find excluded rows in bottom of the table 
london.deficit.valid %>% 
  filter(rowname > min(london.deficit.change.bottom2$rowname)) %>% 
  anti_join(london.deficit.change.bottom2) -> london.deficit.excluded.bottom

# london text variables 

london.surplus.totals.table %>% 
  filter(auth.name == "Total surplus") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> london.surplus.totals.table.sur
  
london.surplus.totals.table %>% 
  filter(auth.name == "Total surplus") %>% 
  select(!!as.name(current.year-1)) %>% 
  pull() -> london.surplus.totals.table.sur.last

london.surplus.totals.table %>% 
  filter(auth.name == "Total surplus") %>% 
  select(change) %>% 
  pull() -> london.surplus.totals.table.sur.ch

london.surplus.totals.table %>% 
  filter(auth.name == "Total") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> london.surplus.totals.table.tot

london.surplus.totals.table %>% 
  filter(auth.name == "Total") %>% 
  select(!!as.name(current.year-1)) %>% 
  pull() -> london.surplus.totals.table.tot.last

london.surplus.totals.table %>% 
  filter(auth.name == "Total") %>% 
  select(change) %>% 
  pull() -> london.surplus.totals.table.tot.ch

london.surplus.totals.table %>% 
  filter(auth.name == "Total deficit") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> london.surplus.totals.table.def

london.surplus.totals.table %>% 
  filter(auth.name == "Total deficit") %>% 
  select(!!as.name(current.year-1)) %>% 
  pull() -> london.surplus.totals.table.def.last


# top 20 surplus table  not london ############################################
# clean up surplus data
la.data %>% 
  filter(year <= current.year,
         auth.type != "L") %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  arrange(desc(.[[6]])) %>% 
  filter(row_number() < 21) -> eng.rest.surplus

# not london full table ############################################
# clean up surplus data
la.data %>% 
  filter(year <= current.year,
         auth.type != "L") %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  arrange(desc(.[[6]])) -> eng.rest.surplus.full


# create totals row for surpluses and deficits. 
la.data %>% 
  filter(auth.type != "L") %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, surplus.total) %>% 
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
  select(poz.neg, contains("surplus")) %>% 
  mutate(poz.neg = c("Total deficit", "Total surplus")) %>% 
  rename_all(~c("auth.name", (current.year-4):(current.year))) -> eng.rest.sur.def

# create totals row for surpluses and deficits. 
la.data %>% 
  filter(year <= current.year,
         auth.type != "L") %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total)  %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name = c("All England excl. London")) -> eng.rest.surplus.total


# bind both tables together
bind_rows(eng.rest.surplus.full, eng.rest.sur.def) %>%
  bind_rows(eng.rest.surplus.total) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100)%>% 
  mutate(change = ifelse(abs(sign(!!as.name(current.year)) -
                               sign(!!as.name(current.year-1))) == 2, NA, 
                         change)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change))) %>% 
  mutate_at(vars(-auth.name, -change), list(~./1000)) -> eng.rest.surplus.totals.table

## save csv table 12
write.csv(eng.rest.surplus.totals.table, here::here(
  paste0("outputs/csv-tables/england-", FunFisc(), "/england-",
         FunFisc(), "-table-12.csv")), row.names = FALSE)


# format for tabulation
eng.rest.surplus.totals.table  %>% 
  slice(1:20, (n()-2):(n())) %>%  
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate(change = ifelse(is.na(change), NA, 
                         paste(FunDec(change, dp.tables), "%"))) %>% 
  mutate(change =cell_spec(change, "latex", 
                           italic = ifelse(is.na(.[[7]]), FALSE,
                                           ifelse(.[[6]] < 0 , TRUE, FALSE)))) %>% 
  mutate(change = ifelse(change == "NA", NA, change)) %>% 
  mutate_at(vars(-auth.name, -change), list(~ifelse(is.na(.), NA, FunDec(., dp.tables)))) ->
  eng.rest.surplus.totals.table.formatted


# all  surplus table  - for appendix only ?################################
# clean up surplus data
la.data %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  arrange(desc(.[[6]]))  -> eng.surplus.full

# get numbers of +/-/0 surplus change
la.data %>% 
  filter(year <= current.year) %>% 
  filter(auth.type != "L") %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  mutate(sign = ifelse(!!as.name(current.year) > 0, "poz",
                       ifelse(!!as.name(current.year) == 0, "zero", "neg"))) %>% 
  group_by(sign) %>% 
  summarise(n = n()) %>% 
  deframe() -> surplus.bin.rest

# create totals row for surpluses and deficits. 
la.data %>% 
  filter(year <= current.year) %>% 
  select(auth.name, year, surplus.total) %>% 
  mutate(poz.neg = ifelse(surplus.total >= 0, "poz", "neg")) %>% 
  group_by(year, poz.neg) %>%
  summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>% 
  full_join(expand.grid(year = (current.year - 4):current.year,
                        poz.neg = c("poz", "neg", NA)) %>% 
              mutate(poz.neg = as.character(poz.neg))) %>% 
  gather(variable, value, -c(year, poz.neg)) %>%
  unite(temp, year, variable) %>%
  spread(temp, value) %>% 
  filter(!is.na(poz.neg)) %>% 
  select(poz.neg, contains("surplus")) %>% 
  mutate(poz.neg = c("Total deficit", "Total surplus")) %>% 
  rename_all(~c("auth.name", (current.year-4):(current.year))) %>% 
  bind_rows(group_by(., auth.name) %>% 
              ungroup() %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name = c("Total"))) -> rest.surplus.totals.full

# bind both tables together
bind_rows(eng.surplus.full, rest.surplus.totals.full) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100)%>% 
  mutate(change = ifelse(abs(sign(!!as.name(current.year)) -
                               sign(!!as.name(current.year-1))) == 2, NA, 
                         change)) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change))) %>% 
  mutate_at(vars(-auth.name, -change), list(~./1000)) -> rest.surplus.totals.table.full


## save csv table 17 appendix
write.csv(rest.surplus.totals.table.full, here::here(
  paste0("outputs/csv-tables/england-", FunFisc(), "/england-",
         FunFisc(), "-table-ap-17.csv")), row.names = FALSE)


# format for tabulation
rest.surplus.totals.table.full  %>% 
  mutate(change = ifelse(is.na(change), NA, 
                         paste(FunDec(change, dp.tables), "%"))) %>% 
  mutate(change =cell_spec(change, "latex", 
                           italic = ifelse(is.na(.[[7]]), FALSE,
                                           ifelse(.[[6]] < 0 , TRUE, FALSE)))) %>% 
  mutate(change = ifelse(change == "NA", NA, change)) %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name, -change), list(~ifelse(is.na(.), NA, FunDec(., dp.tables)))) ->
  rest.surplus.totals.table.full.formatted




# tob bottom three rest of england #####################################################
# get top three LAs and proportion they command
eng.rest.surplus.full %>% 
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
  deframe() -> rest.surplus.top3

# surplus.councils (pozitive)
rest.poz.surplus <- nrow(filter(eng.rest.surplus.full, !!as.name(current.year) >= 0))

# extract "surplus" table where only valid changes are 
eng.rest.surplus.full %>% 
  mutate(change = FunPercent(!!as.name(current.year), 
                                     !!as.name(current.year -1)) - 100) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) >= 0,
         !!as.name(current.year - 1) >= 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> rest.surplus.valid

# get top three relevant surplus changes
rest.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> rest.surplus.change.top3

# find excluded rows in top of the table 
rest.surplus.valid %>% 
  filter(rowname <= max(rest.surplus.change.top3$rowname)) %>% 
  anti_join(rest.surplus.change.top3) -> rest.surplus.excluded.top

# get bottom two relevant surplus changes
rest.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> rest.surplus.change.bottom2

# find excluded rows in bottom of the table 
rest.surplus.valid %>% 
  filter(rowname > min(rest.surplus.change.bottom2$rowname)) %>% 
  anti_join(rest.surplus.change.bottom2) -> rest.surplus.excluded.bottom


# extract "deficit" table where only valid changes are 
eng.rest.surplus.full %>% 
  mutate(change = FunPercent(!!as.name(current.year), 
                             !!as.name(current.year -1)) - 100) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) < 0,
         !!as.name(current.year - 1) < 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> rest.deficit.valid

# get top three relevant surplus changes
rest.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> rest.deficit.change.top3

# find excluded rows in top of the table 
rest.deficit.valid %>% 
  filter(rowname <= max(rest.deficit.change.top3$rowname)) %>% 
  anti_join(rest.deficit.change.top3) -> rest.deficit.excluded.top

# get bottom two relevant surplus changes
rest.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> rest.deficit.change.bottom2

# find excluded rows in bottom of the table 
rest.deficit.valid %>% 
  filter(rowname > min(rest.deficit.change.bottom2$rowname)) %>% 
  anti_join(rest.deficit.change.bottom2) -> rest.deficit.excluded.bottom



# out of london text variables 
# top borough
sur.rest.top.la <-   eng.rest.surplus.totals.table$auth.name[1]
sur.rest.top.amount <-   eng.rest.surplus.totals.table[1,6]
sur.rest.top.ch <- eng.rest.surplus.totals.table$change[1]

la.data %>% 
  filter(year == current.year) %>% 
  select(auth.name, surplus.total) %>% 
  arrange(desc(surplus.total)) %>% 
  mutate(rank = row_number()) %>% 
  filter(auth.name == sur.rest.top.la) %>% 
  pull(rank) -> sur.rest.top.rank



# total sufficit and deficit
rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> rest.surplus.totals.table.tot.net

rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total surplus") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> rest.surplus.totals.table.tot

rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total surplus") %>% 
  select(!!as.name(current.year-1)) %>% 
  pull() -> rest.surplus.totals.table.tot.last

rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total surplus") %>% 
  select(change) %>% 
  pull() -> rest.surplus.totals.table.ch

rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total deficit") %>% 
  select(!!as.name(current.year)) %>% 
  pull() -> rest.surplus.totals.table.def

rest.surplus.totals.table.full %>% 
  filter(auth.name == "Total deficit") %>% 
  select(!!as.name(current.year-1)) %>% 
  pull() -> rest.surplus.totals.table.def.last




## budget comparison ##########################################################

budg.curr <- summary$surplus.total[6]
budg.next <- summary$surplus.total[7]
sur.curr <- summary$surplus.total[5]
# top 10 and bottom 10 over and underpreformers when it comes to budgeting 
## SURPLUS #####################################################################
la.data %>% 
  filter(year == current.year) %>% 
  select(auth.name, year, auth.type, surplus.total, surplus.budget) %>% 
  mutate_at(vars(surplus.total: surplus.budget), list(~./1000)) %>% 
  filter(complete.cases(.)) %>% 
  mutate(over.under = ifelse(surplus.total > surplus.budget, "over", "under"),
         diff = surplus.total - surplus.budget) %>% 
  arrange(desc(diff)) -> budg.diff.table

budg.diff.table %>%   
group_by(over.under) %>% 
  summarise(count = n(), diff = sum(diff)) -> budg.diff.sums

budg.over <- budg.diff.sums %>% filter(over.under == "over") %>% pull(count)

budg.over.amount <- budg.diff.sums %>% filter(over.under == "over") %>% pull(diff)

budg.under <- budg.diff.sums %>% filter(over.under == "under") %>% pull(count)

budg.under.amount <- budg.diff.sums %>% filter(over.under == "under") %>% pull(diff)

# top overpreformer
budg.diff.table %>% 
  filter(row_number() ==1 ) -> budg.over.top

budg.over.la <- budg.over.top$auth.name
budg.over.sur <- budg.over.top$surplus.total
budg.over.bud <- budg.over.top$surplus.budget
budg.over.diff <- budg.over.top$diff

# top underpreformer
budg.diff.table %>% 
  filter(row_number() == nrow(.) ) -> budg.under.top

budg.under.la <- budg.under.top$auth.name
budg.under.sur <- budg.under.top$surplus.total
budg.under.bud <- budg.under.top$surplus.budget
budg.under.diff <- budg.under.top$diff


## COGNESTION CHARGE ##########################################################


data %>% 
  filter(auth.type == "GLA") %>% 
  select(year, income.cong.ch, expend.cong.ch, surplus.cong.ch, budg.cong.ch) %>% 
  mutate_at(vars(income.cong.ch:budg.cong.ch), list(~./1000)) %>% 
  mutate(prop.net.expen = expend.cong.ch/income.cong.ch * 100) %>% 
  rownames_to_column %>%
  gather(variable, value, -rowname) %>% 
  mutate(order = row_number()) %>% 
  group_by(variable) %>% 
  mutate(order = first(order)) %>% 
  spread(rowname, value) %>% 
  arrange(order) %>% 
  filter(variable != "year") %>% 
  select(-order) %>% 
  ungroup()  %>% 
  mutate(variable = c("Income", "Expenditure", "Surplus", "Budgeted surplus", 
                      "Expenditure as \\% of income"))  %>% 
  rename_at(vars(-variable), list(~!!as.character((current.year-4):(current.year+1)))) -> 
  cong.ch.prep


# # save csv table 13
write.csv( cong.ch.prep, here::here(paste0("outputs/csv-tables/england-",
                                          FunFisc(), "/england-",
                                          FunFisc(), "-table-13.csv")),
          row.names = FALSE)


cong.ch.prep %>% 
  mutate_at(vars(-variable),
            function(x) ifelse(is.na(x), NA,
                               ifelse(.$variable == "Expenditure as \\% of income",
                                paste(FunDec(x, dp.tables), "\\%"),
                                FunDec(x, 0)))) -> cong.ch.formatted

# easy access variables for the text
sur.c.c.curr <- pull(cong.ch.prep[3,6])
sur.c.c.last<- pull(cong.ch.prep[3,5])

inc.c.c.ch <- pull(cong.ch.prep[1,6]/cong.ch.prep[1,5]) -1
exp.c.c.ch <- pull(cong.ch.prep[2,6]/cong.ch.prep[2,5]) -1

budg.c.c.curr <- pull(cong.ch.prep[4,6])
budg.c.c.next <- pull(cong.ch.prep[4,7])

prop.net.expen <- pull(cong.ch.prep[5,6])

        
## APPENDICES #################################################################

# ALPHABETICAL LIST OF ALL 353 LAS

la.data %>% 
  filter(year < current.year +1) %>% 
  select(year, auth.name, auth.type, surplus.total) %>%  
  spread(key = year, value = surplus.total ) %>% 
  mutate(ranking = paste0(row_number(desc(!!as.name(current.year))), ".")) %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name, -auth.type, -ranking),
            list(~ifelse(is.na(.), NA, 
                         formatC(., format = "f", digits = 0,  
                                 big.mark = ",")))) -> england.alpha


la.data %>% 
  filter(year < current.year +1) %>% 
  select(year, auth.name, auth.type, surplus.total) %>%  
  spread(key = year, value = surplus.total ) %>% 
  arrange(desc(!!as.name(current.year))) %>% 
  mutate(auth.name = gsub("&", "\\\\&", auth.name)) %>% 
  mutate_at(vars(-auth.name, -auth.type),
            list(~ifelse(is.na(.), NA, 
                         formatC(., format = "f", digits = 0,  
                                 big.mark = ",")))) -> england.sorted








