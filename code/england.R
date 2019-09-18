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
current.year <-   2017#params$current.year
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
source(here::here("code/functions.R"))

# load data
master <- readRDS(here::here("data/03-processed/master.rds"))
orig.eng.name.lookup <- readRDS(here::here("data/01-raw/orig.eng.name.lookup.rds"))
uc <- st_read(here::here("data/01-raw/maps/Local_Administrative_Units_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp"), quiet = TRUE)
rpi <- read.csv(here::here("data/01-raw/rpi.csv"))

# load bibliography from report.name
report.name <- paste0("england-report-", current.year, "-",
                      current.year - 1999)
bib <- readRDS(here::here(paste0("data/03-processed/", report.name, "-bib.rds")))

# pass parameters for decimal points in text and tables
dp.text <-  1# params$dp.text
dp.tables <- 2#  params$dp.tables

# create folder for csv tables if it does not exist already
suppressWarnings(dir.create(here::here(paste0("outputs/csv-tables/england-", FunFisc())), 
                            showWarnings =TRUE))

## data preparation ############################################################
# extract relevant country subset of data for all years
# and calculate on and off surplus
master %>% 
  filter(country == "England") %>% 
  mutate(surplus.on = income.on - expend.on,
         surplus.off = income.off - expend.off,
         income.fnp = income.on - income.pcn,
         surplus.budget = -surplus.budget) %>% 
  filter(year <= current.year + 1) -> data.full

# only for the last four years and next year (for budget)
data.full %>% 
  filter(year >= current.year -4 & year <= current.year + 1) -> data

# data for current year only
data %>% 
  filter(year == current.year) -> data.current

# data for 353 LAs only
data %>% 
  filter(!auth.type %in% c("X", "GLA", "O")) -> la.data

data.current %>% 
  filter(!auth.type %in% c("X", "GLA", "O")) -> la.data.current



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
  filter(country == "England", fiscyear == current.year + 1, content == "budget") %>% 
  pull(refs) -> eng.bib.next.budget

# wpl nottinggan reference
bib %>% 
  filter(country == "England", fiscyear == current.year, content == "wpl") %>% 
  pull(refs) -> eng.bib.wpl

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
  

bind_rows(mutate_at(summary.i.e, vars(year), as.character), summary.budget) -> summary


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
  mutate(change = ifelse(variable == "prop.net.expen", NA,  100*(`5` / `4` - 1))) %>% 
  mutate(change = ifelse(is.na(change), NA, FunDec(change,0))) %>% 
  mutate_at(vars(-variable, -change), function(x) ifelse(.$variable == "prop.net.expen",
                                                FunDec(x, 0),
                                                ifelse(is.na(x), NA, 
                                                formatC(round(x,0), big.mark = ",")))) -> summary.prep


# prepare data for tabulation. 
summary.prep %>% 
  mutate(change = ifelse(variable == "prop.net.expen", NA, 
                         paste(change, "\\%"))) %>% 
  mutate(variable = c("Fees \\& permits", "Penalties", "Total Income", "Expenditure", "Surplus",
         "Total Income", "Expenditure", "Surplus", "Total Income", "Expenditure", "Surplus",
         "Net Expenditure", "Parking surplus as percentage of net transport expenditure")) %>% 
  mutate(collapsed = c(rep("On-street", 5), 
                       rep("Off-street", 3),
                       rep("All parking", 3),
                       "All England transport", "")) %>% 
  mutate_at(vars(-variable, -change, -collapsed), function(x) ifelse(
    .$variable == "Parking surplus as percentage of net transport expenditure", 
    paste0(x, " \\%"), x)) %>% 
  mutate_at(vars(-collapsed), function(x)  ifelse(.$collapsed == "All parking" & 
                                                    .$variable == "Surplus",  
                                                  paste0("\\textbf{", x, "}"), x)) %>% 
  select(collapsed, variable:change) -> eng.summary.formatted
  
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


# prepare data for trends plot
# prepare summary trend data for chart
data.full  %>% 
  select(year, income.total, expend.total, surplus.total, surplus.budget) %>% 
  group_by(year) %>% 
  summarise_at(vars(income.total:surplus.budget), list(~sum(.,na.rm=TRUE))) %>% 
  mutate_at(vars(income.total:surplus.budget), list(~./1000)) %>% 
  mutate_all(function(x) ifelse(x == 0, NA, x)) -> eng.plot

# prepare data for london/rest of england summary table

la.data.current %>% 
  mutate(london = ifelse(auth.type == "L", "london", "rest")) %>% 
  group_by(london) %>% 
  summarise_at(vars(income.on, income.off, income.total,
                    expend.on, expend.off, expend.total,
                    surplus.on, surplus.off, surplus.total), funs(sum(., na.rm = TRUE))) %>% 
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
         prop = paste(FunDec(prop, dp.tables), "\\%") )%>% 
  mutate_at(vars(-variable, -prop), function(x) formatC(round(x/1000,0), big.mark = ",")) -> summary.london.prep


# prepare data for tabulation. 
summary.london.prep %>% 
  mutate(variable = c(rep(c("On-street", "Off-street", "Total"), 3))) %>% 
  mutate(collapsed = c(rep("Income", 3), 
                       rep("Expenditure", 3),
                       rep("Surplus", 3))) %>% 
 mutate_at(vars(-collapsed), function(x)  ifelse(.$variable == "Total",
                                                  paste0("\\textbf{", x, "}"), x)) %>% 
  select(collapsed, variable:prop) -> eng.summary.london.formatted

# easy access for variables in text
lnd.prop.off <- 100*as.numeric(summary.london.prep$london[2])/as.numeric(summary.london.prep$london[3])
rest.prop.off <- 100*as.numeric(summary.london.prep$rest[2])/as.numeric(summary.london.prep$rest[3])
lnd.surplus <- as.numeric(summary.london.prep$london[9])
lnd.prop.surplus <- 100*as.numeric(summary.london.prep$london[9])/
  as.numeric(summary.london.prep$total[9])











