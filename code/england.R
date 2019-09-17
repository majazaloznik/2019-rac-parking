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
  summarise(sum =  sum(surplus.budget)) %>% 
  filter(year %in% c(current.year, current.year +1)) -> summary.budg

# budgeted total for current and next year 


# transpose table and add change variable and budget lines
summary.i.e %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(rowname != "year") %>%  
  mutate(change = ifelse(rowname == "prop.net.expen", NA,  100*(V5 / V4 - 1))) -> summary.prep
  
# budget data for current and next year
la.data %>%
  select(year, surplus.budget) %>% 
  group_by(year) %>% 
  summarise(sum =  sum(surplus.budget)) %>% 
  filter(year %in% c(current.year, current.year +1)) -> summary.budg


# prepare data for tabulation. 

summary.prep %>% 
  mutate(change = ifelse(rowname == "prop.net.expen", NA, 
                         paste(FunDec(change, dp.tables), "\\%"))) %>% 
  mutate(rowname = c("Fees and permits", "Penalties", "Total Income", "Expenditure", "Surplus",
         "Total Income", "Expenditure", "Surplus", "Total Income", "Expenditure", "Surplus",
         "Net Expenditure", "Parking surplus as percentage of net transport expenditure"))%>% 
  mutate_at(vars(-rowname, -change), function(x) FunDec(x,0)) %>% 
  mutate_at(vars(-rowname, -change), function(x) ifelse(
    .$rowname == "Parking surplus as percentage of net transport expenditure", 
    paste0(x, " \\%"), x)) %>% 
  mutate(collapsed = c(rep("On-street", 5), 
                       rep("Off-street", 3),
                       rep("All parking", 3),
                       "All England transport", "")) %>% 
  select(collapsed, rowname:change) -> eng.summary.formatted
  
  
