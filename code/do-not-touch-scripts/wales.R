################################################################################
##              CODE REQUIRED FOR COMPILATION OF WALES REPORT                 ##
##                                                                            ##
##            This script is sourced  by the Wales .Rmd file.                 ##
##                                                                            ##
##              You cannot run this directly - no need anyway.                ##
##                                                                            ##
##                 Also, DO NOT MAKE ANT CHANGES HERE.                        ##
##                                                                            ##
################################################################################

## preliminaries ###############################################################
# change the year variable 
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
font_add(family = "meri", regular = here::here("data/01-raw/fonts/merriweather.google.ttf"))
showtext_auto()

# source functions
source(here::here("code/do-not-touch-scripts/functions.R"))

# load data and shapefiles
master <- readRDS(here::here("data/03-processed/master.rds"))
uc <- st_read(here::here(paste0("data/01-raw/maps/Local_Administrative_Units_",
                                "Level_1_January_2018_Ultra_Generalised_Clipped_",
                                "Boundaries_in_United_Kingdom.shp")), 
              quiet = TRUE)

rpi <- read.csv(here::here("data/01-raw/rpi.csv"))
# load bibliography - get name of the report
report.name <- paste0("wales-report-", current.year, "-",current.year - 1999)
bib <- readRDS(here::here(paste0("data/03-processed/", report.name, "-bib.rds")))

# get the number of decimal points
dp.text <- params$dp.text
dp.tables <- params$dp.tables

# create folder for csv tables if it does not exist already
suppressWarnings(dir.create(here::here(paste0("outputs/csv-tables/wales-", FunFisc())), 
                             showWarnings =TRUE))

## data preparation ############################################################
# extract relevant country subset of data for all years
master %>% 
  filter(country == "Wales") %>% 
  filter(year <= current.year) -> data.full

# only for the last four years
data.full %>% 
  filter(year >= current.year -4 & year <= current.year) -> data

## extract some useful variables like bib references and column headers ########
# current main I.E. reference
bib %>% 
  filter(country == "Wales") %>% 
  pull(refs) -> wal.bib.main

# map reference
bib %>% 
  filter(content == "map") %>% 
  pull(refs) -> wal.bib.map

# rpi reference
bib %>% 
  filter( content == "rpi") %>% 
  pull(refs) -> wal.bib.rpi


# column headers 
yearz <- paste0("\\multirow{1}{*}[0pt]{",(current.year-4):(current.year), "-",
                (current.year-4-1999):(current.year-1999),"}")

################################################################################
## cleaning up data for various tables and charts ##############################
################################################################################

## SUMMARY #####################################################################
# clean data for Summary table
data %>% 
  select(year, income.total, expend.total, surplus.total, transport.total) %>% 
  group_by(year) %>% 
  summarise_at(vars(income.total:transport.total), sum) %>% 
  mutate_at(vars(income.total:transport.total), list(~./1000)) %>% 
  mutate(surplus.of.transport = 100 * surplus.total/transport.total) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  filter(rowname != "year") %>%
  mutate(change = FunPercent(V5,V4) - 100) %>% 
  rename(!!as.name(current.year -4) := V1,
         !!as.name(current.year -3) := V2,
         !!as.name(current.year -2) := V3,
         !!as.name(current.year -1) := V4,
         !!as.name(current.year ) := V5) -> wal.summary

# save csv table 1
write.csv(wal.summary, here::here(paste0("outputs/csv-tables/wales-",
                                         FunFisc(), "/wales-", 
                                         FunFisc(), "-table-01.csv")),
          row.names = FALSE)

# prepare and format data for Summary table
wal.summary %>% 
  mutate(change = ifelse(rowname == "surplus.of.transport", NA, 
                         paste(FunDec(change, dp.tables), "\\%"))) %>% 
  mutate(rowname = c("Income", "Expenditure", "Surplus", "Net expenditure", 
                     "Parking surplus as percentage of net transport expenditure")) %>% 
  mutate_at(vars(-rowname, -change), function(x) FunDec(x, dp.tables)) %>% 
  mutate_at(vars(-rowname, -change), function(x) ifelse(.$rowname == "Parking surplus as 
                                            percentage of net transport expenditure", 
                                            paste0(x, " \\%"), x)) %>%
  mutate(collapsed = c(rep("Parking", 3), "Total transport", "")) %>% 
  select(collapsed, rowname:change) -> wal.summary.formatted

# extract total England transport-surplus ratio (single number used in text)
master %>% 
  filter(country == "England") %>% 
  filter(year == current.year) %>% 
  summarise(transport = sum(transport.total, na.rm = TRUE),
            surplus = sum(surplus.total, na.rm = TRUE)) %>% 
  mutate(proportion = 100*surplus/transport) %>% 
  pull(proportion)  -> eng.trans

# prepare data for summary trend plot
data.full  %>% 
  select(year, income.total, expend.total, surplus.total) %>% 
  group_by(year) %>% 
  summarise_at(vars(income.total:surplus.total), sum) %>% 
  mutate_at(vars(income.total:surplus.total), list(~./1000)) -> data.plot

# comparing summaries for all three countries:
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
         surplus = FunDec(surplus/1000, dp.tables), 
         income = FunDec(income/1000, dp.tables), 
         expend = FunDec(expend/1000, dp.tables),
         prop.of.income = prop.of.income*100) %>% 
  t() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  tibble::rownames_to_column("var") %>% 
  setNames(.[1,]) %>% 
  filter(country != "country") %>% 
  mutate(country = c("Fiscal year", "Parking income", 
                     "Parking expenditure", "Surplus", 
                     "Surplus as proportion of income")) -> sum.gb

# save csv table 2
write.csv(sum.gb, here::here(paste0("outputs/csv-tables/wales-",
                                         FunFisc(), "/wales-", 
                                         FunFisc(), "-table-02.csv")),
          row.names = FALSE)

# same as before, but this time add % formatting to the bottom row
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
  mutate(country = c("Fiscal year", "Parking income", 
                     "Parking expenditure", "Surplus", 
                     "Surplus as proportion of income")) -> sum.gb.formatted

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

# # save csv table 4
# write.csv(sum.gb.change.tab, here::here(paste0("outputs/csv-tables/scotland-",
#                                                FunFisc(), "/scotland-", 
#                                                FunFisc(), "-table-04.csv")),
#           row.names = FALSE)


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
wal.ann <- as.numeric(sum.gb.change.tab$Wales[7])

wal.4av <- as.numeric(sum.gb.change.tab$Wales[6])

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
# clean up income data
data %>% 
  select(auth.name, year, income.total) %>% 
  spread(key = year, value = income.total) %>% 
  arrange(desc(.[[6]])) %>% 
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
                         ifelse(is.infinite(change.4), NA, change.4))) -> wal.income

# save csv table 4
write.csv(wal.income, here::here(paste0("outputs/csv-tables/wales-",
                                               FunFisc(), "/wales-", 
                                               FunFisc(), "-table-04.csv")),
          row.names = FALSE)


# no income councils
wal.no.income <- nrow(filter(wal.income, !!as.name(current.year) == 0))

# split into increase/decrease and NA
wal.income %>% 
  filter(auth.name != "Total") %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                             ifelse(!!as.name(current.year) == 
                                      !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> income.bin

# select only valid changes
wal.income %>% 
  filter(auth.name != "Total") %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> wal.income.valid

# get top three LAs and proportion they command
wal.income.valid %>% 
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
  deframe() -> wal.income.top3

# get top three relevant income changes
wal.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> wal.income.change.top3

# find excluded rows in top of the table 
wal.income.valid %>% 
  filter(rowname <= max(wal.income.change.top3$rowname)) %>% 
  anti_join(wal.income.change.top3) -> wal.income.excluded.top

# get bottom two relevant surplus changes
wal.income.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> wal.income.change.bottom2

# find excluded rows in bottom of the table 
wal.income.valid %>% 
  filter(rowname > min(wal.income.change.bottom2$rowname)) %>% 
  anti_join(wal.income.change.bottom2) -> wal.income.excluded.bottom

# format income data for tabulation
wal.income %>% 
  mutate(change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(wal.income$change, 
                                                c(wal.income$change, 
                                                  wal.income$change.4),
                                                dir = 1, factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                            background = 
                              FunDivergePalette(wal.income$change.4, 
                                                c(wal.income$change, 
                                                  wal.income$change.4),
                                                dir = 1, factor = 1)[[3]])) ->
  wal.income.formatted

## EXPENDITURE #################################################################
# get wales totals for expenditure for each year in data,
# caluclate change on previous year and proportion of income
data %>% 
  select(auth.name, year, expend.total) %>% 
  spread(key = year, value = expend.total) %>% 
  full_join(data %>% 
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
         prop.income = 100 *!!as.name(current.year)/income.total) %>% 
  mutate(change = ifelse(is.nan(change), NA, 
                         ifelse(is.infinite(change), NA, change)),
         change.4 = ifelse(is.nan(change.4), NA, 
                           ifelse(is.infinite(change.4), NA, change.4))) %>% 
  select(-income.total) -> wal.expend

# save csv table 5
write.csv(wal.expend, here::here(paste0("outputs/csv-tables/wales-",
                                               FunFisc(), "/wales-", 
                                               FunFisc(), "-table-05.csv")),
          row.names = FALSE)


# calculate change and prop of income for Wales total row. 
wal.expend %>% 
  filter(auth.name == "Total") %>% 
  mutate(current.change = (.[[6]]-.[[5]])/1000,
         last.change = (.[[5]]-.[[4]])/1000) %>% 
  select(change, current.change, last.change) -> wal.expend.t

# count councils with +/- change
wal.expend %>% 
  filter(!grepl("Total", auth.name)) %>% 
  mutate(dir = ifelse(is.na(!!as.name(current.year) ) | is.na(!!as.name(current.year-1)), "na",
                      ifelse(!!as.name(current.year) > !!as.name(current.year-1), "poz",
                             ifelse(!!as.name(current.year) == 
                                      !!as.name(current.year-1), "zero", "neg")))) %>% 
  group_by(dir) %>% 
  summarise(n = n()) %>% 
  deframe() -> expend.bin

# select only valid
wal.expend %>% 
  filter(auth.name != "Total") %>% 
  filter(!is.nan(change) & !is.infinite(change) & !is.na(change)) %>% 
  arrange(desc(change)) %>%
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> wal.expend.valid

# get top three LAs and proportion they command
wal.expend.valid %>% 
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
  deframe() -> wal.expend.top3

# get top three relevant expenditure changes
wal.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> wal.expend.change.top3

# find excluded rows in top of the table 
wal.expend.valid %>% 
  filter(rowname <= max(wal.expend.change.top3$rowname)) %>% 
  anti_join(wal.expend.change.top3) -> wal.expend.excluded.top

# get bottom two relevant surplus changes
wal.expend.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> wal.expend.change.bottom2

# find excluded rows in bottom of the table 
wal.expend.valid %>% 
  filter(rowname > min(wal.expend.change.bottom2$rowname)) %>% 
  anti_join(wal.expend.change.bottom2) -> wal.expend.excluded.bottom

# compare Cardif to total by expenditure as prop of income
wal.expend %>% 
  select(auth.name, prop.income) %>% 
  filter(auth.name %in% c("Total", "Cardiff")) %>% 
  pull(prop.income) -> wal.expend.p

# format table for printing 
wal.expend %>% 
  mutate(prop.income = ifelse(is.na(prop.income), NA, 
                              paste(FunDec(prop.income, dp.tables), "\\%"))) %>% 
  mutate(change = ifelse(is.na(change), "", paste(FunDec(change, dp.tables), "%")),
         change.4 = ifelse(is.na(change.4), "", paste(FunDec(change.4, dp.tables), "%"))) %>% 
  mutate(change = cell_spec(change, "latex",
                            background = 
                              FunDivergePalette(wal.expend$change,
                                                c(wal.expend$change,
                                                  wal.expend$change.4),dir = -1,
                                                factor = 1)[[3]]),
         change.4 = cell_spec(change.4, "latex",
                            background = 
                              FunDivergePalette(wal.expend$change.4,
                                                c(wal.expend$change,
                                                  wal.expend$change.4),dir = -1,
                                                factor = 1)[[3]])) ->
  wal.expend.formatted

# calculate proportion of expenditure in income for all LA/year combinations
# and also for each year's total. 
data %>% 
  select(auth.name, year, expend.total) %>% 
  full_join(data %>% 
              select(auth.name, year, income.total)) %>% 
  bind_rows(group_by(., year) %>% 
              summarise_at(vars(-auth.name), list(~sum(., na.rm = TRUE))) %>%
              mutate(auth.name='Total'))  %>% 
  mutate(expend.prop = expend.total/income.total*100) %>% 
  select(auth.name, year, expend.prop) %>%
  spread(key = year, value = expend.prop) %>% 
  full_join(data %>% 
              filter( year == current.year) %>% 
              select(auth.name, year, expend.total)) %>% 
  arrange(desc(expend.total)) %>% 
  select(-year, -expend.total) -> wal.expend.of.income

# save csv table 6
write.csv(wal.expend.of.income, here::here(paste0("outputs/csv-tables/wales-",
                                        FunFisc(), "/wales-", 
                                        FunFisc(), "-table-06.csv")),
          row.names = FALSE)


# format expend as income for tabulation
wal.expend.of.income %>% 
  mutate_at(vars(-auth.name), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  mutate_at(vars(-auth.name), function(x) { 
    cell_spec(ifelse(is.na(x), "", paste(FunDec(x, dp.tables), "%")), "latex", 
              bold = F, background  = spec_color(1/x, begin = 0.3,
                                                 end = 0.9, option = "D", 
                                                 na_color = "#FFFFFF"))}) ->
  wal.expend.of.income.formatted



## SURPLUS #####################################################################
# clean up surplus data
data %>% 
  select(auth.name, year, surplus.total) %>% 
  spread(key = year, value = surplus.total) %>% 
  full_join(data %>% 
              select(auth.name, year, transport.total) %>% 
              filter(year == current.year)) %>% 
  select(-year) %>% 
  arrange(desc(.[[6]])) -> wal.surplus

# save csv table 7
write.csv(wal.surplus, here::here(paste0("outputs/csv-tables/wales-",
                                        FunFisc(), "/wales-", 
                                        FunFisc(), "-table-07.csv")),
          row.names = FALSE)


# get number of councils with +/-/0 change in surplus
wal.surplus %>% 
  filter(auth.name != "Total") %>% 
  mutate(change = ifelse(!!as.name(current.year) > 0, "poz",
                         ifelse(!!as.name(current.year) == 0, "zero", "neg"))) %>% 
  group_by(change) %>% 
  summarise(n = n()) %>% 
  deframe() -> surplus.bin

# create totals row for surpluses and deficits. 
data %>% 
  select(auth.name, year, surplus.total, transport.total) %>% 
  mutate(poz.neg = ifelse(surplus.total >= 0, "poz", "neg")) %>% 
  group_by(year, poz.neg) %>%
  summarise_at(vars(-auth.name), sum) %>%
  gather(variable, value, -c(year, poz.neg)) %>%
  unite(temp, year, variable) %>%
  spread(temp, value) %>% 
  select(poz.neg, contains("surplus"), 
         paste0(current.year, "_transport.total"))  %>% 
  mutate(poz.neg = c("Total deficit", "Total surplus")) %>% 
  rename_all(~ c("auth.name", (current.year-4):(current.year), 
                 "transport.total")) %>% 
  bind_rows(group_by(., auth.name) %>% 
              ungroup() %>% 
              summarise_at(vars(-auth.name), sum) %>%
              mutate(auth.name = c("Total"))) -> wal.surplus.totals


# bind both tables together
bind_rows(wal.surplus, wal.surplus.totals) %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100,
         prop.transp = 100*.[[6]]/.[[7]]) %>% 
  mutate(change = ifelse(abs(sign(!!as.name(current.year))-
                               sign(!!as.name(current.year-1))) == 2, 
                         NA, change)) %>% 
  select(-transport.total) -> wal.surplus.totals.table


# get top three LAs and proportion they command
wal.surplus %>% 
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
  deframe() -> wal.surplus.top3

# surplus.councils
wal.poz.surplus <- nrow(filter(wal.surplus, !!as.name(current.year) >= 0))


# extract bottom LAs where "surplus" is under 30.000 (i.e. close to 0)
wal.surplus %>% 
  mutate(change = FunPercent(!!as.name(current.year), !!as.name(current.year -1)) - 100) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) >= 0,
         !!as.name(current.year - 1) >= 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))-> wal.surplus.valid

# get top three relevant surplus changes
wal.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() <= 3) -> wal.surplus.change.top3

# find excluded rows in top of the table 
wal.surplus.valid %>% 
  filter(rowname <= max(wal.surplus.change.top3$rowname)) %>% 
  anti_join(wal.surplus.change.top3) -> wal.surplus.excluded.top

# get bottom two relevant surplus changes
wal.surplus.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() > n()-2) -> wal.surplus.change.bottom2

# find excluded rows in bottom of the table 
wal.surplus.valid %>% 
  filter(rowname > min(wal.surplus.change.bottom2$rowname)) %>% 
  anti_join(wal.surplus.change.bottom2) -> wal.surplus.excluded.bottom


# extract bottom LAs where deficit is under 30.000 (i.e. close to 0)
wal.surplus %>% 
  mutate(change = 100 * (!!as.name(current.year)/!!as.name(current.year 
                                                           - 1) - 1)) %>% 
  filter(!is.nan(change) & !is.infinite(change)) %>% 
  filter(!!as.name(current.year) < 0,
         !!as.name(current.year - 1) < 0) %>% 
  arrange(desc(change)) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname)) -> wal.deficit.valid

# get top relevant surplus changes
wal.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() == 1) -> wal.deficit.change.top1

# find excluded rows in top of the table 
wal.deficit.valid %>% 
  filter(rowname <= max(wal.deficit.change.top1$rowname)) %>% 
  anti_join(wal.deficit.change.top1) -> wal.deficit.excluded.top

# get botto relevant deficit changes
wal.deficit.valid %>% 
  filter(abs(!!as.name(current.year)) >= 30) %>% 
  filter(row_number() == n() ) -> wal.deficit.change.bottom1

# find excluded rows in bottom of the table 
wal.deficit.valid %>% 
  filter(rowname > min(wal.deficit.change.bottom1$rowname)) %>% 
  anti_join(wal.deficit.change.bottom1) -> wal.deficit.excluded.bottom

# format surplus table for tabulation
wal.surplus.totals.table %>% 
  mutate(change = paste(FunDec(change, dp.tables), "%"),
         prop.transp = paste(FunDec(prop.transp, dp.tables), "\\%")) %>% 
  mutate(change =cell_spec(change, "latex", 
                           italic = ifelse(.[[6]] < 0, TRUE, FALSE))) ->
  wal.surplus.formatted







