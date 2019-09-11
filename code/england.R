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

