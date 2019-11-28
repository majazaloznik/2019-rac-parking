# la's not matching for england outturn and budget

not.budg <- anti_join(n,b, by = "auth.code") %>% 
  select(year, auth.name, auth.code)


not.this.year <- anti_join(b,n, by = "auth.code") %>% 
  filter(!is.na(auth.code)) %>% 
  select(year, auth.name, auth.code)


b %>% 
  filter(auth.type != "O") %>% 
  nrow()
## mapping test ###############################################################
library(tidyverse)
library(sf)

uc <- st_read("data/01-raw/maps/Local_Administrative_Units_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp")

uc %>% 
  filter(grepl("^E", lau118cd)) -> eng
plot(st_geometry(eng))
sort(eng$lau118nm )
uc %>% 
  filter(grepl("^S", lau118cd)) -> sco
plot(st_geometry(sco))

## extracting tables from pdf

library(tabulizer)
x <- extract_tables(here::here("data/01-raw/nhm-15-16.pdf"), pages = 81, 
                    method = "decide")
x

x <- extract_tables(here::here("data/01-raw/wal-15-16-pcn.pdf"), pages = 102, 
                    method = "stream")
x

## extracting json files
library(jsonlite)

query1 <- "Column_ItemName_ENG"
query1.value <- "Gross%20expenditure"
query2 <- "Row_ItemName_ENG"
query2.value <- "Parking%20of%20vehicles"

query <- paste0("http://open.statswales.gov.wales/en-gb/dataset/lgfs0009?$filter=",
                query1, "%20eq%20%27", query1.value, "%27%20and%20",
                query2, "%20eq%20%27", query2.value, "%27")
 
x <- jsonlite::fromJSON(query)
data <- x[[2]]
unique(data$Year)




## extracting wales csv files #####################
library(tidyr)
z <- read.csv("data/01-raw/wal-exp-17-18.csv")
# remove total row on top and first column
z <- z[-1, -1]

# make long
z %>% 
  gather(key = year, value = expenditure, 2:(ncol(z))) %>% 
  separate(year, into = c("X", "year"), sep = "(?<=[A-Z])(?=[0-9])", perl = TRUE)


## substr scotland 16/17 la name cell

x<- "Annex A – Service Analysis of Revenue Expenditure and Income, Argyll & Bute, 2016-17"
library(stringr)

gsub("^.+?, |, 2016-17", "", A1)


## creating pnc table for scotland

# spread
scotland.pnc.dpe.16 %>% 
  mutate(id = rep(1:19, 3)) %>% 
  spread(key = key, value = value) %>% 
  select(dpe.now, dpe.next, dpe.not) %>% 
  separate(dpe.now, into = c("dpe.now", "year"), sep = "\\(")  %>% 
  separate(year, into = c("year", "x"), sep = "\\)") %>% 
  separate(dpe.now, into = c("dpe.now", "x"), sep = "\\\r") %>% 
  select( -x) %>% 
  select(year, dpe.now, dpe.next, dpe.not) 


## extract tables from leibling

extract_tables("docs/original-reports/sco-13-14.pdf")

## regex the autority class abreviations off the auth.name

x <- "Worcestershire UA"

sub(" [A-Z]{2}$", "", x)


auth.name <- mutate(auth.name, auth.name = sub(" [A-Z]{2}$", "", auth.name))

## lookup table to recode

data <- sample(c("a", "b", "c", "d"), 10, replace = T) 
lookup <- list(a = "Apple", b = "Pear") 
# do.call(dplyr::recode, c(list(data), lookup))
v1 <- unlist(lookup)[data]
ifelse(is.na(v1), data, v1)

x <- c("hh" = "jh")
c(x, c("lkj" = "LKk"))  

auth.name <- mutate(auth.name, auth.name = recode(auth.name, !!!EnglandNameLookup)) 



### test for updating master
options(stringsAsFactors = FALSE)
library(dplyr)
master <- data.frame(a = rep(1, 10),
                     b = letters[1:10],
                     c = sample(10))

saveRDS(master, "data/03-processed/m.rds")


# update
x <- 3
update <- data.frame(a = rep(2, x),
                     b = LETTERS[1:x],
                     c = sample(x))

master <- readRDS("data/03-processed/m.rds")

master %>% 
  anti_join(update, by = c("a", "b")) %>% 
  bind_rows(update) -> master

## how to write conditional tree for n number of potential elements that 
## need to be listed?



FunMultiText <- function(vec) {
  n <- length(vec)
  ifelse(n == 0, "",
         ifelse(n == 1, paste0(vec[1], "."),
                {text <- vec[1]
                for( e in 2:(n-1) ){
                  if (n > 2) {text <- paste0(text,", ", vec[e])}
                }
                paste0(text, ", and ", vec[n])}
         )
  )
} 
x <- 1:10
x <- c("a", "b", "c", "d") 
x <- c("d", "jj")
x <- c("k")
FunMultiText(x)         
### mapping and palettes #####


library(viridis)
library(classInt)
data <- sco.expend

## functions ################
FunDivergePalette <- function(data, dir = 1){
  top <- ifelse(dir == 1, "D", "C")
  bottom <- ifelse(dir == 1, "C", "D")
  data.clean <- data[!is.na(data)]
  min <- min(data.clean)
  max <- max(data.clean)
  if(min < 0 & 0 < max)
    range <- max-min 
  min.prop <- -round(min(data.clean)/range*100)
  max.prop <- 100 - min.prop
  rb1 <- seq(min, 0, length.out=min.prop + 1)
  rb2 <- seq(0, max, length.out=max.prop)[-1]
  rampbreaks <- c(rb1, rb2)
  cuts <- classIntervals(data, style="fixed",fixedBreaks=rampbreaks)
  if (abs(min) > max) {
    pal.min <- viridis_pal(begin = 0.9, end = 0, 
                           option = bottom)(min.prop)
    end <- 1-0.9/min.prop * max.prop
    pal.max <- viridis_pal(begin = 0.9, end = end, 
                           option = top)(max.prop)
  } else {
    pal.max <- viridis_pal(begin = 0.9, end = 0, 
                           direction = dir, option = top)(max.prop)
    end <- 1-0.9/max.prop * min.prop
    pal.min <- viridis_pal(begin = 0.9, end = end, 
                           direction = dir, option = bottom)(min.prop)}
  pal <- c(rev(pal.min), pal.max)
  return(list(cuts,pal))
}

## legend function ##########
FunScaleLegend <- function(col, data){
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] -  (bx[2] - bx[1]) / 10,
              bx[2] - (bx[2] - bx[1]) / 10 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  xx <- rep(box.cx, each = 2)
  par(xpd = TRUE)
  for(i in 1:n){
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(data, na.rm = TRUE), max(data, na.rm = TRUE)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line= -4)
  par <- opar
}

FunMap <- function(table, country = "^S", dir = 1) { 
  # clean data ##
  uc %>% 
    filter(grepl(country, lau118cd)) %>% 
    mutate(lau118nm = recode(lau118nm, !!!orig.sco.name.lookup)) %>% 
    left_join(select(table, auth.name, change), 
              by = c("lau118nm"= "auth.name")) -> map.data
  
  # vector for colouring
  data <- map.data$change
  
  # get colour palette and breaks
  x <- FunDivergePalette(data, dir = dir)
  pal <- findColours(x[[1]], x[[2]])
  
  # plot map
  plot(map.data["change"], main = "", col = pal)
  
  # plot legend.
  FunScaleLegend(attr(pal, "palette"), data[!is.na(data)])
}

FunMap(sco.income, dir = 1)

## inset
library(ggplot2)

ggplot(map.data["change"], main = "", col = pal, 
     ylim = c(54.63, 59.1))
par("usr")
par(usr = c(0.6, 0.75, 0.6, 0.9), mar = c(0,0,0,0))
plot(map.data["change"], main = "", col = pal, 
     ylim = c(59.6, 60.9), xlim = c(-2, -0.5), add = TRUE)
par <- opar
