################################################################################
## ENGLAND IMPORT FUNCTIONS ####################################################

# function to extract expenditure data from England outturn excel file
FunEnglandOutturn <- function(file =file, 
                              first = first, 
                              last = last, 
                              e.sh = e.sh, 
                              e.on = e.on, 
                              e.off = e.off,
                              e.cc = e.cc,
                              i.sh = i.sh,
                              i.on = i.on,
                              i.off = i.off,
                              i.cc = i.cc,
                              pen.sh = pen.sh,
                              pen.on = pen.on,
                              auth.name = auth.name,
                              auth.type = auth.type,
                              auth.code = auth.code,
                              year = year) {
  cell.range <- paste0(auth.name, first-1, ":",auth.name, last)
  auth.name <- read_excel(file, e.sh, cell.range)
  colnames(auth.name) <- "auth.name"
  auth.name <- mutate(auth.name, auth.name = sub(" [A-Z]{2,3}$", "", auth.name))
  auth.name <- mutate(auth.name, auth.name = recode(auth.name, !!!orig.eng.name.lookup)) 
  cell.range <- paste0(auth.type, first-1, ":",auth.type, last)
  auth.type <- read_excel(file, e.sh, cell.range)
  colnames(auth.type) <- "auth.type"
  cell.range <- paste0(auth.code, first-1, ":",auth.code, last)
  auth.code <- read_excel(file, e.sh, cell.range)
  colnames(auth.code) <- "auth.code"
  cell.range <- paste0(e.on, first-1, ":", e.on, last)
  expend.on <- read_excel(file, e.sh, cell.range)
  colnames(expend.on) <- "expend.on"
  expend.on <- ifelse(!grepl("^-?[0-9.]+$", expend.on$expend.on), NA, 
                      suppressWarnings(as.numeric(expend.on$expend.on)))
  cell.range <- paste0(e.off, first-1, ":", e.off, last)
  expend.off <- read_excel(file, e.sh, cell.range)
  colnames(expend.off) <- "expend.off"
  expend.off <- ifelse(!grepl("^-?[0-9.]+$", expend.off$expend.off), NA, 
                       suppressWarnings(as.numeric(expend.off$expend.off)))
  cell.range <- paste0(e.cc, first-1, ":", e.cc, last)
  expend.cong.ch <- read_excel(file, e.sh, cell.range)
  colnames(expend.cong.ch) <- "expend.cong.ch"
  expend.cong.ch <- ifelse(!grepl("^-?[0-9.]+$", expend.cong.ch$expend.cong.ch), NA, 
                           suppressWarnings(as.numeric(expend.cong.ch$expend.cong.ch)))
  cell.range <- paste0(i.on, first-1, ":", i.on, last)
  income.on <- read_excel(file, i.sh, cell.range)
  colnames(income.on) <- "income.on"
  income.on <- ifelse(!grepl("^-?[0-9.]+$", income.on$income.on), NA, 
                      suppressWarnings(as.numeric(income.on$income.on)))
  cell.range <- paste0(i.off, first-1, ":", i.off, last)
  income.off <- read_excel(file, i.sh, cell.range)
  colnames(income.off) <- "income.off"
  income.off <- ifelse(!grepl("^-?[0-9.]+$", income.off$income.off), NA, 
                       suppressWarnings(as.numeric(income.off$income.off)))
  cell.range <- paste0(i.cc, first-1, ":", i.cc, last)
  income.cong.ch <- read_excel(file, i.sh, cell.range)
  colnames(income.cong.ch) <- "income.cong.ch"
  income.cong.ch <- ifelse(!grepl("^-?[0-9.]+$", income.cong.ch$income.cong.ch), NA, 
                           suppressWarnings(as.numeric(income.cong.ch$income.cong.ch)))
  if(!is.na(pen.sh)){
    cell.range <- paste0(pen.on, first-1, ":", pen.on, last)
    income.pcn <- read_excel(file, pen.sh, cell.range)
    colnames(income.pcn) <- "income.pcn"
    income.pcn <- ifelse(!grepl("^-?[0-9.]+$", income.pcn$income.pcn), NA, 
                         suppressWarnings(as.numeric(income.pcn$income.pcn)))} 
  else {income.pcn <- NA }
  df <- data.frame(auth.name, auth.type, auth.code, expend.on, expend.off, 
                   income.on, income.off, income.pcn, 
                   expend.cong.ch, income.cong.ch,
                   year = year)
  df <- df %>%  mutate(auth.type = ifelse(auth.name == "Greater London Authority", 
                                  "GLA", auth.type))
  # do the math: 
  df$income.total = df$income.on + df$income.off
  df$expend.total = df$expend.on + df$expend.off
  df$surplus.total = df$income.total - df$expend.total
  df
}

## England outturn totals for transport and penalty charge income.
FunEnglandOutturnTotals <- function(file , 
                                    transport.total ,
                                    income.pcn,
                                    year,
                                    sheet = 2 ) {
  transport.total <- colnames(read_excel(file, sheet, transport.total))
  income.pcn <- colnames(read_excel(file, sheet, income.pcn))
  vec <- as.numeric(c(year, transport.total, income.pcn))
  names(vec) <- c("year", "transport.total", "income.pcn")
  vec
}

## England budgeted transport totals 
FunEnglandBudgetTransport <- function(file , 
                                    budg.trans,
                                    year,
                                    sheet = 2 ) {
  budg.trans <- colnames(read_excel(file, sheet, budg.trans))
  vec <- as.numeric(c(year, budg.trans))
  names(vec) <- c("year", "budg.trans")
  vec
}

## England budgeted transport totals 
FunEnglandBudgetCongestion <- function(file ,
                                      budg.cong.ch,
                                      year,
                                      sheet = 2 ) {
  budg.cong.ch <- colnames(read_excel(file, sheet, budg.cong.ch))
  vec <- as.numeric(c(year,  budg.cong.ch))
  names(vec) <- c("year",  "budg.cong.ch")
  vec
}

## England budget surplus import function

FunEnglandBudget <- function(file =file, 
                             first = first, 
                             last = last,
                             sheet = 3,
                             budg.la = budg.la,
                             auth.name = auth.name,
                             auth.type = auth.type,
                             auth.code = auth.code,
                             year = year) {
  cell.range <- paste0(auth.name, first-1, ":",auth.name, last)
  auth.name <- read_excel(file, sheet, cell.range)
  colnames(auth.name) <- "auth.name"
  auth.name <- mutate(auth.name, auth.name = sub(" [A-Z]{2,3}$", "", auth.name))
  auth.name <- mutate(auth.name, auth.name = recode(auth.name, !!!orig.eng.name.lookup)) 
  cell.range <- paste0(auth.type, first-1, ":",auth.type, last)
  auth.type <- read_excel(file, sheet, cell.range)
  colnames(auth.type) <- "auth.type"
  cell.range <- paste0(auth.code, first-1, ":",auth.code, last)
  auth.code <- read_excel(file, sheet, cell.range)
  colnames(auth.code) <- "auth.code"
  cell.range <- paste0(budg.la, first-1, ":", budg.la, last)
  surplus.budget <- read_excel(file, sheet, cell.range)
  colnames(surplus.budget) <- "surplus.budget"
  surplus.budget <- ifelse(!grepl("^-?[0-9.]+$", surplus.budget$surplus.budget), NA, 
                           suppressWarnings(as.numeric(surplus.budget$surplus.budget)))
  df <- data.frame(auth.name, auth.type, auth.code, surplus.budget,
                   year = year)
  df <- df %>%  mutate(auth.type = ifelse(auth.name == "Greater London Authority", 
                                  "GLA", auth.type))
  df
}

################################################################################
## SCOTLAND IMPORT FUNCTIONS ###################################################

# function for extracting Scotlandn income and expenditure from relevant cells
FunScotlandLACels <- function(file, sheet, expend.total, income.total, 
                              transport.total, auth.cell) {
  #auth.name <- colnames(read_excel(file, sheet, auth.cell))
  auth.name <- excel_sheets(file.name)[sheet]
  expend.total <- colnames(read_excel(file, sheet, expend.total))
  income.total <- colnames(read_excel(file, sheet, income.total))
  transport.total <- colnames(read_excel(file, sheet, transport.total))
  c(auth.name, expend.total, income.total, transport.total)
}

# Function that loops through all sheets and extracts relevant cell data 
FunScotlandLoopIE <- function(year,
                              file.name,
                              start.sh,
                              end.sh,
                              exp.cell,
                              inc.cell,
                              transp.cell) {
  
  
  # prepare empty data frame for the data
  df <- data.frame(auth.name = character(),
                   expend.total = character(),
                   income.total = character(),
                   transport.total = character())

  # loop through all the sheets
  for (sheet in start.sh:end.sh){
    x <- FunScotlandLACels(file.name, sheet, exp.cell, inc.cell, transp.cell)
    names(x) <- colnames(df)
    df <- bind_rows(df, x)
  }
  
  # change values to numeric type and add the year variable
  # and caclulate the surplus
  df %>% 
    mutate(expend.total = as.numeric(expend.total),
           income.total = abs(as.numeric(income.total)),
           transport.total = as.numeric(transport.total)) -> df
  df$year <- year
  df <- df %>% mutate(auth.name, auth.name = recode(auth.name, !!!orig.sco.name.lookup)) 
  df
}

# Function to clean up the DPE table extracted from the pdf using tabulizer:
FunScotlandDPE <- function(list, year) {
  #  remove header row and add column names
  df <- list[[1]][-1,]
  
  rows <- nrow(df)
  # turn into single column
  df <- data.frame(value = c(df[,1], df[,2], df[,3]),
                   dpe.status = c(rep("dpe.now", rows),
                           rep("dpe.next", rows),
                           rep("dpe.not", rows)))
  # clean up 
  df %>% 
    filter(value != "") %>% 
    separate(value, into = c("auth.name", "x"), sep = "\\\r", fill = "right") %>% 
    separate(auth.name, into = c("auth.name", "dpe.year"), sep = " \\(", fill = "right")  %>% 
    separate(x, into = c("x", "e.d"), sep = "\\(", fill = "right") %>% 
    unite(dpe.year, e.d, dpe.year) %>% 
    mutate(dpe.year = gsub("NA_", "", dpe.year)) %>% 
    mutate(dpe.year = gsub("_NA", "", dpe.year)) %>% 
    separate(dpe.year, into = c("dpe.year", "x"), sep = "\\)", fill = "right") %>% 
    select( -x)  %>% 
    mutate( year = year,
            dpe.year = suppressWarnings(as.numeric(dpe.year))) -> df
  df <- df %>% mutate(auth.name, auth.name = recode(auth.name, !!!orig.sco.name.lookup)) 
  df
}


# Function to clean up the PNC table extracted from the pdf using tabulizer:
FunScotlandPCN <- function(df, year) {
  column <- paste0("X", year, ".", year-1999)
  df %>% 
    mutate(pcn.number = gsub("[^0-9]", "", (!!as.name(column))),
           auth.name = gsub("[^A-Za-z ]", "", Local.Authority),
           year = year) %>% 
    mutate(pcn.number = ifelse( pcn.number == "", NA, as.numeric(pcn.number))) %>% 
    select(year, auth.name, pcn.number) -> df
  df <- df %>% mutate(auth.name, auth.name = recode(auth.name, !!!orig.sco.name.lookup)) 
  df
}  

# Function to clean up the PNC income and expenditure table extracted 
# from the pdf using tabulizer:
# Careful, this funciton removes an "empty row" because East Dunmartonshire
# is in two rows, and that also removes half its name. This is fixed using
# the scotland lookup table 
FunScotlandTFSIE <- function(df, year) {
  df %>% 
    filter(Local.Authority != "") %>% 
    select(auth.name = Local.Authority,
           income.pcn = PCN,
           income.tfs = Total,
           expend.tfs = Expenditure) %>% 
    mutate(income.pcn = gsub("[^0-9]", "", income.pcn),
           income.tfs = gsub("[^0-9]", "", income.tfs),
           expend.tfs = gsub("[^0-9]", "", expend.tfs),
           auth.name = gsub("[^A-Za-z ]", "", auth.name),
           year = year) %>% 
    mutate(income.pcn = ifelse( income.pcn == "", NA, as.numeric(income.pcn)),
           income.tfs = ifelse( income.pcn == "", NA, as.numeric(income.tfs)),
           expend.tfs = ifelse( income.pcn == "", NA, as.numeric(expend.tfs))) %>% 
    mutate_at(vars(income.pcn:expend.tfs), list(~./1000)) %>% 
    filter(!is.na(income.pcn)) -> df
  df <- df %>%  mutate(auth.name, auth.name = recode(auth.name, !!!orig.sco.name.lookup)) 
  
  df
}

# reshape and clean function  for Wales
FunWalesReshape <- function(DF) {
  var <- deparse(substitute(DF))
  var <- substring(var, 5)
  DF %>% 
    gather(key = year, value = !!var, 2:(ncol(DF))) %>% 
    separate(year, into = c("X", "year"), sep = "(?<=[A-Z])(?=[0-9])")%>%  
    separate (year, into = c("year", "XX"), extra = "drop") %>%  
    mutate(year = as.integer(year)) %>% 
    select(-X, -XX) %>% 
    rename(auth.name = X.1) %>% 
    mutate(auth.name = sub("\\s+$", "", auth.name))
}

# function to capitalise the first letter in a workd
FunFirstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# funciton to round to d decimal points precisely
FunDec <- function(x,d) {
  format(round(x, d), nsmall = d)
}

# function to write out the fiscal year
FunFisc <- function( x = 0, c.y = current.year){
paste0(c.y - x, "-", c.y - 1999 - x)
  }

# function to turn an arbitrarily long vector of elements into listed text with
# commas after each and an "and" at the end. 
FunMultiText <- function(vec) {
  n <- length(vec)
  ifelse(n == 0, "",
         ifelse(n == 1, vec[1],
                {text <- vec[1]
                for( e in 2:(n-1) ){
                  if (n > 2) {text <- paste0(text,", ", vec[e])}
                }
                paste0(text, ", and ", vec[n])}
         )
  )
} 

# functionf for converting numbers to words
FunN2W <- function(x) {
  if (x <= 10) {xfun::numbers_to_words(x)} else{as.character(x)}
}


## functions ################
FunDivergePalette <- function(data, data.full = NULL, dir = 1, factor = 1){
  if(is.null(data.full)) {data.full <- data}
  top <- ifelse(dir == 1, "D", "C")
  bottom <- ifelse(dir == 1, "C", "D")
  data.full.clean <- data.full[!is.na(data.full)]
  min <- min(data.full.clean)
  max <- max(data.full.clean)
  if(min < 0 & 0 < max){
    range <- max-min 
    min.prop <- -round(min(data.full.clean)/range*100)
    max.prop <- 100 - min.prop
    rb1 <- seq(min, 0, length.out=min.prop + 1)
    rb2 <- seq(0, max, length.out=max.prop)[-1]
    rampbreaks <- c(rb1, rb2)
    cuts <- suppressWarnings(classIntervals(data, style="fixed",fixedBreaks=rampbreaks))
    if (abs(min) > max) {
      pal.min <- viridis_pal(begin = 0.93, end = 0, 
                             option = bottom)(min.prop)
      end <- 1-0.9/min.prop * max.prop
      pal.max <- viridis_pal(begin = 0.93, end = end, 
                             option = top)(max.prop)
    } else {
      pal.max <- viridis_pal(begin = 0.93, end = 0, 
                             option = top)(max.prop)
      end <- 1-0.9/max.prop * min.prop
      pal.min <- viridis_pal(begin = 0.93, end = end, 
                             option = bottom)(min.prop)}
    pal.full <- c(rev(pal.min), pal.max)}
  pal.full <- FunLighten(pal.full, factor)
  i <- findCols(cuts)
  pal <- pal.full[i]
  pal.na <- ifelse(is.na(pal), "#FFFFFFFF", pal)
  return(list(pal.full, pal, pal.na))
}

## legend function ##########
FunScaleLegend <- function(col, data){
  opar <- par
  n <- length(col)
  bx <- par("usr")
  box.cx <- c(bx[2] -  (bx[2] - bx[1]) / 10,
              bx[2] - (bx[2] - bx[1]) / 10 + (bx[2] - bx[1]) / 70)
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
  x <- axis(side = 4, las = 2, tick =FALSE,  at = NULL, labels =  NA)
  axis(side = 4, las = 2, tick = FALSE, at = x, labels = 
         paste0(x, " %"), cex.axis = 0.5, line = -3)
  par <- opar
}


FunMap <- function(table, which = 1, shp, country = "^S", dir = 1, factor = 1) { 
  # clean data ##
  if (country == "^S") {
  shp %>% 
    filter(grepl(country, lau118cd)) %>% 
    mutate(lau118nm = recode(lau118nm, !!!orig.sco.name.lookup)) %>% 
    left_join(select(table, auth.name, change, change.4), 
              by = c("lau118nm"= "auth.name")) -> map.data} else {
                shp %>% 
                  filter(grepl(country, lau118cd)) %>% 
                  left_join(select(table, auth.name, change, change.4), 
                            by = c("lau118nm"= "auth.name")) -> map.data}
  
  # vector for colouring
  if (which == 1) {
  data <- map.data$change} else {
    data <-  map.data$change.4}
  full <- c(map.data$change, map.data$change.4)
  # get colour palette and breaks
  pal <- FunDivergePalette(data, full, dir = dir, factor = factor)
  
  # plot map
  plot(map.data["change"], main = "", col = pal[[2]], 
       border = "black", lwd = 0.3)
  # plot copyright 
  if (country == "^S") {
    text(0.75,-0.32,"Contains public sector information licensed\n under the Open Government Licence v3.0.", 
         cex = 0.4, xpd = TRUE)} else {
           text(0.85,-0.32,"Contains public sector information licensed\n under the Open Government Licence v3.0.", 
                cex = 0.4, xpd = TRUE)      }
  
  # plot legend.
  FunScaleLegend(pal[[1]], full[!is.na(full)])
}


# Function to lighten colours by a factor
FunLighten <- function(cols, factor = 1.25) {
  cols1 <- readhex(file = textConnection(paste(cols, collapse = "\n")),
                   class = "RGB")
  #transform to hue/lightness/saturation colorspace
  cols1 <- as(cols1, "HLS")
  #multiplicative decrease of lightness
  cols1@coords[, "L"] <- cols1@coords[, "L"] * factor
  #going via rgb seems to work better  
  cols1 <- as(cols1, "RGB")
  cols1 <- hex(cols1)
 cols1
}

# Function for calculating RPI from the downloaded csv tables
FunRpi <- function(current.year, n = 4) {
  rpi %>% 
    mutate(month = gsub("^[^/]+", "", Month)) %>% 
    select(month, Cost.of.Living) %>% 
    filter(month %in% c(paste0("/04/", current.year - n),
                        paste0("/04/", current.year))) %>% 
    mutate(Cost.of.Living = Cost.of.Living + 100,
           Month = c("start", "end"))  %>% 
    summarise(rpi = Cost.of.Living[Month == "end"] / Cost.of.Living[Month == "start"]) %>% 
    mutate(rpi = (rpi ^ (1/n) - 1) * 100) %>% pull(rpi) -> rpi.annual
  rpi.annual  
}

# function to calculate percentage change, including if the change is fro m0 to 0
# with optional multiplication with 100
FunPercent <- function(new, old, sto = 100) {
  p <- ifelse(new == 0 & old == 0, sto, new/old*sto)
  p 
}
