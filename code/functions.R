# function to extract expenditure data from England outturn excel file


FunEnglandOutturn <- function(file=file, 
                              first=first, 
                              last=last, 
                              e.sh=e.sh, 
                              e.on=e.on, 
                              e.off=e.off,
                              i.sh = i.sh,
                              i.on = i.on,
                              i.off = i.off,
                              auth.name = auth.name,
                              auth.type = auth.type) {
  cell.range <- paste0(auth.name, first-1, ":",auth.name, last)
  auth.name <- read_excel(file, e.sh, cell.range)
  colnames(auth.name) <- "auth.name"
  cell.range <- paste0(auth.type, first-1, ":",auth.type, last)
  auth.type <- read_excel(file, e.sh, cell.range)
  colnames(auth.type) <- "auth.type"
  cell.range <- paste0(e.on, first-1, ":", e.on, last)
  expend.on <- read_excel(file, e.sh, cell.range)
  colnames(expend.on) <- "expend.on"
  expend.on <- ifelse(!grepl("^-?[0-9.]+$", expend.on$expend.on), NA, 
                      as.numeric(expend.on$expend.on))
  cell.range <- paste0(e.off, first-1, ":", e.off, last)
  expend.off <- read_excel(file, e.sh, cell.range)
  colnames(expend.off) <- "expend.off"
  expend.off <- ifelse(!grepl("^-?[0-9.]+$", expend.off$expend.off), NA, 
                       as.numeric(expend.off$expend.off))
  
  cell.range <- paste0(i.on, first-1, ":", i.on, last)
  income.on <- read_excel(file, i.sh, cell.range)
  colnames(income.on) <- "income.on"
  income.on <- ifelse(!grepl("^-?[0-9.]+$", income.on$income.on), NA, 
                      as.numeric(income.on$income.on))
  cell.range <- paste0(i.off, first-1, ":", i.off, last)
  income.off <- read_excel(file, i.sh, cell.range)
  colnames(income.off) <- "income.off"
  income.off <- ifelse(!grepl("^-?[0-9.]+$", income.off$income.off), NA, 
                       as.numeric(income.off$income.off))
  
  df <- data.frame(auth.name, auth.type,expend.on, expend.off, income.on, income.off)
  df
}


#debugonce(FunEnglandOutturn)

# function to 'manually' fix local authority names that are inconsistent
# between the pdf tables. If new inconsistencies crop up, you can add them here:

FunScotlandFixNames <- function(df) {
  df %>% 
    mutate(auth.name = ifelse(auth.name == "Argyll  Bute" |
                                auth.name == "Argyll & Bute", "Argyll and Bute",
                              ifelse(auth.name == "Perth & Kinross", "Perth and Kinross",
                                     ifelse(auth.name == "Edinburgh, City of" |
                                              auth.name == "Edinburgh City"|
                                              auth.name == "Edinburgh, city of", "City of Edinburgh",
                                            ifelse(auth.name == "Dundee", "Dundee City",
                                                   ifelse(auth.name == "Glasgow", "Glasgow City",
                                                          ifelse(auth.name == "Perth  Kinross", "Perth and Kinross",
                                                                 ifelse(auth.name == "East", "East Dunbartonshire",
                                                                        ifelse(auth.name =="Comhairle nan Eilean Siar" |
                                                                                 auth.name == "Western Isles", "Eilean Siar",
                                                                               auth.name)))))))))
}

# function for extracting Scotlandn income and expenditure from relevant cells
FunScotlandLACels <- function(file, sheet, expend.total, income.total, auth.cell) {
  auth.name <- colnames(read_excel(file, sheet, auth.cell))
  expend.total <- colnames(read_excel(file, sheet, expend.total))
  income.total <- colnames(read_excel(file, sheet, income.total))
  c(auth.name, expend.total, income.total)
}

# Function that loops through all sheets and extracts relevant cell data 
FunScotandLoopIE <- function(row, year = year, 
                             file.name = file.name, 
                             start.sh = start.sh, 
                             end.sh = end.sh, 
                             exp.cell = exp.cell, 
                             inc.cell = inc.cell, 
                             auth.cell = auth.cell) {
  
  # prepare empty data frame for the data
  df <- data.frame(auth.name = character(),
                   expend.total = character(),
                   income.total = character())
  
  # loop through all the sheets
  for (sheet in start.sh:end.sh){
    x <- FunScotlandLACels(file.name, sheet, exp.cell, inc.cell, auth.cell)
    names(x) <- colnames(df)
    if(year == 2016) 
      x[1] <- gsub("^.+?, |, 2016-17", "", x[1])
    df <- bind_rows(df, x)
  }
  
  # change values to numeric type and add the year variable
  df %>% 
    mutate(expend.total = as.numeric(expend.total),
           income.total = as.numeric(income.total)) -> df
  df$year <- year
  df <- FunScotlandFixNames(df)
  df
}
# Function to clean up the DPE table extracted from the pdf using tabulizer:
FunScotlandDPE <- function(list, year) {
  #  remove header row and add column names
  df <- list[[1]][-1,]
  
  rows <- nrow(df)
  # turn into single column
  df <- data.frame(value = c(df[,1], df[,2], df[,3]),
                   key = c(rep("dpe.now", rows),
                           rep("dpe.next", rows),
                           rep("dpe.not", rows)))
  # clean up 
  df %>% 
    filter(value != "") %>% 
    separate(value, into = c("auth.name", "dpe.year"), sep = " \\(")  %>% 
    separate(dpe.year, into = c("dpe.year", "x"), sep = "\\)") %>% 
    separate(auth.name, into = c("auth.name", "x"), sep = "\\\r") %>% 
    select( -x)  %>% 
    mutate( year = year,
            dpe.year = as.numeric(dpe.year)) -> df
  df <- FunScotlandFixNames(df)
  df
}


# Function to clean up the PNC table extracted from the pdf using tabulizer:
FunScotlandPCN <- function(df, year) {
  column <- paste0("X", year, ".", year-1999)
  df %>% 
    mutate(pnc.number = gsub("[^1-9]", "", (!!as.name(column))),
           auth.name = gsub("[^A-Za-z ]", "", Local.Authority),
           year = year) %>% 
    mutate(pnc.number = ifelse( pnc.number == "", NA, as.numeric(pnc.number))) %>% 
    select(year, auth.name, pnc.number) -> df
  df <- FunScotlandFixNames(df)
  df
}  


# Function to clean up the PNC income and expenditure table extracted 
# from the pdf using tabulizer:
# Careful, this funciton removes an "empty row" because East Dunmartonshire
# is in two rows, and that also removes half its name. This is fixed using
# the FunScotlandFixNames() function. 
FunScotlandTFSIE <- function(df, year) {
  df %>% 
    filter(Local.Authority != "") %>% 
    select(auth.name = Local.Authority,
           income.pcn = PCN,
           income.tfs = Total,
           expend.tfs = Expenditure) %>% 
    mutate(income.pcn = gsub("[^1-9]", "", income.pcn),
           income.tfs = gsub("[^1-9]", "", income.tfs),
           expend.tfs = gsub("[^1-9]", "", expend.tfs),
           auth.name = gsub("[^A-Za-z ]", "", auth.name),
           year = year) %>% 
    mutate(income.pcn = ifelse( income.pcn == "", NA, as.numeric(income.pcn)),
           income.tfs = ifelse( income.pcn == "", NA, as.numeric(income.tfs)),
           expend.tfs = ifelse( income.pcn == "", NA, as.numeric(expend.tfs))) %>% 
    filter(!is.na(income.pcn)) -> df
  df <- FunScotlandFixNames(df)
  df
}



# reshape and clean function  for Wales
FunWalesReshape <- function(DF) {
  var <- deparse(substitute(DF))
  var <- substring(var, 5)
  DF %>% 
    gather(key = year, value = !!var, 2:(ncol(DF))) %>% 
    separate(year, into = c("X", "year"), sep = "(?<=[A-Z])(?=[0-9])", perl = TRUE) %>% 
    separate (year, into = c("year", "XX")) %>% 
    mutate(year = as.integer(year)) %>% 
    select(-X, -XX) %>% 
    rename(auth.name = X.1)
}
