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
FunScotandLoopIE <- function(row) {
  # get metadata for this year
  year = scotland.i.e.17.18$year[row]
  file.name = scotland.i.e.17.18$file.name[row]
  start.sh =  scotland.i.e.17.18$start.sh[row]
  end.sh =  scotland.i.e.17.18$end.sh[row]
  exp.cell =  scotland.i.e.17.18$exp.cell[row]
  inc.cell =  scotland.i.e.17.18$inc.cell[row]
  auth.cell = scotland.i.e.17.18$auth.cell[row]
  
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
