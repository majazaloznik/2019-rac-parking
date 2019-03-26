################################################################################
## WALES UPDATE TEMPLATE #######################################################
################################################################################
##
##
################################################################################

original.data <- readRDS("data/02-interim/original.data.rds")


# save updated datafile to master
saveRDS(original.data, "data/03-processed/master.rds")





current.year <- 2015

rmarkdown::render("code/report-templates/wales-report-template.Rmd",
                  output_file = paste0("wales-report", 
                                       current.year,
                                       ".pdf"),
                  output_dir = "outputs/reports",
                  params = list(current.year = current.year))
