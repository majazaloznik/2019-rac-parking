################################################################################
##                        DATA MANAGEMENT PIPELINE SCHEME                     ##
################################################################################


# PRELIMINARIES
suppressWarnings(suppressMessages(library(DiagrammeR)))
suppressWarnings(suppressMessages(library(DiagrammeRsvg)))
suppressWarnings(suppressMessages(library(rsvg)))

# DIAGRAM
diagram <- "
digraph boxes_and_circles {

# a 'graph' statement
graph [overlap = false, fontsize = 7,
layout = dot]

# several 'node' statements
node [shape = box,
fontname = Helvetica, color = darkviolet]
'00-orig-data-entry.R'; '01-orig.data.import.R'
'functions.R'; 'sco|wal|eng.R'

node [shape = oval, color = orange]
'+orig.xxx.rds'; 'original.data.rds';
'master.rds'; 'bib.master.rds'
'sco|wal|eng.bib.rds'

node [shape = circle, color = red] 
'/data/01-raw/'

node [shape = box, color = green] 
'/updating-templates/xxx.R'

node [shape = diamond, color = steelblue] 
'/report-templates/xxx.Rmd'

node [shape = diamond, color =  green] 
'/report-rmds/xxx.Rmd'

node [shape = oval, color =  pink] 
'/outputs/reports/xxx.pdf'

node [shape = circle, color =  pink] 
'/outputs/csv-tables/'

# several 'edge' statements
'00-orig-data-entry.R' -> '+orig.xxx.rds'
'original.data.rds' -> 'master.rds'
'+orig.xxx.rds' -> '01-orig.data.import.R'
'functions.R' -> '01-orig.data.import.R'
'01-orig.data.import.R' -> 'original.data.rds'
'/data/01-raw/' -> '01-orig.data.import.R'
'/report-templates/xxx.Rmd' ->'/updating-templates/xxx.R'
'/updating-templates/xxx.R' -> 'master.rds'
'/updating-templates/xxx.R' ->'/report-rmds/xxx.Rmd'
'/report-rmds/xxx.Rmd' -> '/outputs/reports/xxx.pdf'
'/report-rmds/xxx.Rmd' -> '/outputs/csv-tables/'
'sco|wal|eng.R' -> '/report-rmds/xxx.Rmd'
'master.rds' -> 'sco|wal|eng.R'
'/data/01-raw/' -> '/updating-templates/xxx.R'
'functions.R' -> 'sco|wal|eng.R'
'functions.R' -> '/updating-templates/xxx.R'
'functions.R' -> '00-orig-data-entry.R' 
'00-orig-data-entry.R' -> 'bib.master.rds'
'/updating-templates/xxx.R' -> 'bib.master.rds'
'bib.master.rds' -> '/updating-templates/xxx.R' 
'/updating-templates/xxx.R' -> 'sco|wal|eng.bib.rds'
'sco|wal|eng.bib.rds' ->  '/report-rmds/xxx.Rmd' 
'/updating-templates/xxx.R' -> '/data/01-raw/'
}"
png::writePNG(rsvg(charToRaw(export_svg(grViz(diagram)))), 
              here::here("docs/instructions/chart.png"))
   

# png::writePNG(rsvg(charToRaw(export_svg(grViz(diagram)))), here::here("figures/make.png"))

