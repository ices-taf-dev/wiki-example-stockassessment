## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)

mkdir("report")

source.taf("report_plots_saorg.R")

sourceTAF("report_plots.R")
sourceTAF("report_tables.R")
sourceTAF("report_doc.R")
