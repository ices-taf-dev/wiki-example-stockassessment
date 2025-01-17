library(icesTAF)
library(rmarkdown)
library(quarto)


source("utilities.R")

mkdir("report")

# combine into a word and html document
render("report.Rmd",
  output_format = c("word_document", "html_document"),
  encoding = "UTF-8"
)

# move to report folder
cp("report.html", "report", move = TRUE)
cp("report.docx", "report", move = TRUE)

# build quality report

quarto_render("report_quality_report.qmd")
