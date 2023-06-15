library(rmarkdown)

source("utilities.R")

mkdir("report")

# combine into a word and html document
render("report.Rmd",
  output_format = c("word_document", "html_document"),
  encoding = "UTF-8",
  output_dir = "report"
)
