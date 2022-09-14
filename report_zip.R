
# zip up some ouput
files <- dir("report", full = TRUE)

zip("report/report.zip", files = files)
