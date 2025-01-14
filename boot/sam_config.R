sam_assessment <- "Cod_27_5b1"

sam_dir <-
  paste0(
    "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",
    sam_assessment,
    "/conf/"
  )

files <- "model.cfg"

for (file in files) {
  download(paste0(sam_dir, file))
}
