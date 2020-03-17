library(readxl)
library(dplyr)
library(googledrive)

temp <- tempfile(fileext = ".zip")
dl <- drive_download(
  as_id("1qjbm1rmLRf9jnjox1ZhO-ftkOkvKy1UH"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())

Adj_2018 <- read_excel(out[3])
Adj_2019 <- read_excel(out[4])
Adj_2015 <- read_excel(out[5])
Adj_2016 <- read_excel(out[6])
Adj_2017 <- read_excel(out[7])

Of_2015 <- read_excel(out[8])
Of_2016 <- read_excel(out[9])
Of_2017 <- read_excel(out[10])
Of_2018_19  <- read_excel(out[11])

SIAC_18_19 <- read_excel(out[1])
SIAC_15_17 <- read_excel(out[12])

SIAC_18_19$`Año de adjudicación` <- as.numeric(SIAC_18_19$`Año de adjudicación`)

Adj_Total <- bind_rows(Adj_2015, Adj_2016, Adj_2017, Adj_2018, Adj_2019)
Of_Total <- bind_rows(Of_2015, Of_2016, Of_2017, Of_2018_19)
SIAC_Total <-bind_rows(SIAC_15_17,SIAC_18_19)


