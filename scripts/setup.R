# download and load libraries

if(!require(readr)) {
  install.packages(c("tidyverse"))
}

if(!require(metacoder)) {
  install.packages(c("metacoder"))
}

if(!require(vegan)) {
  install.packages(c("vegan"))
}

library(readr)
library(dplyr)
library(readxl)

download.file("https://elifesciences.org/download/aHR0cHM6Ly9jZG4uZWxpZmVzY2llbmNlcy5vcmcvYXJ0aWNsZXMvNDI2OTMvZWxpZmUtNDI2OTMtc3VwcDItdjIueGxzeA--/elife-42693-supp2-v2.xlsx?_hash=6kBRGnqjOYzvDgQ413MFhy2Cza78BBj1v%2Fii6o4606E%3D",
              destfile = "data/elife-data.xlsx")

# load data
data <- read_excel("data/elife-data.xlsx", sheet = "Taxa_rel_abd")

# write out as csv for future exercises
write.csv(data, "data/elife-data.csv")

rm(data)

