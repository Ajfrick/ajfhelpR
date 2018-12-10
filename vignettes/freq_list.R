## ----setup, include = FALSE----------------------------------------------
library(ajfhelpR)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
##Tabulate difference combinations amongst the mtcars dataset
library(stringr)
library(AVRCHelp)
library(tidyverse)

attach(mtcars)
freq_list(table(cyl,vs,am,gear,carb))

# Same as previous, but using head() to extract 5 most frequent combinations
head(freq_list(table(cyl,vs,am,gear,carb)))

freq_list(table(cyl,vs,am))
detach(mtcars)

## ------------------------------------------------------------------------
##tabulate categorical characteristics of diamonds dataset

attach(diamonds)

head(freq_list(table(cut,color,clarity)))

detach(diamonds)

