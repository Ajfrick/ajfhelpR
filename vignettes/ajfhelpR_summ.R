## ----setup, include = FALSE----------------------------------------------
library(ajfhelpR)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = T, echo = T----------------------------------------------
## Packages used for demonstration

# install.packages("tidyverse")
# install.packages("kableExtra")
# install.packages("pander")
# install.packages("stringr")
# install.packages("knitr")

library(tidyverse)
library(kableExtra)
library(pander)
library(rmarkdown)
library(knitr)
library(stringr)

## ------------------------------------------------------------------------
## Group Iris data by Species, and report summary statistics according to 
## different summ_interval input. Then output table with kableExtra

tab = iris %>% dplyr::group_by(Species) %>% 
  dplyr::summarise(Length1 = summ_interval(Sepal.Length, fun = mean),
                   Length2 = summ_interval(Sepal.Length, fun = median,
                                          limits = c(0.2, 0.6), digits = 0),
                   Length3 = summ_interval(Sepal.Length, fun = median,
                                          limits = 0:1, delim = "-"),
                   Length4 = summ_interval(Sepal.Length, fun = mean,
                                          limits = c(0.4, 0.6), delim = " ; ")) %>% t %>% 
  tibble::as.tibble()

##Format Table more cleanly
colnames(tab) = tab[1,]
tab = tab[-1,]

tab = tab %>% dplyr::mutate(summ_interval = 1:4) %>% 
  dplyr::select(c(4,1:3))

knitr::kable(tab,format = 'html') %>% 
  kableExtra::kable_styling(bootstrap_options = "hover")

## ------------------------------------------------------------------------
##Check Count and proportion of observations in mt cars with certain number of 
##cylinders, using logical vectors, and character vector with category

mtcars = mtcars %>% 
  dplyr::mutate(cyl_chr = as.character(cyl))

mtcars %>% dplyr::summarise(Cyl1 = summ_prop(cyl == 4, out = "percent"),
                            Cyl2 = summ_prop(cyl_chr, "4", out = "percent"),
                            Cyl3 = summ_prop(cyl_chr %in% c("4","8"),
                                                 out = "percent", perc_disp = T)) %>% 
  t %>% knitr::kable(format = 'html')  %>% 
  kableExtra::kable_styling(bootstrap_options = "hover")


## ------------------------------------------------------------------------
##Group mtcars by number of cylinders, and then summarise MPG, Count w prop 
## of Automatic Transmission, and Horsepower

mtcars %>% dplyr::group_by(cyl) %>% 
  dplyr::summarise(mpg = summ_interval(mpg),
                   am  = summ_prop(am == 1),
                   hp  = summ_interval(log2(hp+1), digits = 1,
                                      limits = 0:1)) %>% 
  t %>% knitr::kable(format = 'html')  %>% 
  kableExtra::kable_styling(bootstrap_options = "hover")
  

## ------------------------------------------------------------------------
X = 1:100
na.ind = sample(1:100, 15, replace = F)

summ_na(X)
summ_na(X, zero2dash = F)

X[na.ind] = NA

summ_na(X)
summ_na(X, out = "percentage")


