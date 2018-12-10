## ----setup, include=FALSE------------------------------------------------
library(tidyverse)
library(ajfhelpR)
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
#Create data frame/tibble with combination of all bools

x = c(T,F,NA)

bools = as.tibble(expand.grid(x,x))

equals = bools %>% 
  mutate(`==` = (Var1 == Var2),
         `%==%` = (Var1 %==% Var2))

## ------------------------------------------------------------------------
equals

## ------------------------------------------------------------------------

Venns = bools %>% 
  mutate(`|` = (Var1 | Var2),
         `%or%` = (Var1 %or% Var2),
         `&` = (Var1 & Var2),
         `%&%` = (Var1 %&% Var2))


## ------------------------------------------------------------------------
Venns

## ------------------------------------------------------------------------
set.seed(12345)

y = 'blue'

colors = c('blue', 'red', 'yellow', NA)

colorsamp = sample(colors, size = 10, prob = c(0.4,0.2,0.1,0.3),
                   replace = T)


y  ==  colorsamp
y %==% colorsamp

colorsamp[colorsamp == y]

colorsamp[colorsamp %==% y]


## ------------------------------------------------------------------------
set.seed(12356)

tib = tibble(
  x = sample(c("a", "b", NA), size = 10, replace = T),
  y = sample(1:10, size = 10, replace = T)
)

tib


## ------------------------------------------------------------------------

tib = tib %>% 
  mutate(`y %==%` = ifelse(x %==% 'a', x, 0),
         `y ==`   = ifelse(x  ==  'a', x, 0))

tib

