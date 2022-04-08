## Author : Simon Moulds
## Date   : April 2022

library(tidyverse)
library(zoo)
library(devtools)
load_all()

## Load default dataset
x <- nmme()
x

## Let's filter by members:
x <- x %>% subset(members = c(1:5))
x

## Or by space:
x <- x %>% subset(xmin = 5, xmax = 10, ymin = 5, ymax = 10)
x

## We can subset URLs with the `[` generic:
x[1]
x[1:10]
