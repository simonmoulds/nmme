## Author : Simon Moulds
## Date   : April 2022

library(tidyverse)
library(zoo)
library(devtools)
load_all()

## Load default dataset:
x <- nmme()
x

## Dataset dimensions:
dim(x)

## The package provides two ways to select data. The first way is using the the `nmme_subset` function. For example, we can filter by members:
x %>% nmme_subset(members = c(1:5))

## Or by space:
xx <- x %>% nmme_subset(xmin = 5, xmax = 10, ymin = 5, ymax = 10)

## Or both at once:
x %>% nmme_subset(members = c(1:5), xmin = 5, xmax = 10, ymin = 5, ymax = 10)

## Alternatively, we can subset URLs with the `[` generic:
x[1,]
x[1:10,]
x[1,1:5,1:3]

## Once you are happy with the selection you can download the files:
x_test <- x[1,1,1]
x_test <- x_test %>% download_nmme(overwrite = TRUE)

## Now we can read the dataset into R
library(terra) # Note: `terra` is a successor to `raster`, and is better at handling multidimensional raster
r <- rast("prec_GFDL-SPEAR_199101_1.0_199101-199101.nc")
plot(r)

## It is straightforward to write methods for `nmme` objects.
## TODO
