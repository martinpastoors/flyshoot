# test
options(dplyr.summarise.inform = FALSE)

library(rmarkdown)                   # note: requires knitr 1.21
require(tidyverse, quietly=TRUE)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
require(lubridate, quietly=TRUE)     # data handling
require(reshape2, quietly=TRUE)      # reshaping data; e.g. cast
require(RColorBrewer)                # colour schemes
library(viridis)
library(pander)

test <- c("MAC","CTC")
all(!grepl("all", test))
