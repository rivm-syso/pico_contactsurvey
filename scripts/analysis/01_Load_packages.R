################################################################################
#
# Load R packages and functions
# 
################################################################################

library(tidyverse)
library(lubridate)
library(cowplot)
library(patchwork)
library(ISOweek)
library(RColorBrewer)
library(cbsodataR)
library(writexl)

# Source functions
list.files(path = "./R", full.names = TRUE) %>% walk(.f = source)


