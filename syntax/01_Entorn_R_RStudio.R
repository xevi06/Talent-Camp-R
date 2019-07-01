# 01.1 Instalación y carga de paquetes
library(ggplot2)
# install.packages("ggplot2")
# install.packages("rlang")

# 01.2 Definición de workingdirectory
getwd()
setwd("/cloud/project/syntax/")
getwd()
setwd("/cloud/project/")

# 01.3 Workspace
a = 1
b = 'aaa'
save.image(file = "data/myworkspace.Rdata")
rm(a,b)
load(file = "data/myworkspace.Rdata")
rm(list = ls())
