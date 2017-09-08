### Buffer console output
cat(rep("\n", 20))

### Clear environment
rm(list = ls())

### Global options
options(stringsAsFactors = FALSE)

### Detach Packages
detachAllPackages <- function() { 
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
remove(detachAllPackages)

# Function to assert a package has been installed before loading the package
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T, repos = "http://cran.us.r-project.org")
  sapply(pkg, library, character.only = T)
}

# Load required packages
pkgs <-
  c(
    "listviewer",
    "jsonlite",
    "stringr",
    "tidyverse")
ipak(pkgs)
remove(pkgs, ipak)