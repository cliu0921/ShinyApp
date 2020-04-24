library(shiny)
library(shinydashboard)
library(DT)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

nassau = read.csv('./data/nassau.csv',stringsAsFactors = FALSE)
nassau = nassau %>% mutate(.,Sold = as.Date(Sold))
str(nassau)
unique(nassau$Bedrooms)
town_names = sort(unique(nassau$Town))
design = unique(nassau$DesignType)
number_bedrooms = sort(unique(nassau$Bedrooms))

town_names[1]
town_names2 = town_names[1]="all"
town_names2
