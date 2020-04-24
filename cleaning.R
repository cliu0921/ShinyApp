library(shiny)
library(shinydashboard)
library(DT)
library(dbplyr)
library(tidyverse)
library(lubridate)

csv_filenames <- list.files('./data', pattern = 'csv')
csv_filepaths <- paste0( './data/', csv_filenames, sep = '' )
all_dfs <- lapply( csv_filepaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )
whole_data = bind_rows(all_dfs)

street_remove = function(x){
  select(x,-1)
}

all_dfs2= lapply(all_dfs,street_remove)
whole_data = bind_rows(all_dfs2)
head(whole_data,5)
str(whole_data)
whole_data = tbl_df(whole_data)
whole_data2 = whole_data
str(whole_data2)
saveRDS(whole_data2, './data/whole_data2.RDS')
real_estate = readRDS('./data/whole_data2.RDS')
head(real_estate,3)
colnames(real_estate)
real_estate = real_estate %>% rename(.,DesignType = X, Basement = X.1, Garage = X.2,DaysOnMarket = DOM)
real_estate %>% filter(.,TitleDate == 'NA' | TitleDate == '')

head(real_estate$TitleDate,2)
real_estate2 = real_estate %>% mutate(.,TitleDate = parse_date(TitleDate,format ='%m/%d/%Y'))
head(real_estate2$TitleDate)
class(real_estate2$TitleDate)

real_estate5 = real_estate2 %>% mutate(.,SellYM =format(TitleDate,'%Y-%m'))
head(real_estate5$SellYM)
class(real_estate5$SellYM)

real_estate6 = real_estate5 %>% mutate(., Sold = as.Date(paste0(SellYM,'-01'), format = '%Y-%m-%d'))
head(real_estate6$Sold)
class(real_estate6$Sold)
unique(real_estate6$Town)

real_estate7 = real_estate6 %>% mutate(.,Year = format(as.Date(real_estate6$Sold,format = '%Y-%m-%d'),'%Y')) %>% 
  rename(.,Bedrooms = Br)
str(real_estate7)





colnames(real_estate6)
write.csv(real_estate7,"./data/nassau.csv")
nassau = read.csv('./data/nassau.csv',stringsAsFactors = FALSE)
nassau$Sold[1]
str(nassau)

nassau = nassau %>% mutate(.,Sold = as.Date(Sold))
nassau = nassau %>% filter(.,Bedrooms != 41)
nassau2 = as.data.frame(nassau)

unique(nassau$Town)
nassautest = unique(nassau[nassau$Bedrooms == 41,'SoldPrice'])
nassautest
nassau3 = nassau %>% filter(.,Bedrooms != 41)
str(nassau3)
#REMOVE ELMONT 41 ROOM

nassau %>% group_by(.,Town,Sold,Bedrooms,DesignType) %>% summarise(.,mean(SoldPrice)) %>% filter(.,Town == "Great Neck", Bedrooms == 4,
                                                                                                 )

#test = real_estate5$SellYM[1]
#test
#test =paste0(test,'-01')
#test2 = as.Date(test,format ='%Y-%m-%d')
#test2
#class(test2)