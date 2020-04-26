library(shiny)
library(shinydashboard)
library(DT)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(plotly)


csv_filenames <- list.files('./data', pattern = 'csv')
csv_filepaths <- paste0( './data/', csv_filenames, sep = '' )
all_dfs <- lapply( csv_filepaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ) )
whole_data = bind_rows(all_dfs)


nassau %>% filter(.,Year == 2018) %>% group_by(.,Town) %>%  summarise(.,year_ave_sale= mean(SoldPrice),
                                                                               year_ave_dom = mean(DaysOnMarket))




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


max(nassau[nassau$Year == 2017,"SoldPrice"])


ggplot(nassau %>% filter(.,SoldPrice >= 1000000 & SoldPrice<=100000000) %>% 
         group_by(.,Town,Sold) %>% summarise(.,total_contracts = n()) %>% top_n(.,5,total_contracts),aes(x= Sold,y= total_contracts,color = Town))+
  geom_line()

nassau %>% filter(.,SoldPrice >= 1000000 & SoldPrice<=100000000) %>% 
  group_by(.,Town,Year) %>% summarise(.,total_contracts = n())


nassau %>% filter(.,Town == c('Hempstead','Levittown','Freeport','Westbury','Oceanside','Plainview')) %>% 
  group_by(.,Town,Year) %>% summarise(.,total_contracts = n())



top_town=c('Hempstead','Levittown','Freeport','Westbury','Oceanside','Plainview','Great Neck')

nassau_sd = nassau
nassau_sd$SD =ifelse(nassau$SD == 11, 'Carle Place Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 2, 'East Williston District',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 7, 'Great Neck District',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 9, 'Herricks Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 10, 'Mineola Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 13, 'Valley Stream Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 6, 'Manhasset Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 5, 'New Hyde Park/ Garden City Park Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 4, 'Port Washington Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 3, 'Roslyn Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 8, 'Roosevelt Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 15, 'Jericho Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 12, 'Lynnbrook Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 14, 'Woodmere Disctrict',nassau_sd$SD)
nassau_sd$SD =ifelse(nassau$SD == 1, 'Westbury Disctrict',nassau_sd$SD)
unique(nassau_sd$SD)

write.csv(nassau_sd,"./data/nassau.csv")
nassau = read.csv('./data/nassau.csv',stringsAsFactors = FALSE)
nassau$Sold[1]
str(nassau)
nassau = nassau %>% mutate(.,Sold = as.Date(Sold))
nassau = nassau %>% filter(.,Bedrooms != 41)

test = nassau %>% group_by(.,Town) %>% summarise(.,ave_sale = mean(SoldPrice),ave_dom = mean(DaysOnMarket))
test

nassau_ave_price = mean(nassau$SoldPrice)
nassau_ave_dom = mean(nassau$DaysOnMarket)
nassau_ave_dom

ggplotly(ggplot(test,aes(x=ave_dom,y=ave_sale)) + geom_point(), hoverinfo= 'text',text = ~paste(Town, '</br></br>','Days on Market:', round(ave_dom, digits = 0),'</br>',
             'Sale Price:', round(ave_sale,digits = 0)))


mean_dom = mean(as.numeric(nassau$DaysOnMarket))
nassau %>% filter(.,is.na(DaysOnMarket))

mean_price = mean(nassau$SoldPrice)

x = ggplot(test)+geom_point(aes(x=ave_dom,y=ave_sale,group = Town, text= paste(Town, '</br></br>','Days on Market:', round(ave_dom, digits = 0),'</br>', 'Sale Price:', 
                                                                           round(ave_sale,digits = 0)))) +
  geom_vline(xintercept = nassau_ave_dom) + geom_hline(yintercept = mean_price)




ggplotly(ggplot(nassau %>% group_by(.,Town) %>% summarise(.,ave_sale = mean(SoldPrice),ave_dom = mean(DaysOnMarket)))+geom_point(aes(x=ave_dom,y=ave_sale,group = Town, text= paste(Town, '</br></br>','Days on Market:', round(ave_dom, digits = 0),'</br>', 'Sale Price:', 
                                                                                    round(ave_sale,digits = 0)))) +
           geom_vline(xintercept = nassau_ave_dom) + geom_hline(yintercept = mean_price), tooltip = 'text')


mean(nassau[nassau$Year == 2020,"DaysOnMarket"])

plot_ly(data = test,x=~ave_dom,y=~ave_sale,hoverinfo = 'text',text = ~paste(Town, '</br></br>',
                                                                            'Days on Market:', round(ave_dom, digits = 0),'</br>',
                                                                            'Sale Price:', round(ave_sale,digits = 0))) %>% 
  add_segments(x = nassau_ave_dom, xend= nassau_ave_dom,)


nassau = nassau %>% filter(.,Town!= "Plandome Manor")
write.csv(nassau_sd,"./data/nassau.csv")
nassau = read.csv('./data/nassau.csv',stringsAsFactors = FALSE)
nassau$Sold[1]
str(nassau)
nassau = nassau %>% mutate(.,Sold = as.Date(Sold))


ocean = ggplot(nassau %>% group_by(.,SD,Town,Bedrooms_cat) %>% summarise(.,ave_town_rooms_price = mean(SoldPrice)) %>% filter(.,SD == 'Carle Place Disctrict'),
       aes(x= Town, y = ave_town_rooms_price, group = Bedrooms_cat, text = paste(Bedrooms_cat,'</br></br>',
                                                                            'Ave Price:', ave_town_rooms_price,
                                                                            'Town',Town))) + geom_bar(aes(fill=Bedrooms_cat),position = 'dodge',stat= 'identity')



ggplotly(ocean, tooltip = 'text')
nassau %>% group_by(.,SD,Town,Bedrooms_cat) %>% summarise(.,ave_town_rooms_price = mean(SoldPrice)) %>% filter(.,SD == 'Carle Place Disctrict')


nassau %>% filter(.,Town == "Oceanside", Bedrooms_cat == "4")

ggplot(nassau %>% group_by(.,Month) %>% summarise(.,month_ave_price = mean(SoldPrice)),aes(x = as.integer(Month),y = month_ave_price)) + 
  geom_bar(stat = 'identity') + scale_x_continuous(breaks = 1:12,
                                                   labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
                                                   ) + coord_cartesian(ylim = c(600000,800000))




