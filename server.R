function(input, output,session) { 
  #plot1

  output$sales_and_ave_price_plot = renderPlot({
    ggplot(nassau %>% group_by(.,Sold) %>% summarise(.,plot1_ave_price = mean(SoldPrice), plot1_total_sales = n()) %>% 
           filter(.,Sold != as.Date('2017-03-01') & Sold != as.Date('2020-03-01')),aes(x=Sold)) +
    geom_smooth(aes(y= plot1_total_sales*1000),se = FALSE,span= 0.5) +
    geom_smooth(aes(y=plot1_ave_price),se= FALSE, span = 0.3) + 
    scale_y_continuous(
      name = "ttest",
      sec.axis = sec_axis(~./1000, name = 'test')) +
    coord_cartesian(ylim = c(400000,900000))
  })
?sec_axis  
  

  
  #---------------------------------------------------------------------------------------------------------------------
  #plot trend
  output$trend = renderPlot({
    ggplot(
      nassau %>% group_by(.,SD,Sold) %>% summarise(.,ave_sd_price = mean(SoldPrice)) %>% filter(.,SD== input$SDselected),
      aes(x= Sold, y=ave_sd_price)
    ) + geom_line()
    
  })
  
  output$bedrooms_by_sd = renderPlotly({
    ggplotly(
    ggplot(
      nassau %>% group_by(.,SD,Town,Bedrooms_cat) %>% summarise(.,ave_room_price_town = mean(SoldPrice)) %>% 
        filter(.,SD == input$SDselected), 
      aes(x = Town, y = ave_room_price_town, group = Bedrooms_cat, text = paste('Town:', Town, '</br></br>',
                                                                                'Number of Rooms:', Bedrooms_cat,'</br>',
                                                                                'Average Sold Price:', 
                                                                                format(round(ave_room_price_town, digits = 0),
                                                                                            nsmall = 1, big.mark = ",")))) +
        geom_bar(aes(fill= Bedrooms_cat),position = 'dodge',stat = 'identity')
    , tooltip = 'text'
    )
  })
  
  
  #------------------------------------------------------------------------------------------------------------------
  #try one more with geom smooth
  # output$trend2 = renderPlot({
  #   ggplot(
  #     nassau %>% filter(.,SD == input$SDselected),
  #     aes(x= Sold,y= SoldPrice)
  #   ) + geom_point() + geom_smooth(span = 0.01)
  #})
  
  #observeEvent(input$SDselected,{
  #  choices = sort(unique(nassau[nassau$SD == (input$SDselected), "Bedrooms"]))
  #  updateSelectizeInput(session,inputId = "bedroomsselected",choices = choices)
  #}) #filter choices based on input
  
  #observeEvent(input$bedroomsselected,{
  #  choices = unique(nassau[nassau$Bedrooms == (input$bedroomsselected), "DesignType"])
  #  updateSelectizeInput(session, inputId = 'typeselected',choices = append(choices,'All',after = 0))
  #})
  
 
  # dtype = reactive({input$typeselected})
  # 
  # output$trend = renderPlot({
  #   if (dtype() == 'All'){
  #     ggplot(nassau %>% group_by(.,SD,Sold,Bedrooms) %>% summarise(.,ave_price = mean(SoldPrice)) %>%
  #              filter(.,SD == (input$SDselected),Bedrooms == (input$bedroomsselected)), aes(x = Sold,y= ave_price))+
  #     geom_line()
  #   } else {
  #     ggplot(nassau %>% group_by(.,SD,Sold,Bedrooms,DesignType) %>% summarise(.,ave_price = mean(SoldPrice)) %>%
  #                          filter(.,SD == (input$SDselected),Bedrooms == (input$bedroomsselected),DesignType == (input$typeselected)), aes(x = Sold,y= ave_price))+
  #       geom_line()
  # 
  #   }
  # 
  # })
 #------------------------------------------------------------------------------------------------------------------------
  
  monthtype = reactive({input$trend_type})
  
  output$months = renderPlot({
    if(monthtype() == 'Average Price of Home Sold'){
      #price by month
      ggplot(nassau %>% group_by(.,Month) %>% summarise(.,month_ave_price = mean(SoldPrice)),aes(x = as.integer(Month),y = month_ave_price)) + 
        geom_bar(stat = 'identity') + scale_x_continuous(breaks = 1:12,
                                                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        ) + coord_cartesian(ylim = c(600000,800000))
    } else {
      #
      ggplot(nassau %>% group_by(.,Month) %>% summarise(.,month_contracts= n()),aes(x=as.integer(Month),y = month_contracts)) +
        geom_bar(stat = 'identity') + scale_x_continuous(breaks = 1:12,
                                                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        coord_cartesian(ylim = c(500,2700))
    }
  })
  
  #------------------------------------------------------------------------------------------------------------------------
  #plotDOM
  observeEvent(input$DOMyear,{
    if(input$DOMyear == 'All'){
      DOM_max_price = max(nassau$SoldPrice)
      DOM_min_price = min(nassau$SoldPrice)
    } else {
      DOM_max_price = max(nassau[nassau$Year == (input$DOMyear),"SoldPrice"])
      DOM_min_price = min(nassau[nassau$Year == (input$DOMyear),"SoldPrice"])
    }
    updateSliderInput(session, inputId = 'domslider', min = DOM_min_price, max = DOM_max_price, value = c(DOM_min_price,DOM_max_price))
  })
#-----------------------------------------------------------------------------------------------------------------
  domtype = reactive({input$DOMyear})
  
  #descending order graph
  output$DOM = renderPlot({
    if(domtype()== 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,ave_dom = mean(DaysOnMarket)) %>% top_n(.,15,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) + geom_bar(stat='identity')+
      coord_flip()
      #first condition
    } else {ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2], Year == input$DOMyear) %>% group_by(.,Town) %>% 
                     summarise(.,ave_dom=mean(DaysOnMarket)) %>% top_n(.,15,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) +geom_bar(stat='identity')+
        coord_flip()
      #seoncd
    }
  })

  #ascending order graph
  output$DOM_reverse = renderPlot({
    if(domtype()== 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,ave_dom = mean(DaysOnMarket)) %>% top_n(.,-15,ave_dom),aes(x=reorder(Town,-ave_dom),y=ave_dom)) + geom_bar(stat='identity')+
        coord_flip()
      #first condition
    } else {ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2], Year == input$DOMyear) %>% group_by(.,Town) %>% 
                     summarise(.,ave_dom=mean(DaysOnMarket)) %>% top_n(.,-15,ave_dom),aes(x=reorder(Town,-ave_dom),y=ave_dom)) +geom_bar(stat='identity')+
        coord_flip()
      #seoncd
    }
  })
  
  
  
  #scatterplot town 
  output$DOMscatter = renderPlotly({
    if(domtype() == 'All'){
      #condition 1
      ggplotly(
      ggplot(nassau %>% group_by(.,Town) %>% summarise(.,ave_sale = mean(SoldPrice),ave_dom = mean(DaysOnMarket)),aes(x= ave_dom,y= ave_sale, group = Town, text = paste(Town, '</br></br>','Days on Market:',
                                                                                                        round(ave_dom, digits = 0),'</br>', 'Sale Price:',
                                                                                                        round(ave_sale,digits = 0))))+
        geom_point()+
        geom_vline(xintercept = mean(nassau$DaysOnMarket)) + geom_hline(yintercept = mean(nassau$SoldPrice)), # color later
      tooltip = 'text'
      )
    } else {
      #condition 2
      ggplotly(
      ggplot(nassau %>% filter(.,Year == input$DOMyear) %>% group_by(.,Town) %>%  summarise(.,year_ave_sale= mean(SoldPrice),year_ave_dom = mean(DaysOnMarket)))+
             geom_point(aes(x=year_ave_dom, y = year_ave_sale, group = Town,
                             text = paste(
                               Town, '</br></br>','Days on Market:',
                               format(round(year_ave_dom, digits = 0),nsmall=1, big.mark = ','),'</br>', 'Sale Price:',
                               format(round(year_ave_sale,digits = 0),small=1, big.mark= ',')
                             ))) +
        geom_vline(xintercept = mean(nassau[nassau$Year == input$DOMyear,"DaysOnMarket"])) +
        geom_hline(yintercept = mean(nassau[nassau$Year== input$DOMyear,"SoldPrice"])) ,
      tooltip = 'text'
      )
    }
  })
  
  
  
  
  
  
  
  
  #plot4
  observeEvent(input$contract_year,{
    if(input$contract_year == 'All'){
      con_max_price = max(nassau$SoldPrice)
      con_min_price = min(nassau$SoldPrice)
    } else {
      con_max_price = max(nassau[nassau$Year == (input$contract_year),"SoldPrice"])
      con_min_price = min(nassau[nassau$Year == (input$contract_year),"SoldPrice"])
    }
    updateSliderInput(session, inputId = 'contractslider', min = con_min_price, max = con_max_price, value = c(con_min_price,con_max_price))
  })
  
  
  
  
    
  contracttype = reactive({input$contract_year})
  
  output$sales = renderPlot({
    if( contracttype() == 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,15,total_contracts),aes(x = reorder(Town,total_contracts),y= total_contracts))+
             geom_bar(stat = 'identity') + coord_flip()
      
    } else {
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2], Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,15,total_contracts),aes(x = reorder(Town,total_contracts),y= total_contracts))+
               geom_bar(stat = 'identity') + coord_flip()
    }})
  
  #
  output$sales_least_to_most = renderPlot({
    if( contracttype() == 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,-15,total_contracts),aes(x = reorder(Town,-total_contracts),y= total_contracts))+
        geom_bar(stat = 'identity') + coord_flip()
      
    } else {
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2], Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,-15,total_contracts),aes(x = reorder(Town,-total_contracts),y= total_contracts))+
        geom_bar(stat = 'identity') + coord_flip()
    }})
  
  
  #scatterplot 
  output$contracts_scatterplot= renderPlotly({
    if (contracttype() == 'All'){
      #all condition
      ggplotly(
       ggplot(nassau %>% group_by(.,Town) %>% summarise(.,total_sales = n(), ave_sale_price = mean(SoldPrice)), aes( 
         x = total_sales, y = ave_sale_price, group = Town, text = paste(
           'Town:', Town, '</br></br>', 'Total Sales:', format(total_sales,nsmall=1,big.mark = ','), '</br>', 'Average Sale Price', format(
             round(ave_sale_price,digits =0),nsmall =0, big.mark = ',')))) + 
        geom_point() + geom_vline(xintercept = nrow(nassau)/96) + geom_hline(yintercept = mean(nassau$SoldPrice)),
      tooltip = 'text'
      )
    } else {
      #year selected condition
      ggplotly(
      ggplot(nassau %>% filter(.,Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_sales_by_year = n(), ave_sale_price_by_year = mean(SoldPrice)),
             aes(x = total_sales_by_year, y = ave_sale_price_by_year, group = Town, text= paste(
               'Town:', Town, '</br></br>', 'Total Sales:', format(total_sales,nsmall=1,big.mark = ','), '</br>', 'Average Sale Price', format(
                 round(ave_sale_price,digits =0),nsmall =0, big.mark = ',')))) +
        geom_point() + geom_vline(xintercept = nrow(nassau[nassau$Year == input$contract_year,])/  length(unique(
          nassau[nassau$Year == input$contract_year,"Town"]))) + geom_hline(yintercept = mean(nassau[nassau$Year== input$contract_year,'SoldPrice'])),
      tooltip = 'text'
      )
    }
  })
  
    
  # output$contracts_scatterplot = renderPlot({
  #   
  # })
  
  
  
  #output$contracts_overtime= renderPlot({
  #  ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice<=input$contractslider[2],Town %in% top_town,
  #                           Year == c(2017,2018,2019)) %>% 
  #           group_by(.,Town,Year) %>% summarise(.,total_contracts = n()),aes(x= Year,y= total_contracts,color = Town))+
  #           geom_line()
  #})
    
    
    
  
  
  
  
  
  } #final bracket
    
    
  
  
  
  
  