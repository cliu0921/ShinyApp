function(input, output,session) { 
  #Nassau County tab, graph 

  output$sales_and_ave_price_plot = renderPlot({
    ggplot(nassau %>% group_by(.,Sold) %>% summarise(.,plot1_ave_price = mean(SoldPrice), plot1_total_sales = n()) %>% 
           filter(.,Sold != as.Date('2017-03-01') & Sold != as.Date('2020-03-01')),aes(x=Sold)) +annotation_custom(g,xmin = -Inf,xmax = Inf, ymin=-Inf,ymax=Inf)+
    geom_smooth(aes(y= plot1_total_sales*1000),se = FALSE,span= 0.5, color = 'palegreen3') +
    geom_smooth(aes(y=plot1_ave_price),se= FALSE, span = 0.3, color = 'dodgerblue3') + 
    scale_y_continuous(
      name = "Average Price",
      sec.axis = sec_axis(~./1000, name = 'Total Sales'), labels = comma) + labs(x ='Year')+
    coord_cartesian(ylim = c(400000,900000)) + theme_minimal() + annotate(geom = 'text',y =770000,x= as.Date('2018-01-01'), label = 'Average Price of Home Sold') +
      annotate(geom= 'text',y=475000,x=as.Date('2018-01-01'), label = "Total Home Sales")
  })
  
  

  
  #---------------------------------------------------------------------------------------------------------------------
  #School District analysis tab
  #Trend graph of home price in school district
  output$trend = renderPlot({
    ggplot(
      nassau %>% group_by(.,SD,Year) %>% summarise(.,ave_sd_price = mean(SoldPrice)) %>% filter(.,SD== input$SDselected),
      aes(x= Year, y=ave_sd_price)
    ) + geom_smooth(se=FALSE, color = "dodgerblue3") + scale_y_continuous(labels = comma) + labs(x= 'Year', y = ' Average Price') +coord_cartesian(
      ylim = c(400000,1200000)) + theme_minimal() 
    
    
  })
  #Prices of home by bedrooms graph
  output$bedrooms_by_sd = renderPlotly({
    ggplotly(
    ggplot(
      nassau %>% group_by(.,SD,Town,Bedrooms_cat) %>% summarise(.,ave_room_price_town = mean(SoldPrice)) %>% 
        filter(.,SD == input$SDselected), 
      aes(x = Town, y = ave_room_price_town, group = Bedrooms_cat, text = paste('Town:', Town, '</br></br>',
                                                                                'Number of Rooms:', Bedrooms_cat,'</br>',
                                                                                'Average Sold Price:', 
                                                                                format(round(ave_room_price_town, digits = 0),
                                                                                            nsmall = 0, big.mark = ",")))) +
        geom_bar(aes(fill= Bedrooms_cat),position = 'dodge',stat = 'identity') + scale_y_continuous(labels = comma) +
      labs(y = 'Average Price', title = 'Average Sale Price of Homes Within District By Town') + 
      theme(legend.position = "bottom",legend.title = element_text('Bedrooms'),text = element_text(size = 7)) +
      scale_fill_brewer(palette = 'Blues') 
    , tooltip = 'text'
    ) %>% layout(legend = list(orientation = 'h', x = 0.3, y = -0.3)) #move legend to bottom of graph
  })
  
  


 #------------------------------------------------------------------------------------------------------------------------
  #Monthly trend tab, graph
  monthtype = reactive({input$trend_type})
  
  output$months = renderPlot({
    if(monthtype() == 'Average Price of Home Sold'){
      #graph of home sales prices
      ggplot(nassau %>% group_by(.,Month) %>% summarise(.,month_ave_price = mean(SoldPrice)),aes(x = as.integer(Month),y = month_ave_price)) + 
        geom_bar(stat = 'identity', fill = 'palegreen3') + scale_x_continuous(breaks = 1:12,
                                                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        ) + coord_cartesian(ylim = c(600000,800000)) + scale_y_continuous(labels = comma) +labs( x = "Month", y = "Average Price") +theme_minimal()
    } else {
      #graph of total sales done 
      ggplot(nassau %>% group_by(.,Month) %>% summarise(.,month_contracts= n()),aes(x=as.integer(Month),y = month_contracts)) +
        geom_bar(stat = 'identity', fill = 'palegreen3') + scale_x_continuous(breaks = 1:12,
                                                         labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
        coord_cartesian(ylim = c(500,2700))+ scale_y_continuous(labels = comma) + labs( x= 'Month', y = 'Total Sales') +theme_minimal()
    }
  })
  
  #------------------------------------------------------------------------------------------------------------------------
  #DOM tab
  #tailor the min and max of slider to the selected year
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

  
  
  domtype = reactive({input$DOMyear})
  
  #descending graph of DOM
  output$DOM = renderPlot({
    if(domtype()== 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,ave_dom = mean(DaysOnMarket)) %>% top_n(.,15,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) + geom_bar(stat='identity', fill = 'palegreen3')+
      coord_flip() +labs(x='Town', y= 'Average DOM', title = 'Most Days on Market') + theme_minimal()
      
      #graph of all years summarised
      
    } else {ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2], Year == input$DOMyear) %>% group_by(.,Town) %>% 
                     summarise(.,ave_dom=mean(DaysOnMarket)) %>% top_n(.,15,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) +geom_bar(stat='identity', fill = 'palegreen3')+
        coord_flip() +labs(x='Town', y= 'Average DOM', title = 'Most Days on Market') +theme_minimal()
      
      #graph of sepecific year summarised
    }
  })

  #ascending DOM graph
  output$DOM_reverse = renderPlot({
    if(domtype()== 'All'){
      
      #graph if all years selected
      
      ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,ave_dom = mean(DaysOnMarket)) %>% top_n(.,-15,ave_dom),aes(x=reorder(Town,-ave_dom),y=ave_dom)) + geom_bar(stat='identity', fill = 'coral3')+
        coord_flip()+labs(x='Town', y= 'Average DOM', title = 'Least Days on Market') + theme_minimal()
      
    } else {ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2], Year == input$DOMyear) %>% group_by(.,Town) %>% 
        #graph of specific year              
                     summarise(.,ave_dom=mean(DaysOnMarket)) %>% top_n(.,-15,ave_dom),aes(x=reorder(Town,-ave_dom),y=ave_dom)) +geom_bar(stat='identity', fill = 'coral3')+
        coord_flip()+labs(x='Town', y= 'Average DOM', title = 'Least Days on Market') +theme_minimal()
      
    }
  })
  
  
  
  #scatterplot of Towns 
  output$DOMscatter = renderPlotly({
    if(domtype() == 'All'){
      #conditional ALL
      ggplotly(
      ggplot(nassau %>% group_by(.,Town) %>% summarise(.,ave_sale = mean(SoldPrice),ave_dom = mean(DaysOnMarket)),aes(x= ave_dom,y= ave_sale, group = Town, text = paste(Town, '</br></br>','Days on Market:',
                                                                                                        format(round(ave_dom, digits = 0),nsmall = 0,big.mark = ','),'</br>', 'Average Sale Price:',
                                                                                                        format(round(ave_sale,digits = 0), nsmall = 0, big.mark = ','))))+
        geom_point(color = 'dodgerblue3')+
        geom_vline(xintercept = mean(nassau$DaysOnMarket)) + geom_hline(yintercept = mean(nassau$SoldPrice))+ scale_y_continuous(labels = comma) +
        labs(x= 'Average Days on Market', y ='Average Price')+ theme_minimal(), 
      tooltip = 'text'
      )
    } else {
      #conditional Year
      ggplotly(
      ggplot(nassau %>% filter(.,Year == input$DOMyear) %>% group_by(.,Town) %>%  summarise(.,year_ave_sale= mean(SoldPrice),year_ave_dom = mean(DaysOnMarket)))+
             geom_point(aes(x=year_ave_dom, y = year_ave_sale, group = Town,
                             text = paste(
                               Town, '</br></br>','Days on Market:',
                               format(round(year_ave_dom, digits = 0),nsmall=0, big.mark = ','),'</br>', 'Average Sale Price:',
                               format(round(year_ave_sale,digits = 0),nsmall=0, big.mark= ',')
                             )),color = 'dodgerblue3') +
        geom_vline(xintercept = mean(nassau[nassau$Year == input$DOMyear,"DaysOnMarket"])) +
        geom_hline(yintercept = mean(nassau[nassau$Year== input$DOMyear,"SoldPrice"]))+ scale_y_continuous(labels = comma) +
        labs(x= 'Average Days on Market', y ='Average Price')+ theme_minimal(),
      tooltip = 'text'
      )
    }
  })
  
#----------------------------------------------------------------------------------------------------------------------------------------
  #total sales tab
  #adjust min max according to year selection

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
  
  
  #descending graph total sales
  output$sales = renderPlot({
    if( contracttype() == 'All'){
      
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,15,total_contracts),aes(x = reorder(Town,total_contracts),y= total_contracts))+
             geom_bar(stat = 'identity', fill = 'palegreen3') + coord_flip()+ scale_y_continuous(labels = comma) +labs(x = "Town", y='Total Sales', title =
                                                                                                                         'Most Total Sales') + theme_minimal()
      
    } else {
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2], Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,15,total_contracts),aes(x = reorder(Town,total_contracts),y= total_contracts))+
               geom_bar(stat = 'identity', fill = 'palegreen3') + coord_flip()+ scale_y_continuous(labels = comma)+ labs(x = "Town", y='Total Sales', title= 'Most Total Sales')+ 
                                                                                                                         theme_minimal()
    }})
  
  #ascending graph total sales
  
  output$sales_least_to_most = renderPlot({
    if( contracttype() == 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,-15,total_contracts),aes(x = reorder(Town,-total_contracts),y= total_contracts))+
        geom_bar(stat = 'identity', fill = 'coral3') + coord_flip() + scale_y_continuous(labels = comma)+labs(x = "Town", y='Total Sales', title = 'Least Total Sales') +theme_minimal()
      
    } else {
      ggplot(nassau %>% filter(.,SoldPrice >= input$contractslider[1] & SoldPrice <= input$contractslider[2], Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_contracts = n()) %>% top_n(.,-15,total_contracts),aes(x = reorder(Town,-total_contracts),y= total_contracts))+
        geom_bar(stat = 'identity', fill = 'coral3') + coord_flip()+ scale_y_continuous(labels = comma)+labs(x = "Town", y='Total Sales', title = "Least Total Sales") + theme_minimal()
    }})
  
  
  #scatterplot of Towns
  output$contracts_scatterplot= renderPlotly({
    if (contracttype() == 'All'){
      #all condition
      ggplotly(
       ggplot(nassau %>% group_by(.,Town) %>% summarise(.,total_sales = n(), ave_sale_price = mean(SoldPrice)), aes( 
         x = total_sales, y = ave_sale_price, group = Town, text = paste(
           'Town:', Town, '</br></br>', 'Total Sales:', format(total_sales,nsmall=1,big.mark = ','), '</br>', 'Average Sale Price', format(
             round(ave_sale_price,digits =0),nsmall =0, big.mark = ',')))) + 
        geom_point(color = 'dodgerblue3') + geom_vline(xintercept = nrow(nassau)/96) + geom_hline(yintercept = mean(nassau$SoldPrice))+
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + labs(x = "Total Sales", y='Average Price') +
         theme_minimal(),
      tooltip = 'text'
      )
    } else {
      #year selected condition
      ggplotly(
      ggplot(nassau %>% filter(.,Year == input$contract_year) %>% group_by(.,Town) %>% 
               summarise(.,total_sales_by_year = n(), ave_sale_price_by_year = mean(SoldPrice)),
             aes(x = total_sales_by_year, y = ave_sale_price_by_year, group = Town, text= paste(
               'Town:', Town, '</br></br>', 'Total Sales:', format(total_sales_by_year,nsmall=1,big.mark = ','), '</br>', 'Average Sale Price', format(
                 round(ave_sale_price_by_year,digits =0),nsmall =0, big.mark = ',')))) +
        geom_point(color = 'dodgerblue3') + geom_vline(xintercept = nrow(nassau[nassau$Year == input$contract_year,])/  length(unique(
          nassau[nassau$Year == input$contract_year,"Town"]))) + geom_hline(yintercept = mean(nassau[nassau$Year== input$contract_year,'SoldPrice']))+
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma)+labs(x = "Total Sales", y='Average Price') +
        theme_minimal(),
      tooltip = 'text'
      )
    }
  })
  

    

  
  
  
  } #final bracket
    
    
  
  
  
  
  