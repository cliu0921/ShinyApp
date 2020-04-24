function(input, output,session) { 
  #plot1
  
  
  
  #plot trend
  observeEvent(input$townselected,{
    choices = sort(unique(nassau[nassau$Town == (input$townselected), "Bedrooms"]))
    updateSelectizeInput(session,inputId = "bedroomsselected",choices = choices)
  }) #filter choices based on input
  
  observeEvent(input$bedroomsselected,{
    choices = unique(nassau[nassau$Bedrooms == (input$bedroomsselected), "DesignType"])
    updateSelectizeInput(session, inputId = 'typeselected',choices = append(choices,'All',after = 0))
  })
  
 
  dtype = reactive({input$typeselected})
  
  output$trend = renderPlot({
    if (dtype() == 'All'){
      ggplot(nassau %>% group_by(.,Town,Sold,Bedrooms) %>% summarise(.,ave_price = mean(SoldPrice)) %>% 
               filter(.,Town == (input$townselected),Bedrooms == (input$bedroomsselected)), aes(x = Sold,y= ave_price))+
      geom_line()
    } else {
      ggplot(nassau %>% group_by(.,Town,Sold,Bedrooms,DesignType) %>% summarise(.,ave_price = mean(SoldPrice)) %>% 
                           filter(.,Town == (input$townselected),Bedrooms == (input$bedroomsselected),DesignType == (input$typeselected)), aes(x = Sold,y= ave_price))+
        geom_line()
      
    }
    
  })

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

  domtype = reactive({input$DOMyear})
  
  output$DOM = renderPlot({
    if(domtype()== 'All'){
      ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2]) %>% group_by(.,Town) %>% 
               summarise(.,ave_dom = mean(DaysOnMarket)) %>% top_n(.,10,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) + geom_bar(stat='identity')+
      coord_flip()
      #first condition
    } else {ggplot(nassau %>% filter(.,SoldPrice >= input$domslider[1] & SoldPrice <= input$domslider[2], Year == input$DOMyear) %>% group_by(.,Town) %>% 
                     summarise(.,ave_dom=mean(DaysOnMarket)) %>% top_n(.,10,ave_dom),aes(x=reorder(Town,ave_dom),y=ave_dom)) +geom_bar(stat='identity')+
        coord_flip()
      #seoncd
    }
  })

    
    
  } #final bracket
    
    
  
  
  
  
  