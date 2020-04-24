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

  
  
  #trend_df= nassau %>% filter(.,)
  #observeEvent(
  #output$trend = ifelse((input$typeselected) == 'All',
  #  renderPlot({ggplot(nassau %>% group_by(.,Town,Sold,Bedrooms) %>% summarise(.,ave_price = mean(SoldPrice)) %>% filter(.,Town == (input$townselected),
  #                      aes(x = Sold,y= ave_price)) + geom_bar(stat= 'identity'))
  #    
  #  }),
  #  
  #  renderPlot({
  #         ggplot(nassau %>% group_by(.,Town,Sold,Bedrooms,DesignType) %>% summarise(.,ave_price = mean(SoldPrice)) 
  #                %>% filter(.,Town == (input$townselected),Bedrooms == (input$bedroomsselected),DesignType == (input$typeselected)),
  #                aes(x = Sold, y = ave_price)) + geom_bar(stat= 'identity')})
  #  ))
    
    
  } #final bracket
    
    
  
  
  
  
  