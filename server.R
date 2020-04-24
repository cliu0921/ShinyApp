function(input, output,session) { 
  #plot1
  
  
  
  #plot trend
  observeEvent(input$townselected,{
    choices = sort(unique(nassau[nassau$Town == (input$townselected), "Bedrooms"]))
    updateSelectizeInput(session,inputId = "bedroomsselected",choices = choices)
  }) #filter choices based on input
  
  observeEvent(input$bedroomsselected,{
    choices = unique(nassau[nassau$Bedrooms == (input$bedroomsselected), "DesignType"])
    updateSelectizeInput(session, inputId = 'typeselected',choices = choices)
  })
  
  
  #trend_df= nassau %>% filter(.,)
  output$trend = renderPlot({
    ggplot(nassau %>% group_by(.,Town,Sold) %>% summarise(mean(SoldPrice)) ,aes(Sold,))
    
    
    
    
    })
    
    
  }
    
    
  
  
  
  
  