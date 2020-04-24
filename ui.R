

dashboardPage(
  dashboardHeader(title = 'Nassau Home Sales'),#figure out how to write title onto next line if time available
  dashboardSidebar(
    #sidebar tabs for diff graph navigation
    sidebarMenu(
      menuItem("Town Overview/Snapshot",tabName = 'overview'), #add icon
      menuItem('Yearly Trend',tabName = 'trend'), #add icon late
      menuItem('Days On Market', tabName = "days on market" ), #add icon later
      menuItem('Yearly Sales', tabName = 'annual sales') #add icon late
    )
  ),
  dashboardBody(
    #content for each tab
    tabItems(
      #firstpage
      tabItem(tabName = 'overview',
              h2('TBA')),
      
      
      
      #2page
      tabItem(tabName = 'trend',
              h2("TBA"),
              fluidRow( #ontop of eachotehr rather than side wrap
                box(plotOutput('trend',height = 250)),
                #make selected options NULL/ blank if time permits
                box(title = 'Town',
                    selectizeInput('townselected','Select Town', choices = town_names)),
                box(title = 'Number of Bedrooms',
                    selectizeInput('bedroomsselected','Select Number of Bedrooms',choices = number_bedrooms)),
                box(title = 'Design Type',
                    selectizeInput('typeselected','Select Design Type', choices = design)) #potentially merge 3 options into 1 box
                
              ))
      
      
      
      
      
      )
  )
)