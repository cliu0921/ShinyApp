

dashboardPage(
  dashboardHeader(title = 'Nassau Home Sales'),#figure out how to write title onto next line if time available
  dashboardSidebar(
    #sidebar tabs for diff graph navigation
    sidebarMenu(
      menuItem("Town Overview/Snapshot",tabName = 'overview'), #add icon
      menuItem('Monthly Trend',tabName = 'monthly_trend'),
      menuItem('Yearly Trend',tabName = 'trend'), #add icon late
      menuItem('Days On Market', tabName = 'DOM'), #add icon later
      menuItem('Yearly Sales', tabName = 'sales') #add icon late
    )
  ),
  dashboardBody(
    #content for each tab
    tabItems(
      #firstpage
      tabItem(tabName = 'overview',
              h2('TBA')),
              
      #seasonal tab
      tabItem(tabName = 'monthly_trend',
              h2('TBA'),
              fluidRow(
                box(title= 'Monthly Analysis',
                    selectizeInput('trend_type','Select Analysis Parameter', choices = contracts_saleprice)),
                box(plotOutput('months',height = 250))
        
              )),
      
      #2page
      tabItem(tabName = 'trend',
              h2("TBA"), #chnage title eventually
              
              
              fluidRow( #ontop of eachotehr rather than side wrap
                box(plotOutput('trend',height = 250)),
                #make selected options NULL/ blank if time permits
                box(title = 'School District',
                    selectizeInput('SDselected','Select School District', choices = disctrict_names)),
                box(plotlyOutput('bedrooms_by_sd',height = 250, width = 600)))),
                    
              
                #unused for now
                #box(title = 'Number of Bedrooms',
                #    selectizeInput('bedroomsselected','Select Number of Bedrooms',choices = number_bedrooms)),
                #unused for now
                #box(title = 'Design Type',
                #    selectizeInput('typeselected','Select Design Type', choices = design)))), #potentially merge 3 options into 1 box

      #3page
      tabItem(tabName = 'DOM',
              h2('TBA'),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('DOMyear','Select Year', choices = yearsDOM)),
                box(title = 'Price Range',
                    sliderInput('domslider','Price Range of Home Sales:', min = minprice,max = maxprice,value = c(min, max))),
                box(plotOutput('DOM',height = 250)),
                box(plotOutput('DOM_reverse',height = 250)),
                box(plotlyOutput('DOMscatter',height = 400))
              )
            ),
      #4page
      tabItem(tabName = 'sales',
              h2('TBA'),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('contract_year','Select Year', choices = yearscontract)),
                box(title = 'Price Range',
                    sliderInput('contractslider','Price Range of Home Sales:', min = minprice,max = maxprice,value = c(min, max))),
                box(plotOutput('sales',height = 250)),
                box(plotOutput('contracts_overtime', height=400))
              )
      )
    )
  )
)

