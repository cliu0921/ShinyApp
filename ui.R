

dashboardPage(
  dashboardHeader(title = 'Nassau Home Sales'),#figure out how to write title onto next line if time available
  dashboardSidebar(
    #sidebar tabs for diff graph navigation
    sidebarMenu(
      menuItem("Nassau County Home Sales",tabName = 'overview'), #add icon
      menuItem('Monthly Trend',tabName = 'monthly_trend'),
      menuItem('Breakdown by School District',tabName = 'SD'), #add icon late
      menuItem('Days On Market Analysis', tabName = 'DOM'), #add icon later
      menuItem('Annual Sales Analysis', tabName = 'sales') #add icon late
    )
  ),
  dashboardBody(
    #content for each tab
    tabItems(
      #firstpage
      tabItem(tabName = 'overview',
              h2('Nassau County Home Sales'),
              fluidRow(
                box(plotOutput('sales_and_ave_price_plot'), title = 'Average Home Price and Total Sales',width = 12, solidHeader = TRUE, status = 'primary')
              )
              ),
              
      #seasonal tab
      tabItem(tabName = 'monthly_trend',
              fluidRow(
                column(width = 12,
                  box(title= 'Monthly Analysis',
                      selectizeInput('trend_type','Select Parameter', choices = contracts_saleprice),
                      width = NULL,
                      solidHeader = TRUE, status = 'primary'),
                  box(plotOutput('months'), width = NULL, status = 'warning',solidHeader = TRUE)
                )
              )),
      
      #2page
      tabItem(tabName = 'SD',
              h2("School District Analysis"), 
              fluidRow( 
                box(title = 'School District',
                    selectizeInput('SDselected','Select School District', choices = disctrict_names),
                    solidHeader = TRUE, status = 'primary'),
                box(plotOutput('trend',height = 250),status= 'primary',solidHeader = TRUE),
                #make selected options NULL/ blank if time permits
                box(plotlyOutput('bedrooms_by_sd',height = 250),width=12, title = 'Average Sale Price of Homes Within District',
                    solidHeader = TRUE, status = 'warning'))),
                    
              
                #unused for now
                #box(title = 'Number of Bedrooms',
                #    selectizeInput('bedroomsselected','Select Number of Bedrooms',choices = number_bedrooms)),
                #unused for now
                #box(title = 'Design Type',
                #    selectizeInput('typeselected','Select Design Type', choices = design)))), #potentially merge 3 options into 1 box

      #3page
      tabItem(tabName = 'DOM',
              h2('Days on Market (DOM) Analysis '),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('DOMyear','Select Year', choices = yearsDOM),solidHeader = TRUE, status = 'primary'),
                box(title = 'Price Range',
                    sliderInput('domslider','Price Range of Home Sales:', min = minprice,max = maxprice,value = c(min, max)),
                    solidHeader = TRUE, status = 'primary'),
                box(plotOutput('DOM',height = 250),status = 'warning',title = 'Most DOM',solidHeader = TRUE),
                box(plotOutput('DOM_reverse',height = 250),status ='warning', title= 'Least DOM',solidHeader = TRUE),
                box(plotlyOutput('DOMscatter',height = 400),title = 'Average Sale Price by Average Days on Market',width = 12,status = 'success',
                    solidHeader = TRUE)
              )
            ),
      #4page
      tabItem(tabName = 'sales',
              h2('Total Sales Analysis'),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('contract_year','Select Year', choices = yearscontract), solidHeader = TRUE,
                    status = 'primary'),
                box(title = 'Price Range',
                    sliderInput('contractslider','Price Range of Home Sales:', min = minprice,max = maxprice,value = c(min, max)),
                    solidHeader = TRUE,status = 'primary'),
                box(plotOutput('sales',height = 250),status = 'warning', title= 'Most Sales',solidHeader = TRUE),
                box(plotOutput('sales_least_to_most', height=250), status= 'warning',title= 'Least Sales ',solidHeader = TRUE),
                box(plotlyOutput('contracts_scatterplot',height=400),title= 'Average Sale Price by Total Number of Sales',width = 12, status= 'success',
                    solidHeader = TRUE),
              )
      )
    )
  )
)

