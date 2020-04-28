

dashboardPage(
  dashboardHeader(title = 'Nassau Home Sales'),
  dashboardSidebar(
    #sidebar tabs for diff graph navigation
    sidebarMenu(
      menuItem("Nassau County",tabName = 'overview'), 
      menuItem('Monthly Trend',tabName = 'monthly_trend'),
      menuItem('Breakdown by School District',tabName = 'SD'), 
      menuItem('Days On Market Analysis', tabName = 'DOM'), 
      menuItem('Annual Sales Analysis', tabName = 'sales') 
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    #content for each tab
    tabItems(
      #nassau tab
      tabItem(tabName = 'overview',
              h2('Nassau County Home Sales'),
              fluidRow(
                box(plotOutput('sales_and_ave_price_plot'),width = 12)
              )
              ),
              
      #monthly trend
      tabItem(tabName = 'monthly_trend',
              
                fluidRow(
                  
                  column(width = 12,
                    box(title= 'Monthly Analysis',
                        selectizeInput('trend_type','Select Parameter', choices = contracts_saleprice),
                        width = NULL, status = 'success', solidHeader = TRUE),
                    box(plotOutput('months'), width = NULL)
                  )
                )
              ),
      
      #school district
      tabItem(tabName = 'SD',
              h2("School District Analysis"), 
              fluidRow( 
                box(title = 'School District',
                    selectizeInput('SDselected','Select', choices = disctrict_names),
                    solidHeader = TRUE, status = 'primary'),
                box(plotOutput('trend',height = 250)),
                #make selected options NULL/ blank if time permits
                box(plotlyOutput('bedrooms_by_sd',height = 400),width=12
                    ))),
                    
              
                

      #DOM
      tabItem(tabName = 'DOM',
              h2('Days on Market Analysis '),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('DOMyear','Select', choices = yearsDOM),solidHeader = TRUE, status = 'primary'),
                box(title = 'Price Range',
                    sliderInput('domslider','Min and Max:', min = minprice,max = maxprice,value = c(min, max)),
                    solidHeader = TRUE, status = 'primary'),
                box(plotOutput('DOM',height = 250)),
                box(plotOutput('DOM_reverse',height = 250)),
                box(plotlyOutput('DOMscatter'),width = 12,
                    solidHeader = TRUE)
              )
            ),
      #total sales
      tabItem(tabName = 'sales',
              h2('Total Sales Analysis'),
              fluidRow(
                box(title = 'Year',
                    selectizeInput('contract_year','Select', choices = yearscontract), solidHeader = TRUE,
                    status = 'primary'),
                box(title = 'Price Range',
                    sliderInput('contractslider','Min and Max:', min = minprice,max = maxprice,value = c(min, max)),
                    solidHeader = TRUE,status = 'primary'),
                box(plotOutput('sales',height = 250)),
                box(plotOutput('sales_least_to_most', height=250)),
                box(plotlyOutput('contracts_scatterplot',height=400),width = 12,
                    solidHeader = TRUE),
              )
      )
    )
  )
)

