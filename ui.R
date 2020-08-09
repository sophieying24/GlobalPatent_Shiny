
dashboardPage(
    
    dashboardHeader(title = "World Patent"),
    dashboardSidebar(
        sidebarUserPanel("Qing Ying"),
        sidebarMenu(
            menuItem("Welcome Page", tabName = "welcome", icon = icon("info")),
            menuItem("World", tabName = "world", icon = icon("globe")),
            menuItem("Country", tabName = "country", icon = icon("flag")),
            menuItem("Tech Domain", tabName = "domain", icon = icon("robot")),
            menuItem("Data", tabName = "data", icon = icon("database")),
            menuItem("About Me", tabName = "about", icon = icon("user"))
            )
        
    ),
    
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "welcome",

                    fluidRow((
                        column(
                        width = 8, 
                        # align = "center",
                        tags$h1("Explore Patent Trends in Technology around the World!"),
                        br(),
                        img(src = "https://images.unsplash.com/photo-1505664063603-28e48ca204eb?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=3300&q=80",
                            width = "500", height = "300")))),
                    
                    fluidRow(
                        column(
                        width = 6,
                        tags$h3("Introduction"),
                        tags$p("This interactive Shiny App is designed to help you explore recent trends in various technology domains across the world based on number of patents filed at each country. The patent data used for this app is published by the Organization of Economic Cooperation and Development (OECD)."),
                        br(),
                        tags$h4("World", style="text-decoration:underline"),
                        tags$p("Select a year for the analysis, hover over the map to see number of patents filed in a specific country in the selected year. Click on Leaderboard to see leading countries and industries in the selected year."),
                        br(),
                        tags$h4("Country", style="text-decoration:underline"),
                        tags$p("Select a country to see patent trends in various technology domains, ranking of domains in the selected country, and additional insights on other technology indicators such as GDP, foreign collaborations, and Gross Domestic/Higher Education Expenditure on R&D."),
                        br(),
                        tags$h4("Tech Domain", style="text-decoration:underline"),
                        tags$p("Select a technology domain to see patent trends in the leading countries for the selected domain and the ranking of countries for the selected domain in a specific year.")))
                    ),
            
            tabItem(tabName = "about",
                    box(title = "About Me",
                        br(),
                        tags$p("Qing (Sophie) Ying graduated from UC Berkeley with a Master's degree in Industrial Engineering and Operations Research. She has been a product manager in a healthcare technology startup for 3 years, where she developed various data analytics products to help healthcare organizations improve resource utilization. She's passionate about using data science to solve real-life problems."),
                        br(),
                        tags$b("Email: sophieying24@gmail.com"),
                        br(),
                        tags$p(tags$b("LinkedIn:",tags$a(href="https://www.linkedin.com/in/qingying/", target="_blank","https://www.linkedin.com/in/qingying/"))),
                        tags$p(tags$b("Github:",tags$a(href="https://github.com/sophieying24",target = "_blank","https://github.com/sophieying24"))))
                    ),
            
            tabItem(tabName = "world",
                    fluidRow(
                        column(
                        selectInput(
                            "world_year",
                            "Select Year:",
                            choices = unique(df$year),
                            selected = 2018),
                        offset = 0.5, width = 6)),
                    
                    tabsetPanel(
                        tabPanel("World Map",
                             fluidRow(
                                 infoBoxOutput("count_world"),
                                 infoBoxOutput("max_country"),
                                 infoBoxOutput("max_domain")
                                 ),
                            fluidRow(
                                plotlyOutput("map"))),
                        
                        tabPanel("Leaderboard",
                                 box(title = "Top 10 Countries by Total Patents Count", plotlyOutput("country_rank"), width = 6),
                                 box(title = "Rank of Tech Domains by Total Patents Count",plotlyOutput("domain_rank"), width = 6)))
                    ),
            
###################### Country  

            tabItem(tabName = "country",

                    fluidRow(
                        
                        column(selectInput("country",
                                            "Select Country:",
                                            choices = sort(inner_join(unique(info %>% select(country_code, country)),
                                                                      unique(df %>% select(location, country)), by = "country")$country),
                                            selected = "United States"),
                               offset = 0.5, width = 6)
                                ),
                    
                    tabsetPanel(
                        tabPanel("Trend",
                                fluidRow(
                                    infoBoxOutput("box1"),
                                    infoBoxOutput("box2")),
                                fluidRow(
                                    box(plotlyOutput("country_domain"), width = 12))
                                ),
                        tabPanel("Domain Rank",
                                 
                                 fluidRow(
                                     column(selectizeInput("country_year",
                                                           "Select Year:",
                                                           choices = unique(df$year),
                                                           selected = 2018, size = 2),
                                            offset = 0.5, width = 3)),
                                 fluidRow(
                                     infoBoxOutput("country_count")),
                                 fluidRow(
                                     box(plotlyOutput("graph1"), width = 12))),
                        tabPanel("Additional Insights",
                                 box(plotlyOutput("graph2")),
                                 box(plotlyOutput("graph3")),
                                 box(plotlyOutput("graph4")),
                                 box(plotlyOutput("graph5")))
                                )
                    
                    ),
            

###################### Domain 


            tabItem(tabName = "domain",
                    fluidRow(
                        column(selectInput("domain",
                                         "Select Tech Domain:",
                                         choices = sort(unique(df$technology_domain)),
                                         selected = "Computer technology"),
                               offset = 0.5, width = 6)),
                    
                    tabsetPanel(
                        tabPanel("Trend",
                                 fluidRow(
                                     infoBoxOutput("box3"),
                                     infoBoxOutput("box4")),
                                 fluidRow(
                                     box(plotlyOutput("graph6"), width = 12))),
                        tabPanel("Leaderboard",
                                 fluidRow(
                                     column(selectizeInput("domain_year",
                                                           "Select Year:",
                                                           choices = unique(df$year),
                                                           selected = 2018, size = 2),
                                            offset = 0.5, width = 3)),
                                 fluidRow(infoBoxOutput("domain_count")),
                                 fluidRow(box(plotlyOutput("graph7"), width = 12)))
                                )
                    ),

###################### Data

            tabItem(tabName = "data",
                    tabsetPanel(
                        tabPanel("Patent data",
                                 dataTableOutput("patent")),
                        tabPanel("Country data",
                                 dataTableOutput("info"))
                    )

                    )
        )
        
    )
    
    
)






