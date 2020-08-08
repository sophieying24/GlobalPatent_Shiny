
dashboardPage(
    
    dashboardHeader(title = "World Patent"),
    dashboardSidebar(
        sidebarUserPanel("Qing Ying"),
        sidebarMenu(
            menuItem("World", tabName = "world", icon = icon("globe")),
            menuItem("Country", tabName = "country", icon = icon("flag")),
            menuItem("Tech Domain", tabName = "domain", icon = icon("robot")),
            menuItem("Data", tabName = "data", icon = icon("database"))
            )
        
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "world",
                    fluidRow(
                        column(
                        selectInput(
                            "world_year",
                            "Select Year:",
                            choices = unique(df$year),
                            selected = 2017),
                        offset = 0.5, width = 6
                        )),
                    fluidRow(
                        plotlyOutput("map"),
                        ),
                    br(),
                    fluidRow(
                        column(
                        box(plotlyOutput("domain_rank"), width = 6),
                        box(plotlyOutput("country_rank"), width = 4),
                        width = 11, offset = 0.5
                        )
                        )
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
                        tabPanel("Overview",
                                fluidRow(
                                    infoBoxOutput("box1"),
                                    infoBoxOutput("box2")),
                                fluidRow(
                                    box(plotlyOutput("country_domain"), width = 9)),
                                   
                                  
                                fluidRow(
                                    column(selectizeInput("country_year",
                                                    "Select Year:",
                                                    choices = unique(df$year),
                                                    selected = 2017, size = 2),
                                           offset = 0.5, width = 6)
                                    ),
                                fluidRow(
                                    box(plotlyOutput("graph1"), width = 9)
                                )),
                        tabPanel("Additional Insights",
                                 box(plotlyOutput("graph2")),
                                 box(plotlyOutput("graph3")),
                                 box(plotlyOutput("graph4")),
                                 box(plotlyOutput("graph5"))
            
                                 )
                    
                    )
                    
                    ),
            

###################### Domain 


            tabItem(tabName = "domain",
                    fluidRow(selectInput("domain",
                                         "Select Tech Domain:",
                                         choices = sort(unique(df$technology_domain)),
                                         selected = "Computer technology")),
                    ),

###################### Data

            tabItem(tabName = "data",
                    "to be replaced with database")
        )
        
    )
    
    
)






