
    
dashboardPage(
    dashboardHeader(title = "World Patent"),
    dashboardSidebar(
        sidebarUserPanel("Qing Ying"),
        sidebarMenu(
            menuItem("World", tabName = "world", icon = icon("globe")),
            menuItem("Country", tabName = "country", icon = icon("flag")),
            menuItem("Data", tabName = "data", icon = icon("database")))
        
        # sliderInput("world_year",
        #             "Select Year:",
        #             min = 1999,
        #             max = 2017,
        #             value = 2017,
        #             sep = "",
        #             step = 1)
        
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "world",
                    fluidRow(
                        sliderInput("world_year",
                                    "Select Year:",
                                    min = 1999,
                                    max = 2017,
                                    value = 2017,
                                    sep = "",
                                    step = 1)),
                    fluidRow(
                        box(plotlyOutput("map")),
                        box(plotlyOutput("collab"))),
                    fluidRow(
                        box(plotlyOutput("country_rank")),
                        box(plotlyOutput("domain_rank")))),
            
###################### Country  

            tabItem(tabName = "country",
                    fluidRow(
                        selectInput("country",
                                    "Select Country:",
                                    choices = sort(inner_join(unique(info %>% select(country_code, country)),unique(df %>% select(location, country)), by = "country")$country),
                                    selected = "United States")
                    ),
                    fluidRow(
                        infoBoxOutput("box1"),
                        infoBoxOutput("box2")
                    ),
                    fluidRow(
                        box(plotlyOutput("country_domain"), width = 12),
                        box(plotlyOutput("graph1"), width = 9),
                        box(selectInput("country_year",
                                        "Select Year:",
                                        choices = unique(df$year),
                                        selected = 2017), width = 2)
                    )),
            

###################### Data

            tabItem(tabName = "data",
                    "to be replaced with database")
        )
        
    )
    
    
)



# 
# # Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("World Patent"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )
