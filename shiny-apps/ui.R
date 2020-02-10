library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "Medical Note Analysis"),
    dashboardSidebar(
        sidebarMenu(
            # use only "list" instead of "glyphicon glyphicon-list"
            menuItem("Medical Notes", tabName = "medical_note", icon = icon("home")),
            menuItem("Medical Entities", tabName = "medical_entity", icon = icon("table")),
            menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
            menuItem("Clustering", tabName = "cluster", icon = icon("brain")),
            menuItem("House", tabName = "house", icon = icon("bar-chart"))
        )
    ),
    dashboardBody(
        # modify css  ==========================================================
        tags$head(tags$style(includeCSS("asset/custom.css"))),
        
        tabItems(
            # medical_note =========================================================
            tabItem(
                "medical_note",
                h1("Introduction to medical notes"),
                includeMarkdown("./Rmd/introduction_to_medical_notes.Rmd"),
                dataTableOutput("raw_table")
            ),
            
            # medical_entities =====================================================
            tabItem(
                "medical_entity",
                h1("Medical Entity Extraction"),
                p("focus on python in this section, combine wordcloud here, followed by table"),
                fluidRow(
                    column(
                        12,
                        textInput("word", 
                                  "",
                                  width = "415px",
                                  placeholder = "Input a word and see its stats"),
                        htmlOutput("word_stats")
                    )
                ),
                
                br(),
                
                fluidRow(
                    column(
                        12,
                        DT::dataTableOutput("bows")
                    )
                )
            ),
            # word cloud ======================================================
            tabItem(
                "wordcloud",
                fluidRow(
                    column(
                        6,
                        fluidRow(
                            column(
                                4,
                                selectInput("cloud_type_1", 
                                            "Select bag of words",
                                            choices = c("Both", 
                                                        "Gastroenterology", 
                                                        "Neurology"),
                                            selected = "Gastroenterology")
                            ),
                            column(
                                4,
                                selectInput("cloud_method_1",
                                            "Select bag of words",
                                            choices = c("amazon_me", 
                                                        "medacy_me", 
                                                        "top_tf", 
                                                        "top_tfidf"),
                                            selected = "top_tf")
                            )
                        ),
                        plotOutput("wordcloud_1", width = "90%", height = "400px"),
                        plotOutput("bar_1", width = "90%", height = "200px")
                    ),
                    column(
                        6,
                        fluidRow(
                            column(
                                4,
                                selectInput("cloud_type_2", 
                                            "Select sample type",
                                            choices = c("Both", 
                                                        "Gastroenterology", 
                                                        "Neurology"),
                                            selected = "Neurology")
                            ),
                            column(
                                4,
                                selectInput("cloud_method_2",
                                            "Select bag of words",
                                            choices = c("amazon_me", 
                                                        "medacy_me", 
                                                        "top_tf", 
                                                        "top_tfidf"),
                                            selected = "top_tf")
                            )
                        ),
                        plotOutput("wordcloud_2", width = "90%", height = "400px"),
                        plotOutput("bar_2", width = "90%", height = "200px")
                    )
                )
            ),
            tabItem(
                "cluster",
                h1("Identify Medical Subdomains"),
                fluidRow(
                    column(
                        8,
                        h2("kmeans")
                    ),
                    column(
                        4,
                        plotOutput("kmeans_img", height = "auto")
                    )
                ),
                plotOutput("dend")
            )
            
#---            
        )
    )
)