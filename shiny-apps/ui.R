library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "Medical Note Analysis"),
    dashboardSidebar(
        sidebarMenu(
            # use only "list" instead of "glyphicon glyphicon-list"
            menuItem("Overview", tabName = "overview", icon = icon("home")),
            menuItem("Clinical Notes", tabName = "clinical_note", icon = icon("table")),
            menuItem("Medical Named Entities", tabName = "medical_entity", icon = icon("table")),
            menuItem("Clustering", tabName = "cluster", icon = icon("brain")),
            menuItem("Binary Classification", tabName = "binary", icon = icon("brain"))
        )
    ),
    dashboardBody(
        # modify css  ==========================================================
        tags$head(tags$style(includeCSS("asset/custom.css"))),
        
        tabItems(
            # overview
            tabItem(
                "overview",
                includeMarkdown("Rmd/overview.Rmd")
            ),
            
            # clinical note ====================================================
            tabItem(
                "clinical_note",
                h1("Introduction to Clinical Notes"),
                tabsetPanel(
                    # .. clinical notes ====
                    tabPanel(
                        "What is clinical notes", 
                        includeMarkdown("./Rmd/introduction_to_clinical_notes.Rmd")
                    ),
                    
                    # .. mtsamples ====
                    tabPanel(
                        "Clincal notes at mtsamples.com", 
                        fluidPage(
                            column(
                                6,
                                includeMarkdown("./Rmd/mtsamples.Rmd"),
                                style = "padding-left: 0;"
                            ),
                            column(
                                6,
                                plotOutput("section_count"),
                                style = "padding-right: 0;"
                            )
                        ),
                        p("In the table below, we list one example note for each category."),
                        dataTableOutput("raw_table")
                    )
                )
            ),
            
            # medical_entities =====================================================
            tabItem(
                "medical_entity",
                h1("Medical Named Entities"),
                tabsetPanel(
                    # .. extraction ====
                    tabPanel(
                        "Extraction",
                        includeMarkdown("Rmd/medical_named_entity.Rmd"),
                        textInput("word", 
                                  "",
                                  width = "415px",
                                  placeholder = "Input a word and see its stats"),
                        htmlOutput("word_stats"),
                        
                        br(),
                        DT::dataTableOutput("bows")
                    ),
                    
                    # .. wordcloud ====
                    tabPanel(
                        "Wordcloud",
                        includeMarkdown("Rmd/wordcloud.Rmd"),
                        br(),
                        
                        fluidRow(
                            # word cloud 1
                            column(
                                6,
                                fluidRow(
                                    column(1),
                                    column(
                                        4,
                                        selectInput("cloud_type_1", 
                                                    "Select specialty",
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
                                                    selected = "amazon_me")
                                    )
                                ),
                                plotOutput("wordcloud_1", width = "90%", height = "300px"),
                                plotOutput("bar_1", width = "90%", height = "200px")
                            ),
                            
                            # word cloud 2
                            column(
                                6,
                                fluidRow(
                                    column(1),
                                    column(
                                        4,
                                        selectInput("cloud_type_2", 
                                                    "Select specialty",
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
                                                    selected = "amzone_me")
                                    )
                                ),
                                plotOutput("wordcloud_2", width = "90%", height = "300px"),
                                plotOutput("bar_2", width = "90%", height = "200px")
                            )
                        )
                    )
                ),
                
                
            ),
            
            # clustering ======================================================
            tabItem(
                "cluster",
                h1("Identify Medical Subdomains"),
                
                tabsetPanel(
                    # .. kmeans ====
                    tabPanel(
                        "kmeans",
                        fluidRow(
                            column(
                                8,
                                includeMarkdown("Rmd/kmeans.Rmd")
                            ),
                            column(
                                4,
                                plotOutput("kmeans_img", height = "400px")
                            )
                        )
                    ),
                    # .. hcluster ====
                    tabPanel(
                        "hclustering",
                        plotOutput("dend")
                    )
                )
            )
            
            # binary classification ===========================================

                        
#---            
        )
    )
)