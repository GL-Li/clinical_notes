library(shiny)
library(shinydashboard)

dashboardPage(
    dashboardHeader(title = "Clinical Note Analysis"),
    dashboardSidebar(
        sidebarMenu(
            # sidebar menu =====================================================
            menuItem("Overview", 
                     tabName = "overview", 
                     icon = icon("home")),
            menuItem("Clinical Notes", 
                     tabName = "clinical_note", 
                     icon = icon("table")),
            menuItem("Medical Named Entities", 
                     tabName = "medical_entity", 
                     icon = icon("table")),
            menuItem("Clustering", 
                     tabName = "cluster", 
                     icon = icon("brain")),
            menuItem("Classification", 
                     tabName = "classification", 
                     icon = icon("brain"))
        )
    ),
    dashboardBody(
        # modify css  ==========================================================
        tags$head(tags$style(includeCSS("asset/custom.css"))),
        
        tabItems(
            # overview ====
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
                        includeMarkdown("./Rmd/clinical_note_introduction.Rmd"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. mtsamples ====
                    tabPanel(
                        "Clincal notes at mtsamples.com", 
                        includeMarkdown("./Rmd/clinical_note_mtsamples.Rmd"),
                        br(),
                        
                        fluidPage(
                            column(
                                6,
                                plotOutput("specialty_count"),
                                style = "padding-left: 0;"
                            ),
                            column(
                                6,
                                plotOutput("section_count"),
                                style = "padding-right: 0;"
                            )
                        ),
                        
                        br(),
                        h3("Clinical note examples"),
                        p("In the table below, we list one example note for each 
                          category, as well as the section headers for each note."),
                        DT::dataTableOutput("raw_table"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
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
                        includeMarkdown("Rmd/medical_named_entity_extraction.Rmd"),
                        textInput("word", 
                                  "Search word in the table",
                                  width = "415px",
                                  placeholder = "Input a word to see its stats and highlight in the table"),
                        htmlOutput("word_stats"),
                        
                        br(),
                        DT::dataTableOutput("bows"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. wordcloud ====
                    tabPanel(
                        "Wordcloud",
                        includeMarkdown("Rmd/medical_named_entity_wordcloud.Rmd"),
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
                                                    choices = c("All", 
                                                                "Gastroenterology", 
                                                                "Neurology",
                                                                "Urology"),
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
                                                    selected = "top_tfidf")
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
                                                    choices = c("All", 
                                                                "Gastroenterology", 
                                                                "Neurology",
                                                                "Urology"),
                                                    selected = "Gastroenterology")
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
                        ),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    )
                )
                
                
            ),
            
            # clustering ======================================================
            tabItem(
                "cluster",
                h1("Identify Medical Subdomains"),
                includeMarkdown("Rmd/clustering_intro.Rmd"),
                
                tabsetPanel(
                    # .. pca ====
                    tabPanel(
                        "Principle Component",
                        includeMarkdown("Rmd/clustering_pca.Rmd"),
                        br(),
                        radioButtons("pca",
                                     label = "Select corpus",
                                     choices = c("clinical notes",
                                                 "amazon medical entities"),
                                     # selected = "amazon medical entities",
                                     inline = TRUE),
                        plotOutput("pca_plot", height = "800px"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. hcluster ====
                    tabPanel(
                        "hierarchical Clustering",
                        includeMarkdown("Rmd/clustering_hcluster.Rmd"),
                        br(),
                        
                        radioButtons("dend",
                                     label = "Select corpus",
                                     choices = c("clinical notes",
                                                 "amazon medical entities"),
                                     inline = TRUE),
                        plotOutput("dend_plot"),
                        br(),
                        plotOutput("hcluster_plot"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. kmeans ====
                    tabPanel(
                        "K-means Clustering",
                        includeMarkdown("Rmd/clustering_kmeans.Rmd"),
                        br(),
                        
                        fluidRow(
                            column(
                                5,
                                radioButtons("kmeans",
                                             label = "Select corpus",
                                             choices = c("clinical notes",
                                                         "amazon medical entities"),
                                             inline = TRUE)
                            ),
                            column(
                                5,
                                radioButtons("kmeans_pca",
                                             label = "Using principle components?",
                                             choices = c("yes", "no"),
                                             selected = "no",
                                             inline = TRUE)
                            )
                        ),
                        plotOutput("kmeans_plot", height = "400px"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    )
                )
            ),
            
            # classification ===================================================
            tabItem(
                "classification",
                h1("Medical Specialty Classification"),
                includeMarkdown("Rmd/classification_intro.Rmd"),
                tabsetPanel(
                    # .. gas-neu-uro ====
                    tabPanel(
                        "Gastro-Neuro-Urol",
                        includeMarkdown("Rmd/classification_gas_neu_uro.Rmd"),
                        # the html is generated from Rmd and then delete 
                        # everything outside of <body> ... </body>.
                        # The js and css in header mess up with shiny's js & css
                        includeHTML("Rmd/classification_gas_neu_uro_table.html"),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. multiclass ====
                    tabPanel(
                        "Multiclass Classification",
                        includeMarkdown("Rmd/classification_multiclass.Rmd"),
                        
                        
                        fluidRow(
                            # plot confusion matrix 1
                            column(
                                6,
                                fluidRow(
                                    column(1),
                                    column(
                                        5,
                                        selectInput("cm_method_1",
                                                    "Select algorithm and method",
                                                    choices = c("svm + tfidf", 
                                                                "svm + tfidf + pca", 
                                                                "neural network + tfidf", 
                                                                "neural network + tfidf + pca"),
                                                    selected = "svm + tfidf"),
                                        
                                        htmlOutput("acc_1")
                                    ),
                                    column(
                                        4,
                                        radioButtons("cm_type_1",
                                                     "Select type",
                                                     choices = c("recall", "precision"),
                                                     selected = "recall",
                                                     inline = TRUE)
                                    )
                                ),
                                plotOutput("multiclass_1", width = "90%", height = "400px")
                            ),
                            
                            # plot confusion matrix 2
                            column(
                                6,
                                fluidRow(
                                    column(1),
                                    column(
                                        5,
                                        selectInput("cm_method_2",
                                                    "Select algorithm and method",
                                                    choices = c("svm + tfidf", 
                                                                "svm + tfidf + pca", 
                                                                "neural network + tfidf", 
                                                                "neural network + tfidf + pca"),
                                                    selected = "svm + tfidf + pca"),
                                        
                                        htmlOutput("acc_2")
                                    ),
                                    column(
                                        4,
                                        radioButtons("cm_type_2",
                                                     "Select type",
                                                     choices = c("recall", "precision"),
                                                     selected = "recall",
                                                     inline = TRUE)
                                    )
                                ),
                                plotOutput("multiclass_2", width = "90%", height = "400px")
                            )
                        ),
                        
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    ),
                    
                    # .. prediction ====
                    tabPanel(
                        "Deploy Model",
                        includeMarkdown("Rmd/classification_prediction.Rmd"),
                        br(),
                        
                        fluidRow(
                            column(
                                3,
                                fileInput("file_upload", "Choose a file",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                includeText("Rmd/file_upload.txt")
                            ),
                            
                            column(
                                9,
                                # input from text field
                                textAreaInput(
                                    "text_input",
                                    "Input clinical notes",
                                    placeholder = "One paragraph per note",
                                    height = "200px"
                                ) %>% 
                                    # bug in width when set at "100%"
                                    shiny::tagAppendAttributes(style = 'width: 100%;')
                                
                                ## do not use submitButton, it control all input
                                #submitButton("Submit"),
                                #actionButton("submit_text_input", "submit")
                            )
                        ),
                        br(),
                        
                        
                        h4("Prediction:"),
                        verbatimTextOutput("print_prediction"),
                        br(),
                        downloadButton("download", "Download prediciton"),
                        
                        # input using uploaded data
                        br(),
                        br(),
                        br(),
                        tags$a(
                            href="#top", "Go to Top",
                            class = "go-to-top"
                        )
                    )
                ),
            )
        )
    )
)