library(shiny)
ui <- fluidPage(
    # fluidRow(
    #     column(
    #         12,
    #         DT::dataTableOutput("search_table")
    #     )
    # ),
    fluidRow(
        column(
            12,
            DT::dataTableOutput("tfidf")
        )
    )
)