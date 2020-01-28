library(shiny)
ui <- fluidPage(
    fluidRow(
        column(
            12,
            DT::dataTableOutput("search_table_1")
        )
    )
)