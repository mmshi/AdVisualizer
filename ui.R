# ui.R

library(ggvis)

shinyUI(fluidPage(theme = "bootstrap.css",
                  
        titlePanel("Ad Visualizer"),
        
        fluidRow(
                column(3,
                       wellPanel(
                               a(href="Template.csv", target="_blank", "Download Template Here"),
                               br(),
                               br(),
                               br(),
                               fileInput("file", "Upload CSV File", multiple = FALSE, accept = c(".csv")),
                               selectInput("xvar", label = "X-Axis Variable", 
                                           axis_vars, selected = "Spend"),
                               selectInput("yvar", label = "Y-Axis Variable", 
                                           axis_vars, selected = "CPM"),
                               selectInput("legend", label = "Legend", 
                                           c("Age" = "Age",
                                             "Bid Type" = "Bid.Type",
                                             "Gender" = "Gender",
                                             "Placement" = "Placement"), selected = "Age"),
                               #dateRangeInput("dates", label = "Date Range", start = "2015-01-01",
                               #               format = "mm/dd/yyyy"),
                               sliderInput("spend", label = "Spend", 
                                           min = 0, max = 250, value = c(0, 50), step = .5),
                               sliderInput("cpm", label = "CPM", 
                                           min = 0, max = 100, value = c(0, 50), step = .5),
                               sliderInput("cpc", label = "CPC", 
                                           min = 0, max = 100, value = c(0, 50), step = .5),
                               sliderInput("ctr", label = "CTR", 
                                           min = 0, max = .1, value = c(0, .05)),
                               sliderInput("cpa", label = "CPA", 
                                           min = 0, max = 100, value = c(0, 50), step = .5),
                               sliderInput("cr", label = "CR", 
                                           min = 0, max = 1.5, value = c(0, 1))
                       )
                ),
                column(9,
                       ggvisOutput("plot1"),
                       fluidRow(
                               column(5,
                                      wellPanel(
                                              textOutput("num_ads"),
                                              textOutput("spend_text"),
                                              textOutput("impressions_text"),
                                              textOutput("clicks_text"),
                                              textOutput("acquisitions_text"))),
                               column(5,
                                      wellPanel(
                                              textOutput("cpm_text"),
                                              textOutput("cpc_text"),
                                              textOutput("ctr_text"),
                                              textOutput("cpa_text"),
                                              textOutput("cr_text")))
                      )

              )
)))