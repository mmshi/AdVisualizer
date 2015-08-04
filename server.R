library(shiny)
library(ggvis)
library(dplyr)
source('global.r')

shinyServer(function(input, output, session) {
        
        getMyData <- function() ({
                inFile <- input$file
                if(is.null(inFile))
                        return(tbl_df(read.csv("test.csv")))
                else {
                        print('it wokred!!!')
                        return(read.csv(inFile$datapath))
                }
        })
        
        #return data frame of filtered ads
        df <- reactive({
                getMyData() %>%
                filter(
                        Spend >= input$spend[1] & Spend <= input$spend[2],
                        CPM >= input$cpm[1] & CPM <= input$cpm[2],
                        CPC >= input$cpc[1] & CPC <= input$cpc[2],
                        CTR >= input$ctr[1] & CTR <= input$ctr[2],
                        CPA >= input$cpa[1] & CPA <= input$cpa[2],
                        CR >= input$cr[1] & CR <= input$cr[2]
                )
        })
        
       # lol cpm
        ad_tooltip <- function(x) {
               if (is.null(x))
                       return(NULL)
               if (is.null(x$ID))
                       return(NULL)
               myData <- isolate(df())
               ad <- myData[myData$ID == x$ID, ]
               
               paste0(ad$Targeting, "<br>",
                      ad$Age, " ", ad$Gender, "<br>",
                      ad$Image, ", ", ad$Copy)
        }
        
        # A reactive expression with the ggvis plot
        vis <- reactive({
                inFile <- input$file
                if (!is.null(inFile)) {
                        df <- read.csv(inFile$datapath)
                }
                else {
                        print('no file')
                        df <- df
                }
                
                df <- df %>%
                filter(
                        Spend >= input$spend[1] & Spend <= input$spend[2],
                        CPM >= input$cpm[1] & CPM <= input$cpm[2],
                        CPC >= input$cpc[1] & CPC <= input$cpc[2],
                        CTR >= input$ctr[1] & CTR <= input$ctr[2],
                        CPA >= input$cpa[1] & CPA <= input$cpa[2],
                        CR >= input$cr[1] & CR <= input$cr[2]
                )
                
                # Labels for axes
                xvar_name <- names(axis_vars)[axis_vars == input$xvar]
                yvar_name <- names(axis_vars)[axis_vars == input$yvar]
                # legend_name <- names(legend_vars)[legend_vars == input$legend]
                
                xvar <- prop("x", as.symbol(input$xvar))
                yvar <- prop("y", as.symbol(input$yvar))
                # legend <- prop("l", as.symbol(input$legend))
                
                # l = input$legend
                # df <- getMyData()
                # df <- myFilter(df)
                df %>% 
                        ggvis(x = xvar, y = yvar, 
                              fill =~ Placement) %>%
                        layer_points(size := 45, size.hover := 200,
                                     fillOpacity := 0.5, fillOpacity.hover := 0.75) %>%
                        add_tooltip(ad_tooltip, "hover") %>%
                        add_axis("x", title = xvar_name) %>%
                        add_axis("y", title = yvar_name) %>%
                        # add_legend("l", title = legend_name) %>%
                        set_options(width = 600, height = 500)
        })
        
        # plot output
        vis %>% bind_shiny("plot1")
        
        # summary table output
        output$num_ads <- renderText({
                paste("Number of Ads: ", nrow(df())) 
        })
        output$spend_text <- renderText({ 
                data <- df()
                paste("Spend: $", colSums(data[, 2, drop = F]))
        })
        output$impressions_text <- renderText({ 
                data <- df()
                paste("Impressions: ", colSums(data[, 3, drop = F])) 
        })
        output$clicks_text <- renderText({ 
                data <- df()
                paste("Clicks: ", colSums(data[, 5, drop = F])) 
        })
        output$acquisitions_text <- renderText({ 
                data <- df()
                paste("Acquisitions: ", colSums(data[, 8, drop = F])) 
        })
        output$cpm_text <- renderText({ 
                data <- df()
                paste("Average CPM: $", round(colSums(data[, 4, drop = F]) / nrow(df()), 2)) 
        })
        output$cpc_text <- renderText({ 
                data <- df()
                paste("Average CPC: $", round(colSums(data[, 6, drop = F]) / nrow(df()), 2)) 
        })
        output$ctr_text <- renderText({ 
                data <- df()
                paste("Average CTR:", round((colSums(data[, 7, drop = F]) / nrow(df()) * 100), 2), "%") 
        })
        output$cpa_text <- renderText({ 
                data <- df()
                paste("Average CPA: $", round(colSums(data[, 9, drop = F]) / nrow(df()), 2)) 
        })
        output$cr_text <- renderText({ 
                data <- df()
                paste("Average CR:", round((colSums(data[, 10, drop = F]) / nrow(df()) * 100), 2), "%") 
        })
})