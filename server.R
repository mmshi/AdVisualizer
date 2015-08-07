library(shiny)
library(ggvis)
library(dplyr)
library(lubridate)
source('global.r')

shinyServer(function(input, output, session) {
        
        getMyData <- function() ({
                inFile <- input$file
                if(is.null(inFile))
                        return(tbl_df(read.csv("test.csv")))
                else {
                        return(read.csv(inFile$datapath))
                }
        })
        
        #return data frame of filtered ads
        df <- reactive({
                startDate = yday(ymd(as.character(input$dates[1])))
                endDate = yday(ymd(as.character(input$dates[2])))
                getMyData() %>%
                filter(
                        Spend >= input$spend[1] & Spend <= input$spend[2],
                        CPM >= input$cpm[1] & CPM <= input$cpm[2],
                        CPC >= input$cpc[1] & CPC <= input$cpc[2],
                        CTR >= input$ctr[1] & CTR <= input$ctr[2],
                        CPA >= input$cpa[1] & CPA <= input$cpa[2],
                        CR >= input$cr[1] & CR <= input$cr[2],
                        yday(mdy(as.character(Date))) >= startDate & 
                                yday(mdy(as.character(Date))) <= endDate
                )
        })
        
        varlist = function(x) {
                x = str_c('^(',paste(x, collapse='|'),')$')
                str_replace_all(x,'\\.','\\\\.')
        }
        
       # lol cpm
        ad_tooltip <- function(x) {
               myData <- isolate(df())
               
               ad <- myData
               
               if (!is.null(x$Age)) 
                       ad <- ad %>% filter(Age == x$Age)
               if (!is.null(x$Bid.Type))
                       ad <- ad %>% filter(Bid.Type == x$Bid.Type)
               if (!is.null(x$Gender))
                       ad <- ad %>% filter(Gender == x$Gender)
               if (!is.null(x$Placement))
                       ad <- ad %>% filter(Placement == x$Placement)

               if (!is.null(x$CPM))
                       ad <- ad %>% filter(CPM == x$CPM)
               if (!is.null(x$CPC))
                       ad <- ad %>% filter(CPC == x$CPC)
               if (!is.null(x$CTR))
                       ad <- ad %>% filter(CTR == x$CTR)
               if (!is.null(x$CPA))
                       ad <- ad %>% filter(CPA == x$CPA)
               if (!is.null(x$CR))
                       ad <- ad %>% filter(CR == x$CR)
               if (!is.null(x$Spend))
                       ad <- ad %>% filter(Spend == x$Spend)
               if (!is.null(x$Date))
                       ad <- ad %>% filter(Date == x$Date)

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
                        df <- df
                }
                
                startDate = yday(ymd(as.character(input$dates[1])))
                endDate = yday(ymd(as.character(input$dates[2])))
                
                # update graph
                df <- df %>%
                filter(
                        Spend >= input$spend[1] & Spend <= input$spend[2],
                        CPM >= input$cpm[1] & CPM <= input$cpm[2],
                        CPC >= input$cpc[1] & CPC <= input$cpc[2],
                        CTR >= input$ctr[1] & CTR <= input$ctr[2],
                        CPA >= input$cpa[1] & CPA <= input$cpa[2],
                        CR >= input$cr[1] & CR <= input$cr[2],
                        yday(mdy(as.character(Date))) >= startDate & 
                                yday(mdy(as.character(Date))) <= endDate
                )
                
                # Labels for axes
                xvar_name <- names(axis_vars)[axis_vars == input$xvar]
                yvar_name <- names(axis_vars)[axis_vars == input$yvar]

                xvar <- prop("x", as.symbol(input$xvar))
                yvar <- prop("y", as.symbol(input$yvar))

                if (input$legend == "Age")
                        df <- df %>% ggvis(x = xvar, y = yvar, 
                                            fill =~ Age)
                if (input$legend == "Bid.Type")
                        df <- df %>%  ggvis(x = xvar, y = yvar, 
                                        fill =~ Bid.Type) 
                if (input$legend == "Gender")
                        df <- df %>%  ggvis(x = xvar, y = yvar, 
                                            fill =~ Gender) 
                if (input$legend == "Placement")
                        df <- df %>%  ggvis(x = xvar, y = yvar, 
                                            fill =~ Placement) 
                df <- df %>%
                        layer_points(size := 45, size.hover := 200,
                                     fillOpacity := 0.5, fillOpacity.hover := 0.75) %>%
                        add_tooltip(ad_tooltip, "hover") %>%
                        add_axis("x", title = xvar_name) %>%
                        add_axis("y", title = yvar_name) %>%
                        set_options(width = 600, height = 500)
        })
        
        # plot output
        vis %>% bind_shiny("plot1")
        
        # summary table output
        output$num_ads <- renderText({
                paste("Number of Ads: ", format(nrow(df()), big.mark = ",")) 
        })
        output$spend_text <- renderText({ 
                data <- df()
                paste("Spend: $", format(colSums(data[, 2, drop = F]), big.mark = ","))
        })
        output$impressions_text <- renderText({ 
                data <- df()
                paste("Impressions: ", format(colSums(data[, 3, drop = F]), big.mark = ",")) 
        })
        output$clicks_text <- renderText({ 
                data <- df()
                paste("Clicks: ", format(colSums(data[, 5, drop = F]), big.mark = ",")) 
        })
        output$acquisitions_text <- renderText({ 
                data <- df()
                paste("Acquisitions: ", format(colSums(data[, 8, drop = F]), big.mark = ",")) 
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