#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shinyjs)
library(shinydashboard)

source('calc_logic.R')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    
    # Application title
    dashboardHeader(title = "ROC-curve experiment"),
    
    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        tags$style(".form-group{margin-bottom:0px;}"),
        
        sliderInput('n_total_case', label='N total case', min=100, max=100000, value=1000, step=100),
        sliderInput('n_bins', label='histogram bin', min=60, max=300, value=90),
        
        
        tags$fieldset(class='border',
            tags$legend("Positive score modification", style="color:white; font-size:14px; margin-bottom:0px;"),
            checkboxInput('bump_pos', 'score bump'),
            sliderInput('bump_pos_r', 'amount', min=0, max=0.8, value=0.3, step=0.001),
            sliderInput('mean_pos_bump', 'mean', min=-5, max=5, value=0.5, step=0.1),
            sliderInput('sd_pos_bump', 'sd', min=0, max=0.1, value=0, step=0.001)
        ),
        
        tags$fieldset(class='border',
            tags$legend("Negative score modification", style="color:white; font-size:14px; margin-bottom:0px;"),
            checkboxInput('bump_neg', 'score bump'),
            sliderInput('bump_neg_r', 'amoount', min=0, max=0.8, value=0.3, step=0.001),
            sliderInput('mean_neg_bump', 'mean', min=-5, max=5, value=-0.5, step=0.1),
            sliderInput('sd_neg_bump', 'sd', min=0, max=0.1, value=0, step=0.001)
        )
        
    ),
    
    # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(
            sliderInput('negative_ratio', label='Negativa case ratio', min=0, max=1, value=0.5, step=0.001, width='100%'),
            plotOutput('graphs0', height='85px'),
            
            column(6, align='center',
                   sliderInput('score_mean_neg', label='mean negative score', min=-5, max=0, value=-0.5, step=0.05, width='100%')
            ),
            column(6, align='center',
                   sliderInput('score_mean_pos', label='mean positive score', min=0, max=5, value=0.5, step=0.05, width='100%')
            ),
            
            column(6, align='center',
                   sliderInput('score_sd_neg', label='negative score SD', min=0, max=5, value=1, step=0.01, width='100%')
            ),
            column(6, align='center',
                   sliderInput('score_sd_pos', label='positive score SD', min=0, max=5, value=1, step=0.01, width='100%')
            ),
            
            
            sliderInput('threshold', label='Manual score threshold', min=-10, max=10, value=0, step=0.05, width='100%'),
            shiny::uiOutput('optimal_threshold_ui'),
            
            column(6, align='center',
                plotOutput('graphs_score_hist1', height='180px')
            ),
            column(6, align='center',
                plotOutput('graphs_score_hist2', height='180px')
            ),
            column(6, align='center', checkboxInput('hist1_log_y', label='toggle logscale', value=FALSE)),
            column(6, align='center', checkboxInput('hist2_log_y', label='toggle logscale', value=FALSE)),
            
            column(6, align='center',
                plotOutput('graph_roc', height = '300px', width = '300px')
            ),
            column(6, align='center',
                plotOutput('graph_pr', height = '300px', width = '300px')
            ),
            
            column(6, align='center',
                plotOutput('graph_confusion_matrix1', height = '300px', width = '300px')
            ),
            column(6, align='center',
                plotOutput('graph_confusion_matrix2', height = '300px', width = '300px')
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    optimal_threshold <- reactive({
        return(get_threshold(d(), mode='youden'))
    })
    
    optimal_threshold2 <- reactive({
        return(get_threshold(d(), mode='tptn'))
    })

    d <- reactive({
        set.seed(1)
        nega_r <- 2 * input$negative_ratio
        n_negative_case <- as.integer(nega_r * input$n_total_case / 2)
        n_positive_case <- input$n_total_case - n_negative_case

        offset <- input$offset
        if(is.null(offset)){
            offset <- 0
        }
        y_positive <- rnorm(n_positive_case, mean = input$score_mean_pos, sd = input$score_sd_pos)
        y_negative <- rnorm(n_negative_case, mean = input$score_mean_neg, sd = input$score_sd_neg)
        
        if(input$bump_pos){
            n_bump <- floor(n_positive_case * input$bump_pos_r)
            y_positive[1:n_bump] <- rnorm(n_bump, mean=input$mean_pos_bump, sd=input$sd_pos_bump)
        }
        if(input$bump_neg){
            n_bump <- floor(n_negative_case * input$bump_neg_r)
            y_negative[1:n_bump] <- rnorm(n_bump, mean=input$mean_neg_bump, sd=input$sd_neg_bump)
        }
        
        d_result <- data.frame(
            y = c(rep(1, n_positive_case), rep(0, n_negative_case)),
            pred = c(y_positive, y_negative)
        )
        
        return(d_result)
    })
    
    output$graphs0 <- renderPlot({
        tmp_d <- d() %>%
            group_by(y) %>%
            summarise(count=n())
        g <- ggplot(tmp_d, aes(x=count, y="", fill=y, label=as.character(y))) +
            geom_bar(stat='identity')

        g <- g +
            ylab('') +
            theme(legend.position = 'none', plot.background = element_blank()) +
            geom_text(size=10, position = position_stack(vjust = 0.5), colour='white') + 
            ggtitle('Grand truth ratio')
        
        return(g)
    }, bg="transparent")
    
    output$optimal_threshold_ui <- renderText({
        ret <- glue('Optimal threshold with Youden Index = {round(optimal_threshold()$threshold, 6)}<br><br>')
        return(ret)
    })
    
    output$graph_confusion_matrix1 <- renderPlot({
        g <- get_confusion_matrix_plot(d(), th=input$threshold, 'Confusion Matrix(threshold: Manual threshold)')
        return(g)
    }, bg="transparent")
    
    output$graph_confusion_matrix2 <- renderPlot({
        g <- get_confusion_matrix_plot(d(), th=optimal_threshold()$threshold, 'Confusion Matrix(threshold: Youden Index)')
        return(g)
    }, bg="transparent")
    
    output$graphs_score_hist1 <- renderPlot({
        g <- plot_score_histogram(d(), th=input$threshold, n_bins=input$n_bins, opt_th=optimal_threshold()$threshold, log_y=input$hist1_log_y)
        return(g)
    }, bg="transparent")
    
    output$graphs_score_hist2 <- renderPlot({
        g <- plot_score_histogram(d(), th=input$threshold, n_bins=input$n_bins, opt_th=optimal_threshold()$threshold, mode='density', log_y=input$hist2_log_y)
        return(g)
    }, bg="transparent")
    
    output$graph_roc <- renderPlot({
        g <- plot_roc(d())
        g <- add_optimal_opints(g, optimal_threshold())
        return(g)
    }, bg="transparent")
    
    output$graph_pr <- renderPlot({
        return(plot_pr(d()))
    }, bg="transparent")
}

# Run the application 
shinyApp(ui = ui, server = server)
