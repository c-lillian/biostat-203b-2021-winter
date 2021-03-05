# MIMIC-IV Data Visualization Shiny Application for HW 3 
# Lillian Chen BIOSTAT 203B Winter 2021
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages--
library(shiny)
library(tidyverse)

# Load icu_cohort.rds dataset--
cohort <- read_rds("icu_cohort.rds")
shinydata <- cohort %>% 
    # exclude id data, time data, dod (all NAs), and anchor data 
    # (anchor vars included within admitage created variable)
    select(-subject_id, -hadm_id, -stay_id, -intime, -outtime, 
           -admittime, -dischtime, -deathtime, -edregtime, -edouttime,
           -anchor_age, -anchor_year, -anchor_year_group, -dod)

shinydata <- shinydata %>% mutate_if(sapply(shinydata, is.character), as.factor)

names(shinydata) <- c("first care unit", "last care unit", "length of stay",
                      "admission type", "admission location", 
                      "discharge location", "insurance", "language",
                      "marital status", "ethnicity", "hospital expire flag",
                      "gender", "admit age", "heart rate", 
                      "non-invasive systolic blood pressure",
                      "non-invasive mean blood pressure",
                      "respiratory rate", "temperature (F)", 
                      "arterial systolic blood pressure",
                      "arterial mean blood pressure",
                      "bicarbonate", "calcium", "chloride", "creatinine",
                      "glucose", "magnesium", "potassium", "sodium",
                      "hematocrit", "white blood cell count", "lactate",
                      "30-day mortality outcome")
    

# Define UI
ui <- fluidPage(

    # Navigation Bar
    navbarPage("MIMIC-IV Data Visualization",
               # First Panel
               tabPanel("2 Variable Comparison", 
                        # Application title
                        titlePanel("MIMIC-IV Scatter Plot"),
                        
                        # Sidebar with a server side selectizeInput
                        sidebarLayout(
                            sidebarPanel(
                            helpText("Choose 2 variables to compare on
                                     a single plot."),
                            varSelectizeInput('var1', 
                                    'Select 1st variable (x-axis)', shinydata),
                            varSelectizeInput('var2', 
                                    'Select 2nd variable (y-axis)', shinydata),
                            br(),
                            helpText("Choose what type of plot to generate."),
                            helpText("For Best Results:"),
                            helpText("2 continuous variables: scatter plot"),
                            helpText("2 discrete variables: bubble plot"),
                            helpText("1 discrete, 1 continuous: 
                                     box plot/column graph"),
                            selectizeInput('plottype', 'Select plot', 
                                    choices = c("--" = "", 
                                                "box plot", 
                                                "scatter plot", 
                                                "column graph",
                                                "bubble plot")),
                            ),
                
                            # Show a plot of selected vars
                            mainPanel(plotOutput("twovarPlot"))
                        )
                        ),
               
               # Navbar Menu for labevents and chartevents
               navbarMenu("Measurements",
               # First panel: chartevents
               tabPanel("Chart Data", 
                        # Application title
                        titlePanel("MIMIC-IV Chart Measurement Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Select a variable to visualize."),
                                selectizeInput('chartvar', 'Select variable',
                                        choices = c(names(shinydata)[14:20])),
                                sliderInput("chartbins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 200,
                                            value = 100)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(tableOutput("charttable"),
                                      plotOutput("charthistPlot")
                            )
                        )
                        ),
               # Second panel: labevents
               tabPanel("Lab Data", 
                        # Application title
                        titlePanel("Lab Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Select a variable to visualize."),
                                selectizeInput('labvar', 'Select variable',
                                        choices = c(names(shinydata)[21:31])),
                                sliderInput("labbins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 200,
                                            value = 100)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(tableOutput("labtable"),
                                      plotOutput("labhistPlot")
                            )
                        )
               )
               ),
               
               # Navbar Menu for demographic data
               navbarMenu("Admissions",
               # First panel: demographics
               tabPanel("Demographics", 
                        # Application title
                        titlePanel("Patient Demographics Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Select a variable to visualize. Slider
                                         for number of bins only works for 
                                         continuous variables."),
                                selectizeInput('demovar', 'Select variable',
                                        choices = c(names(shinydata)[7:10],
                                                    names(shinydata)[12:13],
                                                    names(shinydata)[32])),
                                sliderInput("demobins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 200,
                                            value = 100)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(tableOutput("demotable"),
                                      plotOutput("demoPlot")
                            )
                        )
               ),
               # Second panel: stay times and locations
               tabPanel("Stay and Location", 
                        # Application title
                        titlePanel("Stay and Location Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                helpText("Select a variable to visualize. Slider
                                         for number of bins only works for 
                                         continuous variables."),
                                selectizeInput('admvar', 'Select variable',
                                        choices = c(names(shinydata)[1:6])),
                                sliderInput("admbins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 200,
                                            value = 100)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(tableOutput("admittable"),
                                      plotOutput("admPlot")
                            )
                        )
               )
               )
    )
)

# Define server logic
server <- function(input, output) {
    
    # scatter plot best for continuous x and continuous y
    scatter <- reactive({
        
        shinydata %>%
            filter(!is.na(!!sym(input$var1))) %>%
            filter(!is.na(!!sym(input$var2))) %>%
        # generate scatter plot
        ggplot(aes_string(x = input$var1, y = input$var2)) +
            geom_point() +
            geom_smooth(method = lm) +
            theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                  legend.position = "right")
            

    })
    
    # box plot best for discrete x and continuous y
    boxp <- reactive({
        
        varscount <- shinydata %>%
            count(!!sym(input$var1), !!sym(input$var2))
        
        names(varscount) <- c("var1", "var2", "n")
        
        if (is.factor(varscount$var1) == T & 
            is.factor(varscount$var2) == T){
            # discrete var1, discrete var2
            varscount %>%
                ggplot(aes(x = var1, y = n)) +
                geom_boxplot() +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      legend.position = "right") +
                facet_wrap(~ as.character(input$var1) + as.character(input$var2), 
                            ncol = 2, scales = "free_y")
        }
        
        else if (is.factor(varscount$var1) == T){
            # discrete var1, continuous var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                select(!!sym(input$var1), !!sym(input$var2)) %>%  
                ggplot(aes_string(x = input$var1, y = input$var2)) +
                geom_boxplot() +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      legend.position = "right") +
                facet_wrap(~ as.character(input$var1), 
                            ncol = 2, scales = "free_y")
            
        }
        
        else if (is.factor(varscount$var2) == T){
            # continuous var1, discrete var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                select(!!sym(input$var1), !!sym(input$var2)) %>%  
                ggplot(aes_string(x = input$var1, y = input$var2)) +
                geom_boxplot() +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      legend.position = "right") +
                facet_wrap(~ as.character(input$var2), 
                            ncol = 2, scales = "free_y")
        }
            
        else{
            # continuous var1, continuous var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                select(!!sym(input$var1), !!sym(input$var2)) %>%  
                ggplot(aes_string(x = input$var1, y = input$var2)) +
                geom_boxplot() +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      legend.position = "right")

        }
        
    })
    
    # column graph best for discrete x and continuous y
    colgraph <- reactive({
        
        varscount <- shinydata %>%
            count(!!sym(input$var1), !!sym(input$var2))
        
        names(varscount) <- c("var1", "var2", "n")
        
        if (is.factor(varscount$var1) == T & 
            is.factor(varscount$var2) == T){
            # discrete var1, discrete var2
            varscount %>%
                ggplot(aes(x = var1, y = var2)) +
                geom_col() +
                scale_x_discrete(guide = guide_axis(n.dodge=1)) +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      axis.text.y = element_blank(), 
                      legend.position = "right") +
                facet_wrap(var1 ~ var2, ncol = 3, scales = "free_y")
        }
        
        else if (is.factor(varscount$var1) == T & 
                 is.factor(varscount$var2) == F){
            # discrete var1, continuous var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                ggplot(aes(x = eval(as.name(input$var1)), 
                           y = eval(as.name(input$var2)))) +
                geom_col(aes(y =  eval(as.name(input$var2)), 
                             fill = eval(as.name(input$var1)))) +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      axis.text.y = element_text(hjust = 0, angle = 90), 
                      legend.position = "bottom")
            
        }
        
        else if (is.factor(varscount$var1) == F &
                 is.factor(varscount$var2) == T){
            # continuous var1, discrete var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                ggplot(aes(x = eval(as.name(input$var1)), 
                           y = eval(as.name(input$var2)))) +
                geom_col(aes(y =  eval(as.name(input$var2)), 
                             fill = eval(as.name(input$var2)))) +
                theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                      axis.text.y = element_text(hjust = 0, angle = 45), 
                      legend.position = "bottom")
        }
        
        else{
            # continuous var1, continuous var2
            shinydata %>%
                filter(!is.na(!!sym(input$var1))) %>%
                filter(!is.na(!!sym(input$var2))) %>%
                ggplot(aes(x = eval(as.name(input$var1)), 
                           y = eval(as.name(input$var2)))) +
                geom_col() +
                theme(axis.text.x = element_text(hjust = 1, angle = 45),
                      axis.text.y = element_text(hjust = 0, angle = 90), 
                      legend.position = "bottom")
            
        }
        
    })
    
    # bubble plot aka geom_count best for discrete x and discrete y
    bubble <- reactive({
        
        bubbleheight <- 0
        
        vars <- shinydata %>%
            filter(!is.na(!!sym(input$var1))) %>%
            filter(!is.na(!!sym(input$var2))) %>%
            select(!!sym(input$var1), !!sym(input$var2))
        

        
        vars %>% 
            ggplot(aes(x = eval(as.name(input$var1)), 
                       y = eval(as.name(input$var2)))) +
            geom_count(aes(color = ..n.., size = ..n..)) +
            theme(axis.text.x = element_text(hjust = 1, angle = 45), 
                  legend.position = "right")
            
        
    })

    # two variable comparison plot output
    output$twovarPlot <-renderPlot({
        
        if (input$plottype == "scatter plot") {
            scatter()
        }

        else if (input$plottype == "box plot") {
            boxp()
        }
        
        else if (input$plottype == "column graph") {
            colgraph()
        }
        
        else if (input$plottype == "bubble plot") {
            bubble()
        }
        
    }, height = 2000)
    
    # chartevents table reactive function
    ctable <- reactive({
        shinydata %>% 
            filter(!is.na(!!sym(input$chartvar))) %>% 
            select(!!sym(input$chartvar)) %>% 
            summarise(n = n(),
                      min = min(!!sym(input$chartvar)),
                      Q1 = quantile(!!sym(input$chartvar), 1/4),
                      median = median(!!sym(input$chartvar)),
                      mean = mean(!!sym(input$chartvar)),
                      Q3 = quantile(!!sym(input$chartvar), 3/4),
                      stdev = sd(!!sym(input$chartvar)),
                      max = max(!!sym(input$chartvar)),
                      missing = as.integer(50048 - n))
                      
    })
    
    # chartevents table output
    output$charttable <- renderTable({
            ctable()
    })
    
    # labevents table reactive function
    ltable <- reactive({
        shinydata %>% 
            filter(!is.na(!!sym(input$labvar))) %>% 
            select(!!sym(input$labvar)) %>% 
            summarise(n = n(),
                      min = min(!!sym(input$labvar)),
                      Q1 = quantile(!!sym(input$labvar), 1/4),
                      median = median(!!sym(input$labvar)),
                      mean = mean(!!sym(input$labvar)),
                      Q3 = quantile(!!sym(input$labvar), 3/4),
                      stdev = sd(!!sym(input$labvar)),
                      max = max(!!sym(input$labvar)),
                      missing = as.integer(50048 - n))
    })
    
    # labevents table output
    output$labtable <- renderTable({
            ltable()
    })

    # demographics table reactive function
    dtable <- reactive({
        
        if (input$demovar == "admit age"){
            
            shinydata %>% 
                filter(!is.na(!!sym(input$demovar))) %>% 
                select(!!sym(input$demovar)) %>% 
                summarise(n = n(),
                          min = min(!!sym(input$demovar)),
                          Q1 = quantile(!!sym(input$demovar), 1/4),
                          median = median(!!sym(input$demovar)),
                          mean = mean(!!sym(input$demovar)),
                          Q3 = quantile(!!sym(input$demovar), 3/4),
                          stdev = sd(!!sym(input$demovar)),
                          max = max(!!sym(input$demovar)),
                          missing = as.integer(50048 - n)) 
           
              }
        
        else{
            
            shinydata %>% 
                filter(!is.na(!!sym(input$demovar))) %>% 
                group_by(!!sym(input$demovar)) %>% 
                summarise(n = n() )    
            
             }
    })
    
    # demographics table output
    output$demotable <- renderTable({
            dtable()
    })  
    
    # admission stay and location table reactive function
    admtable <- reactive({
        
        vars <- c("first care unit", "last care unit", 
                  "admission type", "admission location", "discharge location")

        if (input$admvar %in% vars){
            
            shinydata %>% 
                filter(!is.na(!!sym(input$admvar))) %>% 
                group_by(!!sym(input$admvar)) %>% 
                summarise(n = n() )
            }
        
        else {
            shinydata %>% 
                filter(!is.na(!!sym(input$admvar))) %>% 
                select(!!sym(input$admvar)) %>% 
                summarise(n = n(),
                          min = min(!!sym(input$admvar)),
                          Q1 = quantile(!!sym(input$admvar), 1/4),
                          median = median(!!sym(input$admvar)),
                          mean = mean(!!sym(input$admvar)),
                          Q3 = quantile(!!sym(input$admvar), 3/4),
                          stdev = sd(!!sym(input$admvar)),
                          max = max(!!sym(input$admvar)),
                          missing = as.integer(50048 - n))
            }
    })
    
    # admissions stay and location table output
    output$admittable <- renderTable({
            admtable()
    })   
    
    # chart histogram plot output
    output$charthistPlot <- renderPlot({
        
        x <- shinydata %>% 
            filter(!is.na(!!sym(input$chartvar))) %>%
            select(!!sym(input$chartvar)) %>% 
            pull()
        
        bins <- seq(min(x), max(x), length.out = input$chartbins + 1)
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             main = paste("Histogram of", as.character(input$chartvar)),
             xlab = as.character(input$chartvar))

    })
    
    output$labhistPlot <- renderPlot({
        
        x <- shinydata %>% 
            filter(!is.na(!!sym(input$labvar))) %>%
            select(!!sym(input$labvar)) %>% 
            pull()
        
        bins <- seq(min(x), max(x), length.out = input$labbins + 1)
        
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             main = paste("Histogram of", as.character(input$labvar)),
             xlab = as.character(input$labvar))
        
    })
    
    
    output$demoPlot <- renderPlot({
        
        #make this conditional
        if (input$demovar == "admit age"){
            
            # histogram for continuous variables
            x <- shinydata %>% 
                filter(!is.na(!!sym(input$demovar))) %>%
                select(!!sym(input$demovar)) %>% 
                pull()
            
            bins <- seq(min(x), max(x), length.out = input$demobins + 1)
            
            hist(x, breaks = bins, col = 'darkgray', border = 'white',
                 main = paste("Histogram of", as.character(input$demovar)),
                 xlab = gsub("_", " ", input$demovar))
        }
        
        
        
        else{
            
            #bar chart for categorical variables
            counts <- shinydata %>% 
                group_by(!!sym(input$demovar)) %>% 
                summarise(n = n()) %>% 
                pull(n) %>% 
                t()
            
            shinydata %>% 
                select(!!sym(input$demovar)) %>% 
                ggplot(mapping = aes(eval(as.name(input$demovar)))) +
                geom_bar(aes(fill = eval(as.name(input$demovar)))) +
                ggtitle(as.character(input$demovar)) +
                annotate(geom = "label", x = 1:length(counts), y = counts, 
                         label = as.character(counts), 
                         size = 4, fill = 'white') +
                theme(axis.text.x = element_text(size = 12, 
                                                 hjust = 1, angle = 45), 
                      axis.text.y = element_text(size = 12),
                      axis.title = element_text(size = 14, face = "bold"),
                      plot.title = element_text(size = 20, face = "bold"),
                      legend.position = "right") +
                labs(x = as.character(input$demovar), 
                     y = "count") +
                guides(fill=guide_legend(title=as.character(input$demovar)))
            
        }
    })
    
    
    output$admPlot <- renderPlot({
        
        vars <- c("first care unit", "last care unit", 
                  "admission type", "admission location", "discharge location")
        
        if (input$admvar %in% vars){
            
            #bar chart for categorical variables
            counts <- shinydata %>% 
                group_by(!!sym(input$admvar)) %>% 
                summarise(n = n()) %>% 
                pull(n) %>% 
                t()
            
            shinydata %>% 
                select(!!sym(input$admvar)) %>% 
                ggplot(mapping = aes(eval(as.name(input$admvar)))) +
                ggtitle(as.character(input$admvar)) +
                geom_bar(aes(fill = eval(as.name(input$admvar)))) +
                annotate(geom = "label", x = 1:length(counts), y = counts, 
                         label = as.character(counts), 
                         size = 4, fill = 'white') +
                theme(axis.text.x = element_text(size = 12, 
                                                 hjust = 1, angle = 45), 
                      axis.text.y = element_text(size = 12),
                      axis.title = element_text(size = 14, face = "bold"),
                      plot.title = element_text(size = 20, face = "bold"),
                      legend.position = "right") +
                labs(x = as.character(input$admvar), 
                     y = "count") +
                guides(fill=guide_legend(title=as.character(input$admvar)))
        }
        
        

        else{
            # histogram for continuous variables
            
            x <- shinydata %>% 
                filter(!is.na(!!sym(input$admvar))) %>%
                select(!!sym(input$admvar)) %>% 
                pull()
            
            bins <- seq(min(x), max(x), length.out = input$admbins + 1)
            
            hist(x, breaks = bins, col = 'darkgray', border = 'white',
                 main = paste("Histogram of", as.character(input$admvar)),
                 xlab = gsub("_", " ", input$admvar))
        
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
