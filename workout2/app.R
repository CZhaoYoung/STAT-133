
# Workout2, Stat 133 Fall 2019, Zhaoyang Chen
# Shiny App to simulate three investing scenarios

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Three Investing Scenarios Simulaiton"),
    
    # Sidebar with a slider input for number of bins 
        fluidRow(
            column(3,
                   sliderInput("Initial_Amount",
                               label = "Initial Amount:",
                               min = 0,
                               max = 10000,
                               step = 100,
                               value = 1000),
                   sliderInput("Annual_Con",
                               label = "Annual Contribution:",
                               min = 0,
                               max = 5000,
                               step = 100,
                               value = 200),
                   sliderInput("Annual_Rate",
                               label = "Annual Growth Rate:(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 2),
                   sliderInput("High_Rate",
                               label = "High Yield Rate:(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 2)),
            column(3,
                   sliderInput("High_Volatility",
                               label = "High Yield volatility:(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 0.1),
                   sliderInput("Fixed_Rate",
                               label = "Fixed Income rate (U.S. Bonds):%",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 5),
                   sliderInput("Fixed_Volatility",
                               label = "Fixed Income volatility (U.S. Bonds):(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 4.5),
                   sliderInput("Equity_Rate",
                               label = "US Equity rate (U.S. Stocks):%",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 10)
            ),
            column(3,
                   sliderInput("Equity_Volatility",
                               label = "US Equity volatility (U.S. Stocks):(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 15),
                   sliderInput("Years",
                               label = "Years",
                               min = 0,
                               max = 50,
                               step = 1,
                               value = 20),
                   numericInput("Seed",
                                "Choose a random seed",
                                value = 12345),
                   selectInput("Boolean", 
                               "Facet?", 
                               c(TRUE, FALSE))
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("graph")
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # use the data to draw a graph
    output$graph <- renderPlot({
        # input variable
        amt <-  input$Initial_Amount
        g <-  input$Annual_Rate/100
        annual_con <-  input$Annual_Con
        year <-  input$Years
        
        # build temp vector
        AMOUNT <- c()
        YEAR <- c()
        TYPE <- c()
        
        # US_Bonds
        rate <- input$Fixed_Rate/100
        vol <- input$Fixed_Volatility/100
        
        for (i in 1:year) {
            r1 <- rnorm(1, mean = rate, sd= vol)
            amt = amt*(1+r1)+annual_con*((1+g)^(i-1))
            AMOUNT <- c(AMOUNT, amt)
            YEAR <- c(YEAR, i)
            TYPE <- c(TYPE, "US_Bonds")
        }
        
        # US_Stocks
        # update the variables
        rate <- input$Equity_Rate/100
        vol <- input$Equity_Volatility/100
        amt <-  input$Initial_Amount
        
        for (i in 1:year) {
            r2 <- rnorm(1, mean = rate, sd= vol)
            amt = amt*(1+r2)+annual_con*((1+g)^(i-1))
            AMOUNT <- c(AMOUNT, amt)
            YEAR <- c(YEAR, i)
            TYPE <- c(TYPE, "US_Stocks")
        }
        
        # High Yield Savings Account
        # update the variables
        rate <- input$High_Rate/100
        vol <- input$High_Volatility/100
        amt <-  input$Initial_Amount
        
        for (i in 1:year) {
            r3 <- rnorm(1, mean = rate, sd= vol)
            amt = amt*(1+r3)+annual_con*((1+g)^(i-1))
            AMOUNT <- c(AMOUNT, amt)
            YEAR <- c(YEAR, i)
            TYPE <- c(TYPE, "High_Yield")
        }
        
        # use the temp vector to build the data frame 'Investing'
        Investing <- data.frame("Amt" = AMOUNT, "Year" = YEAR, "Type" = TYPE)
        if(input$Boolean == TRUE)
        {
            ggplot(data = Investing, aes(x = Year, y = Amt, group = Type)) +
                geom_path(aes(color = Type)) +
                ggtitle("Three Investing Scenarios") + facet_grid(.~Type)
        }
        else
        {
            ggplot(data = Investing, aes(x = Year, y = Amt, group = Type)) +
                geom_path(aes(color = Type)) +
                ggtitle("Three Investing Scenarios")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
