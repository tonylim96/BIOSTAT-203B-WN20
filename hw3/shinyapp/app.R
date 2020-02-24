#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(fs)

(confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))

(recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

(death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))

confirmed_long <- confirmed %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "confirmed") %>%
    mutate(Date = (mdy(Date))) # convert string to date-time

recovered_long <- recovered %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "recovered") %>%
    mutate(Date = mdy(Date))

death_long <- death %>%
    pivot_longer(-(`Province/State`:Long), 
                 names_to = "Date", 
                 values_to = "death") %>%
    mutate(Date = mdy(Date))

ncov_tbl <- confirmed_long %>%
    left_join(recovered_long) %>%
    left_join(death_long) %>%
    pivot_longer(confirmed:death, 
                 names_to = "Case", 
                 values_to = "Count")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Coronavirus Disease 2019 (COVID-19) Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(date, "date")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
