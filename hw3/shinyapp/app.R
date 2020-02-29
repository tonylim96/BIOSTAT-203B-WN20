# Library ----

# List of required packages
packages <- c("cowplot", "maps", "scales", "shinythemes", "usmap", "wesanderson")

# Check if packages are installed 
# https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

library(cowplot)
library(fs)
library(ggplot2)
library(grid)
library(lubridate)
library(maps)
library(quantmod)
library(scales)
library(sf)
library(shiny)
library(shinythemes)
library(tidyverse)
library(usmap)
library(wesanderson)

# Function with opposite role of %in%
'%ni%' <- Negate('%in%')
c(1,3,11) %ni% 1:10

# Steps to create ncov_tbl
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

# Steps to form China map
chn_map <- st_read("./bou2_4p.shp", as_tibble = TRUE) %>%
    mutate(NAME = iconv(NAME, from = "GBK"),
           BOU2_4M_ = as.integer(BOU2_4M_),
           BOU2_4M_ID = as.integer(BOU2_4M_ID)) %>%
    mutate(NAME = str_replace_na(NAME, replacement = "澳门特别行政区"))

translate <- function(x) {
    sapply(x, function(chn_name) {
        if (str_detect(chn_name, "澳门")) {
            eng_name <- "Macau"
        } else if (str_detect(chn_name, "台湾")) {
            eng_name <- "Taiwan"
        } else if (str_detect(chn_name, "上海")) {
            eng_name <- "Shanghai"
        } else if (str_detect(chn_name, "云南")) {
            eng_name <- "Yunnan"
        } else if (str_detect(chn_name, "内蒙古")) {
            eng_name <- "Inner Mongolia"
        } else if (str_detect(chn_name, "北京")) {
            eng_name <- "Beijing"
        } else if (str_detect(chn_name, "台湾")) {
            eng_name <- "Taiwan"
        } else if (str_detect(chn_name, "吉林")) {
            eng_name <- "Jilin"
        } else if (str_detect(chn_name, "四川")) {
            eng_name <- "Sichuan"
        } else if (str_detect(chn_name, "天津")) {
            eng_name <- "Tianjin"
        } else if (str_detect(chn_name, "宁夏")) {
            eng_name <- "Ningxia"
        } else if (str_detect(chn_name, "安徽")) {
            eng_name <- "Anhui"
        } else if (str_detect(chn_name, "山东")) {
            eng_name <- "Shandong"
        } else if (str_detect(chn_name, "山西")) {
            eng_name <- "Shanxi"
        } else if (str_detect(chn_name, "广东")) {
            eng_name <- "Guangdong"
        } else if (str_detect(chn_name, "广西")) {
            eng_name <- "Guangxi"
        } else if (str_detect(chn_name, "新疆")) {
            eng_name <- "Xinjiang"
        } else if (str_detect(chn_name, "江苏")) {
            eng_name <- "Jiangsu"
        } else if (str_detect(chn_name, "江西")) {
            eng_name <- "Jiangxi"
        } else if (str_detect(chn_name, "河北")) {
            eng_name <- "Hebei"
        } else if (str_detect(chn_name, "河南")) {
            eng_name <- "Henan"
        } else if (str_detect(chn_name, "浙江")) {
            eng_name <- "Zhejiang"
        } else if (str_detect(chn_name, "海南")) {
            eng_name <- "Hainan"
        } else if (str_detect(chn_name, "湖北")) {
            eng_name <- "Hubei"
        } else if (str_detect(chn_name, "湖南")) {
            eng_name <- "Hunan"
        } else if (str_detect(chn_name, "甘肃")) {
            eng_name <- "Gansu"
        } else if (str_detect(chn_name, "福建")) {
            eng_name <- "Fujian"
        } else if (str_detect(chn_name, "西藏")) {
            eng_name <- "Tibet"
        } else if (str_detect(chn_name, "贵州")) {
            eng_name <- "Guizhou"
        } else if (str_detect(chn_name, "辽宁")) {
            eng_name <- "Liaoning"
        } else if (str_detect(chn_name, "重庆")) {
            eng_name <- "Chongqing"
        } else if (str_detect(chn_name, "陕西")) {
            eng_name <- "Shanxi"
        } else if (str_detect(chn_name, "青海")) {
            eng_name <- "Qinghai"
        } else if (str_detect(chn_name, "香港")) {
            eng_name <- "Hong Kong"
        } else if (str_detect(chn_name, "黑龙江")) {
            eng_name <- "Heilongjiang"
        } else {
            eng_name <- chn_name
        }
        return(eng_name)
    })
}

chn_prov <- chn_map %>% 
    count(NAME) %>%
    mutate(NAME_ENG = translate(NAME))

# UI ----
ui <- fluidPage(
    theme = shinytheme("paper"),

    titlePanel("Coronavirus Disease 2019 (COVID-19)"),
    h6(a(" Data source: JHU CCSE", href = "https://github.com/CSSEGISandData/COVID-19")),

    sidebarLayout(
        
        sidebarPanel(
            style = "position:fixed;width:inherit;",
            
            dateRangeInput(inputId = "date_id", 
                           label = "Date range:", 
                           max = Sys.Date(), 
                           start = min(ncov_tbl$Date), 
                           end = Sys.Date() - 1), 
            
            radioButtons(inputId = "country_id", 
                               label = "Countries:", 
                               choices = c("China", "United States", "Other")),
            
            radioButtons(inputId = "case_id",
                         label = "Case status:",
                         choices = c("Confirmed", "Death", "Recovered")),
            
            helpText("Select an index to examine. 
               Information will be collected from Yahoo Finance."),
            
            selectInput(inputId = "index_id", 
                               label = "Indices:", 
                               choices = c("Dow Jones Industrial Average (^DJI)",
                                           "FTSE 100 (^FTSE)",
                                           "Hang Seng Index (^HSI)",
                                           "KOSPI Composite Index (^KS11)",
                                           "NASDAQ (^IXIC)",
                                           "Nikkei 225 (^N225)",
                                           "Russell 2000 (^RUT)",
                                           "SSE Composite Index (^SSEC)",
                                           "STI Index (^STI)",
                                           "S&P 500 (^GSPC)",
                                           "TSEC Weighted Index (^TWII)")),
            ),

        mainPanel(
            plotOutput("map"),
            plotOutput("bargraph"), 
            plotOutput("line"),
            plotOutput("index")
        )
        
        # Tabs
        # mainPanel(
        #     fluidPage(tabsetPanel(
        #         tabPanel("Bar Graph", plotOutput("bargraph")),
        #         tabPanel("Map", plotOutput("map")),
        #         tabPanel("Index", plotOutput("index"))
        #     ))
        # )
    )
)

# Server ----
server <- function(input, output) {

# Maps ----
    output$map <- renderPlot({
        if (input$country_id == "China") {
            ncov_tbl %>%
                filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                               "Hong Kong", "Taiwan"),
                       Date == input$date_id[2], 
                       Case == tolower(input$case_id)) %>%
                group_by(`Province/State`) %>%  
                top_n(1, Date) %>%
                right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>%
                ggplot() +
                geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
                scale_fill_gradientn(colors = wes_palette("Zissou1", 
                                                          100, 
                                                          type = "continuous"),
                                     trans = "log10") +
                labs(title = str_c(input$case_id, 
                                   " cases in ", 
                                   input$country_id), 
                     subtitle = format(input$date_id[2], 
                                       format = "%b %d, %Y")) +
                theme_map() + 
                theme(plot.title = element_text(face = "plain"))
        } else if (input$country_id =="United States") {
            state <- ncov_tbl %>%
                separate(col = "Province/State", 
                         into = c("City", "state"), 
                         sep = ", ") %>%
                filter(`Country/Region` %in% c("US"), 
                       `Date` == input$date_id[2],
                       `Case` == tolower(input$case_id),
                       `state` %in% state.abb) %>%
                group_by(state) %>%
                summarise(n = n_distinct(state), Count = sum(Count))
            
            plot_usmap(regions = "states", 
                       data = state,
                       values = "Count",
                       color = "black") +
                scale_fill_gradientn(name = "Count", 
                                     colors = wes_palette("Zissou1", 
                                                          100, 
                                                          type = "continuous")) +
                # scale_fill_continuous(name = "Count", label = scales::comma) +
                theme(legend.position = "right") +
                labs(title = str_c(input$case_id, 
                                   " cases in ", 
                                   input$country_id), 
                     subtitle = format(input$date_id[2], 
                                       format = "%b %d, %Y")) 
        }
    })

# Bar graphs ----
    output$bargraph <- renderPlot({
        if (input$country_id == "China") {
            ncov_tbl %>%
                filter(`Country/Region` %in% c("Mainland China", "Macau",
                                               "Hong Kong", "Taiwan"),
                       `Date` == input$date_id[2]) %>%
                group_by(`Province/State`) %>%
                ggplot(mapping = aes(
                    x = `Province/State`,
                    y = `Count`,
                    fill = `Case`)) +
                geom_col(position = "dodge") +
                scale_fill_manual("Case status", 
                                  labels = c("Confirmed", "Death", "Recovered"),
                                  values = c("#FAD50F", "#CB2313", "#273046")) +
                scale_y_log10() +
                labs(title = str_c("COVID-19 data for ",
                                   input$country_id,
                                   sep = ""),
                     subtitle = str_c(format(min(ncov_tbl$Date),
                                             format = "%b %d, %Y"),
                                      " - ",
                                      format(input$date_id[2],
                                             format = "%b %d, %Y"),
                                      sep = ""),
                     x = "Province",
                     y = "log(Count)") +
                theme_light() +
                theme(axis.text.x = element_text(angle = 90))
            } else if (input$country_id == "United States") {
                ncov_tbl %>%
                    filter(`Country/Region` %in% c("US"), 
                           `Date` == input$date_id[2]) %>%
                    separate(col = "Province/State", 
                             into = c("City", "State"), 
                             sep = ", ") %>%
                    group_by(`State`) %>%
                    ggplot() +
                    geom_col(mapping = aes(
                        x = `State`, 
                        y = `Count`, 
                        fill = `Case`)) + 
                    scale_fill_manual("Case status", 
                                      labels = c("Confirmed", 
                                                 "Death", 
                                                 "Recovered"),
                                      values = c("#FAD50F", 
                                                 "#CB2313", 
                                                 "#273046")) +
                    # scale_y_log10() +
                    labs(title = str_c("COVID-19 data for ", 
                                       input$country_id,
                                       sep = ""), 
                         subtitle = str_c(format(min(ncov_tbl$Date), 
                                                 format = "%b %d, %Y"),
                                          " - ",
                                          format(input$date_id[2], 
                                                 format = "%b %d, %Y"),
                                          sep = ""),
                         x = "State") + 
                    theme_light() +
                    theme(axis.text.x = element_text(angle = 90))
                } else if (input$country_id == "Other") {
                    ncov_tbl %>%
                        filter(`Country/Region` %ni% c("Mainland China", 
                                                       "Macau",
                                                       "Hong Kong", 
                                                       "Taiwan",
                                                       "US"), 
                               `Date` == input$date_id[2]) %>%
                        group_by(`Country/Region`) %>%
                        arrange(desc(`Count`)) %>%
                        # summarise(total_count = sum(`Count`)) %>%
                        # filter(rank(desc(`total_count`)) < 6) %>%
                        ggplot() +
                        geom_col(mapping = aes(
                            x = `Country/Region`, 
                            y = `Count`, 
                            fill = `Case`)) + 
                        scale_fill_manual("Case status", 
                                          labels = c("Confirmed", 
                                                     "Death", 
                                                     "Recovered"),
                                          values = c("#FAD50F", 
                                                     "#CB2313", 
                                                     "#273046")) +
                        # scale_y_log10() +
                        labs(title = str_c("COVID-19 data for ", 
                                           input$country_id,
                                           sep = ""), 
                             subtitle = str_c(format(min(ncov_tbl$Date), 
                                                     format = "%b %d, %Y"),
                                              " - ",
                                              format(input$date_id[2], 
                                                     format = "%b %d, %Y"),
                                              sep = "")) +  
                        theme_light() +
                        theme(axis.text.x = element_text(angle = 90))
                    }
        })
    
# Line ----
    output$line <- renderPlot({
        if (input$country_id == "China") {
            ncov_tbl %>%
                filter(`Country/Region` %in% c("Mainland China", 
                                               "Macau", 
                                               "Hong Kong", 
                                               "Taiwan")) %>%
                group_by(Date, Case) %>%  
                summarise(total_count = sum(Count)) %>%
                subset(Date >= input$date_id[1] & Date <= input$date_id[2]) %>%
                ggplot() +
                geom_line(mapping = aes(x = Date, 
                                        y = total_count, 
                                        color = Case), 
                          size = 2) + 
                scale_colour_manual("Case status", 
                                  labels = c("Confirmed", "Death", "Recovered"),
                                  values = c("#FAD50F", "#CB2313", "#273046")) + 
                scale_y_log10() + 
                labs(title = str_c("COVID-19 data for ", 
                                   input$country_id,
                                   " by case status over time",
                                   sep = ""), 
                     subtitle = str_c(format(min(ncov_tbl$Date), 
                                             format = "%b %d, %Y"),
                                      " - ",
                                      format(input$date_id[2], 
                                             format = "%b %d, %Y"),
                                      sep = ""),
                     y = "log(Count)") +
                theme_light()
        } else if (input$country_id == "United States") {
            ncov_tbl %>%
                filter(`Country/Region` %in% c("US")) %>%
                group_by(Date, Case) %>%  
                summarise(total_count = sum(Count)) %>%
                subset(Date >= input$date_id[1] & Date <= input$date_id[2]) %>%
                ggplot() +
                geom_line(mapping = aes(x = Date, 
                                        y = total_count, 
                                        color = Case), 
                          size = 2) + 
                scale_colour_manual("Case status", 
                                    labels = c("Confirmed", 
                                               "Death", 
                                               "Recovered"),
                                    values = c("#FAD50F", 
                                               "#CB2313", 
                                               "#273046")) + 
                scale_y_log10() + 
                labs(title = str_c("COVID-19 data for ", 
                                   input$country_id,
                                   " by case status over time",
                                   sep = ""), 
                     subtitle = str_c(format(min(ncov_tbl$Date), 
                                             format = "%b %d, %Y"),
                                      " - ",
                                      format(input$date_id[2], 
                                             format = "%b %d, %Y"),
                                      sep = ""),
                     y = "log(Count)") +
                theme_light()
        } else if (input$country_id == "Other") {
            ncov_tbl %>%
                filter(`Country/Region` %ni% c("Mainland China", 
                                               "Macau",
                                               "Hong Kong", 
                                               "Taiwan", 
                                               "US")) %>%
                group_by(Date, Case) %>% 
                summarise(total_count = sum(Count)) %>%
                subset(Date >= input$date_id[1] & Date <= input$date_id[2]) %>%
                ggplot() +
                geom_line(mapping = aes(x = Date, 
                                        y = total_count, 
                                        color = Case), 
                          size = 2) +
                scale_colour_manual("Case status",
                                    labels = c("Confirmed", 
                                               "Death", 
                                               "Recovered"),
                                    values = c("#FAD50F", 
                                               "#CB2313", 
                                               "#273046")) + 
                scale_y_log10() + 
                labs(title = str_c("COVID-19 data for ", 
                                   input$country_id,
                                   " by case status over time",
                                   sep = ""),
                     subtitle = str_c(format(min(ncov_tbl$Date),
                                             format = "%b %d, %Y"),
                                      " - ",
                                      format(input$date_id[2],
                                             format = "%b %d, %Y"),
                                      sep = ""),
                     y = "log(Count)") +
                theme_light()
            }
    })

# Indices ----
    output$index <- renderPlot({
        a <- regmatches(input$index_id,
                        gregexpr("(?<=\\().*?(?=\\))",
                                 input$index_id,
                                 perl = T))[[1]]

        b <- str_c(str_remove(a, "[^[:punct:]]"), ".Adjusted", sep = "")

        getSymbols(a,
                   src = "yahoo",
                   auto.assign = FALSE,
                   from = input$date_id[1],
                   to = input$date_id[2]) %>%
            as_tibble(rownames = "Date") %>%
            mutate(Date = date(Date)) %>%
            ggplot() +
            labs(title = input$index_id,
                 subtitle = str_c(format(min(ncov_tbl$Date),
                                         format = "%b %d, %Y"),
                                  " - ",
                                  format(input$date_id[2],
                                         format = "%b %d, %Y"),
                                  sep = ""),
                 y = b) +
            geom_line(mapping = aes(x = Date, y = eval(parse(text = b)))) +
            theme_light()
    })
}

# Run ----
shinyApp(ui = ui, server = server)