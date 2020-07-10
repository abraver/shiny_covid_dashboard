library("tidyverse")
library("readxl")
library("lubridate")
library("magrittr")
# library("grid")
library("shiny")
library("rsconnect")
library("gghighlight")
library("cowplot")

new_data <- read_csv("https://www.dropbox.com/s/ahhveyrj7emn0bz/covid_newdata.csv?dl=1") %>% 
  mutate(date = mdy(date))

mytheme <- theme(
  #   text = element_text(size=16, family="Avenir Next"),
  text = element_text(size=16),
  plot.title = element_text(face="bold",hjust = 0.5),
  plot.subtitle = element_text(face="bold",hjust = 0.5, vjust=8),
  axis.text.x = element_text(color="black", size=rel(1.4)),
  panel.background = element_rect(fill="white"),
  panel.grid.major.y=element_line(color=grey(0.85), size=.1),
  panel.grid.major.x=element_blank(),
  axis.ticks.x=element_blank(),
  axis.title=element_text(size=rel(1.4))
)


# Merge the new stuff we have from my Dropbox spreadsheet with the original lub data
lub_data <- read_csv("./data/lub_data_intermediate.csv") #the original lub data
lub_data <- full_join(select(new_data, date, cases), lub_data, by = c("date", "cases"))
lub_data <- arrange(lub_data, date)

# Add 7-day average and weekend var
lub_data %<>%
  mutate(sevendayavg = zoo::rollmean(cases, 7, align = "right", fill = NA),
         weekend = ifelse(wday(date) %in% c("1", "7"), 1, 0),
         weekend = as_factor(weekend))


# Compute stuff for lub_data
lub_data %<>%
  mutate(lagged1 = lag(cases, n=1),
         delta1 = cases - lagged1,
         sevendaydelta = zoo::rollmean(delta1, 7, align = "right", fill = NA),
         month = month(date, label = TRUE, abbr = TRUE),
         day = day(date),
         label = if_else(day == 1, paste(month, day), paste(day)),
         Specific = ifelse(date == max(lub_data$date), 1, 0))


# Test data ---------------------------------------------------------------
combined_test_data <- read_csv("./data/combined_test_data.csv")
combined_test_data <- full_join(combined_test_data, select(new_data, date, cases, lub_test_total, lub_test_daily, lub_test_positive, lub_test_negative, lub_test_pending), by = "date")

test_ratio <- inner_join(lub_data, combined_test_data, by = c("date")) %>% 
  arrange(date) %>% 
  mutate(cases = ifelse(is.na(cases.x), cases.y, cases.x)) %>% 
  select(-c(cases.x, cases.y)) %>% 
  select(date, cases, everything())

test_ratio <-
  mutate(test_ratio,
         txtestratio = 100*delta1/state_test_daily,
         aarontestratio = 100*delta1/aaron_test_daily,
         lubtestratio = 100*delta1/lub_test_daily,
         lub_test_daily_positive = (lub_test_positive - lag(lub_test_positive, 1)),
         lub_test_percent_positive  = 100*(lub_test_daily_positive / lub_test_daily)) %>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>%
  mutate_at(vars(ends_with("testratio")), ~ ifelse(. > 50, NA, .))






# UI ----------------------------------------------------------------------

ui <- fluidPage(title = "Lub Covid Tracker",
                titlePanel(h1("LubCovidTracker", align = "center")),
                
                # fluidRow(column(12, offset = 0, verbatimTextOutput("the_row"))),
                
                fluidRow(
                  column(6, offset = 3, align = "center",
                         selectInput("plot", label = "",
                                     choices = list("Cases by day with 7-day rolling average" = "cases", "Percent of tests reported positive" = "tests"),
                                     selected = "cases",
                                     width = "400px"))
                ),
                
                conditionalPanel(condition = "input.plot == 'cases'",
                                 fluidRow(
                                   column(8, offset = 2,
                                          h3("New COVID cases in Lubbock by day", align = "center"),
                                          p("With 7 day rolling average", align = "center", style = "font-size:14pt"),
                                          p("Click a day for more information", align = "center", style = "font-size:8pt")
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          div(style = "position:relative",
                                              plotOutput("thegraph",
                                                         # hover = hoverOpts("cases_hover", delay = 0, delayType = "debounce"),
                                                         click = "cases_click")
                                              # ,
                                              # uiOutput("hover_info")
                                          )
                                   ),
                                   
                                 ),
                                 
                                 
                                 
                                 
                                 
                                 
                                 # verbatimTextOutput("x_value"),
                                 # verbatimTextOutput("selected_rows"),
                                 uiOutput("moreInfoBox"),
                                 
                                 
                                 
                                 fluidRow(
                                   column(12, align = "center",
                                          sliderInput("range", 
                                                      label = "Date range:",
                                                      min = ymd("2020-04-01"), max = max(lub_data$date), value = c(ymd("2020-04-01"), max(lub_data$date)),
                                                      timeFormat = "%b %d, %Y",
                                                      ticks = FALSE))
                                 ),
                                 fluidRow(
                                   column(6, offset=4, align = "left",
                                          checkboxInput("display_rolling", label = "Display rolling average values above line", value = FALSE),
                                          div(style = "margin-top:-1em", checkboxInput("display_cases", label = "Display daily cases above bars", value = TRUE))
                                   )
                                 ),
                                 fluidRow(
                                   column(2, offset = 5, align = "center",
                                          actionButton("showCasesModal", "About this data"))
                                 )
                ),
                
                
                conditionalPanel(condition = "input.plot == 'tests'",  
                                 fluidRow(
                                   column(8, offset = 2,
                                          h3("Percent of tests reported positive", align = "center")
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(8, offset=2, align = "center",
                                          p(tags$b("BEWARE:"), tags$i("The data for this graph is extremely dubious!"))
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(9, offset=2, align = "left",
                                          p("Numbers of tests reported comes from three sources.  Data labeled TX DSHS comes from",
                                            a(href = "https://dshs.texas.gov/coronavirus/additionaldata/", "the Texas Department of State Health Services", .noWS="after"),
                                            ".  Data labeled as 'estimated from Lubbock' comes from", tags$i("my own visual estimates"), "of the bar graph provided by",
                                            a(href="https://ci.lubbock.tx.us/departments/health-department/about-us/covid-19", "the Lubbock Health Department", .noWS="outside"),
                                            ".  Data labeled as 'Lubbock reported' comes directly from",
                                            a(href="https://ci.lubbock.tx.us/departments/health-department/about-us/covid-19", "the Lubbock Health Department", .noWS="after"),
                                            ".  This 'Lubbock reported' data isn't reported for past days, so it is not available before I began manually collecting it each day in early June."
                                          ),
                                          p("The DSHS data is dubious because some dates seem to suggest that more than 100% of tests were positive (and indeed, one day is as high as 211%!). The \"max percent positive\" slider eliminates any days where the percent of positive tests is above the value of the slider.  Days with 0 reported tests are also omitted."
                                          )
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(12,
                                          plotOutput("ratiograph")
                                   )
                                 ),
                                 
                                 
                                 fluidRow(
                                   column(12, align = "center",
                                          sliderInput("testrange", 
                                                      label = "Date range:",
                                                      min = min(test_ratio$date), max = max(test_ratio$date), value = c(ymd("2020-04-01"), max(test_ratio$date)),
                                                      timeFormat = "%b %d, %Y",
                                                      ticks = FALSE))
                                 ),
                                 fluidRow(
                                   column(12, align = "center",
                                          sliderInput("sanity", label = "Max percent positive", min = 20, 
                                                      max = 50, value = 100, post = "%")
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(8, offset=3, align = "left",
                                          checkboxInput("ratio_DSHS", label = "Data from TX DSHS (available 4/1/20–6/5/20)", value = TRUE, width = "100%"),
                                          div(style = "margin-top:-1em", checkboxInput("ratio_aaron", label = "Data estimated from City of Lubbock (available 4/1/20–6/5/20)", value = TRUE, width = "100%"), width = "100%"),
                                          div(style = "margin-top:-1em", checkboxInput("ratio_lub", label = "Data reported by City of Lubbock (available from 6/4/20 onwards)", value = TRUE, width = "100%"), width = "100%")
                                   )
                                 ),
                                 
                                 
                ),
                fluidRow(
                  column(8, offset = 2,
                         br(),
                         div("Who made this and should I trust it?", style = "font-weight:bold; text-align: center; margin-bottom:1ex"),
                         p("This app was made by",
                           a(href="http://www.aaronbraver.com", "Aaron Braver", .noWS = "after"),
                           ", an Associate Professor of linguistics at Texas Tech University.",
                           tags$i("You probably shouldn't trust this", .noWS = "after"),
                           ": I am not an epidemiologist, infections disease expert, or a medical professional of any kind.  I made this app for fun, and you should not use it as the basis for any medical decisions." 
                         )
                  )
                )
) # endFluidPage of the UI



# Server logic ----
server <- function(input, output) {
  
  
  # output$thisdateinfo <- renderPrint({
  #   h3(clickdata$x)
  # })
  
  # Print the name of the x value
  
  # output$x_value <- renderPrint({
  #   if (is.null(input$cases_click$x)) return()
  #   # lub_data$date[round(input$cases_click$x)]
  #   
  #   paste0(input$cases_click$x, " ", as_date(round(input$cases_click$x, 0)))
  # })
  # 
  # 
  # 
  # output$selected_rows <- renderPrint({
  #   if(is.null(input$cases_click$x)) return ()
  #   
  #   keeprows <- as_date(round(input$cases_click$x)) == lub_data$date
  #   lub_data[keeprows,]
  # })
  
  
  
  clickdata <- reactiveValues(x = max(lub_data$date), moreInfoBox = "")
  
  prettyDateInfo <- function(theData) {  
    this_date_lub_data <- filter(theData, Specific == 1)
    this_date_test_data <- select(filter(test_ratio, date == this_date_lub_data$date), date, lub_test_daily, lub_test_positive, lub_test_negative, lub_test_pending, lub_test_percent_positive)
    this_date_all_data <- left_join(this_date_lub_data, this_date_test_data, by = "date")
    this_date_pretty <- tibble(
      Date = format(this_date_all_data$date, "%b %d, %Y"),
      `New cases` = as.integer(this_date_all_data$delta1),
      # `New tests reported` = as.integer(this_date_all_data$lub_test_daily),
      `7-day avg` = this_date_all_data$sevendaydelta,
      `Pct tests positive` = paste0(round(this_date_all_data$lub_test_percent_positive,2), "%")
    )
    if (this_date_pretty$`Pct tests positive` == "NA%") {
      this_date_pretty <- select(this_date_pretty, -`Pct tests positive`)
    }
    output$moreInfoBox <- renderUI({
      fluidRow(
        column(12, align = "center",
               renderTable({this_date_pretty},  
                           rownames = FALSE,
                           align = "c")
        )
      )
    })
  }  
  
  
  
  
  prettyDateInfo(lub_data)
  
  
  
  observe({
    req(input$cases_click)
    clickdata$x <- input$cases_click$x
    # this_date_lub_data <- filter(filtered_cases(), Specific == 1)
    # this_date_test_data <- select(filter(test_ratio, date == this_date_lub_data$date), date, lub_test_daily, lub_test_positive, lub_test_negative, lub_test_pending, lub_test_percent_positive)
    # this_date_all_data <- left_join(this_date_lub_data, this_date_test_data, by = "date")
    # this_date_pretty <- tibble(
    #   Date = format(this_date_all_data$date, "%b %d, %Y"),
    #   `New cases` = as.integer(this_date_all_data$delta1),
    #   # `New tests reported` = as.integer(this_date_all_data$lub_test_daily),
    #   `7-day avg` = this_date_all_data$sevendaydelta,
    #   `Pct tests positive` = paste0(round(this_date_all_data$lub_test_percent_positive,2), "%")
    # )
    # if (this_date_pretty$`Pct tests positive` == "NA%") {
    #   this_date_pretty <- select(this_date_pretty, -`Pct tests positive`)
    # }
    # output$moreInfoBox <- renderUI({
    #     fluidRow(
    #       column(12, align = "center",
    #              renderTable({this_date_pretty},  
    #                          rownames = FALSE,
    #                          align = "c")
    #              )
    #     )
    #   })
    prettyDateInfo(filtered_cases())
  })  
  
  
  
  
  
  filtered_cases <- reactive({
    # if(is_empty(clickdata$x)) {
    #   # lub_data$Specific <- 0
    #   # lub_data <- mutate(lub_data, Specific = ifelse(date == max(lub_data$date), 1, 0))
    #   return(lub_data)
    # } else {
    print(clickdata$x)
    print("AAAAAAAAAAAAAAAAAA")
    keeprows <- as_date(round(clickdata$x)) == lub_data$date
    keepdates <- lub_data[keeprows, ]$date
    # }
    # if (is_empty(keepdates)) {
    #   lub_data$Specific <- 0
    # } else {
    lub_data$Specific <- keeprows
    # }
    output$filtered <- renderPrint({clickdata$x})
    return(lub_data)
  })
  
  
  
  
  output$thegraph <- renderPlot({
    ggplot(filtered_cases(), aes(x = date, y = delta1)) +
      geom_col(alpha = .25, aes(fill = Specific)) +
      geom_line(aes(y = (sevendaydelta)), alpha = 1, fill = "black") +
      {if(input$display_cases) geom_text(aes(label = delta1), alpha = .25, size = 2, nudge_y=1)} +
      {if(input$display_rolling) geom_text(aes(label = round(sevendaydelta,0), y=sevendaydelta), alpha = 1, size = 2, nudge_y=1)} +
      scale_x_date(date_breaks = "1 month",date_labels = "%b") +
      scale_fill_manual(values = c("black", "blue"), guide = FALSE) +
      xlim(input$range[1], input$range[2]+1) +
      ylab("New cases") +
      xlab(element_blank()) +
      # coord_cartesian(clip = "off") +
      theme(
        panel.background = element_rect(fill="white"),
        panel.grid.major.y=element_line(color=grey(0.85), size=.1),
        panel.grid.major.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(margin = margin(t = -5), hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      geom_text(aes(x = date, y = 0, label = day), color = "grey", vjust = "top", size = 3, alpha = .5)
    
  })
  
  
  output$ratiograph <- renderPlot({
    
    test_ratio <- mutate_at(test_ratio, vars(ends_with("testratio")), ~ ifelse(. > input$sanity, NA, .))
    
    ggplot(test_ratio, aes(x = date, y = delta1)) +
      {if(input$ratio_aaron) geom_line(aes(y = aarontestratio, linetype = "aaron"), alpha = 1, color = "black") else geom_blank(aes(linetype = "aaron"))} +
      {if(input$ratio_DSHS) geom_line(aes(y = txtestratio, linetype = "tx"), alpha = 1, color = "gray50") else geom_blank(aes(linetype = "tx"))} +
      {if(input$ratio_lub) geom_line(aes(y = lubtestratio, linetype = "lub"), alpha = 1, color = "gray20") else geom_blank(aes(linetype = "tx"))} +
      scale_x_date(date_breaks = "1 month",date_labels = "%b", limits = c(ymd(20200401), NA)) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      scale_linetype_manual(values = c("dashed", "solid", "dotted"), labels = c("Estimated from Lubbock", "Lubbock reported data", "TX DSHS data"), name = "Data source:   ") +
      # ylim(0,40) +
      xlim(input$testrange[1]-1, input$testrange[2]+1) +
      ylab("% of tests positive") +
      xlab(element_blank()) +
      # coord_cartesian(clip = "off") +
      theme(
        panel.background = element_rect(fill="white"),
        panel.grid.major.y=element_line(color=grey(0.85), size=.1),
        panel.grid.major.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(margin = margin(t = -5), hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.key.width=unit(10,"char")
      ) +
      guides(linetype = guide_legend(
        label.position = "bottom",
        title.position = "left", title.hjust = 0.5)) +
      geom_text(aes(x = date, y = 0, label = day, color = as_factor(weekend)), color = "grey", vjust = "top", size = 3, alpha = .5)    })
  
  
  observeEvent(input$showCasesModal, {
    showModal(modalDialog(
      title = "About this data",
      p("Case count data from before 6/4/20 comes from",
        a(href = "https://dshs.texas.gov/coronavirus/additionaldata/", "the Texas Department of State Health Services"),
        "and is shifted by one day to match the data reported by",
        a(href="https://ci.lubbock.tx.us/departments/health-department/about-us/covid-19", "the Lubbock Health Department", .noWS = "after"),
        ", which is used for data from 6/4/20 onwards."
      )
      ,
      p("Rolling average values, when displayed are rounded, but the black line tracking the rolling average is not."),
      easyClose = TRUE
    ))
  })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

# Run app ----
shinyApp(ui, server)

