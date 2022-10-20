library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
Sys.setlocale('LC_TIME', 'English')


options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(

  titlePanel('Traffic Data Visualization Tool'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload the files for evaluation", multiple = TRUE),
      numericInput('id', 'Enter the ID to display diagram (Loop detector data: detector ID; Floating car data: link ID): ', '0'),
      textInput('start', 'For floating car data only: enter the time range to display in the following format. Time start:',value = '2021-06-20 00:00:00'),
      textInput('end', 'Time end: ',value = '2021-06-20 23:59:59')
    ),
    mainPanel(
      tabsetPanel(
        id = 'tabset',
        tabPanel('Loop Detector Data', plotOutput('count1')),
        tabPanel('Floating Car Data', plotOutput('count2')))
    )
  )
)



# Define server logic ----
server <- function(input, output) {
  
  output$count1 <- renderPlot({
    req(input$file)
    req(input$id)
    dt <- rbindlist(lapply(input$file$datapath, fread))
    list_dt <- split(dt,dt$detid)

    colors <- c('time-flow' = 'orange', 'time-occupancy' = 'blue')
    ggplot(list_dt[[paste0(input$id)]], aes(x=time)) +
      geom_line( aes(y=occ, color = 'time-occupancy'), size=0.5) +
      geom_line( aes(y=flow, color = 'time-flow'), size=0.5) + 
      scale_y_continuous('flow [veh]', sec.axis = sec_axis(~., name = 'occupancy [%]')) +
      labs(x = 'time [date hh:mm]', title = 'Diagram of time-flow and time-occupancy relationships', subtitle = input$id, color = 'lines') + 
      scale_color_manual(values = colors)
  })
  
  output$count2 <- renderPlot({
    req(input$file)
    req(input$id)
    dt <- fread(input$file$datapath)
    dt[, localDate:= as.POSIXct(dt$localDate)]
    df <- as.data.frame(dt)
    list_dt <- split(df,df$statistikid)

    colors <- c('Actual Speed' = 'orange', 'Average Speed' = 'blue', 'Reference Speed' = 'green')
    fills <- c('Score' = 'darkgrey')
    ggplot(list_dt[[paste0(input$id)]], aes(x=localDate)) +
      geom_col(aes(y=score, fill = 'Score')) + 
      geom_point(aes(y=speed, color = 'Actual Speed'), size=0.5) + 
      geom_line(aes(y=avgspeed, color = 'Average Speed'), size=0.5) +
      geom_line(aes(y=refspeed, color = 'Reference Speed'), size=0.5) + 
      labs(x0 = 'time [date hh:mm]', y = 'speed [km/h]', title = 'Diagram of speed', subtitle = input$id) + 
      scale_color_manual(name = 'Legend of points and lines', values = colors,) +
      scale_fill_manual(name = 'Legend of bars', values = fills)+
      scale_x_datetime(limit=c(as.POSIXct(paste0(input$start)), as.POSIXct(paste0(input$end))))
    
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)
