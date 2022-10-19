#install.packages("shiny")
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("scales")

library(shiny)
library(dplyr)
library(data.table)
library(scales)

options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  
  titlePanel('Traffic Data Evaluation Tool'),
  
  sidebarLayout(
    sidebarPanel(
      h5('This tool is to evaluate the traffic data quality based on 5 quality features for loop detectors: availability, completeness, consistency, correctness, metric accuracy (based on the occupancy-flow model). For floating car data, assessment of the data score is possible.'),
      br(),
      fileInput("file", "upload the files for evaluation", multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        id = 'tabset',
        tabPanel('Availability', textOutput('count1')),
        tabPanel('Completeness', textOutput('count2')),
        tabPanel('Consistency', textOutput('count3')),
        tabPanel('Correctness', textOutput('count4'), br(), dataTableOutput('table')),
        tabPanel('Accuracy', textOutput('count5'), br(), textOutput('count6')),
        tabPanel('Floating Car Data', textOutput('count7'), br(), dataTableOutput('table2'))
      )
    )
  )
)


server <- function(input, output,session) {
  
  # data.table
  myfiles <- reactive({
    rbindlist(lapply(input$file$datapath, fread, na.strings = ''))
  })
  
  # 1. Availability: count missing data per day by name (datetime) of uploaded files 
  output$count1 <- renderText({ 
    req(input$file)
    num_days <- 0
    filelist <- data.frame(name = input$file$name)
    filelist$name <- sub("\\.csv$",'', as.character(filelist$name))
    filelist$name <- as.POSIXct(filelist$name, format = '%Y-%m-%dT%H_%M_%S')
    
    min <- first(filelist$name)
    max <- last(filelist$name)
    
    num_days <- as.integer(difftime(max, min, units = c('days')))
    df_regular <- seq.POSIXt(min, max, by = '15 min')
    
    # if the data is within 1 day
    if(num_days < 1){
      num_missing <- length(df_regular) - length(filelist$name)
      num_missing_perday <- num_missing
      percent_avai <- 1 - num_missing / length(df_regular)
    }
    
    # more than 1 day: minus 1 for the constant missing data 00:00:00 
    else{
      num_missing <- length(df_regular) - num_days - 1 - length(filelist$name)
      num_missing_perday <- num_missing / num_days
      percent_avai <- 1 - (num_missing) / (length(df_regular) - num_days)
    }
    
    paste('The availability rate is: ', percent(percent_avai,accuracy = 0.0001), '. The number of missing data per day is: ', num_missing_perday, ' (The traffic data are recorded every 15 minutes and 95 sets of traffic data are obtained every day).')
    
  })
  
  
  
  # 2. Completeness: count missing values in each file
  output$count2 <- renderText({ 
    req(input$file)
    df_myfiles <- as.data.frame(myfiles())
    filelist <- data.frame(name = input$file$name)
    value_miss <- sum(is.na(df_myfiles))
    value_miss_p <- value_miss/length(filelist$name)
    percent_comp <- 1 - value_miss / nrow(df_myfiles)
    paste('the completeness rate is: ', percent(percent_comp,accuracy = 0.0001), '. The number of missing value per data file is: ', value_miss_p, ' (Each data file contain records of 645 detectors).')
    
  })
  
  # 3. Consistency: whether format of each file complies  
  output$count3 <- renderText({ 
    req(input$file)
    df <- as.data.frame(myfiles())
    num <-0
    
    headers<-rbindlist(lapply(input$file$datapath, function(y){
      dt <- fread(y)
      header1 <- as.data.table(matrix(colnames(dt), nrow = 1))
      return(header1)
    }), fill = TRUE)
    
    filelist <- data.frame(name = input$file$name)
    filelist$name <- sub("\\.csv$",'', as.character(filelist$name))
    filelist$name <- as.POSIXct(filelist$name, format = '%Y-%m-%dT%H_%M_%S')
    
    min <- first(filelist$name)
    max <- last(filelist$name)
    
    num_days <- as.integer(difftime(max, min, units = c('days')))
    
    # how many files contain incorrect name or order of every column
    for(i in 1:nrow(headers)){
      if(
        (ncol(headers[i,]) != 6 ) |
        (headers[i, 2] != 'detid')|
        (headers[i, 3] != 'time') |
        (headers[i, 4] != 'flow') |
        (headers[i, 5] != 'speed')|
        (headers[i, 6] != 'occ')
      ){
        num <- num + 1
      }
    }
    percent_cons <- 1 - num / nrow(headers)
    
    if(num_days < 1)
      con_miss_p <- num / 1
    else 
    con_miss_p <- num / num_days
    
    if(ncol(df)==6)
      paste('The consistency rate is: ', percent(percent_cons,accuracy = 0.0001), ". Number of data file with format doesn't comply with the standard per day is: ", con_miss_p, ' (The traffic data are recorded every 15 minutes and 95 sets of traffic data are obtained every day).')
    else 
      paste('the number of columns is incorrect')      
  })
  
  # 4.Correctness 
  output$count4 <- renderText({
    req(input$file)
    n_plausibility <- 0

    df <- as.data.frame(myfiles())
    
    for(i in 1:nrow(df)){
      if(
        (df[i, 'flow'] > 750) |
        (df[i, 'occ'] < 0 | df[i,'occ'] > 100) |
        (df[i, 'flow'] == 0 & df[i, 'occ'] > 0)
      ){
        n_plausibility <- n_plausibility + 1
      }
    }
    percent_corr <- 1 - n_plausibility / nrow(df)
    
    paste('The correctness rate is: ', percent(percent_corr,accuracy = 0.0001), '. The data failed the correctness check is shown below.')
  })
  
   output$table <- renderDataTable({
     req(input$file)
     df <- as.data.frame(myfiles())
     fail <- data.frame(x =double(),detid =double(),time =double(),flow =double(),speed =double(),occ =double())
     
     for(i in 1:nrow(df)){
       if(
         (df[i, 'flow'] > 750) |
         (df[i, 'occ'] < 0 | df[i,'occ'] > 100) |
         (df[i, 'flow'] == 0 & df[i, 'occ'] > 0)
       ){
         fail[nrow(fail) + 1,] = df[i,]
       }
     }
     fail$time <- as.POSIXct(fail$time, origin = '1970-01-01 00:00:00')
     
     return(fail)
   })
   
   # 5. Accuracy
   output$count5 <- renderText({ 
     req(input$file)
     dt <- as.data.table(myfiles())
     dt <- dt[, c('time', 'detid', 'flow', 'occ')]
     attributes(dt$time)$tzone <- "CET"
     # free flow speed (v)= 50, jam occupancy (O) = 100
     # 1st loop day and night
     loop1 <- setnames(data.table(matrix(nrow = 0, ncol = 5)), c('k', 'r', 'm', 'abs_error', 'rel_error'))
     for (k in seq(0.05, 0.3, 0.05)){
       for (r in seq(0.2, 1, 0.2)){
         for(m in seq(1, 2, 0.25)){
           
           dt1 <- dt[, paste0('k=', k, ',r=', r, ',m=', m):= k *50 * (1 - (occ/100) ^ r) ^ m * occ]
           dt1 <- dt[, paste0('abs_error_k=', k, ',r=', r, ',m=', m):= abs (flow - eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m))))]
           dt1 <- dt[, paste0('rel_error_k=', k, ',r=', r, ',m=', m):= eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m))) / eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m)))]
           
           dt1[is.na(dt1)] = 0
           invisible(lapply(names(dt1),function(.name) set(dt1, which(is.infinite(dt1[[.name]])), j = .name,value = NA)))
           #  dt1_day[is.infinite(dt1_day)] = 0
           abs_error <- mean(dt1[, eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           rel_error <- mean(dt1[, eval(as.symbol(paste0('rel_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           
           a <- data.table(k, r, m, abs_error, rel_error)
           loop1 <- rbind(loop1, a)
         }
       }
     }
     
     # 2nd loop, 3rd loop
     
     # extract the minimum value of error
     ABE <- min(loop1[, abs_error])
     ARE <- min(loop1[, rel_error])
     paste('The average relative error is: ', percent(ARE, accuracy = 0.0001), ', the average absolute error is: ', ABE, ' vphpl.')
   })
   
   # 5.Accuracy: split into day and night data sets
   output$count6 <- renderText({
     req(input$file)
     dt <- as.data.table(myfiles())
     dt <- dt[, c('time', 'detid', 'flow', 'occ')]
     attributes(dt$time)$tzone <- "CET"
     
     daynight <- function(datetime) {
       paste(c("night", "day", "night")[
           cut(as.numeric(format(datetime, "%H%M")), c(0000, 0559, 2031, 2359))
           ])
     }
     dt$day_night <- daynight(dt$time)
     dt_day <- dt[day_night == 'day']
     dt_night <- dt[day_night == 'night']
     
     loop1_day <- setnames(data.table(matrix(nrow = 0, ncol = 5)), c('k', 'r', 'm', 'abs_error_day', 'rel_error_day'))
     for (k in seq(0.1, 0.4, 0.1)){
       for (r in seq(0.2, 1.2, 0.25)){
         for(m in seq(1, 2, 0.25)){
           
           dt1_day <- dt_day[, paste0('k=', k, ',r=', r, ',m=', m):= k *50 * (1 - (occ/100) ^ r) ^ m * occ]
           dt1_day <- dt_day[, paste0('abs_error_k=', k, ',r=', r, ',m=', m):= abs (flow - eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m))))]
           dt1_day <- dt_day[, paste0('rel_error_k=', k, ',r=', r, ',m=', m):= eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m))) / eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m)))]
           
           dt1_day[is.na(dt1_day)] = 0
           invisible(lapply(names(dt1_day),function(.name) set(dt1_day, which(is.infinite(dt1_day[[.name]])), j = .name,value =0)))
           #  dt1_day[is.infinite(dt1_day)] = 0
           abs_error_day <- mean(dt1_day[, eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           rel_error_day <- mean(dt1_day[, eval(as.symbol(paste0('rel_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           
           a <- data.table(k, r, m, abs_error_day, rel_error_day)
           loop1_day <- rbind(loop1_day, a)
         }
       }
     }
     
     loop1_night <- setnames(data.table(matrix(nrow = 0, ncol = 5)), c('k', 'r', 'm', 'abs_error_night', 'rel_error_night'))
     for (k in seq(0.05, 0.15, 0.02)){
       for (r in seq(0.2, 0.8, 0.2)){
         for(m in seq(1, 2, 0.25)){
           
           dt1_night <- dt_night[, paste0('k=', k, ',r=', r, ',m=', m):= k *50 * (1 - (occ/100) ^ r) ^ m * occ]
           dt1_night <- dt_night[, paste0('abs_error_k=', k, ',r=', r, ',m=', m):= abs (flow - eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m))))]
           dt1_night <- dt_night[, paste0('rel_error_k=', k, ',r=', r, ',m=', m):= eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m))) / eval(as.symbol(paste0('k=', k, ',r=', r, ',m=', m))) ]
           
           dt1_night[is.na(dt1_night)] = 0
           invisible(lapply(names(dt1_night),function(.name) set(dt1_night, which(is.infinite(dt1_night[[.name]])), j = .name,value = NA)))
           abs_error_night <- mean(dt1_night[, eval(as.symbol(paste0('abs_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           rel_error_night <- mean(dt1_night[, eval(as.symbol(paste0('rel_error_k=', k, ',r=', r, ',m=', m)))], na.rm = TRUE)
           
           a <- data.table(k, r, m, abs_error_night, rel_error_night)
           loop1_night <- rbind(loop1_night, a)
         }
       }
     }
     
     ABE_day <- min(loop1_day[, abs_error_day])
     ARE_day <- min(loop1_day[, rel_error_day])
     
     ABE_night <- min(loop1_night[, abs_error_night])
     ARE_night <- min(loop1_night[, rel_error_night])
     
     paste('The average relative error of daytime is: ', percent(ARE_day, accuracy = 0.0001), ', the average absolute error of daytime is: ', ABE_day, ' vphpl. ', 'The average relative error of nighttime is: ', percent(ARE_night, accuracy = 0.0001), ', the average absolute error of nighttime is: ', ABE_night, ' vphpl.')
     
   })
  
   output$count7 <- renderText({
     req(input$file)
     dt <- as.data.frame(myfiles())
     percent_score30 <- sum(dt$score == 30)/nrow(dt)
     
     paste('The high score rate is: ', percent(percent_score30,accuracy = 0.0001), '. (score=30: real-time data, score=20: few real-time data, mainly history data, score=10: no real-time data). The numbers of data records with score less than 30 for each link are displayed below.')
   })
   
   output$table2 <- renderDataTable({
     req(input$file)
     dt <- as.data.table(myfiles())
     dt[, statistikid:= as.numeric(dt$statistikid)]
     dt_list <- dt[, list(score_less_30  = sum(score != 30)), by = statistikid]
     
   })
}

shinyApp(ui = ui, server = server)
