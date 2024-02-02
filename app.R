#Selah Dean
#Shiny App - Pitching Chart


library(shiny)
library(DT)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(shinyFeedback)


ui <- fluidPage(
  
  titlePanel(
    fluidRow(
      column(1, downloadButton("download", "Save Chart")),
      column(1, offset = 1, actionButton("newpitcher", "New Pitcher")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(2,actionButton("newinning", "New Inning")), 
        column(4, offset=2, textOutput("currentinning"))
      ),
      fluidRow(
        column(2,actionButton("newbatter", "New Batter"),style = "padding-top:10px"), 
        column(4, offset=2, textOutput("currentbatter"), style = "padding-top:10px")
      ),
      fluidRow(
        column(2,actionButton("newpitch", "New Pitch"), style = "padding-top:10px"), 
        column(4, offset=2, textOutput("pitchnum"), style = "padding-top:10px"), 
      ),
      fluidRow(
        column(12, plotOutput("plot"), style = "padding-top:20px")
      ),
      fluidRow(
        column(12, plotOutput("pitchplot"), style = "padding-top:20px")
      ),
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Chart", DTOutput("table")), 
        tabPanel("Summary", DTOutput("summary_table"))
        
      )
      
      
    )
  )
)

table_data <- data.frame(`MPH` = as.numeric(), `S/B` = as.character(),  `Type` = as.character(), `Result` = as.character(), 
                         `Batter` = as.numeric(), `Count(B)` = as.numeric(), `Count(S)` = as.numeric(), `Inning` = as.numeric(), check.names = FALSE)


server <- function(input, output, session) {
  
  values <- reactiveValues(df = data.frame(`MPH` = as.numeric(), `S/B` = as.character(), `Type` = as.character(), `Result` = as.character(), 
                                           `Batter` = as.numeric(), `Count(B)` = as.numeric(), `Count(S)` = as.numeric(), `Inning` = as.numeric(),check.names = FALSE))
  observeEvent(input$newpitcher, {
    if(!is.null(values$m)){
      showModal(modalDialog(
        tags$h2('Do you want to clear current pitch data?'),
        footer=tagList(
          actionButton("yes", "Yes"),
          actionButton("no", "No"),
        )
      )
      )}
    else{
      showModal(modalDialog(
        textInput("firstname", "First Name"), 
        textInput("lastname", "Last Name"), 
        dateInput("date", "Date:", format="mm/dd/yyyy"),
        fileInput('uploaded_table','',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    '.csv'
                  )),
        footer=tagList(
          actionButton("submitpitcher", "Save Pitcher"), 
          modalButton("Cancel")
        )
      ))
    }
  })
  
  observeEvent(input$newpitch, {
    if(is.null(input$firstname) | is.null(input$lastname)){
      showNotification("Please enter pitcher name", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    req(input$firstname, input$lastname)
    if(is.null(input$inning) | is.null(input$`#`)){
      showNotification("Please enter inning and/or batter", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    req(input$inning, input$`#`)
    showModal(modalDialog(
      tags$h2('Enter Pitch Info'), 
      numericInput("mph", "MPH", value = NULL, min= 0, max = 100), 
      radioButtons("strike", "S/B", c("Ball", "Called Strike", "Swinging Strike", "Foul")),
      radioButtons("type", "Type", c("Fastball", "Curveball", "Slider", "Changeup", "Cutter", "Spliter", "Other")), 
      textInput("result", "Result"), 
      footer=tagList(
        actionButton("submitpitch", "Save Pitch"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$newbatter, {
    if(is.null(input$firstname) | is.null(input$lastname)){
      showNotification("Please enter pitcher name", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    req(input$firstname, input$lastname)
    showModal(modalDialog(
      numericInput("#", "Batter #", value = NULL, min = 0, max = 100),
      footer=tagList(
        actionButton("submitbatter", "Save Batter"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$newinning, {
    if(is.null(input$firstname) | is.null(input$lastname)){
      showNotification("Please enter pitcher name", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    req(input$firstname, input$lastname)
    showModal(modalDialog(
      numericInput("inning", "Inning", value = NULL, min = 1, max = 30), 
      footer=tagList(
        actionButton("saveinning", "Save Inning"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$submitpitch, {
    removeModal()
    temp <- values$m
    
    last_row <- tail(temp, n=1)
    
    if(is.null(last_row)){
      balls <- 0
      strikes <- 0
    } else if (last_row$`Batter` != input$`#`){
      balls <- 0 
      strikes <- 0 
    } else {
      if (last_row$`S/B` == "Called Strike" | last_row$`S/B`== "Swinging Strike"){
        strikes <- last_row$`Count(S)` + 1
        balls <- last_row$`Count(B)`
      } else if (last_row$`S/B` == "Ball"){
        strikes <- last_row$`Count(S)`
        balls <-last_row$`Count(B)` + 1
      } else if (last_row$`S/B` == "Foul"){
        balls <- last_row$`Count(B)`
        if (last_row$`Count(S)` < 2){
          strikes <- last_row$`Count(S)` + 1
        } else {
          strikes <- last_row$`Count(S)`
        }
      }
    }

    new_row <- data.frame(`MPH` = input$mph, `S/B` = input$strike, `Type` = input$type, `Result` = input$result, `Batter` =  input$`#`, 
                          `Count(B)` = balls, `Count(S)` = strikes, `Inning` = input$inning, check.names = FALSE)
    
    temp <- rbind(temp, new_row)
    
    values$m <- temp
  })
  
  observeEvent(input$submitpitcher, {
    removeModal()
  })
  
  rv <- reactiveValues(
    upload_state = NULL
  )

  observeEvent(input$uploaded_table, {
    rv$upload_state <- "uploaded"
  })
  
  observeEvent(input$submitbatter, {
    removeModal()
  })
  
  observeEvent(input$saveinning, {
    removeModal()
  })
  
  observeEvent(input$yes, {
    values$m <- NULL
    rv$upload_state <- NULL
    removeModal()
    showModal(modalDialog(
      textInput("firstname", "First Name"), 
      textInput("lastname", "Last Name"), 
      dateInput("date", "Date:", format="mm/dd/yyyy"),
      fileInput('uploaded_table','',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      footer=tagList(
        actionButton("submitpitcher", "Save Pitcher"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$no, {
    removeModal()
  })
  
  uploaded_data <- reactive({
    inFile <- input$uploaded_table
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
    df <- select(df, -X)
    colnames(df) <- c("MPH", "S/B", "Type", "Result", "Batter", "Count(B)", "Count(S)", "Inning")
    return(df)
  })
  
  output$table <- renderDataTable({
    if (is.null(values$m) & !is.null(rv$upload_state)){
      values$m <- uploaded_data()
    }
    datatable(values$m, editable = TRUE,
              caption = htmltools::tags$caption(paste0("Current Pitcher: ", input$firstname, " ", input$lastname), style="color:black"),
              options = list(searching=FALSE, pageLength = 150, dom = "t", columnDefs = list(list(visible=FALSE, targets=c(6,7)))))
  })
  
  summary_df <- eventReactive(values$m, {
    values$m %>% group_by(Inning) %>%
      summarise(`Num Pitches` = n(),
                `Num FB` = sum(Type == "Fastball"),
                `Avg FB` = round(mean(ifelse(Type=="Fastball", MPH, NA), na.rm=TRUE)),
                `Swinging Strikes` = as.numeric(sum(`S/B`== "Swinging Strike")) + as.numeric(sum(`S/B`== "Foul")),
                `Called Strikes` = as.numeric(sum(`S/B`== "Called Strike")),
                `Balls` = as.numeric(sum(`S/B` == "Ball")),
                `Swing & Miss` = sum(`S/B` == "Swinging Strike" & Result == ""),
                `K` = sum(toupper(Result) == "K"),
                `BB` = sum(toupper(Result) == "BB"),
                `BF` = sum(`Count(B)` == 0 & `Count(S)` == 0),
                `1st Pitch Strikes` = sum(`Count(B)` == 0 & `Count(S)` == 0 & `S/B` != "Ball"),
                `3-Ball Counts` = sum(`Count(B)` == 3 & Result !=""))
  })
  
  output$summary_table <- renderDataTable({
    datatable(summary_df(), options = list(searching=FALSE, pageLength = 150, dom = "t"))
  })
  
  observeEvent(input$table_cell_edit, {
    row  <- input$table_cell_edit$row
    clmn <- input$table_cell_edit$col
    if (clmn == 5){
      if(values$m[row-1, clmn] != input$table_cell_edit$value){
        values$m[row, 6] <- 0
        values$m[row, 7] <- 0
      } else {
        previous_pitch <- values$m[row-1, 2]
        previous_strike <- values$m[row-1, 7]
        previous_ball <- values$m[row-1, 6]
        if (previous_pitch == "Called Strike" | previous_pitch == "Swinging Strike"){
          strikes <- previous_strike + 1
          balls <- previous_ball
        } else if (previous_pitch == "Ball"){
          strikes <- previous_strike
          balls <- previous_ball + 1
        } else if (previous_pitch == "Foul"){
          balls <- previous_ball
          if (previous_strike < 2){
            strikes <- previous_strike + 1
          } else {
            strikes <- previous_strike
          }
        }
        values$m[row, 7] <- strikes
        values$m[row, 6] <- balls
      }
    }
    values$m[row, clmn] <- input$table_cell_edit$value
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$date, input$lastname, input$firstname, ".csv", sep="")
    },
    content = function(file){
      write.csv(values$m, file)
    }
  )
  
  
  output$plot <- renderPlot({
    
    if(!is.null(values$m)){
      data <- values$m
      
      num_balls <- as.numeric(sum(data$`S/B` == "Ball"))
      
      num_strikes <- as.numeric(sum(data$`S/B`== "Called Strike")) 
      
      num_swinging_strikes <- as.numeric(sum(data$`S/B`== "Swinging Strike")) + as.numeric(sum(data$`S/B`== "Foul"))
      
      p <- barplot(height=c(num_balls, num_strikes, num_swinging_strikes), names=c("Ball", "Called Strike", "Swinging Strike"), las=2, cex.names = 0.7, ylim = c(0, max(c(num_balls, num_strikes, num_swinging_strikes)) + 1), col=c("slategray1", "royalblue4", "royalblue1"), main = "Balls vs Strikes")
      text(x = p, y = c(num_balls, num_strikes, num_swinging_strikes) + 0.5, labels = c(num_balls, num_strikes, num_swinging_strikes))
      }
  })
  
  output$pitchplot <- renderPlot({
    
    if(!is.null(values$m)){
      data <- values$m
      
      pitch_type <- count(data, Type)
      b <-barplot(height = pitch_type$n, names = pitch_type$Type, 
                  col=c("royalblue1",  "royalblue3", "royalblue4", "midnightblue"), 
                  ylim = c(0, max(pitch_type$n + 1)), main = "Pitch Types Thrown \n (with avg velo)")
      
      avg_velo <- aggregate(data$MPH, list(data$Type), FUN=mean, na.rm = TRUE) 
      avg_velo$x <- round(avg_velo$x,2)
      ymax <- max(avg_velo$x)
      
      text(x = b, y = pitch_type$n + 0.5, labels = avg_velo$x)
    }
  })
  
  output$currentbatter <- renderText({paste0("Current Batter: ", input$`#`)})
  output$currentinning <- renderText({paste0("Current Inning: ", input$`inning`)})
  output$pitchnum <- renderText({paste0("Pitches Thrown: ", nrow(values$m))})
  
  observe({print(colnames(values$m))})
  observe({print(!is.null(values$m))})
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


