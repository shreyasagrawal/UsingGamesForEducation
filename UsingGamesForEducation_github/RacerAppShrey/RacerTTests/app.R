#Last Updated on July 23 2020
#Updated July 21 2022 by Shrey Agrawal

# Racer T-Tests #
# Loading Libraries
library(shiny)
library(shinythemes)
library(broom)
library(dplyr)
library(gdata)
library(ggplot2)
library(stringr)
library(readr)
library(curl)
library(tidyr)
library(lubridate)
       
##Importing Data
n <- sample(c(0,1), size = 1)

# if(n == 0){
#   data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/racer/getdata.php")
# 
# } else {
#   data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/racer/getdata.php")
# }

if(n == 0){
  data.all <- readr::read_csv("https://stat2games.sites.grinnell.edu/data/racekartb/getdata.php")

} else {
  data.all <- readr::read_csv("https://stat2games.sites.grinnell.edu/data/racekartb/getdata.php")
}


#Filtering Data
data.all = filter(data.all, data.all$Game > 292, Body == "HotRod" | Body == "Tiny" | Body == "Classic",
                  Track == "TutorialTrack" | Track == "StraightTrack" | Track == "OvalTrack" | Track == "EightTrack" | Track == "ComplexTrack")
data.all = drop.levels(data.all)
data.all = data.all %>% 
  rename("GameDate"= "Date")

data.all <- data.all %>% mutate(Date = str_sub(GameDate, 1, 10))
data.all$Date <- as.Date(data.all$Date, format = "%m/%d/%Y")


Sample1Data <- read_csv("RacerTutorialSample1Data.csv") # You'll need to edit how to read this in.
#Sample2Data <- read_csv("RacerOvalSample2Data.csv") # You'll need to edit how to read this in.
#Sample2Data <- filter(Sample2Data, Game >100)
Sample3Data <- read_csv("RacerOvalSample3Data.csv") # You'll need to edit how to read this in.
Sample2Data <- read_csv("Sample2_SmallCleanTutorial2.csv") # You'll need to edit how to read this in.


Sample1Data <- Sample1Data %>% mutate(Date = str_sub(GameDate, 1, 10))
Sample1Data$Date <- as.Date(Sample1Data$GameDate, format = "%m/%d/%Y")

Sample2Data <- Sample2Data %>% mutate(Date = str_sub(GameDate, 1, 10))
Sample2Data$Date <- as.Date(Sample2Data$GameDate, format = "%m/%d/%Y")

Sample3Data <- Sample3Data %>% mutate(Date = str_sub(GameDate, 1, 10))
Sample3Data$Date <- as.Date(Sample3Data$GameDate, format = "%m/%d/%Y")


all.sample1 <- rbind(Sample1Data, Sample2Data)
all.sample2 <- rbind(all.sample1, Sample3Data)
data.all <- rbind(data.all,all.sample2)

#To Lower
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)


#Making GroupID and PlayerID a character vector
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

#Sort Data in order to create an accurate Order column
data.all <- data.all %>% arrange(Track, GroupID, PlayerID, Game)


#Make a working Order column called Order2
data.all$Order2 <- 0
data.all$Order2[1] <- 1 
for(i in 2:nrow(data.all)){
  if(data.all$PlayerID[i] == data.all$PlayerID[i - 1] &
     data.all$GroupID[i] == data.all$GroupID[i - 1] &
     data.all$Track[i] == data.all$Track[i - 1]){
    
    data.all$Order2[i] <- data.all$Order2[i - 1] + 1
  } 
  
  else {
    data.all$Order2[i] <- 1
  }
}

## We only kept the first two clean races for data.clean
data.clean <- data.all
data.clean <- filter(data.clean, Order2 < 3)


##FIRST ITERATION 
## Don't keep if they only played one race
data.clean$Clean <- "Yes"
for(i in 1:nrow(data.clean)){
  
  if(i == nrow(data.clean)){
    if(data.clean$Order2[i] == 1){
      data.clean$Clean[i] <- "No"
    }
  
  }
  
  else if(i != nrow(data.clean)){
    if(data.clean$Order2[i] == 1 & data.clean$Order2[i+1] == 1){
    data.clean$Clean[i] <- "No"
  }
 }
}

## First Filter for Clean Data
data.clean <- filter(data.clean, Clean == "Yes")

# Don't keep if they played the same CarID twice
for(i in 2:nrow(data.clean)){
  if(data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$CarID[i] == data.clean$CarID[i - 1]){
    data.clean$Clean[i - 1] <- "No" 
    data.clean$Clean[i] <- "No"
  }
}

## Second Filter for Clean Data
data.clean <- filter(data.clean, Clean == "Yes")


#SECOND ITERATION

## Don't keep if they only played one race
data.clean$Clean <- "Yes"
for(i in 1:nrow(data.clean)){
  
  if(i == nrow(data.clean)){
    if(data.clean$Order2[i] == 1){
      data.clean$Clean[i] <- "No"
    }
    
  }
  
  else if(i != nrow(data.clean)){
    if(data.clean$Order2[i] == 1 & data.clean$Order2[i+1] == 1){
      data.clean$Clean[i] <- "No"
    }
  }
}

## First Filter for Clean Data
data.clean <- filter(data.clean, Clean == "Yes")

# Don't keep if they played the same car twice
for(i in 2:nrow(data.clean)){
  if(data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$CarID[i] == data.clean$CarID[i - 1]){
    data.clean$Clean[i - 1] <- "No" 
    data.clean$Clean[i] <- "No"
  }
}

## Second Filter for Clean Data
data.clean <- filter(data.clean, Clean == "Yes")


## ONLY if the data is clean, we can then filter to eliminate BadDrivers
## If the "Only Clean Data" is checked, then we can also checkbox "Only Good Drivers"

## We need to take the track into account for bad drivers.
data.clean$BadDriver <- "No"
for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "OvalTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishedTime[i] + data.clean$FinishedTime[i - 1] > 60 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 4)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "Tutorial" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishedTime[i] + data.clean$FinishedTime[i - 1] > 50 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 6)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "StraightTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishedTime[i] + data.clean$FinishedTime[i - 1] > 40 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if((data.clean$Track[i] == "8Track" | data.clean$Track[i] == "ComplexTrack") &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

##Checkbox for "Only Good Drivers"
data.good = filter(data.clean, BadDriver == "No")

#Making Order 2 a factor for the three data sets
data.all$Order2 <- as.factor(data.all$Order2)
data.clean$Order2 <- as.factor(data.clean$Order2)
data.good$Order2 <- as.factor(data.good$Order2)

#Changing Order to OldOrder and Order2 to Order
data.all <- data.all %>% rename(OldOrder = Order, Order = Order2)
data.clean <- data.clean %>% rename(OldOrder = Order, Order = Order2)
data.good <- data.good %>% rename(OldOrder = Order, Order = Order2)


#To use for Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
all_tracks <- sort(unique(data.all$Track))


#UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # App title ----
  titlePanel("Racer Hypothesis Tests"),
  
  fluidRow(
    
    #First column
    column(2,
           
           selectInput(inputId = "groupID",
                       label = "Group ID:", 
                       choices =  c(all_groups),
                       multiple = TRUE,
                       selectize = TRUE,
                       selected = "stest"),
           
           selectInput(inputId = "playerID",
                       label = "Remove Player ID:",
                       choices =  c(all_players),
                       multiple = TRUE,
                       selectize = TRUE),
           
           selectInput(inputId = "tracks",
                       label = "Track:",
                       choices =  c("Tutorial", "StraightTrack", "OvalTrack", "EightTrack", "ComplexTrack"),
                       multiple = FALSE,
                       selectize = TRUE,
                       selected = "Tutorial"),
           
           selectInput(inputId = "xvar",
                       label = "X Variable:",
                       #columns of the dataset
                       choices = c("Body", "Engine", "Tire", "Track", "Order", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "yvar",
                       label = "Y Variable:",
                       #columns of the dataset
                       choices = c("FinishedTime", "TopSpeed", "TimeTo30", "TimeTo60"),
                       selected = "FinishedTime",
                       multiple = FALSE)
           
           
    ),
    
    #Second column
    column(2, 
           selectInput(inputId = "color",
                       label = "Color by:",
                       choices = c("Body", "Engine", "Tire", "Track", "Order", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "tests",
                       label = HTML("Statistical Tests <br/> (for X Variable)"),
                       choices = c("None", "two-sample t-test", "paired t-test", "ANOVA", "Block Design", 
                                   "Two Sample Randomization Test", "Paired Randomization Test"),
                       selected = "None",
                       multiple = FALSE),
           
           checkboxInput('bplot',"Add boxplot",FALSE),
           checkboxInput("summary", "Show Summary Statistics", FALSE),
           checkboxInput("months", "Use Last 6 Months of Data", FALSE),
           
           radioButtons(inputId = "data",
                        label = "Choose Data:", 
                        choices = c("All Data", "Clean Data"),
                        selected = "All Data",
                        inline = TRUE),
           uiOutput("gooddriver"),
           
           downloadButton('downloadData', label = "Racer Data"),
           
           a(h5("Instructor Details"),
             href="https://stat2labs.sites.grinnell.edu/racer.html", 
             align="left", target = "_blank")
           
           
    ),
    column(8,
           
           #Outputs
           tabsetPanel(
             tabPanel("General",  plotOutput(outputId = "Plot"),
                      verbatimTextOutput("twosamp"), 
                      verbatimTextOutput("paired"),
                      verbatimTextOutput("anova"),
                      verbatimTextOutput("blocked"),
                      tableOutput("summarytable"),
                      verbatimTextOutput("twor"),
                      verbatimTextOutput("pr"),
                      uiOutput("summarytext")),
             
             #Residual plot tab
             tabPanel("Residuals", uiOutput("residualtext"),
                      fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotOutput("rplot1"), plotOutput("rplot2"))))
             
           
    ))
))



#Server
server <- function(input, output,session) {
  
  
  
  #Dynamic Input to filter only the good drivers
  output$gooddriver <- renderUI({

    req(input$data)

    if(input$data == "Clean Data"){

      checkboxInput(inputId = "gooddata",
                    label = "Good Driver Data",
                    value = FALSE)
    }
  })
  
  
  #Reactive Data for all three data types
  plotDataR <- reactive({
    
    req(input$data)
    
    if(input$data == "All Data"){
      data <- filter(data.all, GroupID %in% input$groupID, Track %in% input$tracks, !(PlayerID %in% input$playerID))
 
    } else if(input$data == "Clean Data"){
     data <-  filter(data.clean, GroupID %in% input$groupID, Track %in% input$tracks, !(PlayerID %in% input$playerID))

     #Supressing error messages
      try(if(input$gooddata == "TRUE"){
      data <- filter(data.good, GroupID %in% input$groupID, Track %in% input$tracks, !(PlayerID %in% input$playerID))
       }  , silent = TRUE)
      
    } 
    
    #Last 6 months of data checkbox is selected
    if(input$months == TRUE){
      
      sixmonthsago <- Sys.Date() - months(6)
      data <- filter(data, Date >= sixmonthsago)
    }
    
    return(data)
    
  })
  
  
  #Dynamic Remove PlayerID Input 
  observe({
    
    #Require
    req(input$groupID) 
    
    #6 Months ago variable for later use
    sixmonthsago <- Sys.Date() - months(6)
    
    #All Data
    if(input$data == "All Data"){
      
      if(input$months == FALSE){
      gamedata <- filter(data.all, GroupID %in% input$groupID, Track %in% input$tracks)
      
      } else{
        gamedata <- filter(data.all, GroupID %in% input$groupID, 
                           Track %in% input$tracks, Date >= sixmonthsago)
      }
      
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c(sort(unique(gamedata$PlayerID))))
      
    #Clean Data
    } else if(input$data == "Clean Data"){
      
      if(input$months == FALSE){
      gamedata <- filter(data.clean, GroupID %in% input$groupID, Track %in% input$tracks)
      
      } else{
        gamedata <- filter(data.clean, GroupID %in% input$groupID, 
                           Track %in% input$tracks, Date >= sixmonthsago)
      }
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c(sort(unique(gamedata$PlayerID))))
      
      
        #Supressing error message/Good Data
        try(if(input$gooddata == "TRUE"){
          
          
          if(input$months == FALSE){
          gamedata <- filter(data.good, GroupID %in% input$groupID, Track %in% input$tracks)
        
          } else{
            gamedata <- filter(data.good, GroupID %in% input$groupID, 
                               Track %in% input$tracks, Date >= sixmonthsago)
          }
          
          updateSelectInput(session,
                            "playerID",
                            choices = c(sort(unique(gamedata$PlayerID))))
          }, silent = TRUE)
    }
  
  })
  

  # Creating Vizualizations
  output$Plot <- renderPlot({
    
    #Requiring inputs
    req(input$data)
    req(input$groupID)
    
    #Using Reactive Data
    plotData <- plotDataR()
    
    #If boxplot option is selected
    if (input$bplot == "TRUE"){
      
      #ggplot with manual colors if color by option is Body, Engine, or Tire
      if(input$color %in% c("Body", "Engine", "Tire") == TRUE){
        cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_boxplot() +
          geom_point(position=position_dodge(width = 0.75), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) +
          scale_color_manual(values = cols)
        
        #Using automatic colors
      } else {
        
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_boxplot() +
          geom_point(position=position_dodge(width = 0.75), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) 
        
        
      }
      
      #If boxplot option is not selected  
    } else {
      
      #ggplot with manual colors if color by option is Body, Engine, or Tire
      if(input$color %in% c("Body", "Engine", "Tire") == TRUE){
        cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")   
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
          geom_point(position= position_dodge(width = 0.1), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) +
          scale_color_manual(values = cols)
        
        #Using automatic colors
      } else{
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
          geom_point(position =position_dodge(width = 0.1), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
      }
    }
    
    
    #ANOVA Output
    output$anova = renderPrint({

      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data to run ANOVA test
      if(nrow(plotData) > 0){
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(ColorVariable)
      XVariable = drop.levels(as.factor(XVariable))
      
      if(input$tests == "ANOVA") {
        
        #At least 2 levels for X Variable
        if(nlevels(XVariable) > 1){
        
        #Two way ANOVA
        if(nlevels(ColorVariable) > 1){
          anovatest = anova(aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable))
        }
        
        #One way ANOVA
        else{
          anovatest = aov(YVariable ~ XVariable)
        }
        
        #Making Tidy table and adding columns/rows
        check2 = tidy(anovatest)
        sum_df = sum(check2$df)
        sum_ss = sum(check2$'sumsq')
        sum_df
        sum_ss
        check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
        check2$sumsq = round(check2$sumsq, digits = 2)
        check2$meansq = round(check2$meansq, digits = 2)
        check2$statistic = round(check2$statistic, digits = 2)
        
        return(check2)
        
        #If there is one or less levels for X Variable
        } else{
          "At least two levels are needed for the X Variable to run the ANOVA test."
        }
        
       }
      }
    })
    
    #Blocked Design
    output$blocked = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data to run the block design ANOVA
      if(nrow(plotData) > 0){
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(ColorVariable)
      PlayerID = as.factor(plotData$PlayerID)
       
      #Block design option is selected
      if(input$tests == "Block Design") {
        
        #Error Message if PlayerID is selected as X Variable or Color
        if(input$xvar == "PlayerID" | input$color == "PlayerID"){
          "When using the Block Design, the X-Variabe/Color Variable cannot be PlayerID"
          
        #We can run the block design test 
        } else {
          
          #At least 2 levels for X Variable
          if(nlevels(XVariable) > 1){
          
            #Two Way Blocked ANOVA
            if(nlevels(ColorVariable) > 1){
              anovatest = anova(aov(YVariable ~ XVariable + PlayerID + ColorVariable + XVariable*ColorVariable))
            
            #One Way Blocked
            } else{
              anovatest = anova(aov(YVariable ~ XVariable + PlayerID))
            }
          
          #Making Tidy table and adding columns/rows
          check2 = tidy(anovatest)
          options(digits = 3)
          sum_df = sum(check2$df)
          sum_ss = sum(check2$'sumsq')
          sum_df
          sum_ss
          check2$sumsq = round(check2$sumsq, digits = 2)
          check2$meansq = round(check2$meansq, digits = 2)
          check2$statistic = round(check2$statistic, digits = 2)
          check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
          
          return(check2)
          
          #If there is one or less levels for X Variable
          } else{
            "At least two levels are needed for the X Variable to run the Block design test."
          }
        }
      }
     }
   })
    
  
    
    #Two Sample T-Test
    output$twosamp = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data
      if(nrow(plotData) > 0){
      
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      if(input$tests == "two-sample t-test"){
        
        #X-variable and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X-variable option, run the test
          if(nlevels(dropped) == 2) {
            t.test(YVariable ~ XVariable)
          }
          else{
            "t-tests are only valid when there are exactly two groups."
          }
        }  
        else{
          "The X variable and the Color variable should be the same for a t-test."
        }
      }
     }
   })
    
    
    #Paired T-Test
    output$paired = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data
      if(nrow(plotData) > 0){
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      
      if(input$tests == "paired t-test"){
        
        #Users need to use the Clean Data to run Paired T-Test
        if(input$data == "Clean Data"){
          
          #X-variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
              t.test(YVariable ~ XVariable, data = plotData, paired = TRUE)
              
            } else {
              "paired t-tests are only valid when there are exactly two groups."
            }
            
            
          } else{
            "The X variable and the Color variable should be the same for a t-test."
          }
          
        } else{
          "Only Clean Data can be used for the paired t-test"
        }
        
      }
     }
      
    })
    
    
    #Two Sample Randomization Test
    output$twor <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data
      if(nrow(plotData) > 0){
      
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      
      if(input$tests == "Two Sample Randomization Test"){
      
        #X variable and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X variable option, run the test
          if(nlevels(dropped) == 2) {
            
            #Small Data Frame
            data <- data.frame(XVariable, YVariable)

            #Identifying the Two Groups and creating necessary vectors
            groups <- sort(unique(data$XVariable))
            group1 <- groups[1]
            group2 <- groups[2]

            data1 <- data %>% filter(XVariable == group1)
            data2 <- data %>% filter(XVariable == group2)

            group1vec <- data1$YVariable
            group2vec <- data2$YVariable
 

            #Running the Two Sample Randomization Test

             #Setting up
              meandiff <- mean(group1vec) - mean(group2vec)
              R <- 100000
              results <- numeric()
              
            for(i in 1:R){
                samp <- sample(data$YVariable, size = length(data$YVariable), replace = FALSE)
                
                samp1 <- samp[1:length(group1vec)]
                samp2 <- samp[(length(group1vec) + 1):length(samp)]
                
                results[i] <- mean(samp1) - mean(samp2)
            }
         
             pvalue <- (1 + sum(results >= abs(meandiff)) + sum(results <= -abs(meandiff))) / (R+1)
            
             return(paste("P Value:", round(pvalue,5)))
            
          } else {
            "Two sample randomization tests are only valid when there are exactly two groups."
          }
      
        } else{
          "The X variable and the Color variable should be the same for a t-test."
        }
      }
     }
    
    })
    
    
    #Paired Randomization Test
    
    output$pr <- renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #We need data
      if(nrow(plotData) > 0){
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      
      if(input$tests == "Paired Randomization Test"){
        
        #Users need to use the Clean Data to run Paired Randomization Test
        if(input$data == "Clean Data"){
          
          #X variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
             
              #Small Data Frame
              data <- data.frame(XVariable, YVariable)
              
              #Identifying the Two Groups and creating necessary vectors
              groups <- sort(unique(data$XVariable))
              group1 <- groups[1]
              group2 <- groups[2]
              
              data1 <- data %>% filter(XVariable == group1)
              data2 <- data %>% filter(XVariable == group2)
              
              group1vec <- data1$YVariable
              group2vec <- data2$YVariable
               
              
              #Running the Paired Randomization Test
              
              #Differences Vector
              diffvec <- group1vec - group2vec
              
              #Setting up
              meandiff <- mean(diffvec)
              R <- 100000
              results <- numeric()
            
              for(i in 1:R){
                sign <- sample(c(-1,1), size = length(diffvec), replace = T)
                resamp <- sign * diffvec
               
                results[i] <- mean(resamp)
                
              }
              
              pvalue <- (1 + sum(results >= abs(meandiff)) + sum(results <= -abs(meandiff))) / (R+1)
              
              return(paste("P Value:", round(pvalue,4)))
              
              
            } else {
              "Paired Randomization Tests are only valid when there are exactly two groups."
            }
            
            
          } else{
            "The X variable and the Color variable should be the same for the Paired Randomization Test."
          }
          
        } else{
          "Only Clean Data can be used for the Paired Randomization Test"
        }
        
      }
     }
    })
    
    
    
    #Summary Table Output
    output$summarytable <- renderTable({
      
      #Using reactive data
      plotData <- plotDataR()
      
      if(input$summary == "TRUE"){
        
        #If there is data
        if(nrow(plotData) != 0){
        
        #Creating summary table
        stable <- plotData %>% select(input$xvar, input$yvar) %>% 
          rename(`X Variable` = input$xvar, Yvar = input$yvar) %>%
          group_by(`X Variable`) %>%
          summarize(N = n(), Mean = mean(Yvar), SD = sd(Yvar))
        
        #Removing dynamic help text
        output$summarytext <- renderUI({""})
        
        #If there is no data
        } else{
        
        #Empty data frame to return  
        stable <- data.frame()
        
        #Help Text
        output$summarytext <- renderUI(HTML(paste(
          em("There is no data"))))
        }
        
        return(stable)

      }
    
    })
    
    #Making sure help text goes away if checkbox is unchecked
    observeEvent(input$summary, {
     
       if(input$summary == "FALSE"){
        output$summarytext <- renderUI({""})
      }
    })
    
   
    #Residual Histogram (RPLOT 1)
    output$rplot1 <- renderPlot({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #Need data
      if(nrow(plotData) > 0){
      
      #Test is Not None
      if(input$tests != "None"){
      
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      PlayerID = as.factor(plotData$PlayerID)
    
  
      ##Two sample t-test/Two Sample Randomization test
      if(input$tests %in% c("two-sample t-test", "Two Sample Randomization Test")){
        
        #X-variable and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X-variable option, run the test
          if(nlevels(dropped) == 2) {
            
            #Small Data Frame
            data <- data.frame(XVariable, YVariable)
            
            #Identifying the Two Groups and creating necessary vectors
            groups <- sort(unique(data$XVariable))
            group1 <- groups[1]
            group2 <- groups[2]
            
            data1 <- data %>% filter(XVariable == group1)
            data2 <- data %>% filter(XVariable == group2)
            
            group1vec <- data1$YVariable
            group2vec <- data2$YVariable
            
            #Calculate Residuals
            group1res <- group1vec - mean(group1vec)
            group2res <- group2vec - mean(group2vec)
            
            #Combining residuals into one vector
            residuals <- c(group1res, group2res)
            
            #Remove Message
            output$residualtext <- renderUI({""})
            
            #Creating plot
            plot <- hist(residuals, main = "Histogram of Residuals",
                         xlab = "Residuals",
                         ylab = "Count")
            
            return(plot)
        
            
          } else {
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
          }
          
        } else{
          output$residualtext <- renderUI(HTML(paste(
            em("A valid statistical test must be in place for the residual plots to be generated."))))
        }
          
        
      
        
    ##Paired T-Test/Paired Randomization Test
    } else if(input$tests %in% c("paired t-test", "Paired Randomization Test")){
      
        #Users need to use the Clean Data to run Paired test
        if(input$data == "Clean Data"){
          
          #X-variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
             
              #Small Data Frame
              data <- data.frame(XVariable, YVariable)
              
              #Identifying the Two Groups and creating necessary vectors
              groups <- sort(unique(data$XVariable))
              group1 <- groups[1]
              group2 <- groups[2]
              
              data1 <- data %>% filter(XVariable == group1)
              data2 <- data %>% filter(XVariable == group2)
              
              group1vec <- data1$YVariable
              group2vec <- data2$YVariable
              
              #Differences Vector
              diffvec <- group1vec - group2vec
              
              #Calculating residuals
              residuals <- diffvec - mean(diffvec)
              
              #Remove Message
              output$residualtext <- renderUI({""})
              
              #Creating plot
              plot <- hist(residuals, main = "Histogram of Residuals",
                           xlab = "Residuals",
                           ylab = "Count")
              
              return(plot)
              
            } else{
              output$residualtext <- renderUI(HTML(paste(
                em("A valid statistical test must be in place for the residual plots to be generated."))))
            }
             
          } else{
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
          }
          
        } else{
          output$residualtext <- renderUI(HTML(paste(
            em("A valid statistical test must be in place for the residual plots to be generated."))))
        }
             
            
      
    ##ANOVA
      } else if(input$tests == "ANOVA") {
        
        #At least 2 levels for X Variable
        if(nlevels(XVariable) > 1){
        
        #Two way ANOVA
        if(nlevels(ColorVariable) > 1){
          model <- aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable)
        }
        
        #One way ANOVA
        else{
          model <- aov(YVariable ~ XVariable)
        }
        
        #Remove Message
        output$residualtext <- renderUI({""})
        
        #Creating plot
        plot <- hist(model$residuals, main = "Histogram of Residuals",
                     xlab = "Residuals",
                     ylab = "Count")
        
        return(plot)
        
        #Less than 2 levels for X Variable 
        } else{
          output$residualtext <- renderUI(HTML(paste(
            em("A valid statistical test must be in place for the residual plots to be generated."))))
        }
        
        
    ##Block Design
      } else if (input$tests == "Block Design") {
        
        #Error Message if PlayerID is selected as X-variable or Color
        if(input$xvar == "PlayerID" | input$color == "PlayerID"){
          
          output$residualtext <- renderUI(HTML(paste(
            em("A valid statistical test must be in place for the residual plots to be generated."))))
        
        
        } else {
          
          #At least 2 levels for X Variable
          if(nlevels(XVariable) > 1){
          
          #Two Way Blocked ANOVA
          if(nlevels(ColorVariable) > 1){
            model <-  aov(YVariable ~ XVariable + PlayerID + ColorVariable + XVariable*ColorVariable)
          
          #One Way Blocked
          } else{
            model <-  aov(YVariable ~ XVariable + PlayerID)
          }
          
          #Remove Message
          output$residualtext <- renderUI({""})
          
          #Creating plot
          plot <- hist(model$residuals, main = "Histogram of Residuals",
                       xlab = "Residuals",
                       ylab = "Count")
          
          return(plot)
          
          } else{
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
          }
      
        }
          
      } 
      
      #Test option is none
      } else{
        output$residualtext <- renderUI(HTML(paste(
          em("A valid statistical test must be in place for the residual plots to be generated."))))
      }
     }
        
    })
    
    
    #Normal QQ Plot (RPLOT 2)
    output$rplot2 <- renderPlot({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #Need data
      if(nrow(plotData) > 0){
      
      #Test is Not None
      if(input$tests != "None"){
      
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      PlayerID = as.factor(plotData$PlayerID)
      
      
      ##Two sample t-test/Two Sample Randomization Test
      if(input$tests %in% c("two-sample t-test","Two Sample Randomization Test")){
        
        #X-variable and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X-variable option, run the test
          if(nlevels(dropped) == 2) {
          
            #Small Data Frame
            data <- data.frame(XVariable, YVariable)
            
            #Identifying the Two Groups and creating necessary vectors
            groups <- sort(unique(data$XVariable))
            group1 <- groups[1]
            group2 <- groups[2]
            
            data1 <- data %>% filter(XVariable == group1)
            data2 <- data %>% filter(XVariable == group2)
            
            group1vec <- data1$YVariable
            group2vec <- data2$YVariable
            
            #Calculate Residuals
            group1res <- group1vec - mean(group1vec)
            group2res <- group2vec - mean(group2vec)
            
            #Combining residuals into one vector
            residuals <- c(group1res, group2res)
            
            #Creating plot
            plot <- qqnorm(residuals) 
            plot <- qqline(residuals)
            
            return(plot)
            
          }
        }
        
        ##Paired T-Test/Paired Randomization Test
      } else if(input$tests %in% c("paired t-test", "Paired Randomization Test")){
        
        #Users need to use the Clean Data to run Paired T-Test
        if(input$data == "Clean Data"){
          
          #X-variable and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-variable option, run the test
            if(nlevels(dropped) == 2) {
              
              #Small Data Frame
              data <- data.frame(XVariable, YVariable)
              
              #Identifying the Two Groups and creating necessary vectors
              groups <- sort(unique(data$XVariable))
              group1 <- groups[1]
              group2 <- groups[2]
              
              data1 <- data %>% filter(XVariable == group1)
              data2 <- data %>% filter(XVariable == group2)
              
              group1vec <- data1$YVariable
              group2vec <- data2$YVariable
              
              #Differences Vector
              diffvec <- group1vec - group2vec
              
              #Calculating residuals
              residuals <- diffvec - mean(diffvec)
              
              #Creating plot
              plot <- qqnorm(residuals) 
              plot <- qqline(residuals)
              
              return(plot)
            } 
          } 
        } 
        
        ##ANOVA
      } else if(input$tests == "ANOVA") {
        
        #At least 2 levels for X Variable
        if(nlevels(XVariable) > 1){
        
        #Two way ANOVA
        if(nlevels(ColorVariable) > 1){
          model <- aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable)
        }
        
        #One way ANOVA
        else{
          model <-  aov(YVariable ~ XVariable)
        }
        
        #Creating plot
        plot <- qqnorm(model$residuals) 
        plot <- qqline(model$residuals)
        
        return(plot)
        
        }
        
        ##Block Design
      } else if (input$tests == "Block Design") {
        
        #Error Message if PlayerID is selected as X-variable or Color
        if(input$xvar == "PlayerID" | input$color == "PlayerID"){
          
        } else {
          
          #At least 2 levels for X Variable
          if(nlevels(XVariable) > 1){
          
          #Two Way Blocked ANOVA
          if(nlevels(ColorVariable) > 1){
            model <-  aov(YVariable ~  XVariable + PlayerID + ColorVariable + XVariable*ColorVariable)
            
            #One Way Blocked
          } else{
            model <-  aov(YVariable ~ XVariable + PlayerID)
          }
          
          #Creating plot
          plot <- qqnorm(model$residuals) 
          plot <- qqline(model$residuals)
          
          return(plot)
          }
        }
      }
      
      
      } 
     }
      
    })
    
    #Returning visual
    return(myplot)
 
     })
  
  
  #Download Data
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    })
  
   
} #Closes Server

#Creating Shiny App
shinyApp(ui = ui, server = server)