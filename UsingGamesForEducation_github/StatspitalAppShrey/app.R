#STATSPITAL GAME APP FOR MORE DETAILED DATA

#Loading Libraries
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(gdata)
library(curl)


#Importing Data

#allData = read.csv("detailedData.csv")
allData = read.csv("https://www.stat2games.sites.grinnell.edu/data/statspital/getdetaileddata.php")


# #Filter Tutorial data
# allData <- filter(allData, Level > 0)

#To Lower
allData$PlayerID <- tolower(allData$PlayerID)
allData$GroupID <- tolower(allData$GroupID)

#Convert to Factor/Character
allData$Level <- as.factor(allData$Level)
allData$Patient <- as.factor(allData$Patient)
allData$Med <- as.factor(allData$Med)
allData$PatientType <- as.factor(allData$PatientType)
allData$GroupID <- as.character(allData$GroupID)
allData$PlayerID <- as.character(allData$PlayerID)

#Health improvement and log of health improvement columns
allData <- mutate(allData, HealthImprovement = PostMedHealth - PriorMedHealth)
allData <- mutate(allData, LogHealthImprovement = log(HealthImprovement))



#For Visuals
allData["None"] <- "None"
allData$None <- as.factor(allData$None)

#For UI inputs
all_groups <- sort(unique(allData$GroupID))
all_players <- sort(unique(allData$PlayerID))

#renaming some variables
allData = allData %>%
  rename("PriorHealth" = "PriorMedHealth", "PatientNumber" = "Patient", "TotalPatients" = "NumPatients", "Medicine" = "Med")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Statspital Game Data Visualization"),

    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "groupID",
                    label = "Group ID:", 
                    choices =  all_groups,
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = "test"),
        
        selectInput(inputId = "playerID",
                    label = "Player ID:",
                    choices =  c("all", all_players),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = "all"),        
        
        
        selectInput("levels", "Level:",
                    choices = c("0", "1", "2", "3"),
                    multiple = TRUE,
                    selected = "1"),
        
        selectInput(inputId = "xvar",
                    label = "X Variable:",
                    choices = c("Medicine", "Level", "PatientNumber", "PatientType", "PriorHealth", "PriorMed", "DoseNumber"),
                    selected = "Medicine",
                    multiple = FALSE),
        
        selectInput(inputId = "yvar",
                    label = "Y Variable:",
                    choices = c("HealthImprovement", "LogHealthImprovement", "Time"),
                    selected = "HealthImprovement",
                    multiple = FALSE),
        
        checkboxInput('bplot',"Add boxplot",FALSE),
        
        selectInput(inputId = "color",
                    label = "Color by:",
                    choices = c("None", "PlayerID", "Medicine", "Level", "PatientNumber", "PatientType", "PriorMed", "DoseNumber"),
                    selected = "None",
                    multiple = FALSE),
        
        selectInput(inputId = "facets",
                    label = "Facet by:",
                    choices = c("None", "Win", "PlayerID", "Medicine", "Level", "PatientNumber", "PatientType", "PriorMed", "DoseNumber"),
                    selected = "None",
                    multiple = FALSE),
        
        
        selectInput(inputId = "tests",
                    label = "Statistical Tests:",
                    choices = c("None", "two-sample t-test", "ANOVA",
                                "Two Sample Randomization Test"),
                    selected = "None",
                    multiple = FALSE),
        
        downloadButton('downloadData', label = "Statspital Data")),
  
      mainPanel(
        tabsetPanel(
          tabPanel("General", 
                   plotOutput(outputId = "Plot"),
                   verbatimTextOutput("anova"),
                   verbatimTextOutput("ttest"),
                   verbatimTextOutput("test"),
                   verbatimTextOutput("twor")),
          
          
          tabPanel("Residuals", uiOutput("residualtext"),
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), 
                                 plotOutput("rplot1"), plotOutput("rplot2")))))
        
      
    )
  )
)



#Server
server <- function(input, output, session) {
  
  
  #Reactive Data
  plotDataR <- reactive({
    
    if("all" %in% input$playerID){
      data <- allData %>% filter(Level %in% input$levels, GroupID %in% input$groupID)
    } else {
      data <- allData %>% filter(Level %in% input$levels, PlayerID %in% input$playerID, GroupID %in% input$groupID)
    }
    
    return(data)
  })
  
  
  # Dynamic PlayerID Input
  observe({
    
    # req() requires a selection from GroupID before any output
    # or reactivity occurs (keeps the app from crashing)
    req(input$groupID)   
    
    gamedata <- filter(allData, GroupID %in% input$groupID)
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  })
  
  
  #Dynamic Level Input 
  observe({
    req(input$groupID)   
    
    if("all" %in% input$playerID){
      gamedata <- filter(allData, GroupID %in% input$groupID)
      
    } else{
      gamedata <- filter(allData, GroupID %in% input$groupID, PlayerID %in% input$playerID)
    }
    
    updateSelectInput(session, 
                      "levels",
                      choices = sort(unique(gamedata$Level)),
                      selected = sort(unique(gamedata$Level))[c(2)])
  })
  
  
  #Creating Visual
  output$Plot <- renderPlot({
    
    #Using reactive data
    plotData <- plotDataR()
    
    #Require
    req(input$groupID)
    req(input$playerID)
    req(input$levels)
    
    
    #If boxplot option is selected
    if (input$bplot == "TRUE"){
      
      #If there is a color option selected
      if(input$color != "None"){
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_boxplot(outlier.shape = NA) +
          geom_point(position = position_jitterdodge()) + 
          scale_color_manual(values=c("blue", "red", "orange", "purple", "lightblue", "pink", "green", "gray", "black", "lightsalmon", "lightskyblue", "maroon"))+
          labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) + 
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
        
        #If there no color option selected
      } else{
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, fill = input$color)) +
          geom_boxplot(outlier.shape = NA) +
          geom_point(position = position_jitterdodge()) + 
          scale_fill_manual(values = rep(NA, 2)) +
          labs(title = paste("Plot of",input$yvar, "by",input$xvar)) + 
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
      }
      
      #If boxplot option is not selected  
    } else {
      
      #If there is a color option selected
      if(input$color != "None"){
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_point(position = position_jitterdodge()) + 
          scale_color_manual(values=c("blue", "red", "orange", "purple", "lightblue", "pink", "green", "gray", "black", "lightsalmon", "lightskyblue", "maroon"))+
          labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
        
        #If color option is not selected
      } else{
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, fill = input$color)) +
          geom_point(position = position_jitterdodge()) + 
          scale_fill_manual(values = rep(NA, 2)) + 
          labs(title = paste("Plot of",input$yvar, "by",input$xvar)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
      }
    }
    
    #Facet and No Color
    if(input$facets != "None" & input$color == "None") {
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16)) 
      
      #Facet and Color
    } else if(input$facets != "None" & input$color != "None"){
      myplot <- myplot + facet_wrap(as.formula(paste("~", input$facets))) +
        labs(title = paste("Plot of",input$yvar, "by", input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
        theme(strip.text = element_text(size = 16)) 
      
    }
    
    #Two sample t test
    output$ttest <- renderPrint({
      
      #Reactive data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      YVariable = drop.levels(YVariable)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      FacetVariable = plotData %>% pull(input$facets)
      FacetVariable = drop.levels(as.factor(FacetVariable))
      
      xlevel = nlevels(XVariable)
      colorlevel = nlevels(ColorVariable)
      facetlevel = nlevels(FacetVariable)
      
      if(input$tests == "two-sample t-test"){
        
        if(input$facets == "None"){
          
          if(xlevel == 2){
            
            if(input$color == input$xvar | colorlevel == 1 | input$color == "None"){
              
              t.test(YVariable ~ XVariable)
              
            } else{"Only two groups can be compared in a two sample t-test. Choose a different color option."}
            
            
          } else {"X Variable must have exactly 2 levels to run the two sample t-test."}
          
        } else {"Facet option must be set to None to run the two sample t-test."}
        
      }
      
    })
    
    #ANOVA
    output$anova <- renderPrint({
      
      #Reactive data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      YVariable = drop.levels(YVariable)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      FacetVariable = plotData %>% pull(input$facets)
      FacetVariable = drop.levels(as.factor(FacetVariable))
      
      xlevel = nlevels(XVariable)
      colorlevel = nlevels(ColorVariable)
      facetlevel = nlevels(FacetVariable)  
      
      #Anova test selected (8 possible outcomes)
      if(input$tests == "ANOVA"){
        
        if(xlevel > 1 & colorlevel > 1 & facetlevel > 1){
          
          anovatest <- aov(YVariable ~ XVariable + ColorVariable + FacetVariable + 
                             XVariable*ColorVariable + 
                             XVariable*FacetVariable +
                             ColorVariable*FacetVariable)
          
        } else if(xlevel > 1 & colorlevel > 1 & facetlevel < 2){
          
          
          anovatest <-  aov(YVariable ~ XVariable + ColorVariable +
                              XVariable*ColorVariable)
          
          
          
        } else if(xlevel > 1 & colorlevel < 2 & facetlevel > 1 ){
          
          
          anovatest <-  aov(YVariable ~ XVariable + FacetVariable +
                              XVariable*FacetVariable)
          
          
          
        } else if(xlevel > 1 & colorlevel < 2 & facetlevel < 2){
          
          
          anovatest <-  aov(YVariable ~ XVariable)
          
        } else if(xlevel < 2 & colorlevel > 1 & facetlevel > 1){
          
          
          anovatest <-  aov(YVariable ~ ColorVariable + FacetVariable +
                              ColorVariable*FacetVariable)
          
          
        } else if(xlevel < 2 & colorlevel > 1 & facetlevel < 2){
          
          anovatest <-  aov(YVariable ~ ColorVariable)
          
        } else if(xlevel < 2 & colorlevel < 2 & facetlevel > 1){
          
          anovatest <-  aov(YVariable ~ FacetVariable)
          
        } else if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
          
          message <- "At least two levels are needed to run the ANOVA"
          
        }
        
        #Returning Error message
        if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
          
          return(message)
          
          
          #If we ran an ANOVA   
        } else{
          
          #Making Tidy table and adding columns/rows
          tidyanova = tidy(anovatest)
          sum_df = sum(tidyanova$df)
          sum_ss = sum(tidyanova$'sumsq')
          tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
          tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
          tidyanova$meansq = round(tidyanova$meansq, digits = 2)
          tidyanova$statistic = round(tidyanova$statistic, digits = 2)
          
          return(tidyanova)
          
        } 
        
      }
    })
    
    
    #Two Sample Randomization Test
    output$twor <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      YVariable = drop.levels(YVariable)
      XVariable = plotData %>% pull(input$xvar)
      XVariable = drop.levels(as.factor(XVariable))
      
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      FacetVariable = plotData %>% pull(input$facets)
      FacetVariable = drop.levels(as.factor(FacetVariable))
      
      xlevel = nlevels(XVariable)
      colorlevel = nlevels(ColorVariable)
      facetlevel = nlevels(FacetVariable)
      
      if(input$tests == "Two Sample Randomization Test"){
        
        if(input$facets == "None"){
          
          if(xlevel == 2){
            
            if(input$color == input$xvar | colorlevel == 1 | input$color == "None"){
              
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
              R <- 10000
              results <- numeric()
              
              for(i in 1:R){
                samp <- sample(data$YVariable, size = length(data$YVariable), replace = FALSE)
                
                samp1 <- samp[1:length(group1vec)]
                samp2 <- samp[(length(group1vec) + 1):length(samp)]
                
                results[i] <- mean(samp1) - mean(samp2)
              }
              
              pvalue <- (1 + sum(results >= abs(meandiff)) + sum(results <= -abs(meandiff))) / (R+1)
              
              if(pvalue<0.0001){
                return(paste("P Value: Less than 0.0001"))
              }else{
                return(paste("P Value:", round(pvalue,5)))
              }
              
            } else{"Only two groups can be compared in a two sample randomization test. Choose a different color option."}
            
            
          } else {"X Variable must have exactly 2 levels to run the two sample randomization test."}
          
        } else {"Facet option must be set to None to run the two sample randomization test."}
        
      }
      
    })
    
    
    
    #Residual Histogram (RPLOT 1)
    output$rplot1 <- renderPlot({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #Test is Not None
      if(input$tests != "None"){
        
        #Setting Up
        YVariable = plotData %>% pull(input$yvar)
        YVariable = drop.levels(YVariable)
        XVariable = plotData %>% pull(input$xvar)
        XVariable = drop.levels(as.factor(XVariable))
        
        ColorVariable = plotData %>% pull(input$color)
        ColorVariable = drop.levels(as.factor(ColorVariable))
        FacetVariable = plotData %>% pull(input$facets)
        FacetVariable = drop.levels(as.factor(FacetVariable))
        
        xlevel = nlevels(XVariable)
        colorlevel = nlevels(ColorVariable)
        facetlevel = nlevels(FacetVariable)  
        
        ##Two sample t-test/Two Sample Randomization test
        if(input$tests %in% c("two-sample t-test", "Two Sample Randomization Test")){
          
          if(input$facets == "None"){
            
            if(xlevel == 2){
              
              if(input$color == input$xvar | colorlevel == 1 | input$color == "None"){
                
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
                
                
              } else{
                output$residualtext <- renderUI(HTML(paste(
                  em("A valid statistical test must be in place for the residual plots to be generated."))))
              }
              
              
            } else {
              output$residualtext <- renderUI(HTML(paste(
                em("A valid statistical test must be in place for the residual plots to be generated."))))
            }
            
          } else {
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
          }
          
          ##ANOVA
        } else if(input$tests == "ANOVA"){
          
          if(xlevel > 1 & colorlevel > 1 & facetlevel > 1){
            
            anovatest <- aov(YVariable ~ XVariable + ColorVariable + FacetVariable + 
                               XVariable*ColorVariable + 
                               XVariable*FacetVariable +
                               ColorVariable*FacetVariable)
            
          } else if(xlevel > 1 & colorlevel > 1 & facetlevel < 2){
            
            
            anovatest <-  aov(YVariable ~ XVariable + ColorVariable +
                                XVariable*ColorVariable)
            
            
            
          } else if(xlevel > 1 & colorlevel < 2 & facetlevel > 1 ){
            
            
            anovatest <-  aov(YVariable ~ XVariable + FacetVariable +
                                XVariable*FacetVariable)
            
            
            
          } else if(xlevel > 1 & colorlevel < 2 & facetlevel < 2){
            
            
            anovatest <-  aov(YVariable ~ XVariable)
            
          } else if(xlevel < 2 & colorlevel > 1 & facetlevel > 1){
            
            
            anovatest <-  aov(YVariable ~ ColorVariable + FacetVariable +
                                ColorVariable*FacetVariable)
            
            
          } else if(xlevel < 2 & colorlevel > 1 & facetlevel < 2){
            
            anovatest <-  aov(YVariable ~ ColorVariable)
            
          } else if(xlevel < 2 & colorlevel < 2 & facetlevel > 1){
            
            anovatest <-  aov(YVariable ~ FacetVariable)
            
          } else if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
            
            #No model will be run
            
          }
          
          #Returning Error message
          if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
            
            output$residualtext <- renderUI(HTML(paste(
              em("A valid statistical test must be in place for the residual plots to be generated."))))
            
            #If we ran an ANOVA   
          } else{
            
            #Remove Message
            output$residualtext <- renderUI({""})
            
            #Creating plot
            plot <- hist(anovatest$residuals, main = "Histogram of Residuals",
                         xlab = "Residuals",
                         ylab = "Count")
            
            return(plot)
            
          }
          
          
        }
        
        #Test option is none  
      } else{
        output$residualtext <- renderUI(HTML(paste(
          em("A valid statistical test must be in place for the residual plots to be generated."))))
      }
      
    })
    
    
    #Normal QQ Plot (RPLOT 2)
    output$rplot2 <- renderPlot({
      
      #Using reactive data
      plotData <- plotDataR()
      
      #Test is Not None
      if(input$tests != "None"){
        
        #Setting Up
        YVariable = plotData %>% pull(input$yvar)
        YVariable = drop.levels(YVariable)
        XVariable = plotData %>% pull(input$xvar)
        XVariable = drop.levels(as.factor(XVariable))
        
        ColorVariable = plotData %>% pull(input$color)
        ColorVariable = drop.levels(as.factor(ColorVariable))
        FacetVariable = plotData %>% pull(input$facets)
        FacetVariable = drop.levels(as.factor(FacetVariable))
        
        xlevel = nlevels(XVariable)
        colorlevel = nlevels(ColorVariable)
        facetlevel = nlevels(FacetVariable)  
        
        ##Two sample t-test/Two Sample Randomization test
        if(input$tests %in% c("two-sample t-test", "Two Sample Randomization Test")){
          
          if(input$facets == "None"){
            
            if(xlevel == 2){
              
              if(input$color == input$xvar | colorlevel == 1 | input$color == "None"){
                
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
          } 
          
          ##ANOVA
        } else if(input$tests == "ANOVA"){
          
          if(xlevel > 1 & colorlevel > 1 & facetlevel > 1){
            
            anovatest <- aov(YVariable ~ XVariable + ColorVariable + FacetVariable + 
                               XVariable*ColorVariable + 
                               XVariable*FacetVariable +
                               ColorVariable*FacetVariable)
            
          } else if(xlevel > 1 & colorlevel > 1 & facetlevel < 2){
            
            
            anovatest <-  aov(YVariable ~ XVariable + ColorVariable +
                                XVariable*ColorVariable)
            
            
            
          } else if(xlevel > 1 & colorlevel < 2 & facetlevel > 1 ){
            
            
            anovatest <-  aov(YVariable ~ XVariable + FacetVariable +
                                XVariable*FacetVariable)
            
            
            
          } else if(xlevel > 1 & colorlevel < 2 & facetlevel < 2){
            
            
            anovatest <-  aov(YVariable ~ XVariable)
            
          } else if(xlevel < 2 & colorlevel > 1 & facetlevel > 1){
            
            
            anovatest <-  aov(YVariable ~ ColorVariable + FacetVariable +
                                ColorVariable*FacetVariable)
            
            
          } else if(xlevel < 2 & colorlevel > 1 & facetlevel < 2){
            
            anovatest <-  aov(YVariable ~ ColorVariable)
            
          } else if(xlevel < 2 & colorlevel < 2 & facetlevel > 1){
            
            anovatest <-  aov(YVariable ~ FacetVariable)
            
          } else if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
            
            #No model will be run
            
          }
          
          #Returning Error message
          if(xlevel < 2 & colorlevel < 2 & facetlevel < 2){
            #No model will be run
            
            #If we ran an ANOVA   
          } else{
            
            #Creating plot
            plot <- qqnorm(anovatest$residuals) 
            plot <- qqline(anovatest$residuals)
            
            return(plot)
            
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
