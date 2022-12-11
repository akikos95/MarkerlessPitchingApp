### Alex Kikos ###
### Motion Capture Markerless Pitching Data ###
### Fall 2022 ###


rm(list=ls())

library(here)
library(rjson)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rsconnect)
library(shiny)
library(plotly)


#all pitch json files are stored in a folder called 'Data' in this current path
path <- here("Data")
#creates list for all paths of json files
files <- list.files(path, pattern = "*.json", full.names = TRUE)

#iterates and creates objects for each json file found above
for (i in 1:length(files)){
  assign(paste0("pitch", i),fromJSON(files[i]))
}


## START OF THE UI ##
ui <- fluidPage(
  
  titlePanel(
    h3("Pitch Data App")),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      width = 2,
      selectInput("sel_pitch", "Select Pitch Sequence:",
                  choices = c("pitch1","pitch2","pitch3","pitch4","pitch5","pitch6","pitch7","pitch8"
                              ,"pitch9","pitch10","pitch11","pitch12","pitch13","pitch14","pitch15","pitch16")),
      radioButtons("sel_joint", "Select Joint To Compare Angle Between Right Hip:",
                   choices = c("Right Elbow" = "rElbow",
                               "Right Wrist" = "rWrist",
                               "Right Shoulder" = "rShoulder")),
    ), #SIDEBAR PANEL END
    
    mainPanel(
      plotlyOutput(outputId = "plot", height="80vh", width = "80vw"),
      htmlOutput('text')
      
    ) #main end
  ) #sidebar layout end
) #ui end


#SERVER File
server <- function(input, output, session) {
  
  output$plot<-renderPlotly({
    
    #dataset selection (and usage) depends on what pitch the user chooses to view
    pitch <- paste(input$sel_pitch)
    currPitch <<- get(pitch)
    
    #parses the pitch data to put all centroid info in a df
    centroid <<- as.data.frame(currPitch$samples$people$centroid)
    #parses the pitch data to put all joint info in a df
    joints <<- as.data.frame(currPitch$samples$people$joints)
    #merges centroid & joint data together, based on time variable
    currPitch <<- merge(centroid,joints,by = "time" )
    #separates every column with multiple values in it separated by comma into their own columns (20 cols -> 57)
    # for (x in 2:57){
    #   currPitch <<- currPitch %>% unnest_wider(colnames(currPitch[x]), names_sep=",")
    # }
    
    if (input$sel_joint == "rElbow"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rElbow[x])
        #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Elbow"
    }
    else if (input$sel_joint == "rWrist"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rWrist[x])
        #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Wrist"
    }
    else if (input$sel_joint == "rShoulder"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rShoulder[x])
        # #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Shoulder"
    }
    
    maxAngle <- paste("Max Angle Reached:", round(max(currPitch$angle),1))
    avgAngle <- paste("Average Angle Reached:", round(mean(currPitch$angle),1))
    
    plotTitle <- paste("Angle Changes Before/After Ball Release (t=0) Between Between Right Hip &", currJoint)
    # #plotTitle <- toupper(plotTitle)
    
    
    #creaing the output plot
    anglePlot <-  plot_ly(currPitch, x=~time, y = ~angle, name = 'Average Metric Score', type = 'scatter', mode = 'lines+markers',
                          line = list(color = 'rgb(11,30,93)', width = 3),
                          marker = list(color = 'rgb(221,33,44)', size = 4))
    anglePlot <- anglePlot %>%
      layout(title = plotTitle, yaxis = list(title = 'Angle (Degrees) Between Joints'), xaxis = list(title = 'Time (seconds)'))
    
    return(
      anglePlot
    )
    
  })
  
  
  valueText <- reactive({
    pitch <- paste(input$sel_pitch)
    currPitch <<- get(pitch)
    
    #parses the pitch data to put all centroid info in a df
    centroid <<- as.data.frame(currPitch$samples$people$centroid)
    #parses the pitch data to put all joint info in a df
    joints <<- as.data.frame(currPitch$samples$people$joints)
    #merges centroid & joint data together, based on time variable
    currPitch <<- merge(centroid,joints,by = "time" )
    
    if (input$sel_joint == "rElbow"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rElbow[x])
        #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Elbow"
    }
    else if (input$sel_joint == "rWrist"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rWrist[x])
        #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Wrist"
    }
    else if (input$sel_joint == "rShoulder"){
      for (x in 1:nrow(currPitch)){
        h <- unlist(currPitch$rHip[x])
        e <- unlist(currPitch$rShoulder[x])
        # #dotproduct calculation
        dProd <- h %*% e
        hMag <- sqrt(h[1]^2 + h[2]^2 + h[3]^2)
        eMag <- sqrt(e[1]^2 + e[2]^2 + e[3]^2)
        calc <- dProd / (hMag * eMag)
        angleDiff <- 100*(180 * acos(calc))/pi
        currPitch$angle[x] <- angleDiff
      }
      currJoint <- "Right Shoulder"
    }
    
    maxAngle <- paste("Max Angle Reached:", round(max(currPitch$angle),1))
    avgAngle <- paste("Average Angle Reached:", round(mean(currPitch$angle),1))
    
    vals = paste(maxAngle,  " .......... ", avgAngle)
    
    return (
      vals
    )
  })
  
  #Render the text so that it is available in the UI
  output$text <- renderText(valueText())
  
}

#executes both UI & Server files to run as one
shinyApp(ui, server)
