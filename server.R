library(shiny)
library(ggplot2)
library(plyr)
load("data/sampleEducationalDashboardData.rda")
shinyData = shinySampleData[!is.na(shinySampleData$YEAR),]
shinyData = shinyData[!is.na(shinyData$ADMIT_TERM),]

shinyServer(function(input, output) {
  
  getData <- reactive({
    if (input$sidebar == "totalStuds") {
      data = subset(shinyData, YEAR >= input$yearsStud[1] & YEAR <= input$yearsStud[2] & 
                      ADMIT_TERM >= input$yearsStud[1] & ADMIT_TERM <= input$yearsStud[2])
      return(data)
    }
    else if (input$sidebar == "SAT") {
      data = subset(shinyData, YEAR >= input$yearsSAT[1] & YEAR <= input$yearsSAT[2] & 
                      ADMIT_TERM >= input$yearsSAT[1] & ADMIT_TERM <= input$yearsSAT[2])
      return(data)
    }
    else if (input$sidebar == "GPA") {
      data = subset(shinyData, YEAR >= input$yearsGPA[1] & YEAR <= input$yearsGPA[2] & 
                      ADMIT_TERM >= input$yearsGPA[1] & ADMIT_TERM <= input$yearsGPA[2])
      return(data)
    }
    else if (input$sidebar == "admDem") {
      data = subset(shinyData, YEAR >= input$yearsETHN[1] & YEAR <= input$yearsETHN[2] & 
                      ADMIT_TERM >= input$yearsETHN[1] & ADMIT_TERM <= input$yearsETHN[2])
      finalData = data.frame()
      for (i in 1:length(input$ethn)) {
        tempData = subset(data, ETHN_DESC == input$ethn[i])
        finalData = join(tempData, finalData, type = "full")
      }
      return(finalData)
    }
    else if (input$sidebar == "inc") {
      data = subset(shinyData, YEAR >= input$yearsINC[1] & YEAR <= input$yearsINC[2] & 
                      ADMIT_TERM >= input$yearsINC[1] & ADMIT_TERM <= input$yearsINC[2])
      useData = data.frame()
      if (input$bracket == FALSE) {
        return(data)
      }
      else {
        for (i in 1:length(input$splitIncome)) {
          tempData = subset(data, INCOMEB == input$splitIncome[i])
          useData = join(useData, tempData, type="full")
        }
        return(useData)
      }
    }
  })
  
  output$UIIncome <- renderUI({
    if (input$bracket == T) {
      checkboxGroupInput("splitIncome", label = "Select which income brackets to visualize:",
                         choices = list("0-25K",
                                        "25K-50K",
                                        "50K-75K",
                                        "75K-100K",
                                        "100K-150K",
                                        "150K-200K",
                                        "200K+"))
    }
  })
  
  output$UIIncome2 <- renderUI({
    if (input$bracket == T) {
      radioButtons("linegraph3", label = h4("Line graph or bar graph?"),
                   choice = c("Line Graph" = "LG",
                              "Bar Graph" = "BG"))
    }
  })

  output$plotTotalStuds <- renderPlot({
    if (input$createPlot == 0) {
      return()
    }
    isolate({
      if (input$sidebar == "totalStuds") {
        if (input$studSplit == "total") {
          plot = ggplot(data=getData(), 
                        aes(x=factor(ADMIT_TERM)))
          labels = labs(title = paste("Total Number of Students Admitted from", 
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])) ,
                        x = "Years",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=1))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar()
          }
        }
        else if (input$studSplit == "g") {
          newData = getData()
          newData = subset(newData, GENDER == "M" | GENDER == "F")
          plot = ggplot(data=newData,
                        aes(x=factor(ADMIT_TERM), fill=GENDER))
          labels = labs(title = paste("Total Number of Males and Females Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Years",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=GENDER, color=GENDER)) + scale_color_discrete(name="Gender",
                                                                                                labels=c("Female", "Male"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="Gender",
                                                                              labels=c("Female", "Male"))
          }
        }
        else if (input$studSplit == "a") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), fill=ATHLETE))
          labels = labs(title = paste("Number of Athletes and Non-athletes Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Year",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=ATHLETE, color=ATHLETE)) + scale_color_discrete(name="Admit Type",
                                                                                                  labels=c("Student", "Student-Athlete"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="Admit Type",
                                                                              labels=c("Student", "Student-Athlete"))
          }
        }
        else if (input$studSplit == "v") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), fill=VET))
          labels = labs(title = paste("Total Number of Veterans and Non-Veterans Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Year",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=VET, color=as.factor(VET))) + scale_color_discrete(name="Veteran Status",
                                                                                                     labels=c("Non-Vet", "Veteran"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="Veteran Status",
                                                                              labels=c("Non-Vet", "Veteran"))
          }
        }
        else if (input$studSplit == "t") {
          plot = ggplot(data=getData(),
                        aes(x=factor(ADMIT_TERM), fill=ADMIT_LEVEL))
          labels = labs(title = paste("Total Number of Transfers and Non-transfers Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Years",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=ADMIT_LEVEL, color=ADMIT_LEVEL)) + scale_color_discrete(name="Admit Status")
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="Admit Status")
          }
        }
        else if (input$studSplit == "urm") {
          newData = getData()
          newData = subset(newData, !is.na(IS_URM))
          plot = ggplot(data = newData,
                        aes(x=factor(ADMIT_TERM), fill=factor(IS_URM)))
          labels = labs(title = paste("Total Number of URMs and Non-URMs Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Year",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=IS_URM, color=as.factor(IS_URM))) + 
              scale_color_discrete(name="Underrepresented Minority Status", labels=c("Not URM", "Is URM"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") +
              scale_fill_discrete(name="Underrepresented Minority Status", labels=c("Not URM", "Is URM"))
          }
        }
        else if (input$studSplit == "lis") {
          plot = ggplot(data = subset(getData(), !is.na(LOWINCOME)),
                        aes(x=factor(YEAR), fill=factor(LOWINCOME)))
          labels = labs(title = paste("Total Number of Low Income and Non-Low Income Students Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Year",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=LOWINCOME, color=as.factor(LOWINCOME))) + scale_color_discrete(name="Low Income Status",
                                                                                                                 labels=c("Not Low Income", "Low Income"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="Low Income Status",
                                                                              labels=c("Not Low Income", "Low Income"))
          }
        }
        else if (input$studSplit == "is") {
          newData = getData()
          newData$INTERN[is.na(newData$INTERN)] = 0
          plot = ggplot(data = newData,
                        aes(x=factor(YEAR), fill=as.factor(INTERN)))
          labels = labs(title = paste("Total Number of International and Non-International Students Admitted from",
                                      toString(input$yearsStud[1]), "to", toString(input$yearsStud[2])),
                        x = "Year",
                        y = "Number of Students")
          if (input$linegraph == "LG") {
            plot = plot + geom_freqpoly(aes(group=INTERN, color=factor(INTERN))) + scale_color_discrete(name="International Student Status",
                                                                                                        labels=c("National", "International"))
          }
          else if (input$linegraph == "BG") {
            plot = plot + geom_bar(position = "dodge") + scale_fill_discrete(name="International Student Status",
                                                                              labels=c("National", "International"))
          }
        }
      }
      plot = plot + labels
      print(plot)
    })
  })
  
  output$plotAdmDem <- renderPlot({
    if (input$createPlotAdm == 0) {
      return()
    }
    isolate({
      if (input$sidebar == "admDem") {
        plot = ggplot(data=getData(),
                      aes(x=factor(ADMIT_TERM), fill=ETHN_DESC))
        labels = labs(title = paste("Selected Ethnicities Admitted from", toString(input$yearsETHN[1]), "to", toString(input$yearsETHN[2])),
                      x = "Years",
                      y = "Number of Students")
        if (input$linegraph2 == "LG") {
          plot = plot + geom_freqpoly(aes(group=ETHN_DESC, color=ETHN_DESC)) + 
            scale_color_discrete(name="Ethnicities") +
            theme(legend.position="bottom", legend.direction="vertical")
        }
        else if (input$linegraph2 == "BG") {
          plot = plot + geom_bar(position = "dodge") +
            theme(legend.position="bottom", legend.direction="vertical")
        }
      }
      plot = plot + labels
      print(plot)
    })
  })
  
  output$plotGPA <- renderPlot({
    if (input$createPlotGPA == 0) {
      return()
    }
    isolate({
      if (input$sidebar == "GPA") {
        plot = ggplot(data = getData(),
                      aes(x=factor(YEAR), y=GPA))
        labels = labs(title=paste("High School GPA of Admitted Students from",
                                  input$yearsGPA[1], "to", input$yearsGPA[2]),
                      y = "High School GPA",
                      x = "Year")
        plot = plot + geom_boxplot()
      }
      plot = plot + labels
      print(plot)
    })
  })
  
  output$plotSAT <- renderPlot({
    if (input$createPlotSAT == 0) {
      return()
    }
    isolate({
      if (input$sidebar == "SAT") {
        if (input$satSplits == "SATRT") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), y=SATRT))
          labels = labs(title="Average Total SAT Score Over the Years",
                        ylab = "SAT Score",
                        xlab = "Year")
        }
        else if (input$satSplits == "SATRM") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), y=SATRM))
          labels = labs(title="Average SAT Math Score Over the Years",
                        y = "SAT Score",
                        x = "Year")
        }
        else if (input$satSplits == "SATRW") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), y=SATRW))
          labels = labs(title="Average SAT Writing Score Over the Years",
                        y = "SAT Score",
                        x = "Year")
        }
        else if (input$satSplits == "SATRR") {
          plot = ggplot(data = getData(),
                        aes(x=factor(YEAR), y=SATRR))
          labels = labs(title="Average SAT Reading Score Over the Years",
                        y = "SAT Score",
                        x = "Year")
        }
        plot = plot + geom_boxplot()
      }
      plot = plot + labels
      print(plot)
    })
  })
  
  output$plotINC <- renderPlot({
    if (input$createPlotINC == 0) {
      return()
    }
    isolate({
      if (input$bracket == FALSE) {
        plot = ggplot(data=getData(),
                      aes(x=INCOMEP))
        labels = labs(title = paste("Parental Income of Admitted Students From", 
                                   input$yearsINC[1], "to", input$yearsINC[2]),
                      y = "Count",
                      x = "Reported Parental Income")
        plot = plot + geom_density()
      }
      else if (input$bracket == TRUE) {
        plot = ggplot(data=getData(),
                      aes(x=factor(YEAR), fill=factor(INCOMEB)))
        labels = labs(title="Count of Students With Parental Income in Chosen Brackets",
                      y = "Count",
                      x = "Year")
        if (input$linegraph3 == "LG") {
          plot = plot + geom_freqpoly(aes(group=INCOMEB, color=INCOMEB)) + scale_color_discrete(name="Income Brackets")
        }
        else if (input$linegraph3 == "BG") {
          plot = plot + geom_bar(position = "dodge")
        }
      }
      plot = plot + labels
      print(plot)
    })
  })
})