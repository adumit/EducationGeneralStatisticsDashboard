## ui.R ##
library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Student Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Total Students Attending", tabName = "totalStuds", icon = icon("bar-chart-o")),
      menuItem("SAT Admission Statistics", tabName = "SAT", icon = icon("bar-chart-o")),
      menuItem("High School GPA", tabName = "GPA", icon = icon("bar-chart-o")),
      menuItem("Admission Demographics", tabName = "admDem", icon = icon("line-chart")),
      menuItem("Parental Income Data", tabName = "inc", icon = icon("line-chart")),
      id = "sidebar"
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "totalStuds",
        fluidRow(
          column(width=4,
                 radioButtons("studSplit", "Data splits:",
                              choices = list("Total students admitted" = "total",
                                             "Gender" = "g",
                                             "Athletes" = "a",
                                             "Transfers" = "t",
                                             "Veterans" = "v",
                                             "Underrepresented Minorities" = "urm",
                                             "Low income students" = "lis",
                                             "International Students" = "is")),
                 sliderInput("yearsStud", label=h4("Years to include"),
                             min = 2000, max = 2014, value = c(2003,2007)),
                 radioButtons("linegraph", label = h4("Line graph or bar graph?"),
                              choice = c("Line Graph" = "LG",
                                         "Bar Graph" = "BG")
                              ),
                 actionButton("createPlot", label = "Create Plot")
                 ),
          column(width=8,
                 plotOutput("plotTotalStuds")
                 )
        )
      ),
      tabItem(tabName = "SAT",
        fluidRow(
          column(width=4,
                 helpText("Note: The SAT changed in 2006 by splitting the reading section into reading and writing. 
                          This changed the total SAT score to be out of 2400 instead of 1600 and added the current reading
                          and writing sections."),
                 radioButtons("satSplits", label = "Which SAT Scores would you like to see?",
                             choices = list("Total SAT Score" = "SATRT",
                                            "SAT Math Score" = "SATRM",
                                            "SAT Writing Score" = "SATRW",
                                            "SAT Reading Score" = "SATRR")),
                 sliderInput("yearsSAT", label=h4("Years to include"),
                             min = 2000, max = 2014, value = c(2003,2007)),
                 actionButton("createPlotSAT", label = "Create Plot")
                 ),
          column(width=8,
                 plotOutput("plotSAT")
                 ) 
        )
      ),
      tabItem(tabName = "GPA",
        fluidRow(
          column(width=4,
                 sliderInput("yearsGPA", label=h4("Years to include"),
                             min = 2000, max = 2014, value = c(2003,2007)),
                 actionButton("createPlotGPA", label = "Create Plot")
                 ),
          column(width=8,
                 plotOutput("plotGPA")
                 )     
        )
      ),
      tabItem(tabName = "admDem",
        fluidRow(
          column(width=4,
                 checkboxGroupInput("ethn", label = h4("Choose Ethnicity"),
                                    choices = list("African-American/Black",
                                                   "American Indian/Alaska Native",
                                                   "Chinese-American/Chinese",
                                                   "East Indian/Pakistani",
                                                   "Filipino/Filipino-American",
                                                   "Hawaiian", 
                                                   "Hispanic-other",
                                                   "Japanese American/Japanese",
                                                   "Korean-American/Korean",
                                                   "Latino/Other Spanish",
                                                   "Mexican-Am/Mexican/Chicano",
                                                   "Other",
                                                   "Other Asian",
                                                   "Pacific Islander. other",
                                                   "Puerto Rican-Comm.",
                                                   "SE Asian;not Vietnamese",
                                                   "Vietnamese",
                                                   "White/Caucasian")),
                 radioButtons("linegraph2", label = h4("Line graph or bar graph?"),
                              choice = c("Line Graph" = "LG",
                                         "Bar Graph" = "BG")),
                 sliderInput("yearsETHN", label=h4("Years to include"),
                             min = 2000, max = 2014, value = c(2003,2007)),
                 actionButton("createPlotAdm", label = "Create Plot")
                 ),
          column(width=8,
                 plotOutput("plotAdmDem")
                 )
        )
      ),
      tabItem(tabName = "inc",
        fluidRow(
          column(width=4,
                 checkboxInput("bracket", "Look at admitted students parental income year by year?"),
                 sliderInput("yearsINC", label=h4("Years to include"),
                             min = 2000, max = 2014, value = c(2003,2007)),
                 uiOutput("UIIncome"),
                 uiOutput("UIIncome2"),
                 actionButton("createPlotINC", label = "Create Plot")
                 ),
          column(width=8,
                 plotOutput("plotINC")
                 )     
        )
      )
    )
  )
)