library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
shinyUI(
  fluidPage(
    img(src = "WHO.png", width = c("12%")),
    img(src = "Usher_Org.png", width = c("30%")),
    br(),
    h2("Burden estimation of common communicable diseases in settlements of displaced populations"),
    "Dr. You Li", a(href = "mailto:<You.Li2@ed.ac.uk", " (You.Li2@ed.ac.uk)"), ", University of Edinburgh.",
    "  Last update: 21-Oct-2019",
    hr(),
    sidebarLayout(
      sidebarPanel(h4("Step 1: download and complete the Excel template"),
                   a(href = "https://github.com/leoly2017/WHO_BoDManual/raw/GitHub/Worksheets_BoDManual_YL.xlsx", 
                     "Download xlsx (preferred)"),
                   "OR",
                   a(href = "https://github.com/leoly2017/WHO_BoDManual/raw/GitHub/Worksheets_BoDManual_YL.xls", 
                     "Download xls"),
                   h4("Step 2: upload the completed the Excel template"),
                   uiOutput("uifile"), 
                   uiOutput("uioption"),
                   uiOutput("uioption2"),
                   uiOutput("uioption3"),
                   width = 3),
      mainPanel(uiOutput("uiresults")
                )
    )
    
    
  )
)
