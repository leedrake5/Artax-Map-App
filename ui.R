library(shiny)
library(dplyr)
library(DT)


# Define UI for application that draws a histogram
shinyUI(navbarPage("AES", id="nav",
tabPanel("Map",
titlePanel("Single Element Plot"),
sidebarLayout(
sidebarPanel(
fileInput('file1', 'Choose file to upload',
accept = c('.xlsx')
),



uiOutput('inElements'),

tags$hr(),


selectInput(
"colorramp", "Color Ramp",
c("Terrain" = "terrain.colors(",
"Rainbow" = "rainbow(",
"Heat" = "heat.colors(",
"Topo" = "topo.colors(",
"CM" = "cm.colors("),

selected="Terrain"),

sliderInput("colorrampvalues", label = "Steps", value=10, min=2, max=30),

tags$hr(),



checkboxInput('interpolate', "Interpolation"),

sliderInput("resolution", label = "Interpolation Resolution", value=100, min=10, max=1000),


tags$hr()


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("simpleMap", height = 600,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
)),

tabPanel("Line",
titlePanel("Multi-element lines"),
sidebarLayout(
sidebarPanel(
fileInput('file2', 'Choose file to upload',
accept = c('.xlsx')
),



uiOutput('inElements'),

tags$hr(),


selectInput(
"colorramp", "Color Ramp",
c("Terrain" = "terrain.colors(",
"Rainbow" = "rainbow(",
"Heat" = "heat.colors(",
"Topo" = "topo.colors(",
"CM" = "cm.colors("),

selected="Terrain"),

sliderInput("colorrampvalues", label = "Steps", value=10, min=2, max=30),

tags$hr(),



checkboxInput('interpolate', "Interpolation"),

sliderInput("resolution", label = "Interpolation Resolution", value=100, min=10, max=1000),


tags$hr()


),

mainPanel(
fluidRow(
column(width = 11, class = "well",
plotOutput("simpleMap", height = 600,
dblclick = "plot1_dblclick",
brush = brushOpts(
id = "plot1_brush",
resetOnNew = TRUE
)))))
))

))


