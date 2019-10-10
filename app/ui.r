
library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(leaflet)
library(Rcpp)
library(varhandle)
library(gpclib)
library(shinyWidgets)
library(formattable)
library(RColorBrewer)
library(tigris)
library(rgdal)
library(maptools)
library(rgeos)
library(bit64)

##load data
#data_raw_1 <- fread('../data/Raw Data 1.csv')
#data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw_1 <- fread('Raw Data 1.csv')
data_raw_2 <- fread('Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

#nbhd <- fread('../data/nbhd.csv')
nbhd <- fread('nbhd.csv')
allNbhd <- nbhd$NEIGHBORHOOD

##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')

#########################################################

#All listed cuisines
allCuisines <- sort(unique(data_raw$`CUISINE DESCRIPTION`))
allCuisines <- allCuisines[allCuisines != "Not Listed/Not Applicable"]
cuisine <- allCuisines
#All listed boroughs
allBoros <- unique(data_raw$BORO)

#All Grades
allGrades <- sort(unique(data_raw$GRADE))
allGrades <- allGrades[allGrades != ""]
#Main UI
shinyUI(
  fluidPage(theme = shinytheme("darkly"),
  
  navbarPage(title = p(class="h","Tasty & Safety"),
             
             ##Map Tab
             tabPanel("Score Overview",
                      fluidRow(
                        
                        ##side bar controls
                        column(2,
                               pickerInput("boromap","Select Borough" ,allBoros,options = list('actions-box' = TRUE),multiple = TRUE),
                               br(),
                               pickerInput("cuisinemap","Select Cuisine" ,allCuisines,options = list('actions-box' = TRUE),multiple = TRUE)
        
                        ),
                        
                        ##Tabset
                        column(10, 
                               tabsetPanel(
                                 tabPanel("Map",
                                   #Print out map with slider and underneath a datatable
                                   leafletOutput("nycmap", height = '700px')
                                 ),
                                 
                                 tabPanel("Data Table",

                                    dataTableOutput("map_data_table", height = '400px')

                                 )
                               
                               )
                        )
                      )
             ),
             ##Comparison tab
             tabPanel("Compare",
                      fluidRow(
                        
                        ##side bar controls
                        column(2,
                               selectInput("cuisine1","Cuisine Type 1" ,c('All',allCuisines),multiple = TRUE,selected = 'All'),
                               selectInput("boro1","Borough 1" ,c('All',allBoros),multiple = TRUE,selected = 'All'),
                               br(),
                               selectInput("cuisine2","Cuisine Type 2" ,c('All',allCuisines),multiple = TRUE,selected = 'All'),
                               selectInput("boro2","Borough 2" ,c('All',allBoros),multiple = TRUE,selected = 'All'),
                               br(),
                               br(),
                               br(),
                               br(),
                               sliderInput("slider1", label='Display Number '
                                           ,min=5,max=20,value=10),
                               
                               # textInput('zip_input', "Zip:", value='10027'),
                               checkboxGroupInput("critFlag", "Severity",
                                                  c("Critical" = 'Y',
                                                    "Non-Critical" = 'N'),selected = c('Y','N'))
                        ),
                        
                        ##Tabset
                        column(10,
                               tabsetPanel(
                                 ##table and barchart
                                 tabPanel("Top Violations", 
                                          dataTableOutput("top_vio_table1")
                                          # ,plotlyOutput("top_vio_bar1",height = "auto",width = "auto")
                                          ,dataTableOutput("top_vio_table2")
                                 )
                                 ##Inpsection score distribution
                                 ,tabPanel("Inspection Score" ,
                                           plotOutput("score_hist",height = '1000px')
                                 ),tabPanel("Inspection Grade" ,
                                            br(),
                                            br(),
                                            plotlyOutput("grade_pie",height = '1000px')
                                 )
                               )
                        )
                      )
             ),
             ##Individual Restaurant Info
             tabPanel("Find Your Restaurant",
                      fluidRow(
                        
                        ##side bar controls
                        column(2,                  
                          pickerInput("speech1","Cuisine Type:" ,allCuisines,multiple = TRUE,options = list('actions-box' = TRUE)
                          ),
                          pickerInput("speech2","Borough:" ,allBoros,multiple = TRUE,options = list('actions-box' = TRUE)
                          ),
                          pickerInput("variable", "Grade:",allGrades, multiple=TRUE,options = list('actions-box' = TRUE)),
                          pickerInput("nbhd","Neighborhood:" ,allNbhd,multiple = TRUE,options = list('actions-box' = TRUE)
                          )
                         
                          
                        ),
                        column(10,
                               tabsetPanel(
                                 ##table and barchart
                                 tabPanel("Search", 
                                          leafletOutput("mymap2",height = '300px')
                                          ,
                                          dataTableOutput("NYC_Restaurants")
                                 )
                                 ##Inpsection score distribution
                                 ,tabPanel("Details",
                                           dataTableOutput("Restaurant_Detail")    
                                 )
                               )
                               
                        )
                      )
             ),
             tabPanel("Contact",fluidPage(
               sidebarLayout(
                 sidebarPanel(h1("Contact Information")),
                 mainPanel(
                   # only the last output works
                   
                   hr(),
                   h1(("If you are interested in our project, you can contact us.")),
                   hr(),
                   h6(("Wenyue Wu")),
                   h6(("ww2501@columbia.edu")),
                   h6(("Adam Jordan Kravitz")),
                   h6(("ajk2254@columbia.edu")),
                   h6(("Na Zhuo")),
                   h6(("nz2297@columbie.edu")),
                   h6(("Chongyu He")),
                   h6(("ch3379@columbia.edu")),
                   h6(("Luyue Chen")),
                   h6(("lc3363@columbia.edu"))
                 ))
             ))
  )
  )
)
