#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(plotly)
library(shinyjs)
library(DT)
library(shinyBS)


# Define UI for application that draws a histogram
shinyUI(tagList(
  useShinyjs(),
  dashboardPage(
    title = "Billboard dashboard",
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      div(id= "manual", h3("Manual")),
      selectizeInput("performer", label = "Performer: ", choices = NULL),
      selectInput("song_selected", "Feature song", choices = NULL)
    ),
    
    
    body = dashboardBody(
      fluidRow(div(id= "box0",
                   infoBoxOutput("famous_rank"))),
      fluidRow(
        div(id = "box1", infoBoxOutput("n_song_top")),
        infoBoxOutput("n_song_top10"),
        infoBoxOutput("n_week_top10")
      ),
      fluidRow(column(width = 6,
                      h2("Song characteristics"),
                      plotlyOutput("performer_radar")),
               column(width = 6,
                      h2("Ranking over years"),
                      plotlyOutput("song_rank")))
      
      
    )
    
    
    # Sidebar with a slider input for number of bins
  )
))
