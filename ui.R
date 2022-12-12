## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

shinyUI(
  navbarPage("Project 4",
             tabPanel("System 1",
                      sidebarPanel(
                        selectInput('genre', 'Select Genre', genre_list)
                        ),
                      mainPanel(
                        includeCSS("css/movies.css"),
                        fluidRow(
                          box(
                            width = 12, status = "info", solidHeader = TRUE, collapsible = FALSE,
                            title = "Top 5 Rated Movies For Selected Genre",
                            div(class = "selectgenre",
                                tableOutput('genreTable'),
                                ),
                            div(class = "selectgenre",
                                uiOutput('genreTableImgs'))
                            )
                          )
                        )
                      ),
             tabPanel("System 2",
                      mainPanel(
                        includeCSS("css/movies.css"),
                        fluidRow(
                          box(
                            width = 12, status = "info", solidHeader = TRUE, collapsible = FALSE,
                            title = "Step 1: Rate as many movies as possible",
                            div(class = "rateitems",
                                uiOutput('ratings')
                            )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results")
                          )
                        )
                      )
             )            
  )
)