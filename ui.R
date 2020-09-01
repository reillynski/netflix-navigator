#load packages
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)

ui <- fluidPage(
        includeCSS("styles.css"),
        useShinyjs(),
        div(class="navbar",
            navbarPage(title=span("Netflix Navigator", style = "color: #E50914"),
                 tabPanel("Analysis",
                          sidebarLayout(
                            sidebarPanel(class="sidebar-panel",
                              fluidRow(
                                prettyRadioButtons("typeSelect", h3("Category"),
                                             choices = c("Movie", "TV Show"),
                                             selected = "Movie", status="default")
                              ),
                              fluidRow(
                                  prettyRadioButtons("singleStat", h3("Variable to View"),
                                                    choices = c("Duration", "Genre", "IMDb",
                                                               "Rating", "Rotten Tomatoes",
                                                               "Year"), status="default")
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                wellPanel(class="well-panel",
                                htmlOutput("plotTitle"),
                                plotOutput("statPlot"))
                                ),
                              fluidRow(
                                wellPanel(class="well-panel",
                                h3("Distribution Statistics"),
                                dataTableOutput("statTable")
                                )
                              ))
                          )),
                 tabPanel("Recommendation",
                          sidebarLayout(
                            sidebarPanel(class="sidebar-panel",
                              setSliderColor(color="#E50914", sliderId=c(1:10)),
                              fluidRow(
                                prettyRadioButtons("recType", h3("Category"),
                                             choices = c("Movie", "TV Show"),
                                             selected = "Movie", status="default")
                              ),
                              fluidRow(
                                    actionButton(class="action-button",
                                    "typeChoice", "Get Started")
                              ),
                              fluidRow(
                                hidden(
                                  sliderInput("movieDur", h3("Running Time (minutes)"),
                                              min = 3, max = 312, value = c(80, 120))
                              )),
                              fluidRow(
                                hidden(
                                  sliderInput("tvDur", h3("# of Seasons"),
                                              min = 1, max = 15, value = c(3, 7))
                              )),
                              fluidRow(
                                hidden(
                                  selectInput("movieGenre", h3("Genre"),
                                              choices = c("Action & Adventure", "Anime Features",
                                                          "Children & Family Movies", "Classic Movies",
                                                          "Comedies", "Cult Movies", "Documentaries",
                                                          "Dramas", "Horror Movies", "Independent Movies",
                                                          "International Movies", "Movies", "Music & Musicals",
                                                          "Romantic Movies", "Sci-Fi & Fantasy",
                                                          "Stand-Up Comedy", "Thrillers"))

                              )),
                              fluidRow(
                                hidden(
                                  selectInput("tvGenre", h3("Genre"),
                                              choices = c("Anime Series", "British TV Shows", "Classic & Cult TV",
                                                          "Crime TV Shows", "Docuseries", "International TV Shows",
                                                          "Kids' TV", "Reality TV", "Romantic TV Shows",
                                                          "Spanish-Language TV Shows", "Stand-Up Comedy & Talk Shows",
                                                          "TV Action & Adventure", "TV Comedies", "TV Dramas",
                                                          "TV Horror", "TV Sci-Fi & Fantasy"))
                              )),
                              fluidRow(
                                hidden(
                                  selectInput("rating", h3("Rating"),
                                                     choices = c("G", "PG", "PG-13", "R", "NC-17",
                                                                 "NR", "UR", "TV-G", "TV-Y", "TV-Y7",
                                                                 "TV-Y7-FV", "TV-PG", "TV-14", "TV-MA"))
                              )),
                              fluidRow(
                                hidden(
                                  numericInput("imdb",
                                               h3("Minimum IMDB Rating (1-10)"),
                                               value = 1)
                              )),
                              fluidRow(
                                hidden(
                                  numericInput("rtscore",
                                               h3(paste("Minimum Rotten Tomatoes Score", "(1-100)", sep="\n")),
                                               value = 1)
                              )),
                              fluidRow(
                                hidden(selectInput(("order"), h3("Order By"),
                                       choices = c("Duration", "IMDb", "Rotten Tomatoes", "Year")))
                              ),
                              fluidRow(
                                hidden(
                                      actionButton(class="action-button",
                                                   "submitChoices", "Go!")
                              ))
                            ),
                            mainPanel(

                                htmlOutput("rec"))

                          )
                        ),
                 tabPanel("Regression",
                          sidebarLayout(
                            sidebarPanel(class="sidebar-panel",
                              fluidRow(
                                prettyRadioButtons("catType", h3("Category"),
                                             choices = c("Movie", "TV Show"),
                                             selected = "Movie", status="default")
                              ),
                              fluidRow(
                                h3("Explanatory Variable(s)"),
                                helpText("Select up to 3"),
                                prettyCheckboxGroup("expChoices", "",
                                                   choices = c("Duration", "IMDb", "Rotten Tomatoes", "Year"),
                                                   status="default")
                              ),
                              fluidRow(
                                h3("Response Variable"),
                                helpText("Should be different than your explanatory variable(s)"),
                                selectInput("respChoice", "",
                                            choices = c("Duration", "IMDb", "Rotten Tomatoes", "Year"))
                              ),
                              fluidRow(
                                    actionButton(class="action-button",
                                                 "regChoices", "Go!")
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                wellPanel(class="well-panel",
                                          htmlOutput("regTitle"),
                                          plotOutput("regModel")
                                    )
                                ),
                              fluidRow(
                                htmlOutput("descStats")
                              )
                            )
                          ))
                      ))
)
