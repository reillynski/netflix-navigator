# load packages
library(shiny)
library(shinyjs)
library(tidyverse)

# data set up and cleaning

# read in csv files
tv_basic <- read.csv("./data/streaming_tv.csv")
movie_basic <- read.csv("./data/streaming_movies.csv")
netflix_detail <- read.csv("./data/netflix_tv_movies.csv")

# clean (select only Netflix movies, add new col) and combine tv_basic, movie_basic
tv_basic <- tv_basic %>%
  filter(Netflix == 1) %>%
  select(Title, Year, Age, IMDb, Rotten.Tomatoes) %>%
  mutate(Type = "TV Show")

movie_basic <- movie_basic %>%
  filter(Netflix == 1) %>%
  select(Title, Year, Age, IMDb, Rotten.Tomatoes) %>%
  mutate(Type = "Movie")

basic_combo <- bind_rows(tv_basic, movie_basic)

# clean netflix_detail and join with basic_combo
netflix_detail <- netflix_detail %>%
  select(type, title, director, cast, rating, duration, listed_in, description) %>%
  rename(
    Type = type,
    Title = title,
    Director = director,
    Cast = cast,
    Rating = rating,
    Duration = duration,
    Genre = listed_in,
    Description = description
  )

netflix <- inner_join(basic_combo, netflix_detail, by=c("Title", "Type"))

# clean netflix (make cols easier to deal with)
netflix <- netflix %>%
  mutate(Genre = map_chr(Genre, ~str_split(.x, ",")[[1]][1])) %>%
  mutate(Rating = if_else(Rating =="", "Not Available", Rating)) %>%
  mutate(Genre = factor(Genre),
         Type = factor(Type),
         Rating = factor(Rating)) %>%
  mutate(Rotten.Tomatoes = as.numeric(
    map_chr(
      Rotten.Tomatoes, ~str_replace(.x, "%", "")))) %>%
  mutate(Duration =
           case_when(
             Type == "Movie" ~ as.numeric(map_chr(
               Duration, ~str_replace(.x, " min", ""))),
             TRUE ~ as.numeric(
               map_chr(Duration, ~str_replace(.x, " Seasons| Season", ""))))) %>%
  rename("Rotten Tomatoes" = Rotten.Tomatoes) %>%
  mutate(Director = if_else(Director == "", "Not Available", Director),
         Cast = if_else(Cast == "", "Not Available", Cast))

# function that generates a well panel containing info about a movie/tv show
# stored in a row in a dataframe
generate_recs <- function(df, row_num) {
  return(wellPanel(
    fluidRow(column(width=12,
      h2(df[row_num,][["Title"]]),
      p(strong("Description"), df[row_num,][["Description"]]))
      ),
    fluidRow(
      column(width=6,
             p(strong("Genre: "), df[row_num,][["Genre"]]),
             p(strong("Director: "), df[row_num,][["Director"]]),
             p(strong("Cast:"), df[row_num,][["Cast"]])
      ),
      column(width=4,
             p(strong("Duration: "), df[row_num,][["Duration"]]),
             p(strong("Rating: "), df[row_num,][["Rating"]]),
             p(strong("IMDb: "), df[row_num,][["IMDb"]]),
             p(strong("Rotten Tomatoes: "), df[row_num,][["Rotten Tomatoes"]])
      )
    )
  ))
}

# function that returns a row with coefficients from linear model
generate_coefs <- function(lm_df, exp_var, row_num) {
  return(fluidRow(
          column(width=3,
           p(em(paste(exp_var[[row_num - 1]], ": ", sep="")), round(lm_df[row_num, ][["estimate"]], 4))
        )
      )
    )
}

server <- function(input, output) {
  useShinyjs()
  # analysis tab
  data_type <- reactive({
    selected <- input$typeSelect
    return(netflix %>% filter(Type == selected))
  })

  type_name <- reactive({input$typeSelect})

  to_view <- reactive({input$singleStat})

  # generate title of plot
  output$plotTitle <- renderUI({
    h3(paste(type_name(), to_view(), sep=" "))
    })

  # generate plot
  output$statPlot <- renderPlot({
    data <- data_type()
    view <- to_view()

    col_name <- sym(view)

    # separate by numeric/factor and plot accordingly
    # numeric = histogram
    if(view == "IMDb" | view == "Rotten Tomatoes" | view == "Duration" | view == "Year") {
      ggplot(data) +
        geom_histogram(aes(x=!!col_name), color="black", fill="#E50914", alpha=0.8, bins=20) +
        labs(x=view, y="Count")
    }
    # factor = pie chart
    else {
      ggplot(data, aes(x="", fill=!!col_name)) +
        geom_bar(width = 1) +
        theme(axis.line = element_blank(),
              plot.title = element_text(hjust=0.5)) +
        labs(fill=view,
             x=NULL,
             y=NULL) +
        coord_polar(theta = "y", start=0)
    }
  })

  # generate data table of statistics with distribution of particular variable from selected options
  output$statTable <- renderDataTable({
    data <- data_type()
    view <- to_view()

    # for numeric, min/max/med/mean/sd
    if(view == "IMDb" | view == "Rotten Tomatoes" | view == "Duration" | view == "Year") {
      data.frame("min" = min(data[[view]], na.rm=TRUE), "max" = max(data[[view]], na.rm=TRUE),
                 "median" = median(data[[view]], na.rm=TRUE), "mean" = round(mean(data[[view]], na.rm=TRUE), 2),
                 "standard deviation" = round(sd(data[[view]], na.rm=TRUE), 2))
    }
    # for factor, frequencies
    else {
      data.frame(round(prop.table(table(data[[view]])), 4) * 100) %>%
        rename(view = Var1,
               Percentage = Freq) %>%
        filter(Percentage > 0) %>%
        arrange(desc(Percentage))
    }
  })

  # recommendation tab
  category <- reactive({
    selected <- input$recType
    return(netflix %>% filter(Type == selected))
  })

  # show options depending on tv or movie
  observeEvent(input$typeChoice, {
    rec_type <- input$recType
    if(rec_type == "Movie") {
      showElement("movieDur")
      showElement("movieGenre")
      hideElement("tvDur")
      hideElement("tvGenre")
    }
    else {
      showElement("tvDur")
      showElement("tvGenre")
      hideElement("movieDur")
      hideElement("movieGenre")
    }
    showElement("rating")
    showElement("imdb")
    showElement("rtscore")
    showElement("order")
    showElement("submitChoices")
  })

  #generate recommendations
  observeEvent(input$submitChoices, {
    rec_type <- reactive({
      selected <- input$recType
      return(netflix %>% filter(Type == selected))
    })
    rec_table <- rec_type()

    if (input$recType == "Movie") {
      genre <- input$movieGenre
      duration <- input$movieDur
    }
    else{
      genre <- input$tvGenre
      duration <- input$tvDur
    }

    min <- duration[[1]]
    max <- duration[[2]]

    if(min == max)
      max <- max(rec_table$Duration)

    to_order_temp <- input$order
    to_order <- sym(to_order_temp)

    # filter based on selected choices
    rec_table <- rec_table %>% filter(Genre == genre &
                                      Duration >= min &
                                      Duration < max &
                                      IMDb >= input$imdb &
                                      Rating == input$rating &
                                      !!as.symbol("Rotten Tomatoes") >= input$rtscore) %>%
                               arrange(desc(!!to_order)) %>%
                               head(n=3)

    # use function generate_recs to populate main panel with up to 3 recommendations
    output$rec <- renderUI({
      if(nrow(rec_table) == 0) {
        wellPanel(
          h3("Sorry, there are no recommendations that fulfill your criteria."),
          p("Please try again with different values.")
        )
      }
      else {
        map(1:nrow(rec_table), ~generate_recs(rec_table, .x))
      }
    })
  })

  # regression tab
  cat <- reactive({
    selected <- input$catType
    return(netflix %>% filter(Type == selected))
  })

  # generate regression
  observeEvent(input$regChoices, {
    cat_data <- cat()

    exp_vars <- input$expChoices
    resp_var <- input$respChoice

    # adjust regression equation depending on number of variables
    if (length(exp_vars) == 3) {
      var1 <- exp_vars[[1]]
      var2 <- exp_vars[[2]]
      var3 <- exp_vars[[3]]

      mod <- lm(cat_data[[resp_var]] ~ cat_data[[var1]] * cat_data[[var2]] *
                  cat_data[[var3]], data=cat_data)
    }
    else if (length(exp_vars) == 2) {
      var1 <- exp_vars[[1]]
      var2 <- exp_vars[[2]]
      mod <- lm(cat_data[[resp_var]] ~ cat_data[[var1]] * cat_data[[var2]], data=cat_data)
    }
    else {
      var1 <- exp_vars[[1]]
      mod <- lm(cat_data[[resp_var]] ~ cat_data[[var1]], data=cat_data)
    }

    # generate title of regression plot depending on number of variables
    output$regTitle <- renderUI({
      if(length(exp_vars) == 1) {
        h3(paste(resp_var, " vs. ", exp_vars[[1]], sep=""))
      }
      else if(length(exp_vars) == 2) {
        h3(paste(resp_var, " vs. ", exp_vars[[1]], " * ", exp_vars[[2]], sep=""))
      }
      else {
        h3(paste(resp_var, " vs. ", exp_vars[[1]], " * ", exp_vars[[2]], " * ",
                 exp_vars[[3]], sep=""))
      }
    })

    # generate regression plot depending on number of variables
    output$regModel <- renderPlot({
      sym_resp <- sym(resp_var)
      sym_var1 <- sym(exp_vars[[1]])
      if(length(exp_vars) == 1) {
        ggplot(cat_data, aes(y=!!sym_resp, x=!!sym_var1)) +
          geom_point() +
          geom_smooth(method="lm", col="#E50914")
      }
      else if (length(exp_vars) == 2) {
        sym_var2 <- sym(exp_vars[[2]])
        ggplot(cat_data, aes(y=!!sym_resp, x=!!sym_var1, color=!!sym_var2)) +
          geom_point() +
          geom_smooth(method="lm", se=FALSE, col="#E50914")
      }
      else {
        sym_var2 <- sym(exp_vars[[2]])
        sym_var3 <- sym(exp_vars[[3]])
        ggplot(cat_data, aes(y=!!sym_resp, x=!!sym_var1, color=!!sym_var2,
                             size=!!sym_var3)) +
          geom_point() +
          geom_smooth(method="lm", se=FALSE, col="#E50914")
      }
    })

    # get regression model stats
    tStats <- broom::tidy(mod)
    gStats <- broom::glance(mod)

    # generate panel with regression model stats
    output$descStats <- renderUI({
      wellPanel(class="well-panel",
        h3("Regression Model"),
        fluidRow(column(width=3,
                        p(strong("Coefficients"))),
                 column(width=8,
                        p(strong("Adjusted R-Squared: "), round(gStats$adj.r.squared, 4)))),
        fluidRow(column(width=3,
                        p(em("Intercept: "), round(tStats[1,][["estimate"]], 4))),
                 column(width=8,
                        p(paste("The selected explanatory variable(s) account(s) for ",
                                round(gStats$adj.r.squared, 4)*100, "% of the variation in the selected response variable.",
                                sep="")))),
        if (length(exp_vars) == 1) {
          fluidRow(column(width=3,
                          p(em(paste(exp_vars[[1]], ": ", sep="")), round(tStats[2,][["estimate"]], 4))))
        }
        else {
          map(2:(length(exp_vars) + 1), ~generate_coefs(tStats, exp_vars, .x))
        }
      )
    })
  })
}
