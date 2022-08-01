# Welcome to the StatsBomb Win Probability Match Evaluator code repository!

The StatsBomb Win Probability Match Evaluator is a Shiny app built with R that gives soccer fans and analysts an evidence-based approach to understanding the contributions of individual events and players to the outcome of a match.

## [Link to the app on shinyapps.io!](https://janlmoffett.shinyapps.io/StatsBomb_WP_Match_Evaluator/)

#### _Special thanks to Kevin Tenenbaum who was my mentor on this project!_

Included in this repository are the [source files of the current app](app_final/) as well as the [source files from the history of the build](build_history/).

## Current App

[App.R](app_final/app.R) - Shiny app source code.

[Functions and Constants - Time](app_final/app_functions/functions_and_constants_time2.R) - Reusable code to handle time-related tasks in the app.

[Functions and Constants - Visual](app_final/app_functions/functions_and_constants_visual2.R) - Reusable code to handle visualization-related tasks in the app.

## Build History

There were three main components to the process of building the app:

### Exploration/Experimentation

__[Scripts](build_history/scripts/)__ contains code in which I got familiar with the StatsBomb UEFA 2020 dataset that we were provided for the Hackathon and explored ideas for what sort of data science project I wanted to create.

### App Development

__[Functions Etc](build_history/functions_etc/)__ contains the first versions of the functions and other pieces of reusable code that would become part of the app.

__[App Build](build_history/app_build/)__ contains every iteration of the shiny app source code and functions.

### Win Probability Model

__[Win Probability](build_history/win_probability/)__ contains the scripts in which I created the Win Probability model that powers the predictions in the app.  Inspiration for the model came from [this post from American Soccer Analysis](https://www.americansocceranalysis.com/home/2021/7/16/we-have-a-new-win-probability-model) and [this article by Pieter Robberechts, Jan Van Haaren, and Jesse Davis](https://people.cs.kuleuven.be/~pieter.robberechts/repo/robberechts-mlsa19-iwp.pdf).
To predict Win Probability for soccer, I used the [multinom](https://rpubs.com/beane/n4_2) function from the [nnet](https://CRAN.R-project.org/package=nnet) package to fit a model predicting future goals for the home team and away team at 90 points in time throughout the match, using the locations of events on the pitch and the current score as predictors. This model yielded a discrete probability distribution of possible future goals for each team for each "time bin" in the match, which I then used to simulate the outcome of the match and obtain probabilities for a Win for the home team, a Loss for the home team, and a Draw.

