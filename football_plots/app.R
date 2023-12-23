#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(reactable)
library(shiny)
library(tidyverse)
library(DT)
library(plotly)


current_week <- read_rds("current_week.rds")

`%ni%` <- negate(`%in%`)

game_logs <- read_rds("gamelogs.rds")

game_logs <- game_logs %>% 
  filter(week <= 18)

game_logs <- game_logs[!duplicated(game_logs),]

rec_leaders <- game_logs %>% 
  filter(season == 2023,
         position %in% c("WR", "TE")) %>% 
  group_by(recent_team, player_display_name, position) %>% 
  summarise(avg_share = mean(target_share),
            n = n()) %>%
  drop_na(avg_share) %>%
  filter(n >= 2) %>% 
  select(-n) %>% 
  group_by(position, recent_team) %>% 
  filter(avg_share == max(avg_share, na.rm = T))

rush_leaders <- game_logs %>% 
  filter(season == 2023,
         position == "RB") %>% 
  group_by(recent_team, player_display_name, position) %>% 
  summarise(avg_share = mean(carries),
            n = n()) %>%
  filter(n >= 2) %>% 
  select(-n) %>% 
  drop_na(avg_share) %>% 
  group_by(position, recent_team) %>% 
  filter(avg_share == max(avg_share, na.rm = T))

qb_leaders <- game_logs %>% 
  filter(season == 2023,
         position == "QB") %>% 
  group_by(recent_team, player_display_name, position) %>% 
  summarise(avg_att = mean(attempts),
            n = n()) %>%
  filter(n >= 2) %>% 
  select(-n) %>% 
  drop_na(avg_att) %>% 
  group_by(position, recent_team) %>% 
  filter(avg_att == max(avg_att, na.rm = T))


totals_data <- game_logs %>% 
  group_by(player_display_name, position, season) %>% 
  summarise_if(is.numeric, sum) %>% 
  select(-week)

avgs_data <- game_logs %>% 
  group_by(player_display_name, position, season) %>% 
  summarise_if(is.numeric, list(~ mean(., na.rm = T))) %>% 
  select(-week)

position_table <- game_logs %>% 
  distinct(player_display_name, position)

team_defense <- game_logs %>% 
  group_by(player_display_name, week, season) %>% 
  filter(carries >= 5 | targets >= 5 | attempts >= 5) %>% 
  ungroup() %>%
  group_by(opponent_team, season, position) %>% 
  summarise_if(is.numeric, list(~ mean(., na.rm = T))) %>% 
  filter(position %in% c("QB", "TE", "WR", "RB"))

# Define UI for application that draws a histogram
ui <- navbarPage("Fantasy Football Data",

    ## tab for season stats for each player -------
    tabPanel("Season Stats", 
             selectInput("position", label = "Select a position:",
                         choices = c("QB", "RB", "WR", "TE")),
             selectInput("season_total", "Select a season:", 
                         choices = unique(game_logs$season)),
             checkboxInput("avgs", "Per game stats"),
    reactableOutput("season_totals")),
    ## tab for player game logs --------
    tabPanel("Player Game Logs",
             selectInput("player", label = "Select a player:",
                         choices = unique(game_logs$player_display_name),
                         selected = "Jordan Love"),
             selectInput("season_log", "Select a season:", choices = c(2023, 2022),
                         selected = 2023),
    reactableOutput("player_logs")),
    ## tab for opponent logs --------------
    tabPanel("Opponent Game Logs",
             selectInput("opponent", label = "Select an game:",
                         choices = current_week),
             selectInput("play_position", "Select a position:",
                         choices = c("QB", "RB", "WR", "TE")),
    reactableOutput("opponent_logs_away"),
    reactableOutput("opponent_logs_home")),
    ## tab for player plots -------
    tabPanel("Player Graphs",
             selectInput("player_graph", "Select a player:",
                         choices = unique(game_logs$player_display_name),
                         selected = "Jordan Love"),
             selectInput("stat_choice", "Select a stat:",
                         choices = c("fantasy_points_half_ppr", "receptions", "interceptions",
                                     "receiving_yards", "rushing_yards", "targets",
                                     "target_share", "carries", "passing_yards", "air_yards_share",
                                     "total_tds", "passing_tds", 
                                     "completions")),
             selectInput("season", "Select a season:", choices = c(2023, 2022)),
             numericInput("prop", "Enter a threshold:", 0.5, step = 1),
    plotlyOutput("weekly_plot")),
    ## tab for team weekly plot --------------
    tabPanel("Team Graph",
             selectInput("team_stat", "Select a stat to group by:",
                         choices = c("fantasy_points_half_ppr", "receptions", "interceptions",
                                     "receiving_yards", "rushing_yards", "targets",
                                     "target_share", "carries", "passing_yards", "air_yards_share",
                                     "total_tds", "passing_tds")),
    plotlyOutput("team_plot")),
    ## tab for team defense ------------ 
    tabPanel("Team Defense vs Position",
             selectInput("def_position", "Select a position:",
                         choices = c("QB", "RB", "WR", "TE")),
             reactableOutput("defense_table"))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  ### season totals stats output--------------
  output$season_totals <- renderReactable({
    avgs_data <- avgs_data %>% 
      filter(season == as.numeric(input$season_total))
    
    totals_data <- totals_data %>% 
      filter(season == as.numeric(input$season_total))
    
    if(input$avgs == "TRUE"){
      if("QB" != input$position){
        avgs_data %>% 
          filter(position %in% input$position) %>% 
          select(player_display_name, position, receptions, targets, target_share,
                 carries, receiving_yards, rushing_yards, total_tds,
                 "snap_share" = offense_pct, fantasy_points_half_ppr) %>% 
          mutate_if(is.numeric, list(~ round(.,3))) %>% 
          reactable(defaultPageSize = 20)
      }
      
      else{
        avgs_data %>% 
          filter(position %in% input$position) %>% 
          select(player_display_name, position, completions, rushing_yards,
                 passing_yards, passing_tds, rushing_tds,
                 sacks, interceptions, fantasy_points_half_ppr) %>% 
          mutate_if(is.numeric, list(~ round(.,3))) %>% 
          reactable(defaultPageSize = 20)
      }
    }
    
    else{if("QB" != input$position){
      totals_data %>% 
        filter(position %in% input$position) %>% 
        select(player_display_name, position, receptions, targets,
               carries, receiving_yards, rushing_yards, total_tds,
               "snap_share" = offense_pct, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }
    
    else{
      totals_data %>% 
        filter(position %in% input$position) %>% 
        select(player_display_name, position, completions, rushing_yards,
               passing_yards, passing_tds, rushing_tds,
               sacks, interceptions, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }}
    
  })
  ### player game logs --------------
  output$player_logs <- renderReactable({
    
    position <- position_table %>% 
      filter(player_display_name == input$player) %>% 
      pull(position) %>% .[1]
    
    if(position == "QB"){
      game_logs %>% 
        filter(player_display_name == input$player,
               season == as.numeric(input$season_log)) %>% 
        select(player_display_name, position, opponent_team, week, completions, rushing_yards,
               passing_yards, passing_tds, rushing_tds,
               sacks, interceptions, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }
    else{
      game_logs %>% 
        filter(player_display_name == input$player,
               season == as.numeric(input$season_log)) %>% 
        select(player_display_name, position, opponent_team, week, receptions, targets,
               carries, rushing_yards, total_tds, "snap_share" = offense_pct, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }
  })
  ## opponent game logs output ---------
  output$opponent_logs_away <- renderReactable({
    
    
    if(input$play_position == "QB"){
      
      game_logs %>% 
        filter(position == "QB", opponent_team == str_split(input$opponent, " ")[[1]][1]) %>% 
        group_by(week) %>%
        filter(attempts == max(attempts, na.rm = TRUE)) %>%
        slice(1) %>%
        select(week, 'player_team' = recent_team, opponent_team, player_display_name,
               completions, attempts, passing_yards, rushing_yards, passing_tds, 
               interceptions, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    
    else if(input$play_position == "RB"){
      game_logs %>% 
        filter(position == "RB",
               opponent_team == str_split(input$opponent, " ")[[1]][1]) %>% 
        group_by(week) %>%
        filter(carries == max(carries, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week, 'player_team' = recent_team, opponent_team, player_name, receptions, targets, 
               receiving_yards, carries, rushing_yards, total_tds, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    else if (input$play_position == "WR"){
      
      game_logs %>% 
        filter(position == "WR",
               opponent_team == str_split(input$opponent, " ")[[1]][1]) %>% 
        group_by(week) %>%
        filter(targets == max(targets, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week, 'player_team' = recent_team, opponent_team, player_name,
               targets, receptions, receiving_yards, total_tds, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    else{
      
      game_logs %>% 
        filter(position == "TE",
               opponent_team == str_split(input$opponent, " ")[[1]][1]) %>% 
        group_by(week) %>%
        filter(targets == max(targets, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week, 'player_team' = recent_team, opponent_team, player_name, targets,
               receptions, receiving_yards, total_tds, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
  })
  
  output$opponent_logs_home <- renderReactable({
    
    
    if(input$play_position == "QB"){
      
      game_logs %>% 
        filter(position == "QB", opponent_team == str_split(input$opponent, " ")[[1]][3]) %>% 
        group_by(week) %>%
        filter(attempts == max(attempts, na.rm = TRUE)) %>%
        slice(1) %>%
        select(week, 'player_team' = recent_team, opponent_team, player_display_name,
               completions, attempts, passing_yards, rushing_yards, passing_tds,
               interceptions, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    
    else if(input$play_position == "RB"){
      game_logs %>% 
        filter(position == "RB",
               opponent_team == str_split(input$opponent, " ")[[1]][3]) %>% 
        group_by(week) %>%
        filter(carries == max(carries, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week,'player_team' = recent_team, opponent_team, player_name, receptions, targets, 
               receiving_yards, carries, rushing_yards, total_tds, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    else if (input$play_position == "WR"){
      
      game_logs %>% 
        filter(position == "WR",
               opponent_team == str_split(input$opponent, " ")[[1]][3]) %>% 
        group_by(week) %>%
        filter(targets == max(targets, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week, 'player_team' = recent_team, opponent_team, player_name, 
               targets, receptions, receiving_yards, total_tds, fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
    else{
      
      game_logs %>% 
        filter(position == "TE",
               opponent_team == str_split(input$opponent, " ")[[1]][3]) %>% 
        group_by(week) %>%
        filter(targets == max(targets, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(week, 'player_team' = recent_team, opponent_team,
               player_name, targets, receptions, receiving_yards, total_tds,
               fantasy_points_half_ppr) %>% 
        arrange(week) %>% 
        reactable(defaultPageSize = 20)
    }
  })
  ### weekly stat plot output -------------
  output$weekly_plot <- renderPlotly({
    
    
    
    filtered_data <- game_logs %>% 
      filter(player_display_name == input$player_graph,
             season == as.numeric(input$season)) %>% 
      select(input$stat_choice, week)
    
    threshold_value <- input$prop
    
    plot_col <- colnames(filtered_data)[1]
    
    # Create a new column for color based on the threshold
    filtered_data$Result <- ifelse(filtered_data[[plot_col]] > threshold_value, "Over", "Under")
    
    
    plot <- filtered_data %>% 
      ggplot(aes(x = week, y = !!sym(plot_col), fill = Result)) +
      geom_col() +
      scale_fill_manual(values = c("Over" = "#61db40", "Under" = "#e86e4d")) +
      labs(title = paste(input$player_graph, " - ", str_replace_all(plot_col, "_", " ")),
           y = str_replace_all(plot_col, "_", " ")) +
      theme_minimal() +
      geom_hline(yintercept = input$prop)
    
    ggplotly(plot)
    
  })
  
  ### team stat plot output ---------------
  output$team_plot <- renderPlotly({
    
    teams <- game_logs %>% filter(player_display_name == input$player_graph) %>% pull(recent_team)
    team <- teams[length(teams)]

    focus_stat <- input$team_stat

    plot <- game_logs %>%
      filter(season == as.numeric(input$season)) %>% 
      group_by(recent_team, week, player_display_name) %>%
      summarise(stat = mean(!!sym(focus_stat))) %>%
      filter(recent_team == team,
             stat > 0) %>% 
      ggplot(aes(week, stat, color = player_display_name)) +
      geom_line() +
      theme_minimal()

    ggplotly(plot)
  })
  
  ## defense vs position output ----------
  output$defense_table <- renderReactable({
    
    if("QB" != input$def_position){
      team_defense %>% 
        filter(position == input$def_position & season == 2023) %>% 
        select("team" = opponent_team, receptions, targets,
               carries, rushing_yards, receiving_yards, receiving_tds,
               rushing_tds, "snap_share" = offense_pct, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 32)
    }
    
    else{
      team_defense %>% 
        filter(position == input$def_position & season == 2023) %>%
        select("team" = opponent_team, completions, rushing_yards,
               passing_yards, passing_tds, rushing_tds,
               sacks, interceptions, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 32)
    }
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

