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
`%ni%` <- negate(`%in%`)

game_logs <- read_rds("gamelogs.rds")

game_logs <- game_logs %>% 
  filter(week <= 18)

totals_data <- game_logs %>% 
  group_by(player_display_name, position) %>% 
  summarise_if(is.numeric, sum) %>% 
  select(-c(season, week))

avgs_data <- game_logs %>% 
  group_by(player_display_name, position) %>% 
  summarise_if(is.numeric, list(~ mean(., na.rm = T))) %>% 
  select(-c(season, week))

position_table <- game_logs %>% 
  distinct(player_display_name, position)

# Define UI for application that draws a histogram
ui <- navbarPage("Fantasy Football Data",

    # Sidebar with a slider input for number of bins 
    tabPanel("Season Stats", 
             selectInput("position", label = "Select a position:",
                         choices = c("QB", "RB", "WR", "TE"),
                         ),
             checkboxInput("avgs", "Per game stats"),
             
             
    reactableOutput("season_totals")),
    tabPanel("Player Game Logs",
             selectInput("player", label = "Select a player:",
                         choices = unique(game_logs$player_display_name)),
    reactableOutput("player_logs")),
    tabPanel("Graphs",
             selectInput("player_graph", "Select a player:",
                         choices = unique(game_logs$player_display_name)),
             selectInput("stat_choice", "Select a stat:",
                         choices = c("fantasy_points_half_ppr", "receptions", "interceptions",
                                     "receiving_yards", "rushing_yards", "targets",
                                     "target_share", "carries", "passing_yards", "air_yards_share",
                                     "rushing_tds", "receiving_tds", "passing_tds")),
    plotlyOutput("weekly_plot"))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$season_totals <- renderReactable({
    if(input$avgs == "TRUE"){
      if("QB" != input$position){
        avgs_data %>% 
          filter(position %in% input$position) %>% 
          select(player_display_name, position, receptions, targets, target_share,
                 carries, rushing_yards, receiving_tds,
                 rushing_tds, fantasy_points_half_ppr) %>% 
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
               carries, rushing_yards, receiving_tds,
               rushing_tds, fantasy_points_half_ppr) %>% 
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
  
  output$player_logs <- renderReactable({
    
    position <- position_table %>% 
      filter(player_display_name == input$player) %>% 
      pull(position) %>% .[1]
    
    if(position == "QB"){
      game_logs %>% 
        filter(player_display_name == input$player) %>% 
        select(player_display_name, position, week, completions, rushing_yards,
               passing_yards, passing_tds, rushing_tds,
               sacks, interceptions, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }
    else{
      game_logs %>% 
        filter(player_display_name == input$player) %>% 
        select(player_display_name, position, week, receptions, targets,
               carries, rushing_yards, receiving_tds,
               rushing_tds, fantasy_points_half_ppr) %>% 
        mutate_if(is.numeric, list(~ round(.,3))) %>% 
        reactable(defaultPageSize = 20)
    }
  })
  
  output$weekly_plot <- renderPlotly({
    
    
    
    filtered_data <- game_logs %>% 
      filter(player_display_name == input$player_graph) %>% 
      select(input$stat_choice, week)
    
    
    plot_col <- colnames(filtered_data)[1]
    
    plot <- filtered_data %>% 
      ggplot(aes(x = week, y = !!sym(plot_col))) +
      geom_col() +
      labs(title = paste(input$player_graph, plot_col))
    
    ggplotly(plot)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

