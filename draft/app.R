# app.R — Fantasy Draft Board (mobile-friendly)
# Run: shiny::runApp()  OR deploy to shinyapps.io

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(shinyjs)

# ---------- SETTINGS ----------
DATA_PATH <- "/Users/matthewwankiewicz/Documents/football/fantasy/draft25/output_rankings.csv"
LEAGUE <- list(
  teams = 12,
  roster = c(QB=1, RB=2, WR=2, TE=1, FLEX=2, SUPERFLEX=0, BENCH=6)
)
POS_FOR_FLEX  <- c("RB","WR","TE")
POS_FOR_SFLEX <- c("QB","RB","WR","TE")

# ---------- HELPERS ----------
coalesce_num <- function(x) suppressWarnings(as.numeric(x))
badge_tier <- function(x) {
  ifelse(is.na(x), "", sprintf("<span class='badge bg-primary-subtle text-primary-emphasis'>T%s</span>", x))
}
`%||%` <- function(x, y) if (!is.null(x)) x else y

compute_needs <- function(my_team_df, roster_cfg) {
  have <- my_team_df %>% count(pos, name="count")
  tibble(pos = names(roster_cfg), need = as.integer(roster_cfg)) %>%
    left_join(have, by="pos") %>%
    mutate(count = replace_na(count, 0L),
           remaining = pmax(need - count, 0L))
}

can_fill_pos <- function(pos, needs_row, flex_left, sflex_left) {
  strict_left <- needs_row$remaining[match(pos, needs_row$pos)]
  if (!is.na(strict_left) && strict_left > 0) return(TRUE)
  if (pos %in% POS_FOR_FLEX  && flex_left  > 0) return(TRUE)
  if (pos %in% POS_FOR_SFLEX && sflex_left > 0) return(TRUE)
  FALSE
}

# ---------- UI ----------
theme <- bs_theme(version = 5, bootswatch = "cosmo")

ui <- page_navbar(
  title = "Draft",
  theme = theme,
  useShinyjs(),
  header = tags$head(
    tags$style(HTML("
      .dt-center { text-align:center; }
      .value-card { padding:12px; border-radius:12px; background:#f8f9fa; margin-bottom:10px; }
      .btn-wide { width:100%; padding:12px; font-weight:600; }
      .stack-yes { background:#fff3cd !important; }
      @media (max-width:576px){ .dt-buttons { display:none; } }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('saveState', function(x){
        localStorage.setItem('ff_state', JSON.stringify(x));
      });
      Shiny.addCustomMessageHandler('loadState', function(x){
        var s = localStorage.getItem('ff_state');
        Shiny.setInputValue('loaded_state', s);
      });
    "))
  ),
  
  # -------- Board TAB (Sidebar with filters + actions; main = table) --------
  nav_panel(
    "Board",
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        fileInput("file_csv", "Upload rankings CSV (optional)", accept = ".csv"),
        h4("Positions"),
        selectInput("filter_pos", NULL, choices = c("QB","RB","WR","TE"), multiple = TRUE, selected = c("RB","WR","QB","TE")),
        h4("Tiers"),
        selectInput("filter_tier", NULL, choices = 1:10, multiple = TRUE),
        h4("Search player/team"),
        textInput("filter_search", NULL, ""),
        checkboxInput("only_needs", "Only positions of need", value = FALSE),
        checkboxInput("show_stacks", "Highlight stacking (same team as my QB/WR/TE)", value = TRUE),
        tags$hr(),
        h4("Actions"),
        actionButton("btn_mark_drafted", "Mark Drafted (any team)", class = "btn btn-secondary btn-wide"),
        actionButton("btn_draft_me", "Draft to My Team", class = "btn btn-primary btn-wide"),
        actionButton("btn_undo", "Undo", class = "btn btn-outline-dark btn-wide")
      ),
      card(
        full_screen = TRUE,
        card_header("Available Players"),
        DTOutput("tbl_board")
      )
    )
  ),
  
  # -------- Team TAB (My Team & Needs + Recommendations) --------
  nav_panel(
    "Team",
    layout_columns(
      col_widths = c(6,6),
      card(
        card_header("My Team & Needs"),
        htmlOutput("needs_boxes"),
        DTOutput("tbl_myteam")
      ),
      card(
        card_header("Top Recommendations"),
        DTOutput("tbl_reco")
      )
    )
  ),
  
  # -------- Settings TAB --------
  nav_panel(
    "Settings",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("League Settings"),
        numericInput("num_teams", "Teams", value = LEAGUE$teams, min=4, max=16),
        textInput("roster_str", "Roster (YAML-like)", value = "QB:1, RB:2, WR:2, TE:1, FLEX:2, SUPERFLEX:0, BENCH:6"),
        helpText("Format: POS:n,POS:n,...  (FLEX accepts RB/WR/TE; SUPERFLEX accepts QB/RB/WR/TE)")
      ),
      card(
        card_header("Persistence"),
        actionButton("btn_save_state", "Save State to this device", class="btn btn-success"),
        actionButton("btn_load_state", "Load Saved State", class="btn btn-outline-success"),
        helpText("State is stored in your device's browser localStorage (works on phone).")
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  parse_roster <- reactive({
    x <- strsplit(input$roster_str, ",")[[1]]
    x <- trimws(x)
    kv <- strsplit(x, ":")
    nm <- sapply(kv, `[[`, 1)
    val <- as.integer(sapply(kv, `[[`, 2))
    setNames(val, nm)
  })
  
  rankings_raw <- reactive({
    req(input$file_csv %||% TRUE)
    if (!is.null(input$file_csv$datapath)) {
      read_csv(input$file_csv$datapath, show_col_types = FALSE)
    } else if (file.exists(DATA_PATH)) {
      read_csv(DATA_PATH, show_col_types = FALSE)
    } else {
      validate("No CSV found. Upload output_rankings.csv to begin.")
    }
  })
  
  rankings <- reactive({
    df <- rankings_raw()
    needed <- c("player","team","pos","Score","Tier","overall_ecr","pos_ecr","adp_combined","boom_factor","x1st_5_games_so_s_by_position")
    missing <- setdiff(needed, names(df))
    if (length(missing)) stop(paste("Missing columns in CSV:", paste(missing, collapse=", ")))
    df %>%
      mutate(
        Score = coalesce_num(Score),
        Tier  = coalesce_num(Tier),
        adp_combined = coalesce_num(adp_combined)
      ) %>%
      arrange(overall_ecr) %>%
      mutate(overall = row_number())
  })
  
  drafted <- reactiveVal(tibble(player=character()))
  myteam  <- reactiveVal(tibble(player=character()))
  action_log <- reactiveVal(tibble(action=character(), player=character(), time=Sys.time()))
  
  observeEvent(input$btn_save_state, {
    s <- list(drafted = drafted()$player, myteam = myteam()$player)
    session$sendCustomMessage("saveState", s)
    showNotification("Saved to this device.", type="message")
  })
  observeEvent(input$btn_load_state, { session$sendCustomMessage("loadState", TRUE) })
  observeEvent(input$loaded_state, {
    if (is.null(input$loaded_state) || is.na(input$loaded_state)) return()
    s <- try(jsonlite::fromJSON(input$loaded_state), silent = TRUE)
    if (inherits(s, "try-error")) return()
    if (!is.null(s$drafted)) drafted(tibble(player = s$drafted))
    if (!is.null(s$myteam))  myteam(tibble(player = s$myteam))
    showNotification("Loaded saved state from this device.", type="message")
  })
  
  available <- reactive({
    df <- rankings()
    d  <- drafted()
    if (nrow(d)) df <- anti_join(df, d, by="player")
    if (length(input$filter_pos))  df <- df %>% filter(pos %in% input$filter_pos)
    if (length(input$filter_tier)) df <- df %>% filter(!is.na(Tier) & Tier %in% input$filter_tier)
    if (nzchar(input$filter_search)) {
      q <- tolower(input$filter_search)
      df <- df %>% filter(str_detect(tolower(player), q) | str_detect(tolower(team), q))
    }
    if (isTRUE(input$only_needs)) {
      need <- compute_needs(myteam() %>% left_join(rankings(), by="player"), parse_roster())
      flex_left  <- need$remaining[match("FLEX", need$pos)];  if (is.na(flex_left))  flex_left  <- 0
      sflex_left <- need$remaining[match("SUPERFLEX", need$pos)]; if (is.na(sflex_left)) sflex_left <- 0
      df <- df %>% rowwise() %>% filter(can_fill_pos(pos, need, flex_left, sflex_left)) %>% ungroup()
    }
    df %>% arrange(overall_ecr)
  })
  
  stack_teams <- reactive({
    mt <- myteam() %>% left_join(rankings(), by="player")
    unique(mt$team[mt$pos %in% c("QB","WR","TE")])
  })
  
  output$tbl_board <- renderDT({
    df <- available()
    st <- stack_teams()
    df$TierBadge <- badge_tier(df$Tier)
    
    dat <- df %>%
      transmute(
        Select = "",
        Tier = TierBadge,
        Player = player,
        Team = team,
        Pos = pos,
        Score = round(Score, 3),
        ADP = adp_combined,
        overall_ecr,
        pos_ecr,
        boom_factor,
        x1st_5_games_so_s_by_position,
        points_vor
      ) %>%
      arrange(overall_ecr)
    
    datatable(
      dat,
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        order = list(list(7, "asc"))  # 0-based index -> overall_ecr
      ),
      callback = JS("table.on('click', 'tbody tr', function(){ $(this).toggleClass('selected'); });")
    ) %>%
      formatStyle(
        "Player",
        target = "row",
        backgroundColor = styleEqual(
          df$team, if (isTRUE(input$show_stacks)) ifelse(df$team %in% st, "#fff3cd", NA) else NA
        )
      ) %>%
      formatStyle("Tier", "text-align" = "center")
  })
  
  output$tbl_myteam <- renderDT({
    mt <- myteam() %>% left_join(rankings(), by="player")
    if (!nrow(mt)) mt <- tibble(player=character(), team=character(), pos=character(), Tier=numeric(), Score=numeric())
    dat <- mt %>%
      arrange(pos, Tier, desc(Score)) %>%
      transmute(Player = player, Team = team, Pos = pos, Tier = Tier, Score = round(Score,3))
    datatable(dat, rownames = FALSE, options = list(pageLength = 100, dom = 't'))
  })
  
  output$needs_boxes <- renderUI({
    mt <- myteam() %>% left_join(rankings(), by="player")
    need <- compute_needs(mt, parse_roster())
    flex_need  <- parse_roster()[["FLEX"]];      if (is.na(flex_need))  flex_need  <- 0
    sflex_need <- parse_roster()[["SUPERFLEX"]]; if (is.na(sflex_need)) sflex_need <- 0
    
    tagList(
      fluidRow(
        lapply(need$pos, function(p){
          left <- need$remaining[match(p, need$pos)]
          total <- parse_roster()[[p]]
          pct <- if (total > 0) 100*(total - left)/total else 100
          div(class="value-card",
              tags$b(paste0(p, ": ")),
              sprintf("%d / %d filled", total - left, total),
              tags$div(style="height:6px;background:#e9ecef;border-radius:6px;margin-top:6px;",
                       tags$div(style=sprintf("width:%s%%;height:6px;background:#0d6efd;border-radius:6px;", pct)))
          )
        })
      ),
      if (flex_need>0) div(class="value-card", tags$b("FLEX:"), sprintf("%d remaining", flex_need)),
      if (sflex_need>0) div(class="value-card", tags$b("SUPERFLEX:"), sprintf("%d remaining", sflex_need))
    )
  })
  
  output$tbl_reco <- renderDT({
    df <- available()
    mt <- myteam() %>% left_join(rankings(), by="player")
    need <- compute_needs(mt, parse_roster())
    
    flex_left  <- need$remaining[match("FLEX", need$pos)];      if (is.na(flex_left))  flex_left  <- 0
    sflex_left <- need$remaining[match("SUPERFLEX", need$pos)]; if (is.na(sflex_left)) sflex_left <- 0
    
    df <- df %>%
      mutate(
        need_bonus = case_when(
          (pos %in% need$pos) & (need$remaining[match(pos, need$pos)] > 0) ~ 0.20,
          (pos %in% POS_FOR_FLEX)  & flex_left  > 0 ~ 0.10,
          (pos %in% POS_FOR_SFLEX) & sflex_left > 0 ~ 0.05,
          TRUE ~ 0
        ),
        rec_score = -1*(overall_ecr - need_bonus)
      ) %>%
      arrange(desc(rec_score)) %>%
      slice_head(n = 12) %>%
      transmute(Rank = row_number(), Player = player, Team = team, Pos = pos, Tier = Tier, Score = round(Score,3))
    
    datatable(df, rownames = FALSE, options = list(pageLength = 12, dom = 't'))
  })
  
  selected_player <- reactive({
    idx <- input$tbl_board_rows_selected
    if (is.null(idx) || length(idx)==0) return(NULL)
    available()$player[idx[1]]
  })
  
  observeEvent(input$btn_mark_drafted, {
    p <- selected_player(); req(p)
    if (!(p %in% drafted()$player)) {
      drafted(bind_rows(drafted(), tibble(player = p)))
      action_log(bind_rows(action_log(), tibble(action="drafted_any", player=p, time=Sys.time())))
    }
  })
  
  observeEvent(input$btn_draft_me, {
    p <- selected_player(); req(p)
    if (!(p %in% drafted()$player)) drafted(bind_rows(drafted(), tibble(player = p)))
    if (!(p %in% myteam()$player))  myteam(bind_rows(myteam(),   tibble(player = p)))
    action_log(bind_rows(action_log(), tibble(action="drafted_me", player=p, time=Sys.time())))
  })
  
  observeEvent(input$btn_undo, {
    log <- action_log(); if (!nrow(log)) return()
    last <- tail(log, 1); p <- last$player
    if (last$action %in% c("drafted_me","drafted_any")) drafted(drafted() %>% filter(player != p))
    if (last$action == "drafted_me") myteam(myteam() %>% filter(player != p))
    action_log(head(log, -1))
  })
  
  observe({ updateSelectInput(session, "filter_pos", selected = c("RB","WR","QB","TE")) })
  observe({ LEAGUE$teams <<- input$num_teams })
}

shinyApp(ui, server)
