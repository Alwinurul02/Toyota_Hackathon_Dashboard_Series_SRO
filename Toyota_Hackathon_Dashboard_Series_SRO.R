# --------------------------------------------------------------------
# APLIKASI SHINY HACKATHON TOYOTA GR CUP (v81 - Critical AI Analyst)
# File: app.R
# --------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggrepel)

# --------------------------------------------------------------------
# 1. DATA READING FUNCTIONS
# --------------------------------------------------------------------

clean_val <- function(x) {
  if(is.character(x)) str_trim(str_replace_all(x, "\"", "")) else x
}

parse_racing_time <- function(time_str) {
  time_str <- as.character(time_str)
  convert_one <- function(x) {
    if (is.na(x) || x == "" || x == "0" || x == "0.0" || x == 0) return(NA_real_)
    colons <- str_count(x, ":")
    val <- NA_real_
    if (colons == 2) {
      p <- suppressWarnings(lubridate::hms(x))
      val <- lubridate::period_to_seconds(p)
    } else if (colons == 1) {
      p <- suppressWarnings(lubridate::ms(x))
      val <- lubridate::period_to_seconds(p)
    } else {
      x <- str_replace(x, ",", ".")
      val <- suppressWarnings(as.numeric(x))
    }
    if (!is.na(val) && val == 0) return(NA_real_)
    return(val)
  }
  sapply(time_str, convert_one)
}

# AUTO SCALE TIME FIX
auto_scale_time <- function(x) {
  if (all(is.na(x))) return(x)
  med_val <- median(x, na.rm = TRUE)
  if (is.na(med_val)) return(x)
  if (med_val < 5.0) { return(x * 60) } 
  if (med_val > 1000) { return(x / 1000) } 
  return(x)
}

read_smart_results <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  df <- tryCatch(suppressWarnings(read_delim(filepath, delim = ";", show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(df) || ncol(df) <= 1) {
    df <- tryCatch(suppressWarnings(read_delim(filepath, delim = ",", show_col_types = FALSE)), error = function(e) NULL)
  }
  if (is.null(df) || ncol(df) <= 1) return(NULL)
  
  df <- df %>% rename_with(~ str_trim(str_replace_all(., "\"", ""))) %>% mutate(across(where(is.character), clean_val))
  cols <- colnames(df)
  
  if ("No." %in% cols) df <- df %>% rename(NUMBER = `No.`)
  if ("#" %in% cols) df <- df %>% rename(NUMBER = `#`)
  if ("Driver1" %in% cols) df <- df %>% rename(DRIVER_NAME = Driver1)
  if ("Name" %in% cols) df <- df %>% rename(DRIVER_NAME = Name)
  if ("First Name" %in% cols && "Last Name" %in% cols) df <- df %>% mutate(DRIVER_NAME = paste(`First Name`, `Last Name`))
  if ("DRIVER_FIRSTNAME" %in% cols && "DRIVER_SECONDNAME" %in% cols) df <- df %>% mutate(DRIVER_NAME = paste(DRIVER_FIRSTNAME, DRIVER_SECONDNAME))
  
  if ("FL_TIME" %in% cols) df <- df %>% rename(TIME_RAW = FL_TIME)
  else if ("TIME" %in% cols) df <- df %>% rename(TIME_RAW = TIME)
  else if ("BestLap" %in% cols) df <- df %>% rename(TIME_RAW = BestLap)
  else if ("Best Time" %in% cols) df <- df %>% rename(TIME_RAW = `Best Time`)
  else df$TIME_RAW <- NA
  
  if ("POSITION" %in% cols) df <- df %>% rename(POS_RAW = POSITION)
  else if ("POS" %in% cols) df <- df %>% rename(POS_RAW = POS)
  else if ("Pos" %in% cols) df <- df %>% rename(POS_RAW = Pos)
  else df$POS_RAW <- NA
  
  if (!"NUMBER" %in% colnames(df)) return(NULL)
  if (!"DRIVER_NAME" %in% colnames(df)) df$DRIVER_NAME <- paste("Car", df$NUMBER)
  
  df_final <- df %>%
    mutate(
      NUMBER = suppressWarnings(as.numeric(NUMBER)),
      TIME_SEC = parse_racing_time(TIME_RAW),
      POS_NUM = suppressWarnings(as.numeric(POS_RAW)),
      DRIVER_NAME = str_to_title(str_trim(DRIVER_NAME))
    ) %>%
    select(NUMBER, DRIVER_NAME, TIME_SEC, POS_NUM) %>%
    filter(!is.na(NUMBER)) %>%
    arrange(POS_NUM) %>% 
    group_by(NUMBER) %>% slice(1) %>% ungroup()
  return(df_final)
}

read_smart_laps <- function(filepath) {
  if (!file.exists(filepath)) return(NULL)
  df <- tryCatch(suppressWarnings(read_delim(filepath, delim = ";", show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(df) || ncol(df) <= 1) {
    df <- tryCatch(suppressWarnings(read_delim(filepath, delim = ",", show_col_types = FALSE)), error = function(e) NULL)
  }
  if (is.null(df) || ncol(df) <= 1) return(NULL)
  
  df <- df %>% rename_with(~ str_trim(str_replace_all(., "\"", ""))) %>% mutate(across(where(is.character), clean_val))
  cols <- colnames(df)
  
  if ("No." %in% cols) df <- df %>% rename(NUMBER = `No.`)
  if ("#" %in% cols) df <- df %>% rename(NUMBER = `#`)
  if ("Laps" %in% cols) df <- df %>% rename(LAP_NUMBER = Laps)
  if ("Elapsed Time" %in% cols) df <- df %>% rename(ELAPSED = `Elapsed Time`)
  if ("Name" %in% cols) df <- df %>% rename(Original_Name = Name)
  if ("DRIVER_NAME" %in% cols) df <- df %>% rename(Original_Name = DRIVER_NAME)
  if (!"PIT_TIME" %in% colnames(df)) df$PIT_TIME <- 0 
  
  if ("S1_SECONDS" %in% cols) df$S1_FINAL <- as.numeric(str_replace(as.character(df$S1_SECONDS), ",", "."))
  else if ("S1" %in% cols) df$S1_FINAL <- parse_racing_time(df$S1)
  else df$S1_FINAL <- NA_real_
  
  if ("S2_SECONDS" %in% cols) df$S2_FINAL <- as.numeric(str_replace(as.character(df$S2_SECONDS), ",", "."))
  else if ("S2" %in% cols) df$S2_FINAL <- parse_racing_time(df$S2)
  else df$S2_FINAL <- NA_real_
  
  if ("S3_SECONDS" %in% cols) df$S3_FINAL <- as.numeric(str_replace(as.character(df$S3_SECONDS), ",", "."))
  else if ("S3" %in% cols) df$S3_FINAL <- parse_racing_time(df$S3)
  else df$S3_FINAL <- NA_real_
  
  if(any(!is.na(df$S1_FINAL))) df$S1_FINAL <- auto_scale_time(df$S1_FINAL)
  if(any(!is.na(df$S2_FINAL))) df$S2_FINAL <- auto_scale_time(df$S2_FINAL)
  if(any(!is.na(df$S3_FINAL))) df$S3_FINAL <- auto_scale_time(df$S3_FINAL)
  
  if (all(c("NUMBER", "LAP_NUMBER", "ELAPSED") %in% colnames(df))) {
    df_final <- df %>%
      mutate(
        NUMBER = suppressWarnings(as.numeric(NUMBER)),
        LAP_NUMBER = suppressWarnings(as.numeric(LAP_NUMBER)),
        ELAPSED_SECONDS = parse_racing_time(ELAPSED),
        PIT_TIME = suppressWarnings(as.numeric(PIT_TIME)),
        PIT_TIME = ifelse(is.na(PIT_TIME), 0, PIT_TIME)
      ) %>%
      filter(!is.na(ELAPSED_SECONDS) & !is.na(LAP_NUMBER) & !is.na(NUMBER)) %>%
      select(NUMBER, LAP_NUMBER, ELAPSED_SECONDS, PIT_TIME, S1_FINAL, S2_FINAL, S3_FINAL, any_of("Original_Name")) %>%
      rename(S1 = S1_FINAL, S2 = S2_FINAL, S3 = S3_FINAL)
    return(df_final)
  }
  return(NULL)
}

# --------------------------------------------------------------------
# 2. CONFIGURATION
# --------------------------------------------------------------------
SEASON_FOLDER_NAME <- "2025_season_TGRNA_GR_CUP_NORTH_AMERICA"

CIRCUIT_CHOICES <- c(
  "barber-motorsports-park", "circuit-of-the-americas", "indianapolis",
  "road-america", "sebring", "sonoma", "virginia-international-raceway"
)

TGR_RED <- "#c00000"

# --------------------------------------------------------------------
# 3. UI
# --------------------------------------------------------------------
ui <- fluidPage(
  title = "Toyota Hackathon Dashboard",
  uiOutput("dynamic_style"),
  
  tags$head(tags$style(HTML("
      .container-fluid { padding-left: 0px !important; padding-right: 0px !important; }
      .col-sm-3, .col-sm-9 { padding-left: 25px !important; padding-right: 25px !important; }
      .header-container { width: 100%; padding: 15px 25px; margin-bottom: 20px; border-bottom: 3px solid #c00000; display: flex; align-items: center; white-space: nowrap; overflow: hidden; }
      h1.main-title { font-weight: bold; margin: 0; font-size: 26px; line-height: 1; }
      .nav-tabs { margin-left: 25px; margin-right: 25px; }
      .btn-primary { background-color: #c00000; border-color: #a00000; }
      .btn-primary:hover { background-color: #a00000; border-color: #a00000; }
      h4 { font-weight: bold; margin-top: 20px;}
      .img-box { border: 3px solid #c00000; border-radius: 5px; margin-bottom: 10px; box-shadow: 2px 2px 5px rgba(0,0,0,0.2); }
      .ai-box { 
        margin-top: 15px; padding: 15px; 
        border-radius: 5px; border-left: 5px solid #c00000; 
        font-family: 'Courier New', monospace; font-size: 14px; 
        line-height: 1.5;
      }
      .ai-label { font-weight: bold; color: #c00000; }
  "))),
  
  div(class = "header-container",
      h1(class = "main-title", "Toyota Hackathon Dashboard Series SRO")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      h3("Dashboard Filters"),
      div(style="margin-bottom: 15px; padding: 10px; border: 1px solid #ccc; border-radius: 5px;", checkboxInput("dark_mode", "🌙 Enable Dark Mode", value = FALSE)),
      div(style="padding:10px; background:rgba(192, 0, 0, 0.1); border-left:5px solid #c00000; margin-bottom:20px;", strong("Season:"), br(), "2025 TGRNA GR CUP"),
      p(strong("Filters for 'Race Analysis' (Tabs 1-5)")),
      selectInput("circuit", "1. Select Circuit:", choices = CIRCUIT_CHOICES, selected = "virginia-international-raceway"),
      selectInput("race", "2. Select Race:", choices = c("Race 1", "Race 2"), selected = "Race 1"),
      actionButton("run", "Generate Race Report", icon = icon("play-circle"), width = "100%", class = "btn-primary"), 
      hr(),
      uiOutput("driver_select_ui"),
      hr(),
      imageOutput("driver_photo", height = "auto"),
      hr(),
      p(strong("Status:"), style = "font-size: 0.9em; opacity: 0.7;", "Ready to process data.")
    ),
    
    mainPanel(
      width = 9, 
      tabsetPanel(id = "main_tabs",
                  tabPanel("1. Pace Evolution", 
                           br(), plotOutput("pacePlot", height = "700px"),
                           uiOutput("ai_insight_pace")),
                  tabPanel("2. Biggest Movers", 
                           br(), plotOutput("moversPlot", height = "700px"),
                           uiOutput("ai_insight_movers")),
                  tabPanel("3. Race Story", 
                           br(), plotOutput("storyPlot", height = "600px"), hr(), h4("Strategic Context: Weather"), plotOutput("weatherPlot", height = "200px"),
                           uiOutput("ai_insight_story")),
                  tabPanel("4. Consistency", 
                           h4("Consistency Analysis (Top 10 Fastest)"), 
                           p("Lap time distribution analysis. Shorter box means more consistent."), 
                           plotOutput("consistencyPlot", height = "700px"),
                           uiOutput("ai_insight_cons")),
                  tabPanel("5. Ultimate Lap (Sectors)", 
                           h4("Ultimate Lap vs Actual Best Lap"), 
                           p("Red Diamond = Actual Best Lap. Bars = Theoretical Best."),
                           plotOutput("sectorPlot", height = "700px"),
                           uiOutput("ai_insight_sector")),
                  tabPanel("6. Driver Standings", plotOutput("driver_standings_plot", height = "800px"), uiOutput("ai_insight_d_stand")),
                  tabPanel("7. Team Standings", plotOutput("team_standings_plot", height = "500px"), uiOutput("ai_insight_t_stand"))
      )
    )
  )
)

# --------------------------------------------------------------------
# 4. SERVER
# --------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$dynamic_style <- renderUI({
    if(input$dark_mode) {
      tags$style(HTML("
        body { background-color: #121212 !important; color: #ffffff !important; }
        .well { background-color: #1e1e1e !important; border: 1px solid #333 !important; color: #ddd !important; }
        .header-container { background-color: #1e1e1e; }
        h1.main-title { color: #fff !important; }
        .nav-tabs > li > a { color: #aaa !important; }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color: #fff !important; background-color: #333 !important; border: 1px solid #444 !important; border-bottom-color: transparent !important; }
        h1, h2, h3, h4, h5 { color: #fff !important; }
        .selectize-input, .selectize-control.single .selectize-input.input-active { background: #333333 !important; color: #ffffff !important; border-color: #555555 !important; }
        .selectize-dropdown { background: #333333 !important; color: #ffffff !important; border-color: #555555 !important; }
        .selectize-dropdown .active { background-color: #c00000 !important; color: #ffffff !important; }
        .ai-box { background-color: #222; color: #0f0; border-left: 5px solid #c00000; } 
      "))
    } else {
      tags$style(HTML("
        body { background-color: #FFFFFF; color: #000000; }
        .header-container { background-color: #f8f9fa; }
        h1.main-title { color: #333; }
        h1, h2, h3, h4, h5 { color: #333; }
        .ai-box { background-color: #f9f9f9; color: #333; border: 1px solid #ccc; } 
      "))
    }
  })
  
  get_plot_theme <- reactive({
    if (input$dark_mode) {
      theme_minimal(base_size = 14) + theme(plot.background = element_rect(fill = "#121212", color = NA), panel.background = element_rect(fill = "#121212", color = NA), text = element_text(color = "#ffffff"), axis.text = element_text(color = "#bbbbbb"), plot.title = element_text(face="bold", size=18, color = "#ffffff"), plot.subtitle = element_text(size=12, color = "#aaaaaa"), panel.grid.major = element_line(color = "#333333"), panel.grid.minor = element_blank(), legend.position = "bottom")
    } else {
      theme_minimal(base_size = 14) + theme(plot.title = element_text(face="bold", size=18, color = "#000000"), plot.subtitle = element_text(size=12, color = "#666666"), axis.title = element_text(face="bold", size=12), axis.text = element_text(color = "#333333"), legend.position = "bottom", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(color = "black", linewidth = 0.5))
    }
  })
  
  get_colors <- reactive({
    if(input$dark_mode) { list(bg = "#121212", txt = "#ffffff", grey = "#444444", light_grey = "#333333")
    } else { list(bg = "#ffffff", txt = "#000000", grey = "#6e6e6e", light_grey = "#cccccc") }
  })
  
  rv_pace <- reactiveVal(NULL); rv_story <- reactiveVal(NULL); rv_positions <- reactiveVal(NULL); rv_weather <- reactiveVal(NULL); rv_consistency <- reactiveVal(NULL); rv_sectors <- reactiveVal(NULL)
  
  create_placeholder_plot <- function(msg) { ggplot() + annotate("text", x=0, y=0, label=msg, size=6, color="gray50") + theme_void() }
  
  output$driver_photo <- renderImage({
    blank_return <- list(src = "", contentType = "image/png", style = "display: none;")
    req(input$highlight_driver)
    if (input$highlight_driver == "All Drivers") return(blank_return)
    if (!dir.exists("www")) return(blank_return)
    files_in_www <- list.files("www", full.names = TRUE); files_short  <- list.files("www", full.names = FALSE)
    target_clean <- str_to_lower(str_replace_all(input$highlight_driver, "\\s+", ""))
    files_clean  <- str_to_lower(str_replace_all(tools::file_path_sans_ext(files_short), "[_\\s]+", ""))
    match_idx <- which(files_clean == target_clean)
    if (length(match_idx) > 0) { list(src = files_in_www[match_idx[1]], contentType = "image/jpeg", width = "100%", style = "border: 3px solid #c00000; border-radius: 5px; margin-bottom: 10px; box-shadow: 2px 2px 5px rgba(0,0,0,0.2);", alt = input$highlight_driver) } else { return(blank_return) }
  }, deleteFile = FALSE)
  
  driver_standings_data <- reactive({
    path <- file.path(SEASON_FOLDER_NAME, "GR Drivers Championship-1.csv")
    if (!file.exists(path)) return(NULL)
    tryCatch({ df <- read_delim(path, delim = ";", col_types = cols(.default = col_character())) %>% rename_with(str_trim); df %>% select(Participant, Points) %>% mutate(Points=as.numeric(Points), Participant=str_trim(Participant)) %>% filter(Points > 0) %>% mutate(Avg=mean(Points,na.rm=T), Dev=Points-Avg, Pos=Dev>0, Participant=reorder(Participant, Dev)) }, error = function(e) NULL)
  })
  team_standings_data <- reactive({
    path <- file.path(SEASON_FOLDER_NAME, "GR Teams Championship.csv")
    if (!file.exists(path)) return(NULL)
    tryCatch({ df <- read_delim(path, delim = ";", col_types = cols(.default = col_character())) %>% rename_with(str_trim); df %>% select(Participant, Points) %>% mutate(Points=as.numeric(Points)) %>% filter(Points>0) %>% arrange(Points) %>% mutate(Participant=factor(Participant, levels=.$Participant)) }, error = function(e) NULL)
  })
  
  observeEvent(input$run, {
    showNotification("Processing data...", type = "message", duration = 1)
    season_folder <- SEASON_FOLDER_NAME; circuit <- isolate(input$circuit); race_num <- str_extract(isolate(input$race), "\\d+")
    f_race <- sprintf("%s/%s/03_Results GR Cup Race %s Official.csv", season_folder, circuit, race_num)
    f_prac <- sprintf("%s/%s/03_Results_Practice %s.csv", season_folder, circuit, race_num)
    f_qual <- sprintf("%s/%s/03_Results_Qualify %s.csv", season_folder, circuit, race_num)
    f_laps <- sprintf("%s/%s/23_AnalysisEnduranceWithSections_Race %s.csv", season_folder, circuit, race_num)
    f_weath <- sprintf("%s/%s/26_Weather_Race %s.csv", season_folder, circuit, race_num)
    
    df_race_raw <- read_smart_results(f_race); df_prac_raw <- read_smart_results(f_prac); df_qual_raw <- read_smart_results(f_qual); df_laps_raw <- read_smart_laps(f_laps)
    if (is.null(df_race_raw)) { showNotification("Failed to read Race Results.", type = "warning"); rv_pace(NULL); return() }
    master_list <- df_race_raw %>% select(NUMBER, DRIVER_NAME, POS_NUM)
    
    df_pace <- master_list %>% left_join(df_race_raw %>% select(NUMBER, TIME_SEC), by = "NUMBER") %>% rename(Race_Time = TIME_SEC)
    if (!is.null(df_prac_raw)) df_pace <- df_pace %>% left_join(df_prac_raw %>% select(NUMBER, TIME_SEC), by = "NUMBER") %>% rename(Practice_Time = TIME_SEC) else df_pace$Practice_Time <- NA
    if (!is.null(df_qual_raw)) df_pace <- df_pace %>% left_join(df_qual_raw %>% select(NUMBER, TIME_SEC), by = "NUMBER") %>% rename(Quali_Time = TIME_SEC) else df_pace$Quali_Time <- NA
    df_pace <- df_pace %>% mutate(Gap_Race_Quali = Race_Time - Quali_Time) %>% pivot_longer(cols = any_of(c("Race_Time", "Practice_Time", "Quali_Time")), names_to = "Session", values_to = "Lap_Time") %>% mutate(Session = factor(Session, levels = c("Practice_Time", "Quali_Time", "Race_Time"), labels = c("Practice", "Qualifying", "Race"))) %>% filter(!is.na(Lap_Time) & Lap_Time > 0) %>% ungroup() %>% arrange(Session)
    
    df_story <- NULL
    if (!is.null(df_qual_raw) && "POS_NUM" %in% colnames(df_qual_raw)) {
      df_story <- master_list %>% rename(Race_Pos = POS_NUM) %>% inner_join(df_qual_raw %>% select(NUMBER, POS_NUM) %>% rename(Quali_Pos = POS_NUM), by="NUMBER") %>% mutate(Positions_Gained = Quali_Pos - Race_Pos, DRIVER_NAME = reorder(DRIVER_NAME, Positions_Gained)) %>% filter(!is.na(Positions_Gained)) %>% distinct(NUMBER, .keep_all = TRUE)
    }
    
    df_positions <- NULL; df_consistency <- NULL
    if (!is.null(df_laps_raw)) {
      df_positions <- df_laps_raw %>% inner_join(master_list %>% select(NUMBER, DRIVER_NAME), by = "NUMBER") %>% rename(Final_Name = DRIVER_NAME) %>% arrange(LAP_NUMBER, ELAPSED_SECONDS) %>% group_by(LAP_NUMBER) %>% mutate(Position = rank(ELAPSED_SECONDS, ties.method = "first")) %>% ungroup()
      df_consistency <- df_positions %>% arrange(NUMBER, LAP_NUMBER) %>% group_by(NUMBER) %>% mutate(Individual_Lap_Time = ELAPSED_SECONDS - lag(ELAPSED_SECONDS, default = 0)) %>% ungroup() %>% group_by(NUMBER) %>% filter(LAP_NUMBER > 1) %>% mutate(Median_Pace = median(Individual_Lap_Time, na.rm = TRUE)) %>% filter(Individual_Lap_Time > 0) %>% filter(Individual_Lap_Time < Median_Pace * 1.2) %>% ungroup()
    }
    
    df_sectors <- NULL
    if (!is.null(df_laps_raw) && all(c("S1", "S2", "S3") %in% colnames(df_laps_raw))) {
      best_sectors <- df_laps_raw %>% group_by(NUMBER) %>% summarise(Best_S1 = min(S1, na.rm = TRUE), Best_S2 = min(S2, na.rm = TRUE), Best_S3 = min(S3, na.rm = TRUE), .groups = "drop") %>% filter(Best_S1 > 0 & Best_S2 > 0 & Best_S3 > 0 & is.finite(Best_S1))
      if(nrow(best_sectors) > 0) {
        df_sectors <- best_sectors %>% inner_join(master_list %>% select(NUMBER, DRIVER_NAME), by = "NUMBER") %>% inner_join(df_race_raw %>% select(NUMBER, TIME_SEC), by = "NUMBER") %>%
          mutate(Theoretical_Sum = Best_S1 + Best_S2 + Best_S3) %>%
          pivot_longer(cols = c(Best_S1, Best_S2, Best_S3), names_to = "Sector_Raw", values_to = "Sec_Time") %>%
          mutate(Sector = case_when(Sector_Raw == "Best_S1" ~ "Sector 1", Sector_Raw == "Best_S2" ~ "Sector 2", Sector_Raw == "Best_S3" ~ "Sector 3"), Sector = factor(Sector, levels = c("Sector 3", "Sector 2", "Sector 1")))
      }
    }
    
    df_weather <- NULL
    if (file.exists(f_weath)) try({ w <- read_delim(f_weath, delim = ";", show_col_types = FALSE); if(ncol(w) > 1) df_weather <- w %>% mutate(Time = mdy_hms(TIME_UTC_STR), TRACK_TEMP = as.numeric(TRACK_TEMP), AIR_TEMP = as.numeric(AIR_TEMP)) }, silent = TRUE)
    
    rv_pace(df_pace); rv_story(df_story); rv_positions(df_positions); rv_weather(df_weather); rv_consistency(df_consistency); rv_sectors(df_sectors)
    drivers <- sort(unique(master_list$DRIVER_NAME))
    output$driver_select_ui <- renderUI({ selectInput("highlight_driver", "4. Highlight Driver:", choices = c("All Drivers", drivers), selected = "All Drivers") })
    showNotification("Report generated successfully!", type = "message")
  })
  
  # --- AI ANALYST V81 (CRITICAL & COMPARATIVE) ---
  generate_ai_text <- function(type, driver, data) {
    if (is.null(data)) return("No data available for analysis.")
    
    # 1. PACE ANALYSIS
    if (type == "pace") {
      if (driver == "All Drivers") {
        fastest <- data %>% filter(Session == "Race") %>% arrange(Lap_Time) %>% slice(1)
        # Compare Quali vs Race Pace of the field
        avg_pace_drop <- data %>% pivot_wider(names_from = Session, values_from = Lap_Time) %>% mutate(Diff = Race - Qualifying) %>% summarise(M = mean(Diff, na.rm=TRUE)) %>% pull(M)
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), 
                   p(HTML(paste0("Average field pace degradation is <b>+", round(avg_pace_drop, 2), "s</b> from Qualifying. Fastest race lap: <b>", round(fastest$Lap_Time, 2), "s</b> by ", fastest$DRIVER_NAME, ".")))))
      } else {
        d_data <- data %>% filter(DRIVER_NAME == driver) %>% pivot_wider(names_from = Session, values_from = Lap_Time)
        if(nrow(d_data) > 0 && !is.na(d_data$Qualifying) && !is.na(d_data$Race)) {
          diff <- d_data$Race - d_data$Qualifying
          # Comparative Analysis
          field_avg_diff <- data %>% pivot_wider(names_from = Session, values_from = Lap_Time) %>% mutate(Diff = Race - Qualifying) %>% summarise(M = mean(Diff, na.rm=TRUE)) %>% pull(M)
          
          status <- if(diff < field_avg_diff) "<span style='color:#00ff00'>Better than Average</span>" else "<span style='color:#ff4444'>High Degradation</span>"
          
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), 
                     p(HTML(paste0("Race Pace Delta: <b>+", round(diff, 2), "s</b>.<br>Verdict: ", status, " (Field Avg: +", round(field_avg_diff, 2), "s).")))))
        }
      }
    }
    
    # 2. MOVERS ANALYSIS
    if (type == "movers") {
      if (driver == "All Drivers") {
        top_mover <- data %>% arrange(desc(Positions_Gained)) %>% slice(1)
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p(paste("Hard Charger: ", top_mover$DRIVER_NAME, " (+", top_mover$Positions_Gained, " positions)."))))
      } else {
        d_data <- data %>% filter(DRIVER_NAME == driver)
        if(nrow(d_data) > 0) {
          pg <- d_data$Positions_Gained
          msg <- if(pg > 0) "Aggressive drive, moving up the field." else if(pg == 0) "Stable race, held position." else "Defensive race, lost track position."
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), p(paste("Net Position Change: ", pg, ". ", msg))))
        }
      }
    }
    
    # 3. RACE STORY
    if (type == "story") {
      if (driver == "All Drivers") {
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p("Visualizing position battles. Sharp drops indicate pit stops or incidents.")))
      } else {
        d_laps <- data %>% filter(Final_Name == driver)
        if(nrow(d_laps) > 0) {
          start_pos <- head(d_laps$Position, 1); end_pos <- tail(d_laps$Position, 1); best_pos <- min(d_laps$Position)
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), 
                     p(HTML(paste0("Start: P", start_pos, " &#8594; Finish: P", end_pos, ".<br>Highest Position reached: <b>P", best_pos, "</b>.")))))
        }
      }
    }
    
    # 4. CONSISTENCY (Z-SCORE LOGIC)
    if (type == "cons") {
      if (driver == "All Drivers") {
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p("Box width represents Lap Time Variance (IQR). Narrower = More Consistent.")))
      } else {
        d_data <- data %>% filter(Final_Name == driver)
        if(nrow(d_data) > 0) {
          iqr_val <- IQR(d_data$Individual_Lap_Time, na.rm=TRUE)
          
          # Calculate Grid Average Consistency
          all_iqrs <- data %>% group_by(Final_Name) %>% summarise(IQR = IQR(Individual_Lap_Time, na.rm=TRUE))
          avg_iqr <- mean(all_iqrs$IQR, na.rm=TRUE)
          
          verdict <- if(iqr_val < avg_iqr) "High Precision (Above Average)" else "Erratic (Below Average)"
          
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), 
                     p(HTML(paste0("Lap Variance (IQR): <b>", round(iqr_val, 3), "s</b>.<br>Stability Rating: <b>", verdict, "</b> (Grid Avg: ", round(avg_iqr, 3), "s).")))))
        }
      }
    }
    
    # 5. SECTOR ANALYSIS (FIXED TOTAL SUM)
    if (type == "sector") {
      if (driver == "All Drivers") {
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p("Gap between Bar & Diamond = Unlocked Potential. Zero gap = Perfect Lap Execution.")))
      } else {
        d_data <- data %>% filter(DRIVER_NAME == driver)
        if(nrow(d_data) > 0) {
          actual_time <- d_data$TIME_SEC[1]
          # IMPORTANT: Calculate theoretical sum manually from the distinct sectors
          sectors_unique <- d_data %>% select(Sector, Sec_Time) %>% distinct()
          theoretical_time <- sum(sectors_unique$Sec_Time, na.rm = TRUE)
          
          # Auto Scale Correction for Text
          if (actual_time / theoretical_time > 30) theoretical_time <- theoretical_time * 60
          
          gap <- actual_time - theoretical_time
          execution <- round((theoretical_time / actual_time) * 100, 1)
          
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), 
                     p(HTML(paste0("Theoretical Best: <b>", round(theoretical_time, 2), "s</b>.<br>Actual Best: <b>", round(actual_time, 2), "s</b>.<br>Execution Score: <b>", execution, "%</b> (Gap: ", round(gap, 2), "s).")))))
        }
      }
    }
    
    # 6. DRIVER STANDINGS (CRITICAL Z-SCORE)
    if (type == "d_stand") {
      avg_pts <- mean(data$Points, na.rm=TRUE)
      if (driver == "All Drivers") {
        leader <- data %>% arrange(desc(Points)) %>% slice(1)
        return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p(paste("Championship Leader:", leader$Participant, "with", leader$Points, "points. The grid average is", round(avg_pts,0), "points."))))
      } else {
        d_row <- data %>% filter(Participant == driver)
        if(nrow(d_row) > 0) {
          diff <- d_row$Points - avg_pts
          color <- ifelse(diff > 0, "#00ff00", "#ff4444")
          status <- ifelse(diff > 0, "ABOVE", "BELOW")
          
          return(div(class="ai-box", h4(paste("🤖 Analysis for", driver, ":")), 
                     p(HTML(paste0("Points: <b>", d_row$Points, "</b>.<br><span style='color:", color, "'>Performance is ", round(abs(diff), 0), " points ", status, " the grid average.</span>")))))
        }
        else return(div(class="ai-box", h4("Note:"), p("No championship points recorded.")))
      }
    }
    
    # 7. TEAM STANDINGS
    if (type == "t_stand") {
      leader <- data %>% arrange(desc(Points)) %>% slice(1)
      return(div(class="ai-box", h4("🤖 Race Engineer Bot:"), p(paste("Leading Team:", leader$Participant, "with", leader$Points, "points."))))
    }
    return(NULL)
  }
  
  # --- OUTPUTS ---
  output$ai_insight_pace <- renderUI({ generate_ai_text("pace", input$highlight_driver, rv_pace()) })
  output$ai_insight_movers <- renderUI({ generate_ai_text("movers", input$highlight_driver, rv_story()) })
  output$ai_insight_story <- renderUI({ generate_ai_text("story", input$highlight_driver, rv_positions()) })
  output$ai_insight_cons <- renderUI({ generate_ai_text("cons", input$highlight_driver, rv_consistency()) })
  output$ai_insight_sector <- renderUI({ generate_ai_text("sector", input$highlight_driver, rv_sectors()) })
  output$ai_insight_d_stand <- renderUI({ generate_ai_text("d_stand", input$highlight_driver, driver_standings_data()) })
  output$ai_insight_t_stand <- renderUI({ generate_ai_text("t_stand", input$highlight_driver, team_standings_data()) })
  
  output$pacePlot <- renderPlot({
    df <- rv_pace(); if(is.null(df)) return(create_placeholder_plot("No Data")); req(input$highlight_driver)
    cols <- get_colors(); my_theme <- get_plot_theme()
    df_wide <- df %>% filter(Session %in% c("Qualifying", "Race")) %>% select(NUMBER, DRIVER_NAME, Session, Lap_Time) %>% pivot_wider(names_from = Session, values_from = Lap_Time) %>% mutate(Delta = Race - Qualifying)
    has_conn <- "Qualifying" %in% colnames(df_wide) && "Race" %in% colnames(df_wide); if(has_conn) df_wide <- df_wide %>% filter(!is.na(Qualifying) & !is.na(Race))
    p <- ggplot(df, aes(x = Lap_Time, y = reorder(DRIVER_NAME, Lap_Time, min, na.rm=TRUE))) + stat_summary(fun.min = min, fun.max = max, geom = "linerange", aes(group = DRIVER_NAME), color = cols$light_grey, size = 0.3) + labs(title = "1. Pace Evolution", subtitle = "Qualifying vs Race Pace (Left is Faster)", x = "Lap Time (s)", y = NULL, color = "Session") + my_theme
    if(has_conn) {
      p <- p + geom_segment(data=df_wide, aes(x=Qualifying, xend=Race, y=DRIVER_NAME, yend=DRIVER_NAME), arrow=arrow(length=unit(0.2,"cm")), color=cols$txt, size=0.8, alpha=0.5)
      if(input$highlight_driver != "All Drivers") { hl_wide <- df_wide %>% filter(DRIVER_NAME == input$highlight_driver); p <- p + geom_text(data=hl_wide, aes(x = (Qualifying+Race)/2, y = DRIVER_NAME, label = sprintf("+%.2fs", Delta)), vjust = -0.8, color = TGR_RED, fontface="bold") }
    }
    if(input$highlight_driver == "All Drivers") { p <- p + geom_point(aes(color = Session), size = 4, alpha = 0.8) + scale_color_manual(values = c("Practice"=cols$light_grey, "Qualifying"=cols$txt, "Race"=TGR_RED))
    } else { hl <- df %>% filter(DRIVER_NAME == input$highlight_driver); p <- p + geom_point(aes(color = Session), size = 3, alpha = 0.1) + geom_point(data = hl, aes(color = Session), size = 5, stroke = 1.5, shape = 21, fill = NA) + geom_point(data = hl, aes(color = Session), size = 5) + scale_color_manual(values = c("Practice"=cols$light_grey, "Qualifying"=cols$txt, "Race"=TGR_RED)) + theme(axis.text.y = element_text(color = ifelse(levels(reorder(df$DRIVER_NAME, df$Lap_Time, min)) == input$highlight_driver, TGR_RED, cols$txt))) }
    p
  })
  
  output$moversPlot <- renderPlot({
    df <- rv_story(); if(is.null(df)) return(create_placeholder_plot("No Data")); req(input$highlight_driver)
    cols <- get_colors(); my_theme <- get_plot_theme()
    df <- df %>% mutate(Fill = case_when(input$highlight_driver == "All Drivers" & Positions_Gained > 0 ~ TGR_RED, input$highlight_driver == "All Drivers" ~ cols$grey, DRIVER_NAME == input$highlight_driver ~ TGR_RED, TRUE ~ cols$light_grey))
    ggplot(df, aes(x = Positions_Gained, y = DRIVER_NAME, fill = Fill)) + geom_col(alpha = 0.9) + geom_text(aes(label = sprintf("Start P%s -> P%s", Quali_Pos, Race_Pos)), hjust = ifelse(df$Positions_Gained > 0, -0.1, 1.1), size = 3.5, fontface = "bold", color = cols$txt) + scale_fill_identity() + labs(title = "2. Biggest Movers", subtitle = "Positive = Gained Positions", x = "Positions Gained", y = NULL) + my_theme + theme(plot.margin = margin(r = 60)) 
  })
  
  output$storyPlot <- renderPlot({
    df <- rv_positions(); if(is.null(df)) return(create_placeholder_plot("No Data")); req(input$highlight_driver)
    cols <- get_colors(); my_theme <- get_plot_theme()
    max_lap <- max(df$LAP_NUMBER, na.rm = TRUE)
    p <- ggplot(df, aes(x = LAP_NUMBER, y = Position, group = Final_Name)) + scale_y_reverse(breaks = seq(1, 40, 2)) + scale_x_continuous(breaks = seq(0, max_lap, 2), limits = c(0, max_lap + 6)) + labs(title = "3. Race Story", subtitle = "Lap by Lap Position", x = "Lap", y = "Posisi") + theme(legend.position = "none") + my_theme
    if(input$highlight_driver == "All Drivers") { top3 <- df %>% filter(LAP_NUMBER == max_lap, Position <= 3) %>% pull(Final_Name); df_top3 <- df %>% filter(Final_Name %in% top3) %>% mutate(LineColor = ifelse(Position == 1, TGR_RED, cols$txt)); p <- p + geom_line(color = cols$light_grey, alpha = 0.5) + geom_line(data = df_top3, aes(color = LineColor), linewidth = 1) + scale_color_identity() + geom_text_repel(data = df %>% filter(LAP_NUMBER == max_lap), aes(label = Final_Name), nudge_x = 2, color=cols$txt)
    } else { hl <- df %>% filter(Final_Name == input$highlight_driver); p <- p + geom_line(color = cols$light_grey, alpha = 0.3) + geom_line(data = hl, color = TGR_RED, linewidth = 1.5) + geom_text_repel(data = df %>% filter(LAP_NUMBER == max_lap), aes(label = Final_Name), nudge_x = 2, color = ifelse(df$Final_Name[df$LAP_NUMBER==max_lap] == input$highlight_driver, TGR_RED, cols$txt)) }
    if(any(df$PIT_TIME > 0)) p <- p + geom_point(data = df %>% filter(PIT_TIME > 0), shape = "P", color = TGR_RED, size = 3)
    p
  })
  
  output$weatherPlot <- renderPlot({
    df <- rv_weather(); if(is.null(df)) return(create_placeholder_plot("No Data"))
    cols <- get_colors(); my_theme <- get_plot_theme()
    ggplot(df, aes(x = Time)) + geom_line(aes(y = TRACK_TEMP, color = "Track"), linewidth = 1) + geom_line(aes(y = AIR_TEMP, color = "Air"), linetype = "dashed") + scale_color_manual(values = c("Track" = TGR_RED, "Air" = cols$grey)) + labs(title = "Track Temperature", y = "Temp (C)", x = "Time") + my_theme
  })
  
  output$consistencyPlot <- renderPlot({
    df <- rv_consistency(); if(is.null(df)) return(create_placeholder_plot("No Data")); req(input$highlight_driver)
    cols <- get_colors(); my_theme <- get_plot_theme()
    driver_ranks <- df %>% group_by(Final_Name) %>% summarise(Median_Val = median(Individual_Lap_Time, na.rm = TRUE)) %>% arrange(Median_Val) %>% mutate(Rank = row_number()) %>% select(Final_Name, Rank, Median_Val)
    df_sorted <- df %>% left_join(driver_ranks, by = "Final_Name")
    if (input$highlight_driver == "All Drivers") { df_sorted <- df_sorted %>% mutate(Fill_Color = ifelse(Rank <= 10, TGR_RED, cols$light_grey), Alpha_Val = ifelse(Rank <= 10, 0.9, 0.4))
    } else { df_sorted <- df_sorted %>% mutate(Fill_Color = ifelse(Final_Name == input$highlight_driver, TGR_RED, cols$light_grey), Alpha_Val = ifelse(Final_Name == input$highlight_driver, 0.9, 0.4)) }
    ggplot(df_sorted, aes(x = reorder(Final_Name, Individual_Lap_Time, FUN = median), y = Individual_Lap_Time)) + geom_jitter(color = cols$txt, size = 0.2, alpha = 0.1, width = 0.1) + geom_boxplot(aes(fill = Fill_Color, alpha = Alpha_Val), outlier.shape = NA, width = 0.6, size = 0.3, color = cols$txt) + geom_text(data=driver_ranks, aes(x=Final_Name, y=Median_Val, label=sprintf("%.2fs", Median_Val)), hjust=-1.2, size=3, color=cols$txt) + scale_fill_identity() + scale_alpha_identity() + coord_flip() + labs(title = "Lap Time Consistency (Top 10 Highlighted)", subtitle = "Text shows Median Lap Time. Shorter Box = More Consistent.", x = NULL, y = "Lap Time (s)") + scale_y_continuous(n.breaks = 6) + my_theme
  })
  
  output$sectorPlot <- renderPlot({
    df <- rv_sectors()
    if(is.null(df)) return(create_placeholder_plot("No Sector Data"))
    req(input$highlight_driver)
    cols <- get_colors(); my_theme <- get_plot_theme()
    
    df <- df %>% group_by(DRIVER_NAME) %>% mutate(Total_Theory = sum(Sec_Time)) %>% ungroup()
    med_theory <- median(df$Total_Theory, na.rm=TRUE); med_actual <- median(df$TIME_SEC, na.rm=TRUE)
    if (!is.na(med_theory) && !is.na(med_actual) && (med_actual / med_theory > 30)) { df$Sec_Time <- df$Sec_Time * 60; df$Total_Theory <- df$Total_Theory * 60 }
    
    df <- df %>% mutate(DRIVER_NAME = reorder(DRIVER_NAME, Total_Theory, FUN=min))
    if (input$highlight_driver == "All Drivers") { df <- df %>% mutate(Alpha_Level = 0.9, Is_Highlighted = TRUE)
    } else { df <- df %>% mutate(Alpha_Level = ifelse(DRIVER_NAME == input$highlight_driver, 0.9, 0.2), Is_Highlighted = DRIVER_NAME == input$highlight_driver) }
    
    p <- ggplot(df, aes(x = DRIVER_NAME, y = Sec_Time)) +
      geom_bar(aes(fill = Sector, alpha = Alpha_Level), stat = "identity", width = 0.7) +
      geom_point(aes(y = TIME_SEC), shape = 18, size = 4, color = TGR_RED, data = df %>% distinct(DRIVER_NAME, TIME_SEC, Alpha_Level) %>% filter(Alpha_Level > 0.5)) + 
      coord_flip() +
      scale_fill_manual(values = c("#999999", "#666666", "#333333")) +
      scale_alpha_identity() +
      geom_text(aes(label=ifelse(Is_Highlighted & Sec_Time > 15, sprintf("%.1f", Sec_Time), "")), position=position_stack(vjust=0.5), size=3, color="white") +
      geom_text(data = df %>% distinct(DRIVER_NAME, TIME_SEC, Is_Highlighted) %>% filter(Is_Highlighted), aes(y=TIME_SEC, label=sprintf("%.1f", TIME_SEC)), nudge_y = 8, size=3.5, color=cols$txt, fontface="bold") +
      labs(title = NULL, subtitle = "Bar = Theoretical Best (Sum of Sectors) | Diamond = Actual Best Lap", x = NULL, y = "Time (s)") +
      my_theme
    
    p
  })
  
  output$driver_standings_plot <- renderPlot({ 
    d <- driver_standings_data(); if(is.null(d)) return(create_placeholder_plot("No Data")); 
    cols <- get_colors(); my_theme <- get_plot_theme()
    ggplot(d, aes(x=Participant, y=Dev, fill=Pos)) + geom_col(alpha=0.9) + coord_flip() + geom_hline(yintercept=0, color=cols$txt) + scale_fill_manual(values=c("TRUE"=TGR_RED, "FALSE"=cols$light_grey), guide="none") + geom_text(aes(label=sprintf("%+.0f", Dev)), hjust=ifelse(d$Pos, -0.2, 1.2), fontface="bold", color=cols$txt) + labs(title="Driver Standings: Deviation", subtitle="Points above/below average", x=NULL, y="Points +/- Avg") + my_theme
  })
  output$team_standings_plot <- renderPlot({ 
    d <- team_standings_data(); if(is.null(d)) return(create_placeholder_plot("No Data")); 
    cols <- get_colors(); my_theme <- get_plot_theme()
    ggplot(d, aes(x=Participant, y=Points)) + geom_col(fill = TGR_RED, width = 0.7, alpha = 0.9) + geom_text(aes(label=Points), hjust=-0.2, color=cols$txt, fontface="bold") + coord_flip() + labs(title="Team Standings", x=NULL, y="Total Points") + theme(panel.grid.major.y=element_blank()) + my_theme
  })
}

shinyApp(ui = ui, server = server)