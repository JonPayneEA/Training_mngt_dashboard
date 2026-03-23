# ============================================================
# app.R - Training Sign-off Dashboard
#
# Dependencies: shiny, bslib, DT, data.table, yaml
# No tidyverse.
#
# Usage:
#   Set DATA_DIR to the folder containing .yaml sign-off files.
#   Run with: shiny::runApp("app.R")
#
# Reference: Staff Training Framework v1.0
# ============================================================

library(shiny)
library(bslib)
library(DT)
library(data.table)
library(yaml)

if (!exists("%||%", baseenv())) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# --- Configuration -----------------------------------------------------------

DATA_DIR     <- "examples"
TEMPLATE_DIR <- "templates"

TRACK_LABELS <- c(
  core                      = "Core",
  hydrology_fundamentals    = "Hydrology Fundamentals",
  hydraulics_fundamentals   = "Hydraulics Fundamentals",
  data_coding_fundamentals  = "Data & Coding Fundamentals",
  cross_cutting             = "Cross-cutting",
  hydrological_modelling    = "Hydrological Modelling",
  hydraulic_modelling       = "Hydraulic Modelling",
  fmp_crossover             = "FMP API Crossover",
  r_development             = "R Development",
  python_ml                 = "Python & ML"
)

# --- Data functions ----------------------------------------------------------

read_signoff <- function(path) {
  y <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(y)) return(NULL)
  required <- c("staff_name", "role", "track", "year", "items")
  if (length(setdiff(required, names(y))) > 0L) return(NULL)
  items <- rbindlist(lapply(y$items, function(item) {
    data.table(
      id          = item$id          %||% NA_character_,
      description = item$description %||% NA_character_,
      section     = item$section     %||% NA_character_,
      completed   = item$completed   %||% NA_character_,
      verified_by = item$verified_by %||% NA_character_,
      notes       = item$notes       %||% NA_character_
    )
  }), fill = TRUE)
  items[, `:=`(
    staff_name = y$staff_name, role = y$role,
    track = y$track, year = as.integer(y$year), file = basename(path)
  )]
  items[]
}

load_all_data <- function(dir) {
  if (!dir.exists(dir)) return(data.table())
  files <- list.files(dir, pattern = "\\.yaml$", full.names = TRUE, recursive = FALSE)
  if (length(files) == 0L) return(data.table())
  out <- rbindlist(lapply(files, read_signoff), fill = TRUE)
  if (is.null(out)) data.table() else out
}

make_summary <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0L) return(data.table())
  dt[, .(
    total = .N, done = sum(!is.na(completed)),
    verified = sum(!is.na(verified_by)), gaps = sum(is.na(completed)),
    pct = round(100 * sum(!is.na(completed)) / .N, 1)
  ), by = .(staff_name, role, track, year)]
}

track_label <- function(x) {
  if (length(x) == 0L) return(character(0))
  out <- TRACK_LABELS[x]
  ifelse(is.na(out), x, out)
}

# --- Custom CSS (matching the HTML preview) ----------------------------------

app_css <- "
:root {
  --dark-blue: #1F3864;
  --mid-blue: #2E75B6;
  --light-blue: #D5E8F0;
  --good: #548235;
  --warn: #BF9000;
  --bad: #C00000;
  --bg: #f8f9fa;
  --border: #dee2e6;
  --text: #1F3864;
  --text-light: #6c757d;
}

body { background: var(--bg) !important; color: var(--text); font-family: 'Segoe UI', system-ui, -apple-system, sans-serif; }

/* Navbar */
.navbar { background-color: var(--dark-blue) !important; box-shadow: 0 2px 8px rgba(0,0,0,.15); }
.navbar .navbar-brand, .navbar .nav-link { color: white !important; font-size: 13px; }
.navbar .nav-link.active { font-weight: 700 !important; border-bottom: 3px solid white; }

/* Value boxes */
.vbox-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 12px; margin-bottom: 18px; }
@media (min-width: 768px) { .vbox-grid { grid-template-columns: repeat(4, 1fr); } }
.vbox { background: white; border-radius: 8px; padding: 16px; border-left: 4px solid var(--mid-blue); box-shadow: 0 1px 3px rgba(0,0,0,.08); }
.vbox.green { border-left-color: var(--good); }
.vbox.amber { border-left-color: var(--warn); }
.vbox-value { font-size: 28px; font-weight: 700; color: var(--dark-blue); line-height: 1.1; }
.vbox-label { font-size: 11px; color: var(--text-light); text-transform: uppercase; letter-spacing: .5px; margin-top: 4px; }

/* Cards */
.dash-card { background: white; border-radius: 8px; border: 1px solid var(--border); margin-bottom: 18px; box-shadow: 0 1px 3px rgba(0,0,0,.06); overflow: hidden; }
.dash-card-header { padding: 12px 16px; font-weight: 700; font-size: 13px; color: var(--dark-blue); border-bottom: 2px solid var(--mid-blue); background: #fafbfc; }
.dash-card-body { padding: 0; }
.dash-card-body.padded { padding: 16px; }

/* Summary table */
.sum-table { width: 100%; border-collapse: collapse; font-size: 13px; }
.sum-table th { background: var(--light-blue); color: var(--dark-blue); font-weight: 600; text-align: left;
  padding: 10px 12px; font-size: 11px; text-transform: uppercase; letter-spacing: .3px; white-space: nowrap; }
.sum-table td { padding: 9px 12px; border-bottom: 1px solid #eee; vertical-align: middle; }
.sum-table tr:hover td { background: #f5f8ff; }
.sum-table .fw-bold { font-weight: 700; }

/* Badges */
.badge-good { display:inline-block; background: var(--good); color: white; padding: 3px 9px; border-radius: 4px; font-size: 11px; font-weight: 600; }
.badge-warn { display:inline-block; background: var(--warn); color: white; padding: 3px 9px; border-radius: 4px; font-size: 11px; font-weight: 600; }
.badge-bad  { display:inline-block; background: var(--bad);  color: white; padding: 3px 9px; border-radius: 4px; font-size: 11px; font-weight: 600; }

/* Progress bars */
.pbar-wrap { background: #eee; border-radius: 4px; height: 8px; width: 100%; min-width: 60px; }
.pbar { height: 100%; border-radius: 4px; }
.pbar.good { background: var(--good); }
.pbar.warn { background: var(--warn); }
.pbar.bad  { background: var(--bad); }

/* Bar chart */
.bar-chart { padding: 16px; }
.bar-row { display: flex; align-items: center; margin-bottom: 10px; }
.bar-label { width: 180px; flex-shrink: 0; font-size: 12px; font-weight: 500; text-align: right; padding-right: 14px; }
.bar-track { flex: 1; background: #eee; border-radius: 4px; height: 24px; overflow: hidden; }
.bar-fill { height: 100%; border-radius: 4px; display: flex; align-items: center; justify-content: flex-end;
  padding-right: 8px; font-size: 11px; font-weight: 600; color: white; min-width: 40px; }

/* Dots */
.dot { display: inline-block; width: 10px; height: 10px; border-radius: 50%; margin-right: 6px; vertical-align: middle; }
.dot-good { background: var(--good); }
.dot-warn { background: var(--warn); }
.dot-bad  { background: var(--bad); }

/* Alerts */
.dash-alert { padding: 14px 16px; border-radius: 6px; margin-bottom: 16px; font-size: 13px; font-weight: 500; }
.dash-alert-ok  { background: #e8f5e9; color: #2e7d32; border: 1px solid #c8e6c9; }
.dash-alert-warn { background: #fff8e1; color: #f57f17; border: 1px solid #ffecb3; }
.dash-alert-bad  { background: #fce4ec; color: #c62828; border: 1px solid #ef9a9a; }

/* Onboard result */
.onboard-ok { background: #e8f5e9; border: 1px solid #c8e6c9; border-radius: 6px; padding: 16px; font-size: 13px; color: #2e7d32; }
.onboard-ok code { background: #c8e6c9; padding: 1px 5px; border-radius: 3px; font-size: 12px; }

/* DT overrides */
.dataTables_wrapper { font-size: 13px; }
table.dataTable thead th { background: var(--light-blue) !important; color: var(--dark-blue) !important;
  font-size: 11px; text-transform: uppercase; letter-spacing: .3px; }
table.dataTable tbody td { padding: 9px 12px !important; }
table.dataTable tbody tr:hover td { background: #f5f8ff !important; }

/* Sidebar overrides */
.bslib-sidebar-layout > .sidebar { background: white !important; border-right: 1px solid var(--border) !important; }
.sidebar .form-label, .sidebar label { font-size: 12px; font-weight: 600; color: var(--dark-blue); }

/* Checkbox grid for onboard */
.ck-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 4px; }
.ck-grid label { font-weight: 400 !important; font-size: 12px !important; }
"

# --- UI ----------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(style = "font-weight:700", "F&W Training Dashboard"),
  id = "nav",
  theme = bs_theme(
    version = 5, bg = "#FFFFFF", fg = "#1F3864",
    primary = "#2E75B6", secondary = "#D5E8F0", font_scale = 0.92,
    base_font = font_collection("Segoe UI", "Arial", "sans-serif"),
    heading_font = font_collection("Segoe UI", "Arial", "sans-serif")
  ),
  header = tags$head(tags$style(HTML(app_css))),

  # ---- Overview ----
  nav_panel("Overview", icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(title = "Filters", width = 260,
        selectInput("ov_staff", "Staff member", choices = NULL, multiple = TRUE),
        selectInput("ov_track", "Track", choices = NULL, multiple = TRUE),
        selectInput("ov_year",  "Year",  choices = NULL, multiple = FALSE),
        hr(),
        actionButton("refresh", "Reload files", icon = icon("rotate"),
                     class = "btn-outline-primary w-100")
      ),
      uiOutput("overview_vboxes"),
      layout_columns(col_widths = c(7, 5),
        tags$div(class = "dash-card",
          tags$div(class = "dash-card-header", "Completion by staff and track"),
          tags$div(class = "dash-card-body", uiOutput("summary_html"))
        ),
        tags$div(class = "dash-card",
          tags$div(class = "dash-card-header", "Completion by track"),
          tags$div(class = "dash-card-body", uiOutput("bar_chart_html"))
        )
      )
    )
  ),

  # ---- Gaps ----
  nav_panel("Gaps", icon = icon("exclamation-circle"),
    layout_sidebar(
      sidebar = sidebar(title = "Filters", width = 260,
        selectInput("gap_staff", "Staff member", choices = NULL, multiple = TRUE),
        selectInput("gap_track", "Track", choices = NULL, multiple = TRUE)
      ),
      tags$div(class = "dash-card",
        tags$div(class = "dash-card-header", uiOutput("gaps_header_text")),
        tags$div(class = "dash-card-body", DTOutput("gaps_table"))
      )
    )
  ),

  # ---- Tier 1 Check ----
  nav_panel("Tier 1 Check", icon = icon("shield-halved"),
    layout_sidebar(
      sidebar = sidebar(title = "Tier 1 Custodians", width = 300,
        helpText("Enter current Tier 1 custodian names, one per line.
                  Must match the staff_name field in YAML files exactly."),
        textAreaInput("tier1_names", NULL, rows = 8,
                      placeholder = "A. Jones\nB. Taylor\nJ. Smith"),
        actionButton("run_tier1", "Run check", icon = icon("magnifying-glass"),
                     class = "btn-primary w-100")
      ),
      tags$div(class = "dash-card",
        tags$div(class = "dash-card-header", "Tier 1 readiness"),
        tags$div(class = "dash-card-body padded",
          uiOutput("tier1_alert"),
          uiOutput("tier1_html")
        )
      )
    )
  ),

  # ---- Staff Detail ----
  nav_panel("Staff Detail", icon = icon("user"),
    layout_sidebar(
      sidebar = sidebar(title = "Select", width = 260,
        selectInput("detail_staff", "Staff member", choices = NULL),
        selectInput("detail_track", "Track", choices = NULL)
      ),
      tags$div(class = "dash-card",
        tags$div(class = "dash-card-header", textOutput("detail_header")),
        tags$div(class = "dash-card-body", uiOutput("detail_html"))
      )
    )
  ),

  # ---- Onboard ----
  nav_panel("Onboard", icon = icon("user-plus"),
    layout_columns(col_widths = c(5, 7),
      tags$div(class = "dash-card",
        tags$div(class = "dash-card-header", "Create sign-off files for a new starter"),
        tags$div(class = "dash-card-body padded",
          textInput("onboard_name", "Full name", placeholder = "J. Smith"),
          textInput("onboard_role", "Role", placeholder = "Flood Forecasting Officer"),
          numericInput("onboard_year", "Year", value = as.integer(format(Sys.Date(), "%Y")),
                       min = 2024, max = 2030, step = 1),
          tags$label(class = "form-label", "Tracks to create"),
          tags$div(class = "ck-grid",
            checkboxGroupInput("onboard_tracks", NULL,
              choices = TRACK_LABELS, selected = TRACK_LABELS)
          ),
          hr(),
          actionButton("onboard_go", "Create files", icon = icon("file-circle-plus"),
                       class = "btn-primary w-100")
        )
      ),
      tags$div(class = "dash-card",
        tags$div(class = "dash-card-header", "Result"),
        tags$div(class = "dash-card-body padded", uiOutput("onboard_result"))
      )
    )
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  all_data <- reactiveVal(data.table())

  load_data <- function() {
    dt <- load_all_data(DATA_DIR)
    all_data(dt)
    update_filters(dt)
  }

  update_filters <- function(dt) {
    if (is.null(dt) || nrow(dt) == 0L) {
      for (id in c("ov_staff","ov_track","ov_year","gap_staff","gap_track","detail_staff","detail_track"))
        updateSelectInput(session, id, choices = character(0))
      return()
    }
    staff  <- sort(unique(dt$staff_name))
    tracks <- sort(unique(dt$track))
    years  <- sort(unique(dt$year), decreasing = TRUE)
    tc <- if (length(tracks) > 0L) setNames(tracks, track_label(tracks)) else character(0)
    updateSelectInput(session, "ov_staff",     choices = staff)
    updateSelectInput(session, "ov_track",     choices = tc)
    updateSelectInput(session, "ov_year",      choices = years, selected = years[1])
    updateSelectInput(session, "gap_staff",    choices = staff)
    updateSelectInput(session, "gap_track",    choices = tc)
    updateSelectInput(session, "detail_staff", choices = staff, selected = staff[1])
    update_detail_tracks()
  }

  observe({ load_data() }, priority = 100)
  observeEvent(input$refresh, { load_data() })

  ov_filtered <- reactive({
    dt <- all_data()
    if (is.null(dt) || nrow(dt) == 0L) return(data.table())
    if (!is.null(input$ov_year) && nchar(input$ov_year) > 0L)
      dt <- dt[year == as.integer(input$ov_year)]
    if (length(input$ov_staff) > 0L) dt <- dt[staff_name %chin% input$ov_staff]
    if (length(input$ov_track) > 0L) dt <- dt[track %chin% input$ov_track]
    dt
  })

  ov_summary <- reactive({ make_summary(ov_filtered()) })

  # ---- Overview: value boxes (custom HTML) ----
  output$overview_vboxes <- renderUI({
    s <- ov_summary()
    dt <- ov_filtered()
    n_staff  <- if (nrow(dt) > 0L) uniqueN(dt$staff_name) else 0
    pct_val  <- if (nrow(s) > 0L) paste0(round(100 * sum(s$done) / sum(s$total), 1), "%") else "--"
    gap_val  <- if (nrow(s) > 0L) sum(s$gaps) else "--"
    n_tracks <- if (nrow(dt) > 0L) uniqueN(dt$track) else 0
    tags$div(class = "vbox-grid",
      tags$div(class = "vbox blue",
        tags$div(class = "vbox-value", n_staff),
        tags$div(class = "vbox-label", "Staff")),
      tags$div(class = "vbox green",
        tags$div(class = "vbox-value", pct_val),
        tags$div(class = "vbox-label", "Completion")),
      tags$div(class = "vbox amber",
        tags$div(class = "vbox-value", gap_val),
        tags$div(class = "vbox-label", "Outstanding items")),
      tags$div(class = "vbox blue",
        tags$div(class = "vbox-value", n_tracks),
        tags$div(class = "vbox-label", "Tracks on file"))
    )
  })

  # ---- Overview: summary table (custom HTML) ----
  output$summary_html <- renderUI({
    s <- ov_summary()
    if (nrow(s) == 0L) return(tags$p(style = "padding:16px; color:#999", "No data loaded. Check DATA_DIR."))

    header <- tags$tr(
      tags$th("Staff"), tags$th("Track"), tags$th("Done"), tags$th("Total"),
      tags$th("Progress"), tags$th("Status")
    )

    rows <- lapply(seq_len(nrow(s)), function(i) {
      r <- s[i]
      bar_class <- if (r$pct == 100) "good" else if (r$pct >= 50) "warn" else "bad"
      badge <- if (r$pct == 100) tags$span(class = "badge-good", "Complete")
               else if (r$pct >= 50) tags$span(class = "badge-warn", "In progress")
               else tags$span(class = "badge-bad", "Behind")
      tags$tr(
        tags$td(class = "fw-bold", r$staff_name),
        tags$td(track_label(r$track)),
        tags$td(r$done), tags$td(r$total),
        tags$td(tags$div(class = "pbar-wrap",
          tags$div(class = paste("pbar", bar_class), style = paste0("width:", r$pct, "%")))),
        tags$td(badge)
      )
    })

    tags$div(style = "overflow-x:auto",
      tags$table(class = "sum-table", tags$thead(header), tags$tbody(rows))
    )
  })

  # ---- Overview: bar chart (custom HTML) ----
  output$bar_chart_html <- renderUI({
    s <- ov_summary()
    if (nrow(s) == 0L) return(tags$p(style = "padding:16px; color:#999", "No data."))

    by_track <- s[, .(done = sum(done), total = sum(total)), by = track]
    by_track[, pct := round(100 * done / total, 1)]
    setorder(by_track, -pct)

    bars <- lapply(seq_len(nrow(by_track)), function(i) {
      r <- by_track[i]
      col <- if (r$pct == 100) "var(--good)" else if (r$pct >= 50) "var(--warn)" else "var(--bad)"
      tags$div(class = "bar-row",
        tags$div(class = "bar-label", track_label(r$track)),
        tags$div(class = "bar-track",
          tags$div(class = "bar-fill",
            style = paste0("width:", max(r$pct, 5), "%; background:", col),
            paste0(r$pct, "%")
          )
        )
      )
    })

    tags$div(class = "bar-chart", bars)
  })

  # ---- Gaps ----
  gap_filtered <- reactive({
    dt <- all_data()
    if (is.null(dt) || nrow(dt) == 0L) return(data.table())
    dt <- dt[is.na(completed)]
    if (length(input$gap_staff) > 0L) dt <- dt[staff_name %chin% input$gap_staff]
    if (length(input$gap_track) > 0L) dt <- dt[track %chin% input$gap_track]
    dt
  })

  output$gaps_header_text <- renderUI({
    n <- nrow(gap_filtered())
    paste0("Incomplete training items (", n, ")")
  })

  output$gaps_table <- renderDT({
    dt <- gap_filtered()
    if (nrow(dt) == 0L) return(datatable(data.table(Message = "No gaps found.")))
    display <- dt[, .(Staff = staff_name, Track = track_label(track),
                      Section = section, ID = id, Description = description)]
    datatable(display, rownames = FALSE,
      options = list(pageLength = 25, dom = "ftip")
    ) |> formatStyle("Staff", fontWeight = "bold")
  })

  # ---- Tier 1 ----
  tier1_result <- eventReactive(input$run_tier1, {
    names_raw <- trimws(unlist(strsplit(input$tier1_names, "\n")))
    names_raw <- names_raw[nchar(names_raw) > 0L]
    if (length(names_raw) == 0L) return(list(ok = FALSE, msg = "No names entered.", dt = data.table()))
    dt <- all_data()
    if (is.null(dt) || nrow(dt) == 0L) return(list(ok = FALSE, msg = "No data loaded.", dt = data.table()))

    tier1_items <- dt[staff_name %chin% names_raw]
    missing <- setdiff(names_raw, unique(tier1_items$staff_name))
    gaps <- if (nrow(tier1_items) > 0L) {
      tier1_items[is.na(completed), .(Gaps = .N), by = .(staff_name, track)]
    } else {
      data.table(staff_name = character(0), track = character(0), Gaps = integer(0))
    }

    # Add complete entries too
    complete <- if (nrow(tier1_items) > 0L) {
      tier1_items[, .(Gaps = sum(is.na(completed))), by = .(staff_name, track)]
    } else { data.table() }

    if (length(missing) > 0L) {
      complete <- rbindlist(list(complete,
        data.table(staff_name = missing, track = NA_character_, Gaps = NA_integer_)
      ), fill = TRUE)
    }

    all_ok <- nrow(complete) > 0L && all(!is.na(complete$Gaps) & complete$Gaps == 0L)
    msg <- if (all_ok) "All Tier 1 custodians have complete training on file."
           else paste0(length(unique(complete[is.na(Gaps) | Gaps > 0L, staff_name])), " custodian(s) with issues.")
    list(ok = all_ok, msg = msg, dt = complete)
  })

  output$tier1_alert <- renderUI({
    res <- tier1_result()
    cls <- if (res$ok) "dash-alert dash-alert-ok" else "dash-alert dash-alert-warn"
    icon_txt <- if (res$ok) "\u2714" else "\u26A0"
    tags$div(class = cls, paste(icon_txt, res$msg))
  })

  output$tier1_html <- renderUI({
    res <- tier1_result()
    if (nrow(res$dt) == 0L) return(NULL)

    header <- tags$tr(tags$th("Custodian"), tags$th("Track"), tags$th("Gaps"), tags$th("Status"))
    rows <- lapply(seq_len(nrow(res$dt)), function(i) {
      r <- res$dt[i]
      trk <- if (is.na(r$track)) "NO FILES FOUND" else track_label(r$track)
      badge <- if (is.na(r$Gaps)) tags$span(class = "badge-bad", "Missing")
               else if (r$Gaps == 0L) tags$span(class = "badge-good", "Ready")
               else tags$span(class = "badge-warn", paste(r$Gaps, "gaps"))
      gap_txt <- if (is.na(r$Gaps)) "" else r$Gaps
      tags$tr(
        tags$td(class = "fw-bold", r$staff_name),
        tags$td(style = if (is.na(r$track)) "color:var(--bad)" else "", trk),
        tags$td(gap_txt), tags$td(badge)
      )
    })
    tags$div(style = "overflow-x:auto",
      tags$table(class = "sum-table", tags$thead(header), tags$tbody(rows)))
  })

  # ---- Staff Detail ----
  update_detail_tracks <- function() {
    dt <- all_data()
    staff <- input$detail_staff
    if (is.null(staff) || nchar(staff) == 0L || is.null(dt) || nrow(dt) == 0L) {
      updateSelectInput(session, "detail_track", choices = character(0))
      return()
    }
    tracks <- sort(unique(dt[staff_name == staff, track]))
    if (length(tracks) == 0L) {
      updateSelectInput(session, "detail_track", choices = character(0))
      return()
    }
    updateSelectInput(session, "detail_track",
      choices = setNames(tracks, track_label(tracks)), selected = tracks[1])
  }
  observeEvent(input$detail_staff, { update_detail_tracks() })

  output$detail_header <- renderText({
    req(input$detail_staff, input$detail_track)
    paste0(input$detail_staff, " \u2014 ", track_label(input$detail_track))
  })

  output$detail_html <- renderUI({
    req(input$detail_staff, input$detail_track)
    dt <- all_data()
    if (is.null(dt) || nrow(dt) == 0L) return(tags$p(style = "padding:16px; color:#999", "No data."))

    items <- dt[staff_name == input$detail_staff & track == input$detail_track]
    if (nrow(items) == 0L) return(tags$p(style = "padding:16px; color:#999", "No data."))

    header <- tags$tr(
      tags$th("ID"), tags$th("Section"), tags$th("Description"),
      tags$th("Completed"), tags$th("Verified by"), tags$th("Status")
    )

    rows <- lapply(seq_len(nrow(items)), function(i) {
      r <- items[i]
      if (!is.na(r$verified_by)) {
        status <- tags$span(tags$span(class = "dot dot-good"), "Verified")
      } else if (!is.na(r$completed)) {
        status <- tags$span(tags$span(class = "dot dot-warn"), "Not verified")
      } else {
        status <- tags$span(tags$span(class = "dot dot-bad"), "Outstanding")
      }
      tags$tr(
        tags$td(r$id), tags$td(r$section), tags$td(r$description),
        tags$td(if (is.na(r$completed)) "" else r$completed),
        tags$td(if (is.na(r$verified_by)) "" else r$verified_by),
        tags$td(status)
      )
    })

    tags$div(style = "overflow-x:auto",
      tags$table(class = "sum-table", tags$thead(header), tags$tbody(rows)))
  })

  # ---- Onboard ----
  onboard_msg <- reactiveVal(NULL)

  observeEvent(input$onboard_go, {
    name <- trimws(input$onboard_name)
    role <- trimws(input$onboard_role)
    year <- input$onboard_year
    tracks_selected <- input$onboard_tracks
    if (nchar(name) == 0L) { onboard_msg(tags$div(class = "dash-alert dash-alert-bad", "Enter a name.")); return() }
    if (is.null(tracks_selected) || length(tracks_selected) == 0L) {
      onboard_msg(tags$div(class = "dash-alert dash-alert-bad", "Select at least one track.")); return()
    }
    track_keys <- names(TRACK_LABELS)[match(tracks_selected, TRACK_LABELS)]
    track_keys <- track_keys[!is.na(track_keys)]
    if (!dir.exists(TEMPLATE_DIR)) {
      onboard_msg(tags$div(class = "dash-alert dash-alert-bad", paste0("Templates not found: ", TEMPLATE_DIR))); return()
    }
    templates <- list.files(TEMPLATE_DIR, pattern = "\\.yaml$", full.names = TRUE)
    selected  <- templates[tools::file_path_sans_ext(basename(templates)) %in% track_keys]
    if (length(selected) == 0L) {
      onboard_msg(tags$div(class = "dash-alert dash-alert-bad", "No matching templates.")); return()
    }

    name_slug <- gsub("_+", "_", gsub("[^a-z0-9]", "_", tolower(name)))
    name_slug <- gsub("^_|_$", "", name_slug)
    if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)

    created <- character(0)
    for (tmpl in selected) {
      y <- yaml::read_yaml(tmpl)
      y$staff_name <- name; y$role <- role; y$year <- as.integer(year)
      fn <- sprintf("%s_%s_%d.yaml", name_slug, y$track, year)
      yaml::write_yaml(y, file.path(DATA_DIR, fn))
      created <- c(created, fn)
    }

    file_list <- paste0("<li><code>", created, "</code></li>", collapse = "")
    onboard_msg(tags$div(class = "onboard-ok",
      tags$strong(paste0("Created ", length(created), " file(s) for ", name, ":")),
      tags$ul(HTML(file_list)),
      tags$p(style = "margin-top:8px; margin-bottom:0",
        "Click ", tags$strong("Reload files"), " on the Overview tab to see them.")
    ))
  })

  output$onboard_result <- renderUI({ onboard_msg() })
}

shinyApp(ui, server)
