# ============================================================
# training_audit.R
# Reads all YAML training sign-off files and produces an audit summary.
# Uses data.table and yaml only. No tidyverse.
#
# Usage:
#   source("training_audit.R")
#   report <- training_audit("docs/training")
#   report$summary       # one row per person per track
#   report$incomplete    # all incomplete items
#   report$tier1_check   # Tier 1 custodians with gaps
#
# Reference: Staff Training Framework v1.0, Section 7
# ============================================================

library(data.table)
library(yaml)
library(cli)

#' Read a single YAML sign-off file and return a data.table of items
#'
#' @param path Character. Path to a .yaml sign-off file.
#' @return A data.table with columns: staff_name, role, track, year,
#'   id, description, section, completed, verified_by, notes, file.
read_signoff <- function(path) {

  y <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse {.file {path}}: {e$message}")
      return(NULL)
    }
  )

  if (is.null(y)) return(NULL)

  # Validate required top-level fields

  required <- c("staff_name", "role", "track", "year", "items")
  missing  <- setdiff(required, names(y))
  if (length(missing) > 0L) {
    cli::cli_alert_warning(
      "{.file {basename(path)}}: missing top-level fields: {.val {missing}}"
    )
    return(NULL)
  }

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
    staff_name = y$staff_name,
    role       = y$role,
    track      = y$track,
    year       = y$year,
    file       = basename(path)
  )]

  # Coerce completed to Date where present
  items[!is.na(completed), completed_date := as.Date(completed)]

  items[]
}


#' Read all YAML sign-off files from a directory
#'
#' @param dir Character. Path to docs/training/ or equivalent.
#' @param pattern Character. File pattern. Default "*.yaml".
#' @return A data.table of all items across all files.
read_all_signoffs <- function(dir, pattern = "\\.yaml$") {

  files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = FALSE)

  if (length(files) == 0L) {
    cli::cli_alert_warning("No .yaml files found in {.path {dir}}")
    return(data.table())
  }

  cli::cli_alert_info("Reading {length(files)} sign-off file{?s} from {.path {dir}}")
  rbindlist(lapply(files, read_signoff), fill = TRUE)
}


#' Produce the full audit report
#'
#' @param dir Character. Path to docs/training/.
#' @param tier1_custodians Character vector. Names of current Tier 1
#'   custodians (as they appear in staff_name fields). If NULL, the
#'   Tier 1 check is skipped.
#' @param required_tracks Named list. Each name is a role keyword, each
#'   value is a character vector of track names required for that role.
#'   Defaults to the framework's role-based requirements.
#' @return A list with elements: all_items, summary, incomplete, tier1_check.
training_audit <- function(
    dir,
    tier1_custodians = NULL,
    required_tracks  = NULL
) {

  all_items <- read_all_signoffs(dir)

  if (nrow(all_items) == 0L) {
    cli::cli_alert_danger("No valid sign-off data found.")
    return(list(
      all_items   = all_items,
      summary     = data.table(),
      incomplete  = data.table(),
      tier1_check = data.table()
    ))
  }

  # --- Summary: one row per person per track ---
  summary <- all_items[, .(
    total_items = .N,
    completed   = sum(!is.na(completed)),
    verified    = sum(!is.na(verified_by)),
    incomplete  = sum(is.na(completed)),
    pct_done    = round(100 * sum(!is.na(completed)) / .N, 1)
  ), by = .(staff_name, track, year)]

  setorder(summary, staff_name, track)

  # --- Incomplete items ---
  incomplete <- all_items[is.na(completed),
    .(staff_name, track, id, description, section)
  ]
  setorder(incomplete, staff_name, track, id)

  # --- Tier 1 custodian check ---
  tier1_check <- data.table()
  if (!is.null(tier1_custodians) && length(tier1_custodians) > 0L) {

    # Default required tracks for Tier 1 custodians
    if (is.null(required_tracks)) {
      # All Tier 1 custodians need core + cross_cutting at minimum.
      # Specialist tracks depend on the asset type, but we flag any
      # incomplete items across whatever tracks they have on file.
      required_tracks <- "core"
    }

    tier1_items <- all_items[staff_name %chin% tier1_custodians]

    if (nrow(tier1_items) > 0L) {
      tier1_check <- tier1_items[is.na(completed), .(
        gaps = .N
      ), by = .(staff_name, track)]

      if (nrow(tier1_check) > 0L) {
        cli::cli_alert_warning(
          "{nrow(tier1_check)} Tier 1 custodian/track combination{?s} with incomplete training"
        )
      } else {
        cli::cli_alert_success("All Tier 1 custodians have complete training records on file.")
      }
    }

    # Check for custodians with no files at all
    missing_entirely <- setdiff(
      tier1_custodians,
      unique(tier1_items$staff_name)
    )
    if (length(missing_entirely) > 0L) {
      cli::cli_alert_danger(
        "Tier 1 custodian{?s} with NO training files: {.val {missing_entirely}}"
      )
      tier1_check <- rbindlist(list(
        tier1_check,
        data.table(staff_name = missing_entirely, track = "NO FILES FOUND", gaps = NA_integer_)
      ), fill = TRUE)
    }
  }

  # --- Console summary ---
  cli::cli_h2("Training Audit Summary")
  cli::cli_alert_info("{uniqueN(all_items$staff_name)} staff member{?s} across {uniqueN(all_items$track)} track{?s}")
  cli::cli_alert_info("{sum(summary$completed)} / {sum(summary$total_items)} items completed ({round(100 * sum(summary$completed) / sum(summary$total_items), 1)}%)")

  if (nrow(incomplete) > 0L) {
    cli::cli_alert_warning("{nrow(incomplete)} incomplete item{?s} across all staff")
  } else {
    cli::cli_alert_success("All items complete.")
  }

  list(
    all_items   = all_items,
    summary     = summary,
    incomplete  = incomplete,
    tier1_check = tier1_check
  )
}


#' Create a new sign-off file for a staff member from a template
#'
#' @param template_path Path to the template YAML file.
#' @param staff_name Staff member's full name.
#' @param role Staff member's role.
#' @param year Integer. Training year.
#' @param output_dir Directory to write the new file to.
#' @return The path to the created file (invisibly).
create_signoff <- function(template_path, staff_name, role, year, output_dir) {

  y <- yaml::read_yaml(template_path)
  y$staff_name <- staff_name
  y$role       <- role
  y$year       <- as.integer(year)

  # Build filename: lowercase, underscores, no spaces
  name_slug <- gsub("[^a-z0-9]", "_", tolower(staff_name))
  name_slug <- gsub("_+", "_", name_slug)
  name_slug <- gsub("^_|_$", "", name_slug)

  filename <- sprintf("%s_%s_%d.yaml", name_slug, y$track, year)
  outpath  <- file.path(output_dir, filename)

  yaml::write_yaml(y, outpath)
  cli::cli_alert_success("Created {.file {outpath}}")
  invisible(outpath)
}


#' Create all required sign-off files for a new starter
#'
#' @param template_dir Directory containing the template YAML files.
#' @param staff_name Staff member's full name.
#' @param role Staff member's role.
#' @param year Integer. Training year.
#' @param tracks Character vector of track names to create. Use "all"
#'   for every template in the directory.
#' @param output_dir Directory to write new files to.
#' @return Character vector of created file paths (invisibly).
onboard_new_starter <- function(
    template_dir,
    staff_name,
    role,
    year,
    tracks = "all",
    output_dir
) {

  templates <- list.files(template_dir, pattern = "\\.yaml$", full.names = TRUE)

  if (identical(tracks, "all")) {
    selected <- templates
  } else {
    selected <- templates[
      tools::file_path_sans_ext(basename(templates)) %in% tracks
    ]
  }

  if (length(selected) == 0L) {
    cli::cli_alert_danger("No matching templates found in {.path {template_dir}}")
    return(invisible(character(0)))
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  cli::cli_h2("Onboarding {staff_name}")
  paths <- vapply(selected, function(tmpl) {
    create_signoff(tmpl, staff_name, role, year, output_dir)
  }, character(1))

  cli::cli_alert_success("Created {length(paths)} sign-off file{?s}")
  invisible(unname(paths))
}
