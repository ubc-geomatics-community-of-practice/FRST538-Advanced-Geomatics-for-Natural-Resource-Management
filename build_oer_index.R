# scripts/build_oer_index.R
suppressPackageStartupMessages({
  library(yaml)
  library(jsonlite)
  library(stringi)
  library(tools)
})

# Bloom’s taxonomy mapping -------------------------------------------
blooms_map <- list(
  define="remember", list="remember", identify="remember", recall="remember", describe="remember",
  explain="understand", summarize="understand", classify="understand", discuss="understand",
  interpret="understand", recognize="understand",
  apply="apply", use="apply", execute="apply", implement="apply", compute="apply", calculate="apply",
  configure="apply", perform="apply", solve="apply",
  analyze="analyze", differentiate="analyze", examine="analyze", contrast="analyze", investigate="analyze",
  evaluate="evaluate", assess="evaluate", critique="evaluate", justify="evaluate", argue="evaluate",
  create="create", design="create", construct="create", formulate="create", compose="create",
  produce="create", develop="create", propose="create"
)
blooms_order <- c("remember","understand","apply","analyze","evaluate","create")

normalize_verb <- function(w) {
  w <- tolower(w)
  w <- gsub("[^a-z-]", "", w)
  gsub("(ing|ed|es|s)$", "", w)
}

extract_blooms <- function(outcomes) {
  verbs <- character()
  levels_found <- character()
  if (length(outcomes)) {
    for (ln in outcomes) {
      tokens <- unlist(stri_extract_all_regex(ln, "\\b[A-Za-z-]+\\b"))
      if (length(tokens)) {
        for (tk in head(tokens, 6)) {
          v <- normalize_verb(tk)
          lvl <- blooms_map[[v]]
          if (!is.null(lvl)) {
            verbs <- unique(c(verbs, v))
            levels_found <- unique(c(levels_found, lvl))
          }
        }
      }
    }
  }
  list(
    blooms_verbs = verbs,
    blooms_levels = blooms_order[blooms_order %in% levels_found]
  )
}

# --------------------------------------------------------------------
rmds <- list.files(pattern = "\\.Rmd$", recursive = FALSE)
items <- list()

site_base <- Sys.getenv("OER_SITE_BASE_URL", unset = "")

for (f in rmds) {
  txt <- readLines(f, warn = FALSE)
  if (length(txt) < 3 || txt[1] != "---") next
  end <- which(txt[-1] == "---")[1] + 1
  if (is.na(end)) next
  yml <- yaml.load(paste(txt[2:end-1], collapse = "\n"))
  if (is.null(yml$title)) next
  
  html_name <- sub("\\.Rmd$", ".html", f)
  page_url <- if (nzchar(site_base)) paste0(site_base, html_name) else html_name
  blooms <- extract_blooms(yml$learning_outcomes %||% character())
  
  items[[length(items)+1]] <- list(
    id = yml$lab_id %||% file_path_sans_ext(basename(f)),
    title = yml$title,
    course_code = yml$course_code %||% NULL,
    page_url = page_url,
    summary = yml$summary %||% NULL,
    topics = yml$topics %||% list(),
    software = yml$software %||% list(),
    keywords = yml$keywords %||% list(),
    license = yml$license %||% "CC-BY-4.0",
    learning_outcomes = yml$learning_outcomes %||% list(),
    blooms_verbs = blooms$blooms_verbs,
    blooms_levels = blooms$blooms_levels
  )
}

dir.create("docs", showWarnings = FALSE)
writeLines(
  jsonlite::toJSON(list(items = items, generated_at = format(Sys.time(), "%FT%TZ", tz="UTC")),
                   auto_unbox = TRUE, pretty = TRUE),
  "docs/oer-assignments.json"
)
cat(sprintf("Wrote %d lab items -> docs/oer-assignments.json\n", length(items)))
