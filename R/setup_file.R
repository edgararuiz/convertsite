#' @export
blogdown_setup_file <- function(project_folder = here::here(),
                                setup_override = list()
                                ) {


  toml_file <- path(project_folder, "config.toml")

  if (file_exists(toml_file)) {
    tf <- read_toml(toml_file)
  } else {
    tf <- list()
  }

  qy <- setup_file(
    setup_override = setup_override,
    title = tf$title
  )

  if (!is.null(tf$googleAnalytics) & is.null(qy$site$`google-analytics`)) qy$site$`google-analytics` <- tf$googleAnalytics

  sbc <- toml_side_navigation(tf, project_folder = project_folder)
  if(is.null(sbc)) sbc <- folder_side_navigation()
  qy$site$sidebar$contents <- sbc

  qy
}

#' @export
makefile_setup_file <- function(project_folder = here::here(),
                                setup_override = list()
                                ) {
  raw_mkdocs <- readLines(path(project_folder, "mkdocs.yml"))

  filter_docs <- raw_mkdocs[raw_mkdocs != ""]

  split_docs <- strsplit(filter_docs, ":")

  line_name <- keep(split_docs, ~.x[1] == "site_name1")

  if(length(line_name > 1)) {
    line_name <- line_name[[1]]
    site_name <- paste0(line_name[2:length(line_name)], collapse = "")
  } else {
    site_name <- "Default"
  }

  qy <- setup_file(
    setup_override = setup_override,
    title = site_name
  )

  qy$site$sidebar$contents <- folder_side_navigation(folder = qy$project$`output-dir`)

  qy
}

#' @export
save_quarto_yaml <- function(x, location) {
  write_yaml(x, location)
  ql <- readLines(location)
  nql <- str_replace(ql, ": yes", ": true")
  writeLines(nql, location)
}

setup_file <- function(setup_override = list(), title = NULL) {
  if(is.null(title)) title <- "Default"
  qy <- setup_override
  if (is.null(qy$project$type)) qy$project$type <- "site"
  if (is.null(qy$project$`output-dir`)) qy$project$`output-dir` <- "_site"
  if (is.null(qy$site$title)) qy$site$title <- title

  if (is.null(qy$format$html$toc)) qy$format$html$toc <- TRUE
  if (is.null(qy$format$html$`code-copy`)) qy$format$html$`code-copy` <- TRUE
  if (is.null(qy$format$html$`code-overflow`)) qy$format$html$`code-overflow` <- "wrap"
  if (is.null(qy$format$html$css)) qy$format$html$css <- "styles.css"

  if (is.null(qy$format$html$theme$light)) qy$format$html$theme$light <- `[cosmo, theme.scss]`
  if (is.null(qy$format$html$theme$dark)) qy$format$html$theme$dark <- `[cosmo, theme-dark.scss]`

  if (is.null(qy$site$navbar$search)) qy$site$navbar$search <- TRUE
  if (is.null(qy$site$navbar$background)) qy$site$navbar$background <- "light"
  if (is.null(qy$site$navbar$type)) qy$site$navbar$type <- "light"

  qy
}

toml_side_navigation <- function(toml_list, project_folder = ".blogdown") {
  if (!is.null(toml_list$menu)) {
    tbl_tf <- toml_list$menu %>%
      transpose() %>%
      map_dfr(as.data.frame)

    mp <- is.na(tbl_tf$main.parent)
    tbl_tf$group[mp] <- tbl_tf$main.name[mp]
    tbl_tf$group[!mp] <- tbl_tf$main.parent[!mp]

    tbl_tf$id <- str_replace_all(tolower(tbl_tf$main.name), " ", "-")

    content_folder <- path(project_folder, "content")

    actual_doc <- map_chr(
      tbl_tf$main.url, ~ {
        if (!is.na(.x)) {
          fls <- dir_ls(
            path(content_folder, path_dir(.x)),
            type = "file"
          )
          fls <- fls[path_ext(fls) != "html"]
          l_fls <- tolower(path_file(fls))
          l_x <- tolower(path_file(.x))
          fls <- fls[str_detect(l_fls, l_x)]
          if (length(fls) > 0) {
            fls <- fls[[1]]
            substr(fls, nchar(content_folder) + 2, nchar(fls))
          } else {
            NA
          }
        } else {
          NA
        }
      }
    )
    tbl_tf$actual <- actual_doc
    pg <- unique(tbl_tf$group)
    sbc <- pg %>%
      map(~ {
        tbl_group <- tbl_tf[tbl_tf$group == .x, ]
        sh <- tbl_group[is.na(tbl_group$main.parent), ]
        its <- tbl_group[!is.na(tbl_group$main.parent), ]
        lits <- map(transpose(its), ~ {
          nit <- list()
          nit$text <- .x$main.name
          nit$href <- .x$actual
          nit
        })
        sid <- list(
          section = sh$main.name,
          contents = lits
        )
      })
  } else {
    NULL
  }
}

folder_side_navigation <- function(folder = "_site") {
  folder <- "_site"
  index_folder <- ".quarto/index"
  json_files <- dir_ls(index_folder, recurse = TRUE, type = "file", glob = "*.json")

  file_list1 <- map(
    json_files,
    ~{
      rj <- read_json(.x)
      file_name <- path_ext_remove(path_ext_remove(path_file(.x)))
      file_ext <- path_ext(path_ext_remove(path_file(.x)))
      fn1 <- path(substr(.x, nchar(index_folder) + 2, nchar(.x)))
      fn2 <- path_dir(fn1)
      fp <- path(fn2, file_name, ext = file_ext)
      ps <- path_split(fp)[[1]]
      list(
        text = rj$title,
        href = fp,
        level1 = ifelse(ps[[1]] == ".", "", ps[[1]]),
        level2 = ifelse(length(ps) > 2, ps[[2]], "")
      )
    }
  )
  file_list <- unname(file_list1)
  level1 <- sort(unique(map_chr(file_list, ~.x$level1)))
  map(
    level1,
    ~{
      lv1 <- .x
      cl <- keep(file_list, ~.x$level1 == lv1)
      level2 <- sort(unique(map_chr(cl, ~.x$level2)))
      level2 <- level2[level2 != ""]
      hd <- prepare_level(cl, lv1, "")
      cnt <- map(level2, ~ prepare_level(cl, lv1, .x))
      if(length(cnt) > 0) hd$contents <- cnt
      hd
    })
}

prepare_level <- function(file_list, l1, l2) {
  cl <- keep(file_list, ~.x$level1 == l1)
  il <- keep(cl, ~ str_detect(.x$href, "index.") && .x$level2 == l2)
  rl <- keep(cl, ~ !str_detect(.x$href, "index.") && .x$level2 == l2)
  newl <- list()
  if(length(il) == 1) {
    newl$section <- il[[1]]$text
    newl$href <- il[[1]]$href
  } else {
    newl$section <- l2
  }
  al <- map(rl, ~.x[c("text", "href")])
  if(length(al) > 0) newl$contents <- al
  newl
}

