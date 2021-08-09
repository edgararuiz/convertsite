#' @importFrom stringr str_detect str_replace str_replace_all
#' @importFrom purrr map map_chr walk map_dfr transpose keep
#' @importFrom blogdown read_toml
#' @importFrom yaml write_yaml read_yaml
#' @importFrom magrittr `%>%`
#' @importFrom jsonlite read_json
#' @import here
#' @import fs

#' @export
blogdown_setup_file <- function(folder = here::here(),
                                blogdown_folder = ".blogdown",
                                setup_override = list()
                                ) {
  
  toml_file <- path(blogdown_folder, "config.toml")

  if (file_exists(toml_file)) {
    tf <- read_toml(toml_file)

    qy <- setup_override
    if (is.null(qy$project$type)) qy$project$type <- "site"
    if (is.null(qy$project$`output-dir`)) qy$project$`output-dir` <- "_site"
    if (is.null(qy$site$title)) qy$site$title <- tf$title

    if (is.null(qy$format$html$toc)) qy$format$html$toc <- TRUE
    if (is.null(qy$format$html$`code-copy`)) qy$format$html$`code-copy` <- TRUE

    if (is.null(qy$format$html$theme$light)) qy$format$html$theme$light <- "cosmo"
    if (is.null(qy$format$html$theme$dark)) qy$format$html$theme$dark <- "dakly"

    if (is.null(qy$site$navbar$search)) qy$site$navbar$search <- TRUE
    if (is.null(qy$site$navbar$background)) qy$site$navbar$background <- "light"
    if (is.null(qy$site$navbar$type)) qy$site$navbar$type <- "light"


    if (!is.null(tf$googleAnalytics) & is.null(qy$site$`google-analytics`)) qy$site$`google-analytics` <- tf$googleAnalytics

    if (!is.null(tf$menu)) {
      tbl_tf <- tf$menu %>%
        transpose() %>%
        map_dfr(as.data.frame)

      mp <- is.na(tbl_tf$main.parent)
      tbl_tf$group[mp] <- tbl_tf$main.name[mp]
      tbl_tf$group[!mp] <- tbl_tf$main.parent[!mp]

      tbl_tf$id <- str_replace_all(tolower(tbl_tf$main.name), " ", "-")

      content_folder <- path(blogdown_folder, "content")

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

      tbl_tf$acutal <- actual_doc

      pg <- unique(tbl_tf$group)

      sbc <- pg %>%
        map(~ {
          tbl_group <- tbl_tf[tbl_tf$group == .x, ]
          sh <- tbl_group[is.na(tbl_group$main.parent), ]
          its <- tbl_group[!is.na(tbl_group$main.parent), ]
          lits <- map(transpose(its), ~ {
            nit <- list()
            nit$text <- .x$main.name
            nit$href <- .x$acutal
            nit
          })
          sid <- list(
            section = sh$main.name,
            contents = lits
          )
        })

      qy$site$sidebar$contents <- sbc
    }

    quarto_file <- path(folder, "_quarto.yml")

    write_yaml(qy, quarto_file)

    ql <- readLines(quarto_file)
    nql <- str_replace(ql, ": yes", ": true")
    writeLines(nql, quarto_file)
  }
}

#' @export
makefile_setup_file <- function(folder = here::here(),
                                makefile_folder = ".makefile",
                                setup_override = list()
                                ) {
    qy <- setup_override
    if (is.null(qy$project$type)) qy$project$type <- "site"
    output_dir <- ifelse(is.null(qy$project$`output-dir`), "_site", qy$project$`output-dir`) 
    qy$project$`output-dir` <- output_dir
    if (is.null(qy$site$title)) qy$site$title <- "Default"
    
    if (is.null(qy$format$html$toc)) qy$format$html$toc <- TRUE
    if (is.null(qy$format$html$`code-copy`)) qy$format$html$`code-copy` <- TRUE
    
    if (is.null(qy$format$html$theme$light)) qy$format$html$theme$light <- "cosmo"
    if (is.null(qy$format$html$theme$dark)) qy$format$html$theme$dark <- "dakly"
    
    if (is.null(qy$site$navbar$search)) qy$site$navbar$search <- TRUE
    if (is.null(qy$site$navbar$background)) qy$site$navbar$background <- "light"
    if (is.null(qy$site$navbar$type)) qy$site$navbar$type <- "light"
    
    qy$site$sidebar$contents <- folder_side_navigation(folder = output_dir)
    
    quarto_file <- path(folder, "_quarto.yml")
    
    write_yaml(qy, quarto_file)
    
    ql <- readLines(quarto_file)
    nql <- str_replace(ql, ": yes", ": true")
    writeLines(nql, quarto_file)
  
}


#' @export
makefile_to_quarto <- function(folder = here::here(),
                               makefile_folder = ".makefile",
                               setup_override = list()) {
  convert_to_quarto(
    folder = folder,
    archive_folder = makefile_folder,
    sub_folders = "docs"
  )
  
  makefile_setup_file(
    folder = folder,
    makefile_folder = makefile_folder,
    setup_override = setup_override
  )
}


#' @export
blogdown_to_quarto <- function(folder = here::here(),
                               blogdown_folder = ".blogdown",
                               setup_override = list()) {
  convert_to_quarto(
    folder = folder,
    archive_folder = blogdown_folder,
    sub_folders = c("content", "static")
  )
  
  alf <- dir_ls(folder, recurse = TRUE)
  af <- alf[is_file(alf)]
  ixf <- substr(path_file(af), 1, 6) == "_index" 
  has_index <- af[ixf]
  new_name <- path(
    path_dir(has_index), 
    substr(path_file(has_index), 2, nchar(path_file(has_index)))
    )
  file_move(has_index, new_name)
  
  
  blogdown_setup_file(
    folder = folder,
    blogdown_folder = blogdown_folder,
    setup_override = setup_override
  )
}


#' @export
convert_to_quarto <- function(folder = here::here(),
                              archive_folder = ".blogdown",
                              sub_folders = c("content", "static")) {
  if (!file_exists(archive_folder)) {
    full_file_copy(
      folder = folder,
      new_folder = archive_folder
    )
  }

  if(dir_exists(folder)) {
    cfs <- dir_ls(folder)
    file_delete(cfs)    
  }

  walk(
    sub_folders,
    ~ full_file_copy(
        folder = path(archive_folder, .x),
        new_folder = folder,
        exclude_exts = "html"
    )
  )
  
  pf <- dir_ls(archive_folder, glob = "*.Rproj")
  file_move(pf, path(folder, path_file(pf)))
  
}

#' @export
full_file_copy <- function(folder, new_folder, exclude_exts = NULL) {
  if (!dir_exists(new_folder)) dir_create(new_folder)
  fls <- sanitized_file_list(
    folder = folder,
    exclude_exts = exclude_exts
  )
  fd <- folder_list(fls)
  dir_create(path(new_folder, fd))
  walk(fls, ~ file_copy(path(folder, .x), path(new_folder, .x), overwrite = TRUE))
}

folder_list <- function(file_list) {
  dj <- as_fs_path(unique(path_dir(file_list)))
  dj[dj != "."]
}

#' @export
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


full_file_list <- function(folder, exclude_exts = NULL) {
  fc <- dir_ls(folder, recurse = TRUE)
  if (!is.null(exclude_exts)) {
    for (i in 1:length(exclude_exts)) {
      fc <- fc[path_ext(fc) != exclude_exts[i]]
    }
  }
  fc
}

sanitized_file_list <- function(folder, exclude_exts = NULL) {
  fc <- full_file_list(
    folder = folder,
    exclude_exts = exclude_exts
  )
  pl <- length(path_split(path_common(fc))[[1]])
  fcs <- path_split(fc)
  rf <- map(fcs, ~ .x[(pl + 1):length(.x)])
  rj <- path_join(rf)
  rj[is_file(fc)]
}


