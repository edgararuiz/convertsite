#' @importFrom purrr map map_chr walk map_dfr transpose
#' @importFrom magrittr `%>%`
#' @importFrom blogdown read_toml
#' @import stringr
#' @import fs
#' @import here
#' @importFrom yaml write_yaml



#' @export
convert_setup_file <- function(folder = here::here(),
                               blogdown_folder = ".blogdown",
                               setup_override = list()
                               ) {
  toml_file <- path(blogdown_folder, "config.toml")
  
  if(file_exists(toml_file)) {
    tf <- read_toml(toml_file)
    
    qy <- setup_override
    if(is.null(qy$project$type)) qy$project$type <- "site"
    if(is.null(qy$project$`output-dir`)) qy$project$`output-dir` <- "_site"
    if(is.null(qy$site$title)) qy$site$title <- tf$title
    
    if(is.null(qy$format$html$toc)) qy$format$html$toc <- TRUE
    if(is.null(qy$format$html$`code-copy`)) qy$format$html$`code-copy` <- TRUE
    
    if(is.null(qy$format$html$theme$light)) qy$format$html$theme$light <- "cosmo"
    if(is.null(qy$format$html$theme$dark)) qy$format$html$theme$dark <- "cosmo"
    
    if(is.null(qy$site$navbar$search)) qy$site$navbar$search <- TRUE
    if(is.null(qy$site$navbar$background)) qy$site$navbar$background <- "light"
    if(is.null(qy$site$navbar$type)) qy$site$navbar$type <- "light"
    
    
    if(!is.na(tf$googleAnalytics) & is.null(qy$site$`google-analytics`)) qy$site$`google-analytics` <- tf$googleAnalytics
  
    if(!is.na(tf$menu)) {
      
      tbl_tf <- tf$menu %>%
        transpose() %>%
        map_dfr(as.data.frame)
      
      mp <- is.na(tbl_tf$main.parent)
      tbl_tf$group[mp] <- tbl_tf$main.name[mp]
      tbl_tf$group[!mp] <- tbl_tf$main.parent[!mp]
      
      tbl_tf$id <- str_replace_all(tolower(tbl_tf$main.name), " ", "-")
      
      content_folder <- path(blogdown_folder, "content")
      
      actual_doc <- map_chr(
        tbl_tf$main.url, ~{
          if(!is.na(.x)) {
            fls <- dir_ls(
              path(content_folder, path_dir(.x)),
              type = "file"
            )
            fls <- fls[path_ext(fls) != "html"]
            l_fls <- tolower(path_file(fls))
            l_x <- tolower(path_file(.x))
            fls <- fls[str_detect(l_fls, l_x)]
            if(length(fls) > 0) {
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
          sh <- tbl_group[is.na(tbl_group$main.parent),]
          its <- tbl_group[!is.na(tbl_group$main.parent),]
          lits <- map(transpose(its), ~{
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
convert_to_quarto <- function(folder = here::here(),
                              blogdown_folder = ".blogdown", 
                              setup_override = list()
                              ) {
  if(!file_exists(blogdown_folder)) {
    full_file_copy(
      folder = folder,
      new_folder = blogdown_folder,
      exclude_exts = "Rproj"
    )
    
    fd <- dir_ls(folder)
    fld <- fd[is_dir(fd)]
    dir_delete(fld)
    
    file_delete(fd[path_ext(fd) == "toml"])
    
    walk(
      c("content", "static"),
      ~ full_file_copy(
        folder = path(blogdown_folder, .x),
        new_folder = folder,
        exclude_exts = "html"
      )
    )
    
    if(file_exists("_index.md")) file_move("_index.md", "index.md")
    if(file_exists("_index.Rmd")) file_move("_index.Rmd", "index.Rmd")
  }
  convert_setup_file(
    folder = folder,
    blogdown_folder = blogdown_folder,
    setup_override = setup_override
  )
}

#' @export 
full_file_copy <- function(folder, new_folder, exclude_exts = NULL) {
  if(!dir_exists(new_folder)) dir_create(new_folder)
  fls <- sanitized_file_list(
    folder = folder, 
    exclude_exts = exclude_exts
    )
  fd <- folder_list(fls)
  dir_create(path(new_folder, fd))
  walk(fls, ~ file_copy(path(folder, .x), path(new_folder, .x), overwrite = TRUE))
}

#' @export 
folder_list <- function(file_list) {
  dj <- as_fs_path(unique(path_dir(file_list))) 
  dj[dj != "."]
}

#' @export
full_file_list <- function(folder, exclude_exts = NULL) {
  fc <- dir_ls(folder, recurse = TRUE)
  if(!is.null(exclude_exts)) {
    for(i in 1:length(exclude_exts)) {
      fc <- fc[path_ext(fc) != exclude_exts[i]]
    }
  }
  fc
}

#' @export
sanitized_file_list <- function(folder, exclude_exts = NULL) {
  fc <- full_file_list(
    folder = folder,
    exclude_exts = exclude_exts
  )
  pl <- length(path_split(path_common(fc))[[1]])
  fcs <- path_split(fc)
  rf <- map(fcs, ~.x[(pl+1):length(.x)])
  rj <- path_join(rf)
  rj[is_file(fc)]
}