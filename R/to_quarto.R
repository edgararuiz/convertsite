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
  full_file_copy(
    system.file("inst/theme", "convertsite"),
    path(folder, "theme")
  )
}
