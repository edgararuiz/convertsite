#' @export
makefile_to_quarto <- function(project_folder = here::here(),
                               quarto_folder = new_quarto_folder(),
                               setup_override = list()
                               ) {
  convert_to_quarto(
    project_folder = project_folder,
    quarto_folder = quarto_folder,
    sub_folders = "docs"
  )

  ms <- makefile_setup_file(
    project_folder = project_folder,
    setup_override = setup_override
  )

  save_quarto_yaml(ms, path(quarto_folder, "_quarto.yml"))
}

#' @export
new_quarto_folder <- function(home_folder = path_dir(here::here()),
                              project_name = path_file(here::here()),
                              new_name = paste0(project_name, "_quarto")
                              ) {
  path(home_folder, new_name)
}

#' @export
blogdown_to_quarto <- function(project_folder = here::here(),
                               quarto_folder = new_quarto_folder(),
                               setup_override = list()
                               ) {
  convert_to_quarto(
    project_folder = project_folder,
    quarto_folder = quarto_folder,
    sub_folders = c("content", "static")
  )

  alf <- dir_ls(quarto_folder, recurse = TRUE)
  af <- alf[is_file(alf)]
  ixf <- substr(path_file(af), 1, 6) == "_index"
  has_index <- af[ixf]

  new_name <- path(
    path_dir(has_index),
    substr(path_file(has_index), 2, nchar(path_file(has_index)))
  )

  file_move(has_index, new_name)

  bs <- blogdown_setup_file(
    project_folder = project_folder,
    setup_override = setup_override
  )

  save_quarto_yaml(bs, path(quarto_folder, "_quarto.yml"))

  rstudioapi::openProject(quarto_folder)
}

#' @export
convert_to_quarto <- function(project_folder = here::here(),
                              quarto_folder = new_quarto_folder(),
                              sub_folders = c("content", "static")
                              ) {

  if(!dir_exists(quarto_folder)) dir_create(quarto_folder)

  walk(
    sub_folders,
    ~ full_file_copy(
        folder = path(project_folder, .x),
        new_folder = quarto_folder,
        exclude_exts = "html"
    )
  )

  pf <- dir_ls(project_folder, glob = "*.Rproj")

  if(length(pf) != 0) {
    file_copy(pf, path(quarto_folder, path_file(pf)))
  }

  full_file_copy(
    system.file("theme", package = "convertsite"),
    path(quarto_folder, "theme")
  )
}
