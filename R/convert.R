#' Converts the current site to a Quarto site
#'
#' @param project_folder Root folder location of the original site
#' @param quarto_folder Target of new Quarto site. It defaults to a new project
#' folder with the same name as the `project_folder`, but with "_quarto" suffixed
#' to it.
#' @param setup_override A list of Quarto settings to be used at conversion
#'
#' @export
makefile_to_quarto <- function(project_folder = here::here(),
                               quarto_folder = new_quarto_folder(),
                               setup_override = list()) {
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

#' @rdname makefile_to_quarto
#' @export
blogdown_to_quarto <- function(project_folder = here::here(),
                               quarto_folder = new_quarto_folder(),
                               setup_override = list()) {
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

new_quarto_folder <- function(home_folder = path_dir(here::here()),
                              project_name = path_file(here::here()),
                              new_name = paste0(project_name, "_quarto")) {
  path(home_folder, new_name)
}

convert_to_quarto <- function(project_folder = here::here(),
                              quarto_folder = new_quarto_folder(),
                              sub_folders = c("content", "static")) {
  if (!dir_exists(quarto_folder)) dir_create(quarto_folder)

  walk(
    sub_folders,
    ~ full_file_copy(
      folder = path(project_folder, .x),
      new_folder = quarto_folder,
      exclude_exts = "html"
    )
  )

  dir_copy(
    path(project_folder, ".git"),
    path(quarto_folder)
  )

  pf <- dir_ls(project_folder, glob = "*.Rproj")

  if (length(pf) != 0) {
    file_copy(pf, path(quarto_folder, path_file(pf)))
  }

  full_file_copy(
    system.file("theme", package = "convertsite"),
    quarto_folder
  )
}
