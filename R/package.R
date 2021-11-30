#' @export
package_build_documentation <- function(pkg_folder = "",
                                        project_folder = "",
                                        root_folder = here::here(),
                                        readme = TRUE,
                                        news = TRUE,
                                        articles = TRUE,
                                        reference = TRUE
                                        ) {

  if(readme | news) msg_bold_blue("- - - - - - - Top files - - - - - - - - -")

  if(readme) package_readme(pkg_folder = pkg_folder,
                            project_folder = project_folder,
                            root_folder = root_folder
                            )

  if(news) package_news(pkg_folder = pkg_folder,
                        project_folder = project_folder,
                        root_folder = root_folder
                        )

  if(articles) package_articles(pkg_folder = pkg_folder,
                                project_folder = project_folder,
                                root_folder = root_folder
                                )

  if(reference) package_reference(pkg_folder = pkg_folder,
                                  project_folder = project_folder,
                                  root_folder = root_folder
                                  )

}

#' @export
package_articles <- function(pkg_folder = "",
                             source = "vignettes",
                             target = "articles",
                             project_folder = "",
                             root_folder = here::here()) {

  msg_bold_blue("- - - - - - Article files - - - - - - - -")

  a_folder <- path(pkg_folder, source)

  if(dir_exists(a_folder)) {
    full_file_copy(
      a_folder,
      path(root_folder, project_folder, target)
    )
    msg_green("Vignette folder copied to", path(project_folder, target))
  } else {
    msg_yellow("Vignette folder not found")
  }
}

#' @export
package_readme <- function(pkg_folder = "",
                           target = "",
                           file_names = c("readme.md"),
                           project_folder = "",
                           root_folder = here::here()) {
  package_file_copy(
    pkg_folder = pkg_folder,
    target = target,
    file_names = file_names,
    override_name = "index.md",
    project_folder = project_folder,
    root_folder = root_folder
  )
}

#' @export
package_news <- function(pkg_folder = "",
                         target = "",
                         file_names = c("news.md", "news.Rmd"),
                         project_folder = "",
                         root_folder = here::here()) {
  package_file_copy(
    pkg_folder = pkg_folder,
    target = target,
    file_names = file_names,
    project_folder = project_folder,
    root_folder = root_folder
  )
}

#' @export
package_file_copy <- function(pkg_folder = "",
                              target = "project_folder",
                              file_names = c("name.md", "name.Rmd"),
                              override_name = NULL,
                              project_folder = "",
                              root_folder = here::here()) {

  file_present <- file_exists(path(pkg_folder, file_names))
  file_numbers <- setNames(file_present, 1:length(file_present))
  file_there <- file_numbers[file_numbers == TRUE]
  file_min <- min(as.integer(names(file_there)))
  file_use <- file_present[file_min]
  file_name <- names(file_use)

  dest_folder <- path(root_folder, project_folder, target)

  create_folder_if_missing(dest_folder)

  file_n <- ifelse(is.null(override_name), path_file(file_name), override_name)

  file_copy(
    file_name,
    path(dest_folder, file_n),
    overwrite = TRUE
  )
  msg_green("Copied: ", path(project_folder, target, file_n))
}

#' @export
package_reference <- function(pkg_folder = "",
                              root_folder = here::here(),
                              project_folder = "",
                              reference_folder = "reference") {
  pkg <- pkgdown::as_pkgdown(pkg_folder)

  msg_bold_blue("- - - - - - Reference files - - - - - - -")

  create_folder_if_missing(path(root_folder, project_folder, reference_folder))

  package_reference_index(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )

  package_reference_pages(
    pkg = pkg,
    project_folder = project_folder,
    root_folder = root_folder,
    reference_folder = reference_folder
  )
}

#' @export
package_repo_clone_git <- function(url = "",
                                   target_folder = tempdir(),
                                   branch = "main") {
  tf <- path(target_folder, path_file(url))
  system(paste0("git clone ", url, " -b ", branch, " ", tf))
  tf
}
