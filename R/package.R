#' @export
package_reference <- function(pkg_folder = "",
                              reference_folder = "reference",
                              rd_folder = "man"
                              ) {
  walk(
    dir_ls(path(pkg_folder, rd_folder)),
    ~{
      new_name <- path(path_ext_remove(path_file(.x)) , ext = "md")
      Rd2md::Rd2markdown(
        .x,
        here::here("reference", new_name)
      )
    }
  )

}

#' @export
package_reference_index <- function(pkg_folder = "",
                              reference_folder = "reference",
                              rd_folder = "man"
                              ) {
  pkg <- pkgdown::as_pkgdown(pkg_folder)

  pkg_ref <- pkg$meta$reference
  pkg_topics <- pkg$topics

  sections_list <- map(
    pkg_ref, ~ {
      ref <- .x
      matched_names <- map_chr(
        ref$contents,
        ~ {
          cr <- .x
          ma <- map_lgl(pkg_topics$alias, ~ any(cr == .x))
          pkg_topics$name[ma]
        }
      )
      unique_names <- unique(matched_names)

      refs_html <- map(unique_names, ~ {
        me <- pkg_topics[pkg_topics$name == .x, ]
        fns <- me$funs[[1]]
        if(length(fns) > 0) {
          fn2 <- paste0("[", fns,"](/", reference_folder, "/", me$file_out,")")
          fn3 <- paste0(fn2, collapse = " ")
          fn3 <- paste0(fn3, " | ", me$title)
        }
      })

      null_refs <- map_lgl(refs_html, is.null)

      refs_chr <- refs_html[!null_refs]

      ref_section <- c(
        paste0("## ", ref$title),
        "",
        paste0("Function(s) | Description"),
        paste0("------------- |----------------"),
        refs_chr,
        ""
      )

    })

  sections_chr <- map_chr(flatten(sections_list), ~.x)

  writeLines(sections_chr, path(reference_folder, "index.md"))
}


#' @export
package_repo_clone <- function(url = "",
                         target_folder = tempdir(),
                         branch = "main"
                         ) {
  tf <- path(target_folder, path_file(url))
  system(paste0("git clone ", url, " -b ", branch  ," ", tf))
  tf
}



#pkg_location <- "/var/folders/l8/v1ym1mc10_b0dftql5wrrm8w0000gn/T/Rtmpc4dBpB/sparklyr"
