#' @export
package_reference <- function(pkg_folder = "",
                              reference_folder = "reference",
                              rd_folder = "man") {
  walk(
    dir_ls(path(pkg_folder, rd_folder)),
    ~ {
      new_name <- path(path_ext_remove(path_file(.x)), ext = "md")
      Rd2md::Rd2markdown(
        .x,
        here::here(reference_folder, new_name)
      )
    }
  )

  # pkg_location <- package_repo_clone("https://github.com/sparklyr/sparklyr")
  # pkg <- pkgdown::as_pkgdown(pkg_location)
  # topics <- transpose(pkg$topics)
  # out <- parse_topic(topics$ml_decision_tree.Rd)
  # writeLines(out, "test.md")

}

parse_topic <- function(topic) {
  tag_names <- purrr::map_chr(topic$rd, ~ class(.)[[1]])
  tags <- split(topic$rd, tag_names)
  c(
    paste0("# ", topic$name),
    parse_section(tags$tag_title),
    parse_section(tags$tag_description, "## Description"),
    parse_section(tags$tag_usage, "## Usage"),
    parse_section_arguments(tags$tag_arguments, "## Arguments"),
    parse_section(tags$tag_value, "## Value"),
    parse_section(tags$tag_examples, "## Examples"),
    parse_section(tags$tag_seealso)
  )
}

parse_section <- function(x, title = NULL) {
  if(!is.null(x)) {
    c(
      "\n",
      title,
      parse_tag(x[[1]]),
      "\n"
    )
  } else {
    ""
  }
}

parse_section_arguments <- function(x, title = NULL) {
  if(!is.null(x)) {
    args_p <- map_chr(x[[1]], ~{
      et <- .x
      paste0(map(et, parse_tag), collapse = " | ")
    })
    args_ret <- map_lgl(args_p, ~.x != "\n")
    c(
      "\n",
      title,
      "\n",
      "Argument      |Description",
      "------------- |----------------",
      args_p[args_ret],
      "\n"
    )
  } else {
    NULL
  }
}
parse_tag <- function(x) {
  tg_res <- map(x, ~{
    lv1 <- .x
    if(length(lv1) > 0) {
      if(length(lv1) == 1) {
        res <- parse_line_tag(lv1)
      } else {
        lv2 <- map(lv1, parse_line_tag)
        res <- paste0(lv2, collapse = " ")
      }
      if("tag_dontrun" %in% class(lv1)) res <- paste0("```r\n", res, "\n```")
    } else {
      res <- ""
    }
    res
  })
  if(all(map_lgl(x, ~ "RCODE" %in% class(.x)))) {
    tg_res <- c("```r", tg_res, "```")
  }
  paste0(tg_res, collapse = " ")
}

parse_line_tag <- function(x) {
  tg_res <- map(x, ~{
    if(length(.x) > 0) {
      res <- as.character(.x)
      if("RCODE" %in% class(.x)) res <- paste0("`", res, "`")
    } else {
      res <- ""
    }
    res
  })
  if("tag_item" %in% class(x)) tg_res <- "\n* "
  paste0(tg_res, collapse = " ")
}

#' @export
package_reference_index <- function(pkg_folder = "",
                                    reference_folder = "reference",
                                    rd_folder = "man") {
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
        if (length(fns) > 0) {
          fn2 <- paste0("[", fns, "](/", reference_folder, "/", me$file_out, ")")
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
    }
  )

  sections_chr <- map_chr(flatten(sections_list), ~.x)

  writeLines(sections_chr, path(reference_folder, "index.md"))
}


#' @export
package_repo_clone <- function(url = "",
                               target_folder = tempdir(),
                               branch = "main") {
  tf <- path(target_folder, path_file(url))
  system(paste0("git clone ", url, " -b ", branch, " ", tf))
  tf
}

