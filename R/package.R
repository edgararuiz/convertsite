#' @export
package_reference <- function(pkg_folder = "",
                              rd_folder = "man",
                              reference_folder = "reference"
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
package_repo <- function(url = "",
                         branch = "main",
                         target_folder = tempdir()
                         ) {
  tf <- path(target_folder, path_file(url))
  system(paste0("git clone ", url, " -b ", branch  ," ", tf))
  tf
}

#pkg_location <- "/var/folders/l8/v1ym1mc10_b0dftql5wrrm8w0000gn/T//RtmpsOTPQV/sparklyr"
