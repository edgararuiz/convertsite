#' @importFrom purrr map map_chr walk
#' @importFrom magrittr `%>%`
#' @import stringr
#' @import fs
#' @import here

full_file_list <- function(folder, new_folder, exclude_exts = NULL) {
  fc <- dir_ls(folder, recurse = TRUE)
  if(!is.null(exclude_exts)) {
    for(i in 1:length(exclude_exts)) {
      fc <- fc[path_ext(fc) != exclude_exts[i]]
    }
  }
  if(!dir_exists(new_folder)) dir_create(new_folder)
  pl <- length(path_split(path_common(fc))[[1]])
  fcs <- path_split(fc)
  rf <- map(fcs, ~.x[(pl+1):length(.x)])
  rj <- path_join(rf)
  dj <- as_fs_path(unique(path_dir(rj))) 
  fd <- dj[dj != "."]
  dir_create(path(new_folder, fd))
  rj[is_file(fc)]
}

#' @export 
full_file_copy <- function(folder, new_folder, exclude_exts = NULL) {
  fls <- full_file_list(folder, new_folder, exclude_exts)
  walk(fls, ~ file_copy(path(folder, .x), path(new_folder, .x)))
}

#' @export 
full_file_move <- function(folder, new_folder, exclude_exts = NULL) {
  fls <- full_file_list(folder, new_folder, exclude_exts)
  walk(fls, ~ file_move(path(folder, .x), path(new_folder, .x)))
  dir_delete(folder)
}





