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
  fcs <- path_split(fc)
  rf <- map(fcs, ~.x[2:length(.x)])
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

#' @export
clone_github_repo <- function(github_repo = "rstudio/db.rstudio.com",
                              github_branch = "master", 
                              package_folder = ".repos"
                              ){
  pr <- str_split(github_repo, "/")
  pr <- pr[[1]][2]
  repo <- path(package_folder, pr)
  if(dir_exists(repo)) dir_delete(repo)
  sys_command <- paste0("git clone -b ", github_branch," https://github.com/", github_repo, " ", repo)
  system(sys_command)
}

#' @export
remove_from_path <- function(file_list, remove_this) {
  file_list %>% 
    map(str_split, pattern = "/") %>% 
    flatten() %>% 
    map(~.x[.x != remove_this]) %>% 
    map_chr(~ paste(.x, collapse = "/")) %>% 
    as_fs_path()
}

#' @export
list_all_files <- function(site_name = "db.rstudio.com", 
                           repo_folder = ".repos",
                           ignore_html = TRUE
                           ) {
  base_folder <- path(repo_folder, site_name)
  content_ls <- dir_ls(path(base_folder, "content"), recurse = TRUE)
  static_ls <- dir_ls(path(base_folder, "static"), recurse = TRUE)
  all_ls <- c(content_ls, static_ls)
  all_files <- all_ls[is_file(all_ls)]
  if(ignore_html) all_files <- all_files[path_ext(all_files) != "html"]
  as_fs_path(all_files)
}

#' @export
prepare_files <- function(file_list = list_all_files()) {
    map(
      file_list,
      ~{
      fp <- str_locate(.x, "/static/")[[2]]
      if(is.na(fp)) fp <- str_locate(.x, "/content/")[[2]]
      dest1 <- as_fs_path(str_sub(.x, fp + 1, nchar(.x))) 
      dest2 <- path("site", dest1)
      dest <- str_replace(dest2, "/_", "/")
      list(
        origin = .x, 
        destination = as_fs_path(dest),
        sub_folder = as_fs_path(path_dir(dest1))
      )
    })
}


hold_function <- function()  {
  if(dir_exists("docs")) dir_delete("docs")
  dir_create("docs")
  
  content_ls <- dir_ls("content", recurse = TRUE)
  
  rmd_list <- content_ls[str_detect(content_ls, ".Rmd") | str_detect(content_ls, ".md")]
  
  static_ls <- dir_ls("static", recurse = TRUE)
  

  
  all_ls <-  as_fs_path(c(rmd_list, static_ls))
  
  all_ls <- all_ls[!str_detect(all_ls, "static/visualization/content")]
  
  all_folders <- all_ls[is_dir(all_ls)]
  
  all_files <- all_ls[is_file(all_ls)]
  
  clean_ls <- all_files %>% 
    remove_from_path("content") %>% 
    remove_from_path("static") %>% 
    map_chr(~ path("docs", .x)) %>% 
    as_fs_path()
  
  clean_ls %>% 
    path_dir() %>% 
    unique() %>% 
    dir_create()
  
  seq_along(clean_ls) %>%
    map(~file_copy(all_files[[.x]], clean_ls[[.x]]))
  
  file_move("docs/_index.md", "docs/index.md")

  
  clone_github_repo(github_branch = "quarto")
  
  uf <- unique(map_chr(prepare_files(), ~.x$sub_folder))
  dir_create(path("site", uf)) 
  
  map(prepare_files(), ~ file_copy(.x$origin, .x$destination) )
  
}




