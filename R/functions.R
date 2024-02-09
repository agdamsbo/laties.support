# renv::install(paste0("agdamsbo/",c("REDCapCAST","stRoke")))

#' Get data for calendar generation
#'
#' @return tibble
#' @export
#'
#' @examples
#' # ds <- calendar_data()
calendar_data <- function() {
  REDCapCAST::easy_redcap(
    project.name = "laties.support",
    widen.data = FALSE,
    uri = "https://redcap.au.dk/api/",
    fields = c(
      "record_id",
      "arrivaltime",
      "hvem_indtaster"
    )
  )[[1]]
}

#' Simple data modification
#'
#' @param data data set
#'
#' @return tibble
#' @export
#'
#' @examples
#' # data <- ds
data_mod <- function(data) {
  data |> dplyr::mutate(
    followup = lubridate::as_date(arrivaltime + lubridate::dmonths(3))+lubridate::hours(9))
}

#' Small wrapper for if_else in glue string
#'
#' @param data vector
#'
#' @return glue string
#' @export
#'
glue_if <- function(data){
  dplyr::if_else(is.na(data),'',glue::glue(', RING: {data}'))
}

#' Standard function to create ical
#'
#' @param data data set
#'
#' @return tibble
#' @export
#'
laties_cal <- function(data){
  data |> stRoke::ds2ical(
    start = followup,
    end = followup,
    location = NULL,
    summary.glue.string = "ID {record_id}{glue_if(hvem_indtaster)}"
  )
}

#' Creates
#'
#' @param data data set
#' @param assessor assessor var
#' @param dir output dir
#' @param base.name file base name
#'
#' @return NULL
#' @export
#'
#' @examples
#' # data <- ds |> data_mod()
#' # data |> export_ical()
export_ical <- function(data, assessor = "hvem_indtaster", dir = here::here("data/"), base.name = "laties_followup") {
  if (!is.null({{assessor}})) {
    data |> split(REDCapCAST::clean_redcap_name(data[[{{assessor}}]])) |>
      lapply(laties_cal) |>
      (\(i){
        nms <- names(i)
        i |> purrr::map2(nms,function(x,y){
          x |> calendar::ic_write(file=file.path(dir,glue::glue("{base.name}_{y}.ics")))
        })
      })()
  }

  data |> laties_cal() |> calendar::ic_write(file=file.path(dir,glue::glue("{base.name}_all.ics")))
}

#' Commit and push .ics calendar file
#'
#' @param f.path file paths(s)
#' @param c.message commit message
#'
#' @return NULL
#' @export
git_commit_push <- function(f.path, c.message=paste("calendar update",Sys.Date())) {
  git2r::add(path = f.path)
  # Suppressing error if nothing to commit
  tryCatch(git2r::commit(message = c.message), error = function(e) {})
  git2r::push(
    name = "origin",
    refspec = "refs/heads/main",
    credentials = git2r::cred_ssh_key(),
    set_upstream = FALSE
  )
}

#' All in one
#'
#' @return NULL
#' @export
#' @examples
#' # laties.db2cal()
laties.db2cal <- function(){
  calendar_data() |> data_mod() |> export_ical()
  # list.files(here::here("data"),pattern = ".ics$")[[1]] |> git_commit_push()
  system("git add *.ics")

  git2r::commit(all=TRUE, message=paste("calendar update",Sys.time()))

  system("/usr/bin/git push origin HEAD:refs/heads/main")

}



