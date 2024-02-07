#' Get data for calendar generation
#'
#' @return tibble
#' @export
#'
#' @examples
#' ds <- calendar_data()
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

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#' data <- ds
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
#' @param data
#'
#' @return
#' @export
#'
#' @examples
laties_cal <- function(data){
  data |> REDCapCAST::ds2ical(
    start = followup,
    end = followup,
    location = NULL,
    summary.glue.string = "ID {record_id}{glue_if(hvem_indtaster)}"
  )
}

#' Creates
#'
#' @param data
#' @param assessor
#' @param dir
#' @param base.name
#'
#' @return
#' @export
#'
#' @examples
#' data <- ds |> data_mod()
#' data |> export_ical()
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
#' @param ics.path
#'
#' @return
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
#' @return
#' @export
#' @examples
#' laties.db2cal()
#'
laties.db2cal <- function(){
  calendar_data() |> data_mod() |> export_ical()
  list.files(here::here("data"),pattern = ".ics$") |> git_commit_push()
}



