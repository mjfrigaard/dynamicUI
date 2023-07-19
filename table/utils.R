#' golem's `golem_add_external_resources()` function
#'
#' @return nothing--internal
#' 
#' @export 
#'
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rsst"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' golem's `app_sys()` function
#'
#' @param ... args passed to `system.file()`
#'
#' @return path to installed files/folders
#' 
#' @export
#'
app_sys <- function(...) {
  system.file(..., package = "rsst")
}