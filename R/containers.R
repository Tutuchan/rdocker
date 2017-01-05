#' containers
#'
#' @import httr
#' @import jsonlite
#' @import tibble
#'
#' @export
get_containers <- function(server, ...) {
  uri <- file.path(server, "containers/json")
  parameters <- list(...)

  uri <- add_parameters(uri, parameters)

  r <- GET(uri)
  body <- fromJSON(content(r, "text"))

  # Transform data.frames into list for tibble transformation
  is_df <- lapply(body, is.data.frame) %>% unlist()
  if (any(is_df)) body[is_df] <- lapply(body[is_df], as.list)

  is_null <- lapply(body, is.null) %>% unlist()
  if (any(is_null)) body[is_null] <- NULL

  as_tibble(body)
}

#' inspect a container
#'
#' @export
inspect <- function(server, container, ...) {
  uri <- file.path(server, sprintf("containers/%s/json", container))
  parameters <- list(...)

  uri <- add_parameters(uri, parameters)

  r <- GET(uri)
  body <- fromJSON(content(r, "text"), simplifyVector = FALSE)

  # Transform data.frames into list for tibble transformation
  is_df <- lapply(body, is.data.frame) %>% unlist()
  if (any(is_df)) body[is_df] <- lapply(body[is_df], as.list)

  is_null <- lapply(body, is.null) %>% unlist()
  if (any(is_null)) body[is_null] <- NULL

  l <- lapply(body, length) %>% unlist()
  if (any(l != 1)) body[l != 1] <- lapply(body[l != 1], function(x) list(x))



  as_tibble(body)
}

#' create a container
#'
#' @examples
#' \dontrun{
#'   create("192.168.254.35:2375", Image = "datapole/r-ci", Env = I(c("PROJECT_NAME = test")))
#' }
#'
#' @export
create <- function(server, ...) {
  uri <- file.path(server, "containers/create")
  parameters <- list(...)

  r <- POST(uri, body = parameters, encode = "json")

  suppressMessages(fromJSON(content(r, "text"))$Id)
}

#' start a container
#'
#' @export
start <- function(server, container, ...) {
  uri <- file.path(server, sprintf("containers/%s/start", container))
  parameters <- list(...)

  uri <- add_parameters(uri, parameters)

  r <- POST(uri, body = parameters, encode = "json")

  status_code(r)

}
