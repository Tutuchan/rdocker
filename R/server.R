#' server
#'
#' @param host the IP or URL of the Docker server
#' @param port the port of the Docker server
#'
#' @return an object of class \code{docker_server}
#'
#' @import httr
#' @import jsonlite
#' @import tibble
#' @import magrittr
#'
#' @export
docker_server <- function(host, port) {
  structure(
    list(
      host = host,
      port = port
    ),
    class = "docker_server"
  )
}

#' list elements on a server
#'
#' @param server a \code{\link{docker_server}} object
#' @param type a character
#' @param ... a named list of parameters
#'
#' @return a tibble
#'
#' @source https://docs.docker.com/engine/reference/api/docker_remote_api_v1.24/#list-containers
#'
#' @export
docker_list <- function(server, type = c("containers", "images", "networks", "volumes"), ...) {

  uri_tail <- if (type %in% c("containers", "images")) "%s/json" else "%s"

  uri <- make_uri(
    server,
    sprintf(uri_tail, type),
    ...
  )

  r <- GET(uri)
  body <- suppressMessages(fromJSON(content(r, "text")))

  # Transform data.frames into list for tibble transformation
  # TODO : ok for containers, not for the others
  is_df <- lapply(body, is.data.frame) %>% unlist()
  if (any(is_df)) {
    if (sum(is_df) > 1) body[is_df] <- lapply(body[is_df], as.list) else body[, is_df] <- list(body[, is_df])
  }

  is_null <- lapply(body, is.null) %>% unlist()
  if (any(is_null)) body[is_null] <- NULL

  as_tibble(body)
}

#' @export
print.docker_server <- function(x, ...) {
  cat(sprintf('[Docker server]\n%s:%s\n', x$host, x$port))
}
