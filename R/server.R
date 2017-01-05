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

#' list containers on a server
#'
#' @param server a \code{\link{docker_server}} object
#' @param ... a named list of parameters
#'
#' @return a tibble
#'
#' @source https://docs.docker.com/engine/reference/api/docker_remote_api_v1.24/#list-containers
#'
#' @export
list_containers <- function(server, ...) {
  uri <- make_uri(server, "containers/json", ...)

  r <- GET(uri)
  body <- fromJSON(content(r, "text"))

  # Transform data.frames into list for tibble transformation
  is_df <- lapply(body, is.data.frame) %>% unlist()
  if (any(is_df)) body[is_df] <- lapply(body[is_df], as.list)

  is_null <- lapply(body, is.null) %>% unlist()
  if (any(is_null)) body[is_null] <- NULL

  as_tibble(body)
}

#' @export
print.docker_server <- function(x, ...) {
  cat(sprintf('[Docker server]\n%s:%s\n', x$host, x$port))
}
