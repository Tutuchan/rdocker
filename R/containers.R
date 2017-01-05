#' ctontainer
#'
#' @param server a \code{\link{docker_server}} object
#' @param id the container id
#'
#' @return an object of class \code{docker_container}
#'
#' @export
docker_container <- function(server, id) {
  container <- inspect(server, id)
  structure(
    list(
      server = server,
      container = container
    ),
    class = "docker_container"
  )
}

#' create a container
#'
#' @param server a \code{\link{docker_server}} object
#' @param ... a named list of parameters
#'
#' @return a \code{\link{docker_container}} object
#'
#' @source https://docs.docker.com/engine/reference/api/docker_remote_api_v1.24/#/create-a-container
#'
#' @export
create <- function(server, ...) {
  r <- post_uri(server, "containers/create", ...)

  docker_container(
    server = server,
    id = suppressMessages(fromJSON(content(r, "text"))$Id)
  )
}

#' start a container
#'
#' @param container a \code{\link{docker_container}} object
#' @param ... a named list of parameters
#'
#' @return a numeric, the status code of the response
#'
#' @source https://docs.docker.com/engine/reference/api/docker_remote_api_v1.24/#start-a-container
#'
#' @export
start <- function(container, ...) {
  r <- post_uri(container$server, sprintf("containers/%s/start", container), ...)

  status_code(r)

}

#' inspect a container
#'
#' @param server a \code{\link{docker_server}} object,
#' @param id a numeric, the docker container id
#' @param ... a named list of parameters
#'
#' @return a tibble containing information about this container
#'
#' @source https://docs.docker.com/engine/reference/api/docker_remote_api_v1.24/#inspect-a-container
#'
#' @keywords internal
inspect <- function(server, id, ...) {
  uri <- make_uri(server, sprintf("containers/%s/json", id), ...)

  r <- GET(uri)
  body <- fromJSON(content(r, "text"), simplifyVector = FALSE)

  # Transform data.frames into list for tibble transformation
  is_df <- lapply(body, is.data.frame) %>%
    unlist()
  if (any(is_df)) body[is_df] <- lapply(body[is_df], as.list)

  is_null <- lapply(body, is.null) %>%
    unlist()
  if (any(is_null)) body[is_null] <- NULL

  l <- lapply(body, length) %>% unlist()
  if (any(l != 1)) body[l != 1] <- lapply(body[l != 1], function(x) list(x))



  as_tibble(body)
}

#' @export
print.docker_container <- function(x, ...) {
  cat(
    sprintf(
      "[Docker container] \n%s - %s \n  - created: %s\n  - image: %s\n  - args: [%s]\n  - path: %s",
      substr(x$container$Name, 2, nchar(x$container$Name)),
      substr(x$container$Id, 1, 8),
      x$container$Created,
      x$container$Image,
      toString(x$container$args[[1]]),
      x$container$Path
    )
  )
}
