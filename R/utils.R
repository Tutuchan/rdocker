# URIs ---------------------------------------------------------------------
make_uri <- function(server, endpoint, ...) {
  uri <- sprintf("%s:%s/%s", server$host, server$port, endpoint)
  parameters <- list(...)

  add_parameters(uri, parameters)
}

post_uri <- function(server, uri, ... ) {
  stopifnot(inherits(server, "docker_server"))

  uri <- make_uri(server, uri)
  parameters <- list(...)

  POST(uri, body = parameters, encode = "json")
}

delete_uri <- function(server, uri, ... ) {
  stopifnot(inherits(server, "docker_server"))

  uri <- make_uri(server, uri)
  parameters <- list(...)

  DELETE(uri, body = parameters, encode = "json")
}

add_parameters <- function(uri, parameters) {
  if (length(parameters) == 0) return(uri)
  parameters_str <- paste(names(parameters), unlist(parameters), sep = "=", collapse = "&")

  sprintf("%s?%s", uri, parameters_str)
}

# S3 ----------------------------------------------------------------------

docker_remove <- function(x, ...) {
  UseMethod("docker_remove")
}


# Transform ---------------------------------------------------------------

transform_list_containers <- function(x) {
  to_chr <- function(col) {
    nulls <- vapply(col, is.null, logical(1))
    if (any(nulls)) col[nulls] <- list("")
    unlist(col)
  }
  x$RepoTags <- to_chr(x$RepoTags)
  x$RepoDigests <- to_chr(x$RepoDigests)

  as_tibble(
    cbind(
      x[, -which(names(x) == "Labels")],
      x$Labels
    )
  )
}

