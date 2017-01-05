make_uri <- function(server, endpoint, ...) {
  uri <- sprintf("%s:%s/%s", server$host, server$port, endpoint)
  parameters <- list(...)

  add_parameters(uri, parameters)
}

post_uri <- function(server, uri, ... ) {
  stopifnot(inherits(server), "docker_server")

  uri <- make_uri(server, uri)
  parameters <- list(...)

  POST(uri, body = parameters, encode = "json")
}

add_parameters <- function(uri, parameters) {
  if (length(parameters) == 0) return(uri)
  parameters_str <- paste(names(parameters), unlist(parameters), sep = "=", collapse = "&")

  sprintf("%s?%s", uri, parameters_str)
}
