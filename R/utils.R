add_parameters <- function(uri, parameters) {
  if (is.null(parameters)) return(uri)
  parameters_str <- paste(names(parameters), unlist(parameters), sep = "=", collapse = "&")

  sprintf("%s?%s", uri, parameters_str)
}
