#' images
#'
#' @param server a \code{\link{docker_server}} object
#' @param id the image id
#'
#' @return an object of class \code{docker_image}
#'
#' @export
docker_image <- function(server, id) {
  image <- inspect(server, id)
  structure(
    list(
      server = server,
      image = image
    ),
    class = "docker_image"
  )
}
