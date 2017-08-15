#' metaboVis
#' @param tool A string denoting which visualisation tool to use
#' @examples
#' \dontrun{
#' metaboVis('PCA')
#' }
#' @export

metaboVis <- function(tool){
  tool <- get(tool)
  suppressWarnings(
    suppressMessages(
      capture.output(
        tool()
      )
    )
  )
}
