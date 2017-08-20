#' metaboVis
#' @param tool A string denoting which visualisation tool to use
#' @param ... Parameters to be passed on to selected tool
#' @importFrom utils capture.output
#' @examples
#' \dontrun{
#' metaboVis('PCA')
#' }
#' @export

metaboVis <- function(tool,...){
  tool <- get(tool)
  suppressWarnings(
    suppressMessages(
      capture.output(
        tool(...)
      )
    )
  )
  return()
}
