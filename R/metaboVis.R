#' metaboVis
#' @examples
#' \dontrun{
#' metaboVis('PCA')
#' }
#' @export

metaboVis <- function(tool){
  tool <- get(tool)
  tool()
}
