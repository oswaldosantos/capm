#' Graphic interface to use capm functions
#' @export
#' 
#' @examples
#' GraphicInterface('SelectSamplingUnits')
#' 
GraphicInterface <- function(set.func) {
  runApp(paste0(system.file('shinyApps/', package = 'capm'), set.func))
}