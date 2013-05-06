#' Owned dog sterilization
#' @description System of ordinary differential equations to simulate the effect of owned dogs sterilization on population dynamics.
#' @param pars \code{\link{vector}} of length 4. The values are point estimates of birth rate, death rate, carrying capacity and sterilization rate. The names of this values must be "b", "d", "k" and "s", respectively.
#' @param state \code{\link{vector}} of length 2. The values are initial population size and initial proportion of sterilized animals. The names of this values must be "n" and "q", respectively.
#' @param time time sequence for which output is wanted; the first value of times must be the initial time.
#' @param ster.range optional sequence (between 0 and 1) of the sterilization rates to be simulated.
#' @param ... further arguments passed to \link[deSolve]{ode} function.
#' @details The implemented model is described by Amaku, et. al., 2009 and the function is a wrapper around the defaults of \link[deSolve]{ode} function, whose help page must be consulted for details.
#' @return \code{\link{list}} of class \code{sterowned}. The first element, \code{*$model}, is the model function. The second, third and fourth elements are vectors (\code{*$pars}, \code{*$state}, \code{*$time}, respectively) containing the \code{pars}, \code{state} and \code{time} arguments of the function. The fifth element \code{*$results} is a \code{\link{data.frame}} with up to as many rows as elements in time. First column contains the time, second column the population size and third column the proportion of sterilized animals. If \code{ster.range} is specified, fourth column contains the used sterilization rates.
#' @references Amaku M, Dias R and Ferreira F (2009). Dinamica populacional canina: potenciais efeitos de campanhas de esterilizacao. Revista Panamericana de Salud Publica, 25(4), pp. 300-304.
#' 
#' Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' @seealso \link[deSolve]{ode}.
#' @export
#' @examples 
#' # Parameters and initial conditions from estimates   
#' # obtained in examples section from svysumm function.
#' pars.od <- c(b = 0.167, d = 0.094, k = 125027.411 * 1.1, s = .059)
#' state.od <- c(n = 125027.411, q = 0.188)
#' 
#' # Solve for a specific sterilization rate.
#' ster.od <- sterowned(pars = pars.od, state = state.od, time = 0:30)
#' 
#' # Solve for a range of sterilization rates.
#' ster.range.od <- sterowned(pars = pars.od, state = state.od, time = 0:30, ster.range = seq(0, .4, length.out = 50))
sterowned = function(pars = NULL, state = NULL, time = NULL, ster.range = NULL, ...) {
  sterownedfu <- function(pars = NULL, state = NULL, time = NULL) {
    sterowned.fu = function(time, state, pars) {
      with(as.list(c(state, pars)), {
        nat = b - (b - d) * (n / k)
        mor = d
        dn = n * (nat * (1 - q) - mor)
        dq = (1 - q) * (s - q * nat)
        list(c(dn, dq))
      })
    }
    state <- c(state['n'], state['q'])
    sterowned.out <- ode(times = time, func = sterowned.fu, 
                         y = state, parms = pars, ...)
    return(as.data.frame(sterowned.out))
  }
  
  if (!is.null(ster.range)) {
    output <- NULL
    paras <- pars
    for(i in 1:length(ster.range)) {
      paras['s'] <- ster.range[i]
      tmp <- sterownedfu(pars = paras, state = state, time = time)
      output <- rbind(output, tmp)
    }   
    ster.rate <- rep(ster.range , each = length(time))   
    sterowned <- list(
      model = sterownedfu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(cbind(output, ster.rate))
      )
    class(sterowned) <- 'sterowned'
    return(sterowned)
    
  } else {
    output <- sterownedfu(pars = pars, state = state, time = time)
    sterowned <- list(
      model = sterownedfu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(output)
    )
    class(sterowned) <- 'sterowned'
    return(sterowned)
  }
}