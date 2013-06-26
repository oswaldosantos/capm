#' Sterilization and immigration effects.
#' @description System of ordinary differential equations to simulate the effect of sterilization and immigration on population dynamics.
#' @param pars \code{\link{vector}} of length 4. The values are point estimates of birth rate, death rate, carrying capacity and sterilization rate. The names of this values must be "b", "d", "k" and "s", respectively.
#' @param state \code{\link{vector}} of length 2. The values are initial population size and initial proportion of sterilized animals. The names of this values must be "n" and "q", respectively.
#' @param time time sequence for which output is wanted; the first value of times must be the initial time.
#' @param dd string equal to \code{b} or \code{d} to define if density-dependece act on birth or death rartes respectively.
#' @param imm a number representing the total of immigrants per time unit.
#' @param ster.range optional sequence (between 0 and 1) of the sterilization rates to be simulated.
#' @param ... further arguments passed to \link[deSolve]{ode} function.
#' @details The implemented model is described by Amaku, et. al., 2009 and the function is a wrapper around the defaults of \link[deSolve]{ode} function, whose help page must be consulted for details.
#' @return \code{\link{list}} of class \code{sterim}. The first element, \code{*$model}, is the model function. The second, third and fourth elements are vectors (\code{*$pars}, \code{*$state}, \code{*$time}, respectively) containing the \code{pars}, \code{state} and \code{time} arguments of the function. The fifth element \code{*$results} is a \code{\link{data.frame}} with up to as many rows as elements in time. First column contains the time, second column the population size and third column the proportion of sterilized animals. If \code{ster.range} is specified, fourth column contains the used sterilization rates.
#' @note Logistic growth models are not intended for scenarios in which population size is greater than carrying capacity and growth rate is negative.
#' @references Amaku M, Dias R and Ferreira F (2009). Dinamica populacional canina: potenciais efeitos de campanhas de esterilizacao. Revista Panamericana de Salud Publica, 25(4), pp. 300-304.
#' 
#' Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' @seealso \link[deSolve]{ode}.
#' @export
#' @examples 
#' # Parameters and initial conditions from estimates   
#' # obtained in examples section from svysumm function but
#' # estimating a proportion insted of a total for births.
#' pars.sterim = c(b = 0.245, d = 0.101, 
#'              k = 98050.49, s = .048)
#' state.sterim = c(n = 89136.810, q = 0.198)
#' 
#' # Solve for a specific sterilization rate.
#' sterim.pt = sterim(pars = pars.sterim, state = state.sterim, 
#'                   time = 0:30, dd = 'b', imm = 100)
#' 
#' # Solve for a range of sterilization rates.
#' sterim.rg = sterim(
#'    pars = pars.sterim, state = state.sterim,
#'    time = 0:30, dd = 'b', imm = 100, 
#'    ster.range = seq(0, .4, length.out = 50))
#'    
sterim = function(pars = NULL, state = NULL, time = NULL, dd = 'b', imm = 0, ster.range = NULL, ...) {
  sterimfu = function(pars = NULL, state = NULL, time = NULL) {
    sterim.fu = function(time, state, pars) {
      with(as.list(c(state, pars)), {
        if (dd == 'b') {
          nat = b - (b - d) * (n / k)
          mor = d
        }
        if (dd == 'd') {
          nat = b
          mor = d + (b - d) * (n / k)
        }
        dn = n * (nat * (1 - q) - mor) + imm
        dq = (1 - q) * (s - q * nat)
        list(c(dn, dq))
      })
    }
    state = c(state['n'], state['q'])
    sterim.out = ode(times = time, func = sterim.fu, 
                         y = state, parms = pars, ...)
    return(as.data.frame(sterim.out))
  }
  
  if (!is.null(ster.range)) {
    output = NULL
    paras = pars
    for(i in 1:length(ster.range)) {
      paras['s'] = ster.range[i]
      tmp = sterimfu(pars = paras, state = state, time = time)
      output = rbind(output, tmp)
    }   
    ster.rate = rep(ster.range , each = length(time))   
    sterim = list(
      model = sterimfu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(cbind(output, ster.rate))
      )
    class(sterim) = 'sterim'
    return(sterim)
    
  } else {
    output = sterimfu(pars = pars, state = state, time = time)
    sterim = list(
      model = sterimfu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(output)
    )
    class(sterim) = 'sterim'
    return(sterim)
  }
}