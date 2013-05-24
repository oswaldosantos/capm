#' Replacement, abandonment, sterilization and adoption of companion animals
#' @description System of ordinary differential equations to simulate the effect of replacement of owned dogs, abandonment, sterilization of owned and stray dogs and adoption, on population dynamics.
#' @param pars a named \code{\link{vector}} of length 21, with point estimates of model parameters (see details).
#' @param state a named \code{\link{vector}} of length 8, with point estimates of model parameters (see details).
#' @param time time sequence for which output is wanted; the first value of times must be the initial time.
#' @param ster.range optional sequence (between 0 and 1) of the sterilization rates to be simulated.
#' @param aban.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of abandonment rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param adop.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of adoption rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param repl.range optional \code{\link{vector}} of length 2, with range of values of replacement rates to be assessed. This must be expressed as a percentage of carrying capacity.
#' @param ster.fm logical. If \code{TRUE}, ster.range is used for females and males and if \code{FALSE}, it is only used for females (for males, the point estimate given in \code{pars} is used.)
#' @param ... further arguments passed to \link[deSolve]{ode} function.
#' @details The \code{pars} argument must contain named values, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males. Then:
#' 
#'  
#' \code{bf1}, \code{bm1}, \code{bf2} and \code{bm2}: birth rate.
#' 
#' \code{df1}, \code{dm1}, \code{df2} and \code{dm2}: death rate.
#' 
#' \code{sf1}, \code{sm1}, \code{sf2} and \code{sm2}: sterilization rate.
#' 
#' \code{k1} and \code{k2}: carrying capacity.
#' 
#' \code{h1} and \code{h2}: mean harem size.
#' 
#' \code{ab}: abandonment rate.
#' 
#' \code{ad}: adoption rate.
#' 
#' \code{r}: replacement rate.
#' 
#' 
#' The \code{state} argument must contain named values for the inital number of animals, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males; and \code{s}: sterilized. Then, number values must be given for the categories:
#' 
#' \code{f1}, \code{cf1}, \code{m1}, \code{cm1}, \code{f2}, \code{cf2}, \code{m2} and \code{cm2}.
#' 
#' The function is a wrapper around the defaults of \link[deSolve]{ode} function, whose help page must be consulted for details. An exception is the method argument which is defined as "rk4".
#' @return \code{\link{list}} of class \code{rasa}. The first element, \code{*$model}, is the model function. The second, third and fourth elements are vectors (\code{*$pars}, \code{*$state}, \code{*$time}, respectively) containing the \code{pars}, \code{state} and \code{time} arguments of the function. The fifth element \code{*$results} is a \code{\link{data.frame}} with up to as many rows as elements in time. Using the conventions for state argument (see details), the first fourth columns contain the variables: \code{f}, \code{sf}, \code{m} and \code{sm}. The fifth and sisxth columns contain the number of animals and the group respectively (\code{n} and \code{group}). Other optional arguments are:
#' 
#' \code{ster}: instance fo sterilization rate (if \code{ster.range} is specified).
#' 
#' \code{aban}: instance fo abandonment rate (if \code{aban.range} is specified).
#' 
#' \code{adop}: instance fo adoption rate (if \code{adop.range} is specified).
#' 
#' @note Logistic growth models are not intended for scenarios in which
#' population size is greater than carrying capacity and growth rate is negative.
#' @references Soetaert K, Cash J and Mazzia F (2012). Solving differential equations in R. Springer.
#' @note The implemented model is part of an ongoing PhD thesis (student: Oswaldo Santos; adviser: Fernando Ferreira) to be finished at the end of 2013.
#' @seealso \link[deSolve]{ode}.
#' @export
#' @examples 
#' # Parameters and initial conditions from estimates   
#' # obtained in examples section from svysumm function.
#' # Note that there is not estimates for all arguments
#' # and thus, some are a prior defined.
#' # For example, carrying capacities were estimated as 
#' # 10 % greater than population sizes and birth and  
#' # death rates for stray animals were estimated as 
#' # 10 % greater than those rates in owned animals.
#' # The consequences of those "guesses" can be quantified
#' # with globalsens and localsens functions.
#' pars.rasa = c(
#'    bf1 = 0.262, bm1 = 0.262, bf2 = 0.288, bm2 = 0.288,
#'    df1 = 0.081, dm1 = 0.069, df2 = 0.089, dm2 = 0.076,
#'    sf1 = 0.064, sf2 = 0.05, sm1 = 0.048, sm2 = 0.05,
#'    k1 = 90785.01, k2 = 9078.501, h1 = 1, h2 = 1, 
#'    ab = 0.065, ad = 0.095, r = 0.111
#' )
#' state.rasa = c(
#'    f1 = 41641.785, cf1 = 8423.503, 
#'    m1 = 40890.046, cm1 = 8647.503, 
#'    f2 = 4164.179, cf2 = 208.209, 
#'    m2 = 4089.005, cm2 = 204.45
#' )
#' 
#' # Solve for point estimates.
#' rasa.pt <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30)
#' 
#' # Solve for parameter ranges.
#' rasa.rg <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30,
#'                 ster.range = seq(0, .5, length.out = 50), 
#'                 aban.range = c(0, .2), 
#'                 adop.range = c(0, .2),
#'                 repl.range = c(0, .1))
#'                 
rasa = function(pars = NULL, state = NULL, time = NULL, ster.range = NULL, aban.range = NULL, adop.range = NULL, repl.range = NULL, ster.fm = TRUE, ...) {
  state['n1'] = sum(state[c('f1', 'm1')])
  state['n2'] = sum(state[c('f2', 'm2')])
  state['n'] = sum(state[c('n1', 'n2')])
  rasafu <- function(pars, state, time) {
    rasa.fu = function(time, state, pars) {
      with(as.list(c(state, pars)), {
        r = (k1 * r) / 2
        x1 = ((h1 * (m1 - cm1) + (f1 - cf1)) * bf1) /
          (2 * h1 * (f1 - cf1) * (m1 -cm1))
        x2 = ((h2 * (m2 - cm2) + (f2 - cf2)) * bf2) /
          (2 * h2 * (f2 - cf2) * (m2 -cm2))
        wf1 = ((m1 - cm1) * x1) / (h1^(-1) * (f1 - cf1) + (m1 - cm1))
        bet.f1 = wf1 - (wf1 - df1) * (f1 + m1) / k1
        gam.f1 = df1
        d.f1 = (f1 * (bet.f1 * (1 - (cf1 / f1)) - gam.f1)
               - ab * f1
               + ad * f2 * (1 - ((f1 + m1) / k1))
               + r * (1 - ((f1 + m1) / k1))
        )
        d.cf1 = (- gam.f1 * cf1
                + sf1 * (f1 - cf1 + ad * (f2 - cf2))
                - ab * cf1
                + ad * cf2 * (1 - ((f1 + m1) / k1))
        )
        wm1 = ((f1 - cf1) * x1) / (h1^(-1) * (f1 - cf1) + (m1 - cm1))
        bet.m1 = wm1 - (wm1 - dm1) * (f1 + m1) / k1
        gam.m1 = dm1
        d.m1 = (m1 * (bet.m1 * (1 - (cm1 / m1)) - gam.m1)
               - ab * m1
               + ad * m2 * (1 - ((f1 + m1) / k1))
               + r * (1 - ((f1 + m1) / k1))
        )
        d.cm1 = (- gam.m1 * cm1
                + sm1 * (m1 - cm1 + ad * (m2 - cm2))
                - ab * cm1
                + ad * cm2 * (1 - ((f1 + m1) / k1))
        )
        bet.f2 = ((m2 - cm2) * x2) / (h2^(-1) * (f2 - cf2) + (m2 - cm2))
        gam.f2 = df2 + (bet.f2 - df2) * ((f2 + m2) / k2)
        d.f2 = (f2 * (bet.f2 * (1 - (cf2 / f2)) - gam.f2) 
               + ab * f1 * (1 - ((f2 + m2) / k2))
               - ad * f2
        )
        d.cf2 = (- gam.f2 * cf2
                + sf2 * (f2 - cf2 + ab * (f1 - cf1))
                - ad * cf2
                + ab * cf1 * (1 - ((f2 + m2) / k2))
        )
        bet.m2 = ((f2 - cf2) * x2) / (h2^(-1) * (f2 - cf2) + (m2 - cm2))
        gam.m2 = dm2 + (bet.m2 - dm2) * ((f2 + m2) / k2)
        d.m2 = (m2 * (bet.m2 * (1 - (cm2 / m2)) - gam.m2)
               + ab * m1 * (1 - ((f2 + m2) / k2))
               - ad * m2
        ) 
        d.cm2 = (- gam.m2 * cm2
                + sm2 * (m2 - cm2 + ab * (m1 - cm1))
                - ad * cm2
                + ab * cm1 * (1 - ((f2 + m2) / k2))
        )
        d.n1 = d.f1 + d.cf1 + d.m1 + d.cm1
        d.n2 = d.f2 + d.cf2 + d.m2 + d.cm2
        d.n = d.n1 + d.n2
        list(c(d.f1, d.cf1, d.m1, d.cm1, d.f2, d.cf2, 
               d.m2, d.cm2, d.n1, d.n2, d.n))
      })
    }
    state = c(state['f1'], state['cf1'], 
              state['m1'], state['cm1'],
              state['f2'], state['cf2'], 
              state['m2'], state['cm2'],
              state['n1'], state['n2'], state['n'])
    rasa.out = ode(times = time, 
                   func = rasa.fu, 
                   y = state, 
                   parms = pars, 
                   method = 'rk4',
                   ...)
    return(as.data.frame(rasa.out))
  } 
  if (is.null(ster.range) & is.null(aban.range) & 
        is.null(adop.range)) {
    output <- rasafu(pars = pars, state = state, time = time)
    rasa <- list(
      model = rasafu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(output)
    )
    class(rasa) <- 'rasa'
    return(rasa)
  } else {
    if(length(aban.range) != 2) {
      stop('The length of aban.range must be equal to 2.')
    }
    if(length(adop.range) != 2) {
      stop('The length of adop.range must be equal to 2.')
    }
    if(length(repl.range) != 2) {
      stop('The length of repl.range must be equal to 2.')
    }
    output <- NULL
    paras <- pars
    aban.range = c(aban.range[1], pars['ab'], aban.range[2])
    adop.range = c(adop.range[1], pars['ad'], adop.range[2])
    for (i in 1:length(repl.range)) {
      for (i1 in 1:length(aban.range)) {
        for (i2 in 1:length(adop.range)) {
          for (i3 in 1:length(ster.range)) {
            if (ster.fm) {
              paras[c('sf1', 'sm1', 'ad', 'ab', 'r')] = 
                c(ster.range[i3], ster.range[i3], 
                  adop.range[i2], aban.range[i1],
                  repl.range[i]
                )
            } else {
              paras[c('sf1', 'ad', 'ab', 'r')] = 
                c(ster.range[i3], adop.range[i2], 
                  aban.range[i1], repl.range[i]
                )
            }
            output = rbind(
              output,
              rasafu(pars = paras, 
                     state = state, 
                     time = time)
            )
          }
        }
      }
    }
    names(output) = c(1,2:5, 2:5)
    output = data.frame(
    rbind(output[, 1:5], output[, c(1, 6:9)]),
    n = c(rowSums(output[, c(2, 4)]), rowSums(output[, c(6, 8)])),
    group = rep(1:2, each = nrow(output)),
    ster = rep(ster.range, each = length(time)),
    adop = rep(adop.range, 
               each = length(time) * length(ster.range)),
    aban = rep(aban.range, 
               each = length(time) * length(ster.range) * 
                 length(adop.range)),
    repl = rep(repl.range, 
               each = length(time) * length(ster.range) * 
                 length(adop.range) * length(aban.range))
    )
    names(output)[1:5] = c('t', 'f', 'cf', 'm', 'cm')
    rasa <- list(
      model = rasafu,
      pars = pars,
      state = state,
      time = time,
      results = as.data.frame(output)
    )
    class(rasa) <- 'rasa'
    return(rasa)
  }
}