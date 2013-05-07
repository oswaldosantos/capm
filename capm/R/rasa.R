#' Recruitment, abandonment, sterilization and adoption of dogs
#' @description System of ordinary differential equations to simulate the effect of recruitment of owned dogs, abandonment, sterilization of owned and stray dogs and adoption, on population dynamics.
#' @param pars a named \code{\link{vector}} of length 21, with point estimates of model parameters (see details).
#' @param state a named \code{\link{vector}} of length 8, with point estimates of model parameters (see details).
#' @param time time sequence for which output is wanted; the first value of times must be the initial time.
#' @param ster.range optional sequence (between 0 and 1) of the sterilization rates to be simulated.
#' @param aban.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of abandonment rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param adop.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of adoption rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param ster.fm logical. If \code{TRUE}, ster.range is used for females and males and if \code{FALSE}, it is only used for females (for males, the point estimate given in \code{pars} is used.)
#' @param ... further arguments passed to \link[deSolve]{ode} function.
#' @details The \code{pars} argument must contain named values, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males. Then:
#' 
#'  
#' \code{af1}, \code{am1}, \code{af2} and \code{am2}: birth rate.
#' 
#' \code{bf1}, \code{bm1}, \code{bf2} and \code{bm2}: death rate.
#' 
#' \code{ef1}, \code{em1}, \code{ef2} and \code{em2}: sterilization rate.
#' 
#' \code{k1} and \code{k2}: carrying capacity.
#' 
#' \code{z1} and \code{z2}: mean harem size.
#' 
#' \code{h}: abandonment rate.
#' 
#' \code{j}: adoption rate.
#' 
#' \code{v}: recruitment rate.
#' 
#' 
#' The \code{state} argument must contain named values for the inital number of animals, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males; and \code{s}: sterilized. Then, number values must be given for the categories:
#' 
#' \code{f1}, \code{sf1}, \code{m1}, \code{sm1}, \code{f2}, \code{sf2}, \code{m2} and \code{sm2}.
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
#' # 10 % greater than population sizes; and birth and  
#' # death rates for stray animals were estimated as 
#' # 10 % greater than those rates in owned animals.
#' # The consequences of those "guesses" can be quantified
#' # with globalsens and localsens functions.
#' pars.rasa = c(
#'    af1 = 0.219, am1 = 0.219, af2 = 0.241, am2 = 0.241,
#'    bf1 = 0.091, bm1 = 0.091, bf2 = 0.1, bm2 = 0.1,
#'    ef1 = 0.074, ef2 = 0.01, em1 = 0.047, em2 = 0.01,
#'    k1 = 137176.8, k2 = 13854.86, z1 = 1, z2 = 1, 
#'    h = 0.051, j = 0.111, v = 0.1
#' )
#' state.rasa = c(
#'    f1 = 46181.12, sf1 = 13309.497, 
#'    m1 = 49681.91, sm1 = 15533.682, 
#'    f2 = 5949.062, sf2 = 59.491, 
#'    m2 = 6521.56, sm2 = 65.216
#' )
#' 
#' # Solve for point estimates.
#' rasa.pt <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30)
#' 
#' # Solve for parameter ranges.
#' # rasa.rg <- rasa(pars = pars.rasa, 
#'                 state = state.rasa, 
#'                 time = 0:30,
#'                 ster.range = seq(0, .5, by = .1), 
#'                 aban.range = c(0, .2), 
#'                 adop.range = c(0, .2))
#'                 
rasa = function(pars = NULL, state = NULL, time = NULL, ster.range = NULL, aban.range = NULL, adop.range = NULL, ster.fm = TRUE, ...) {
  rasafu <- function(pars = NULL, state = NULL, time = NULL) {
    rasa.fu = function(time, state, pars) {
      with(as.list(c(state, pars)), {
        v = k1 * v
        x1 = ((z1 * m1 + f1) * af1 * (f1 + sf1 + m1 + sm1)) /
          (2 * z1 * f1 * m1)
        x2 = ((z2 * m2 + f2) * af2 * (f2 + sf2 + m2 + sm2)) /
          (2 * z2 * f2 * m2)
        # femeas domiciliadas
        alf1 = (af1 * (2 * m1 * x1) / (z1^(-1) * f1 + m1))
        wf1 = alf1 - (alf1 - bf1) * (f1 + m1) / k1 # natalidade.
        yf1 = bf1 # mortalidedade.
        
        # no. femeas nao esterilizadas.
        df1 = (f1 * (wf1 * (1 - (sf1 / f1)) - yf1) # crescimento.
               - h * f1 # abandono.
               + j * f2 * (1 - ((f1 + m1) / k1)) # adocao.
               + v * (1 - ((f1 + m1) / k1)) # reposicao.
        )
        
        # no. femeas esterilizadas.
        dsf1 = (- yf1 * sf1 # mortalidade.
                + ef1 * (f1 - sf1 + j * (f2 - sf2)) # novos esterilizados.
                - h * sf1 # abandono.
                + j * sf2 * (1 - ((f1 + m1) / k1)) # adocao.
        )
        
        # machos domiciliados
        alm1 = (am1 * (2 * f1 * x1) / (z1^(-1) * f1 + m1)) # fertilidade.
        wm1 = alm1 - (alm1 - bm1) * (f1 + m1) / k1 # natalidade.
        ym1 = bm1 # mortalidade.
        
        # no. machos nao esterilizadas.
        dm1 = (m1 * (wm1 * (1 - (sm1 / m1)) - ym1) # crescimento.
               - h * m1 # abandono.
               + j * m2 * (1 - ((f1 + m1) / k1)) # adocao.
               + v * (1 - ((f1 + m1) / k1)) # reposicao.
        )
        
        # no. machos esterilizadas.
        dsm1 = (- ym1 * sm1 # mortalidade.
                + em1 * (m1 - sm1 + j * (m2 - sm2)) # novos esterilizados.
                - h * sm1 # abandono.
                + j * sm2 * (1 - ((f1 + m1) / k1)) # adocao.
        )      
        
        # femeas nao domiciladas
        alf2 = (af2 * (2 * m2 * x2) / (z2^(-1) * f2 + m2)) # fertilidade.
        wf2 = alf2 # natalidade.
        yf2 = bf2 + (wf2 - bf2) * ((f2 + m2) / k2) # mortalidade.
        
        # no. femeas nao esterilizadas.
        df2 = (f2 * (wf2 * (1 - (sf2 / f2)) - yf2) # crescimento 
               + h * f1 * (1 - ((f2 + m2) / k2)) # abandono.
               - j * f2 # adocao.
        )
        
        # no. femeas nao esterilizadas.
        dsf2 = (- yf2 * sf2 # mortalidade.
                + ef2 * (f2 - sf2 + h * (f1 - sf1)) # novos esterilizados.
                - j * sf2 # adocao.
                + h * sf1 * (1 - ((f2 + m2) / k2)) # abandono.
        ) 
        
        # machos nao domiciliados
        alm2 = (am2 * (2 * f2 * x2) / (z2^(-1) * f2 + m2)) # fertilidade.
        wm2 = alm2 # natalidade.
        ym2 = bm2 + (wm2 - bm2) * ((f2 + m2) / k2) # mortalidade.
        
        # no. machos esterilizadas.
        dm2 = (m2 * (wm2 * (1 - (sm2 / m2)) - ym2) # crescimento.
               + h * m1 * (1 - ((f2 + m2) / k2)) # abandono.
               - j * m2 # adocao.
        )
        
        # no. machos esterilizadas.  
        dsm2 = (- ym2 * sm2 # mortalidade.
                + em2 * (m2 - sm2 + h * (m1 - sm1)) # novos esterilizados.
                - j * sm2 # adocao.
                + h * sm1 * (1 - ((f2 + m2) / k2)) # abandono.
        )
        
        list(c(df1, dsf1, dm1, dsm1, df2, dsf2, dm2, dsm2))
      })
    }
    state = c(state['f1'], state['sf1'], 
               state['m1'], state['sm1'],
               state['f2'], state['sf2'], 
               state['m2'], state['sm2'])
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
    output$n1 = rowSums(output[, 2:5])
    output$n2 = rowSums(output[, 6:9])
    output$n = rowSums(output[, 2:9])
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
    output <- NULL
    paras <- pars
    aban.range = c(aban.range[1], pars['h'], aban.range[2])
    adop.range = c(adop.range[1], pars['j'], adop.range[2])
    for (i1 in 1:length(aban.range)) {
      for (i2 in 1:length(adop.range)) {
        for (i3 in 1:length(ster.range)) {
          if (ster.fm == T) {
            paras[c('ef1', 'ef2', 'j', 'h')] = 
              c(ster.range[i3], ster.range[i3], 
                adop.range[i2], aban.range[i1]
                )
          } else {
            paras[c('ef1', 'j', 'h')] = 
              c(ster.range[i3], adop.range[i2], 
                aban.range[i1]
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
    names(output) = c(1,2:5, 2:5)
    output = data.frame(
    rbind(output[, 1:5], output[, c(1, 6:9)]),
    n = c(rowSums(output[, 2:5]), rowSums(output[, 6:9])),
    group = rep(1:2, each = nrow(output)),
    ster = rep(ster.range, each = length(time)),
    aban = rep(aban.range, each = length(time) * length(ster.range)),
    adop = rep(adop.range, each = length(time) * length(ster.range) * length(aban.range))
    )
    names(output)[1:5] = c('t', 'f', 'sf', 'm', 'sm')
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