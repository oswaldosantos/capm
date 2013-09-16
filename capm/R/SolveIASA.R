#' Immigration, abandonment, sterilization and adoption of companion animals
#' @description System of ordinary differential equations to simulate the effect of immigration of owned dogs, abandonment, sterilization of owned and stray dogs and adoption, on population dynamics.
#' @param pars a named \code{\link{vector}} of length 21, with point estimates of model parameters (see details).
#' @param init a named \code{\link{vector}} of length 8, with point estimates of model parameters (see details).
#' @param time time sequence for which output is wanted; the first value of times must be the initial time.
#' @param ster.range optional sequence (between 0 and 1) of the sterilization rates to be simulated.
#' @param aban.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of abandonment rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param adop.range optional \code{\link{vector}} of length 2, with range (ie, confidence interval) of adoption rates to be assessed. If given, the rates evaluated are those specified by the argument plus the point estimate given in \code{pars}.
#' @param im.range optional \code{\link{vector}} of length 2, with range of values of immigration rates to be assessed. This must be expressed as a percentage of owned animals carrying capacity.
#' @param ster.fm logical. If \code{TRUE}, ster.range is used for females and males and if \code{FALSE}, it is only used for females (for males, the point estimate given in \code{pars} is used.)
#' @param ... further arguments passed to \link[deSolve]{ode} function.
#' @details The \code{pars} argument must contain named values, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males. Then:
#' 
#'  
#' \code{b1} and \code{b2}: number of births.
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
#' \code{v}: immigration rate.
#' 
#' \code{vc}: proportion of sterilized immigrants.
#' 
#' The \code{init} argument must contain named values for the inital number of animals, using the following conventions: \code{1}: owned animals; \code{2}: stray animals; \code{f}: females; \code{m}: males; and \code{s}: sterilized. Then, number values must be given for the categories:
#' 
#' \code{f1}, \code{cf1}, \code{m1}, \code{cm1}, \code{f2}, \code{cf2}, \code{m2} and \code{cm2}.
#' 
#' If any range is specified (e.g \code{ster.range}), the ramining ranges must be specified too (\code{aban.range}, \code{adop.range} and \code{im.range}).
#' The function is a wrapper around the defaults of \link[deSolve]{ode} function, whose help page must be consulted for details. An exception is the method argument which is defined as "rk4".
#' @return \code{\link{list}} of class \code{SolveIASA}. The first element, \code{*$model}, is the model function. The second, third and fourth elements are vectors (\code{*$pars}, \code{*$init}, \code{*$time}, respectively) containing the \code{pars}, \code{init} and \code{time} arguments of the function. The fifth element \code{*$results} is a \code{\link{data.frame}} with up to as many rows as elements in time. Using the conventions for init argument (see details), the first fourth columns contain the variables: \code{f}, \code{sf}, \code{m} and \code{sm}. The fifth and sisxth columns contain the number of animals and the group respectively (\code{n} and \code{group}). Other optional arguments are:
#' 
#' \code{ster}: instance fo sterilization rate (if \code{ster.range} is specified).
#' 
#' \code{aban}: instance fo abandonment rate (if \code{aban.range} is specified).
#' 
#' \code{adop}: instance fo adoption rate (if \code{adop.range} is specified).
#' 
#' @note Logistic growth models are not intended for scenarios in which
#' population size is greater than carrying capacity and growth rate is negative.
#' @note The implemented model is part of an ongoing PhD thesis (student: Oswaldo Santos; adviser: Fernando Ferreira) to be finished on the next months.
#' @seealso \link[deSolve]{ode}.
#' @export
#' @examples 
#' # Parameters and initial conditions from estimates   
#' # obtained in examples section from svysumm function.
#' # Note that there is not estimates for all arguments
#' # and thus, some are a priori defined.
#' # For example, carrying capacities were estimated as 
#' # 10 % greater than population sizes and birth and  
#' # death rates for stray animals were estimated as 
#' # 20 % greater than those rates in owned animals.
#' # The consequences of those "guesses" can be quantified
#' # with globalsens and localsens functions.
#' pars.SolveIASA = c(
#'    b1 = 21870.897, b2 = 4374.179,
#'    df1 = 0.104, dm1 = 0.098, df2 = 0.1248, dm2 = 0.1176,
#'    sf1 = 0.069, sf2 = 0.05, sm1 = 0.028, sm2 = 0.05,
#'    k1 = 98050.49, k2 = 8055.456, h1 = 1, h2 = .5,
#'    ab = 0.054, ad = 0.1, v = 0.2, vc = 0.1)
#'    
#' init.SolveIASA = c(
#'    f1 = 33425.19, cf1 = 10864.901,
#'    m1 = 38038.96, cm1 = 6807.759,
#'    f2 = 3342.519, cf2 = 108.64901,
#'    m2 = 3803.896, cm2 = 68.07759)
#'    
#' 
#' # Solve for point estimates.
#' SolveIASA.pt <- SolveIASA(pars = pars.SolveIASA, 
#'                           init = init.SolveIASA, 
#'                           time = 0:30, method = 'rk4')
#' 
#' # Solve for parameter ranges.
#' SolveIASA.rg <- SolveIASA(pars = pars.SolveIASA, 
#'                           init = init.SolveIASA, 
#'                           time = 0:20,
#'                           ster.range = seq(0, .4, l = 20), 
#'                           aban.range = c(0, .2), 
#'                           adop.range = c(0, .2),
#'                           im.range = c(0, .1),
#'                           method = 'rk4')
#'
SolveIASA <- function(pars = NULL, init = NULL, time = NULL, ster.range = NULL, aban.range = NULL, adop.range = NULL, im.range = NULL, ster.fm = TRUE, ...) {
  if(!setequal(names(pars), c('b1', 'b2', 'df1', 'dm1', 
                              'df2', 'dm2', 'sf1', 'sf2', 
                              'sm1', 'sm2', 'k1', 'k2', 'h1',
                              'h2', 'ab', 'ad', 'v', 'vc'))) {
    stop('Values in pars must have the following names:\nb1, b2, df1, dm1, df2, dm2, sf1, sf2, sm1, sm2, k1, k2, h1, h2, ab, ad, v, vc')
  }
  if(!setequal(names(init), c('f1', 'cf1', 'm1', 'cm1',
                              'f2', 'cf2', 'm2', 'cm2'))) {
    stop('Values in init must have the following names:\nf1, cf1, m1, cm1, f2, cf2, m2, cm2')
  }
  init['n1'] <- sum(init[c('f1', 'm1')])
  init['cn1'] <- sum(init[c('cf1', 'cm1')])
  init['n2'] <- sum(init[c('f2', 'm2')])
  init['cn2'] <- sum(init[c('cf2', 'cm2')])
  init['N1'] <- sum(init[c('n1', 'cn1')])
  init['N2'] <- sum(init[c('n2', 'cn2')])
  init['N'] <- sum(init[c('N1', 'N2')])
  
  SolveIASAfu <- function(pars, init, time) {
    SolveIASA.fu <- function(time, init, pars) {
      with(as.list(c(init, pars)), {
        
        if (f1 + cf1 + m1 + cm1 <= k1) {
          omega1 <- f1 + cf1 + m1 + cm1
        } else {
          omega1 <- k1
        }
        
        if (f2 + cf2 + m2 + cm2 <= k2) {
          omega2 <- f2 + cf2 + m2 + cm2
        } else {
          omega2 <- k2
        }
        
        x1 <- (b1 * (h1 * m1 + f1)) / (2 * h1 * f1 * m1)
        wf1 <- (x1 * m1) / (m1 + f1 * h1 ^ (-1))
        bet.f1 <- wf1 - (wf1 - df1) * (omega1 / k1)
        gam.f1 <- df1
        phi <- k1 * v * (1 - vc) / 2
        phic <- k1 * v * vc / 2
        
        d.f1 <- (bet.f1 - gam.f1 - sf1 - ab) * f1 +
          (ad * f2 + phi) * (1 - (omega1 / k1))
        
        d.cf1 <- - (gam.f1 + ab) * cf1 + sf1 * f1 + 
          (ad * cf2 + phic) * (1 - (omega1 / k1))
        
        wm1 <- (x1 * f1) / (m1 + f1 * h1 ^ (-1))
        bet.m1 <- wm1 - (wm1 - dm1) * (omega1 / k1)
        gam.m1 <- dm1
        
        d.m1 <- (bet.m1 - gam.m1 - sm1 - ab) * m1 +
          (ad * m2 + phi) * (1 - (omega1 / k1))
        
        d.cm1 <- - (gam.m1 + ab) * cm1 + sm1 * m1 + 
          (ad * cm2 + phic) * (1 - (omega1 / k1))
        
        x2 <- (b2 * (h2 * m2 + f2)) / (2 * h2 * f2 * m2)
        bet.f2 <- (m2 * x2) /  (m2 + f2 * h2 ^ (-1))
        gam.f2 <- df2 + (bet.f2 - df2) * (omega2 / k2)
        
        d.f2 <- (bet.f2 - gam.f2 - sf2 - ad) * f2 +
          ab * f1 * (1 - (omega2 / k2))
        
        d.cf2 <- - (gam.f2 + ad) * cf2 + sf2 * f2 +
          ab * cf1 * (1 - (omega2 / k2))
        
        bet.m2 <- (f2 * x2) / (m2 + f2 * h2 ^ (-1))
        gam.m2 <- dm2 + (bet.m2 - dm2) * (omega2 / k2)
        
        d.m2 <- (bet.m2 - gam.m2 - sm2 - ad) * m2 +
          ab * m1 * (1 - (omega2 / k2))
        
        d.cm2 <- - (gam.m2 + ad) * cm2 + sm2 * m2 +
          ab * cm1 * (1 - (omega2 / k2))
        
        d.n1 <- d.f1 + d.m1
        d.cn1 <- d.cf1 + d.cm1
        d.n2 <- d.f2 + d.m2
        d.cn2 <- d.cf2 + d.cm2
        d.N1 <- d.n1 + d.cn1
        d.N2 <- d.n2 + d.cn2
        d.N <- d.N1 + d.N2
        
        list(c(d.f1, d.cf1, d.m1, d.cm1, d.f2, d.cf2, 
               d.m2, d.cm2, d.n1, d.cn1, d.n2, d.cn2,
               d.N1, d.N2, d.N))
      })
    }
    
    init <- c(init['f1'], init['cf1'], 
              init['m1'], init['cm1'],
              init['f2'], init['cf2'], 
              init['m2'], init['cm2'],
              init['n1'], init['cn1'],
              init['n2'], init['cn2'],
              init['N1'], init['N2'], init['N'])
    
    SolveIASA.out <- ode(times = time, 
                         func = SolveIASA.fu, 
                         y = init, 
                         parms = pars,
                         ...)
    
    return(as.data.frame(SolveIASA.out))
  } 
  if (is.null(ster.range) & is.null(aban.range) & 
        is.null(adop.range)) {
    output <- SolveIASAfu(pars = pars, init = init, time = time)
    SolveIASA <- list(
      model = SolveIASAfu,
      pars = pars,
      init = init,
      time = time,
      results = output)
    class(SolveIASA) <- 'SolveIASA'
    return(SolveIASA)
  } else {
    if(length(aban.range) != 2) {
      stop('The length of aban.range must be equal to 2.')
    }
    if(length(adop.range) != 2) {
      stop('The length of adop.range must be equal to 2.')
    }
    if(length(im.range) != 2) {
      stop('The length of im.range must be equal to 2.')
    }
    if(any(aban.range > 1 | adop.range > 1 | im.range > 1)) {
      stop('Values in aban.range, adop.range,\nim.range and ster.range must be lesser or equal to 1.')
    }
    if(any(ster.range > 1)) {
      stop('Values in ster.range must be lesser or equal to 1.')
    }
    output <- NULL
    paras <- pars
    aban.range <- c(aban.range[1], pars['ab'], aban.range[2])
    adop.range <- c(adop.range[1], pars['ad'], adop.range[2])
    for (i in 1:length(im.range)) {
      for (i1 in 1:length(aban.range)) {
        for (i2 in 1:length(adop.range)) {
          for (i3 in 1:length(ster.range)) {
            if (ster.fm) {
              paras[c('sf1', 'sm1', 'sf2', 'sm2',
                      'ad', 'ab', 'v')] <- 
                c(ster.range[i3], ster.range[i3],
                  ster.range[i3], ster.range[i3],
                  adop.range[i2], aban.range[i1],
                  im.range[i])
            } else {
              paras[c('sf1', 'sf2', 'ad', 'ab', 'v')] <- 
                c(ster.range[i3], ster.range[i3],
                  adop.range[i2], aban.range[i1], 
                  im.range[i])
            }
            output <- rbind(
              output,
              SolveIASAfu(pars = paras, 
                          init = init, 
                          time = time))
          }
        }
      }
    }
    names(output) <- c(1,2:5, 2:5)
    output <- data.frame(
      rbind(output[, 1:5], output[, c(1, 6:9)]),
      n = c(rowSums(output[, c(2, 4)]), rowSums(output[, c(6, 8)])),
      cn = c(rowSums(output[, c(3, 5)]), rowSums(output[, c(7, 9)])),
      N = c(rowSums(output[, 2:5]), rowSums(output[, 6:9])),
      group = rep(1:2, each = nrow(output)),
      ster = rep(ster.range, each = length(time)),
      adop = rep(adop.range, 
                 each = length(time) * length(ster.range)),
      aban = rep(aban.range, 
                 each = length(time) * length(ster.range) * 
                   length(adop.range)),
      im = rep(im.range, 
               each = length(time) * length(ster.range) * 
                 length(adop.range) * length(aban.range)))
    names(output)[1:5] <- c('t', 'f', 'cf', 'm', 'cm')
    SolveIASA <- list(
      model = SolveIASAfu,
      pars = pars,
      init = init,
      time = time,
      results = output)
    class(SolveIASA) <- 'SolveIASA'
    return(SolveIASA)
  }
}