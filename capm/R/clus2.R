#' Two-stage cluster sampling size and composition
#' @description Calculates sample size and composition for a two-stage cluster sampling design to estimate a total.
#' @param psu.ssu \code{\link{data.frame}} with all primary sampling units (psu). First column contains psu unique identifiers. Second column contains \code{\link{numeric}} psu sizes.
#' @param psu.x \code{\link{data.frame}}. Each row corresponds to a secondary sampling unit (ssu) surveyed in a pilot study. First column contains the psu identifiers to which the ssu belongs to. Second column contains the totals observed in the ssu and must be numeric \code{\link{numeric}}.
#' @param level the confidence level required.
#' @param error the maximum relative difference between the estimate and the unknown population value.
#' @param cost the ratio of the cost of sampling a psu to the cost of sampling a ssu.
#' @return Matrix with the sample size and composition and with estimates of variability.
#' @details It is assumed that psu from the pilot are selected with probability proportional to size (pps) and with replacement. ssu are assumed to be selected via simple random sampling.
#' 
#' psu must have the same identifiers in \code{psu.ssu} and in \code{psu.x}.
#' @references Levy P and Lemeshow S (1999). Sampling of populations: methods and applications, Second edition. John Wiley and Sons, Inc.
#' @export
#' @examples 
#' # Load data with psu identifiers and sizes.
#' data(psu.ssu)
#' 
#' # Load data from a pilot sample.
#' data(pilot)
#' 
#' # Calculate sample size and composition.
#' sample.sc = clus2(psu.ssu, pilot, level = 0.95, error = 0.1, cost = 12)

clus2 = function(psu.ssu = NULL, psu.x = NULL, level = .95, error = 0.1, cost = 12) {
  psu.ssu.x = merge(psu.ssu, psu.x, by = 1)
  tmp = nrow(psu.ssu.x)
  psu.ssu.x[, 1] = paste0(psu.ssu.x[, 1], 1:tmp)
  M = nrow(psu.ssu)
  N = sum(psu.ssu[ , 2])
  Ni = psu.ssu[ , 2]
  Nip = tapply(psu.ssu.x[, 2], psu.ssu.x[ , 1], unique)
  Nb = mean(Ni)
  nip = tapply(psu.ssu.x[, 2], psu.ssu.x[ , 1], length)
  nbp = mean(nip)
  mp = length(unique(psu.ssu.x[ , 1])) 
  np = nrow(psu.ssu.x)
  xi = tapply(psu.ssu.x[ , 3], psu.ssu.x[ , 1], sum)
  Xi = xi * Nip / nip
  vec = sum((Xi - mean(Xi)) ^ 2) / mp
  qua = (psu.ssu.x[ , 3] - mean(psu.ssu.x[ , 3])) ^ 2
  vdc = sum((Nip / (Nip - 1)) * 
              (tapply(qua, psu.ssu.x[ , 1], sum))) / sum(nip)
  dpec = sqrt(vec)
  dpdc = sqrt(vdc)
  d = (((M / (M - 1)) * vec) - (Nb * vdc)) / 
    (((M / (M - 1)) * vec) + (Nb * (Nb - 1) * vdc))
  d = if (d <= 0) {d = 1e-03} else {d = d}
  nb = ceiling(sqrt(cost * ((1 - d) / d)))
  X = sum(N / sum(nip) * tapply(psu.ssu.x[ , 3], psu.ssu.x[ , 1], sum))
  z = abs(round(qnorm((1 - level) / 2, 0, 1), 2))
  m = ceiling((z ^ 2) * sum((((N * xi) / nbp) - X) ^ 2) / 
    ((error ^ 2) * (X ^ 2) * (mp - 1)))
  if(m > M) {m = M}

  sam = matrix(c(m * nb, m, nb, vec, vdc, d), 
               ncol = 1)
  rownames(sam) = c('Sample size', 'Number of psu to be sampled', 'Number of ssu to be sampled in each psu', 'Intercluster variance', 'Intracluster variance', 'Intraclass correlation coefficient')
  colnames(sam) = 'Value'
  return(sam)
}